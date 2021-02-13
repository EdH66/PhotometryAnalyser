#Photometry analyser script. Use to prepare photometry data exported from spike for plotting.
#Lining up photometry in Spik2 first allows equal down sampling for all channels e.g Suggest export at 10 Hz or slower.
#Export from spike as spreadsheet text. Open file and remove channel number from column names. e.g "1 Delta(701)" becomes "Delta"
#Constructs data frame for scatter plots in origin
#Calculates exact length of each sleep episode from sleep scored hypno channel. Hypno script must ahve been run in Spike2.
#This script will truncate data longer than the portion of photometry recording.
#If photometry starts later than the start of the SPike2 file - export form the start of photometry. 
#Input data is all channels exported from Spike2 e.g Delta power, EMG, Hypnogram of sleep state e.g 111222333
#To run script highlight all text and click run.
#Use the pop up window to select imput file
#Use pop up window to save the first output for scatter plots file inc ".csv"
#Use the second pop up window to save the second output for episode length comparison file inc ".csv"
#For episode length photometry data is best aggregated as variance.
#data is summarised in histograms in the first plot window
#Data is summarised using ggplot2 in the second plot window 
#EC HARDING, May 2020

library(tidyverse)
library(plyr)
library(dplyr)
library (tidyr)
library(readtext)
library(car)
library(ggplot2)
library(ggpubr)
library(PerformanceAnalytics)


photo_dat = read.table("NOS1Photometry_1Hz.txt", sep="\t", header = TRUE)
photo_dat2 <- photo_dat %>% mutate_at(vars(Hypno), funs(round(., 1)))
photo_dat3 <- photo_dat2 %>% group_by(Fluo) 
photo_dat4 <-photo_dat3[photo_dat3$Hypno != "0", ]
photo_dat5 <-photo_dat4[photo_dat4$Hypno != "4", ]
Photo_dat6 <-photo_dat5[photo_dat5$Hypno != "5", ]
Fluo_length <- length(Photo_dat6$Fluo[!is.na(Photo_dat6$Fluo)])
Sleep_stage <- top_n(Photo_dat6, Fluo_length, Fluo) %>% dplyr::rename(State = Hypno)

#uses rle(run length encoding) to count consequntive lengths of the same number.#Uses new length count to assign new group(g) to data frame.
Sleep_stage_group <- Sleep_stage %>% transform(g=with(rle(Sleep_stage$State),{ rep(seq_along(lengths), lengths)}))%>%                                   
  group_by(g)

Sleep_stage_dist <- count_(Sleep_stage_group, vars = "State", wt = NULL)
Sleep_stage_dist <- transform(Sleep_stage_dist, n = (n*5)/60)

## now sort Fluorescence by grouping variable g and aggregating other other variables (delta, emg e.t.c) into episode binds. 

# aggregate by variance
ncol_stg = ncol(Sleep_stage_group)-1
Sleep_episode_aggvar <- aggregate(Sleep_stage_group[, 2:ncol_stg], list(Sleep_stage_group$g), var)
Sleep_episode_aggvar <- subset(Sleep_episode_aggvar, select=-c(Group.1))
Sleep_episode_aggvar <- subset(Sleep_episode_aggvar, select=-c(State))

#Bind into new dataframe
Sleep_vardistbyepisode <- cbind(Sleep_episode_aggvar, Sleep_stage_dist) %>% dplyr::rename(Ep_length = n)

## writing to csv ##
write.csv(Sleep_stage_group, file = file.choose(TRUE))       
write.csv(Sleep_vardistbyepisode, file = file.choose(TRUE))  

## preparing data for plotting ##

WAKE <- filter(Sleep_stage, State == 1)
NREM <- filter(Sleep_stage, State == 2)
REM <- filter(Sleep_stage, State == 3)

WAKE_Fluo <- WAKE$Fluo
NREM_Fluo <- NREM$Fluo
REM_Fluo <- REM$Fluo

par(mfrow = c(2,2))
hist(WAKE_Fluo, main = "DeltaF-WAKE", xlab="F amplitude", ylab="Frequency", xlim=c(0.3,1), ylim = c(0,460),
     col="#0000FF")
hist(NREM_Fluo,main = "DeltaF-NREM", xlab="F amplitude", ylab="Frequency", xlim=c(0.3,1), ylim = c(0,460),
     col="#00FF66")
hist(REM_Fluo,main = "DeltaF-REM", xlab="F amplitude", ylab="Frequency", xlim=c(0.3,1), ylim = c(0,460),
     col="#33FFFF")

## ploting graphs with ggplot2##

a <- ggplot(Sleep_stage, aes(x=EMG, y=Fluo, color= as.factor(State))) +
  geom_point(shape = ".") + scale_x_continuous(trans='log10') + xlab("EMG") + 
  ylab("Raw F") +
  scale_colour_manual(values = c("Blue", "green", "cyan"))

b <- ggplot(Sleep_vardistbyepisode, aes(x=Ep_length, y=Fluo, color= as.factor(State))) +
  geom_point() +
  scale_y_log10() + 
  scale_x_continuous(trans='log10') +
  xlab("Episode length") +
  ylab("F variance") +
  scale_colour_manual(values = c("Blue", "green", "cyan"))

c <- ggplot(Sleep_stage, aes(x=Delta, y=Fluo, color= as.factor(State))) +
  geom_point(shape = ".") + scale_x_continuous(trans='log10') + xlab("Delta Power") + 
  ylab("Raw F") +
  scale_colour_manual(values = c("Blue", "green", "cyan"))

d <- ggplot(Sleep_vardistbyepisode, aes(x=Delta, y=Fluo, color= as.factor(State))) +
  geom_point() + scale_x_continuous(trans='log10') + xlab("Delta var per ep") + 
  ylab("F variance") +
  scale_colour_manual(values = c("Blue", "green", "cyan"))


ggarrange(a, b, c, d,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)




##end graph plotting##



## K means test. Not yet useful
#set.seed(20)
#SleepclusterPrep <- na.omit(Sleep_stage) # listwise deletion of missing
#Sleepcluster <- kmeans(SleepclusterPrep[, 2:3], 3, nstart = 20)
#fit <- kmeans(SleepclusterPrep, 3)
#library(cluster)
#clusplot(SleepclusterPrep, fit$cluster, color=TRUE, shade=TRUE,
#         labels=2, lines=0)
#plotCluster(SleepclusterPrep, fit$cluster)
