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

#photo_dat = read.table("NOS1Photometry_10Hz.txt", sep="\t", header = TRUE)
photo_dat <- read.csv(file.choose(),header=T)

#Select groups to downsample by variance and assign new sample group. 

Photo_ave_select <- photo_dat
Photo_ave_select_no <- nrow(Photo_ave_select)
Photo_ave_select_matchno <- Photo_ave_select_no / 10
Photometry_group <- as.data.frame(rep(1:Photo_ave_select_matchno, each=10))
Photometry_group_no <-nrow(Photometry_group)
Photo_Fill_no <- Photo_ave_select_no - Photometry_group_no
Photo_m <- matrix(NA, ncol = 1, nrow = Photo_Fill_no)
Photo_m <- data.frame(Photo_m)
Photo_fill <-rbind.fill(Photometry_group, Photo_m)
Photo_fill <- subset(Photo_fill, select=-c(Photo_m))
names(Photo_fill)[1] <- "Downdsampleg"
Photo_ave_select$Downsampleg <- Photo_fill$Downdsampleg

#Downsample by variance 
ncol_stg = ncol(Photo_ave_select)-1
Photo_transitions_var_agg <- aggregate(Photo_ave_select[, 2:ncol_stg], list(Photo_ave_select$Downsampleg), var)

## writing to csv ##
write.csv(Photo_transitions_var_agg, file = file.choose(TRUE))   
