library(tidyverse)
library(dplyr)
library(ggplot2)
photo_dat <- read.csv(file.choose(),header=T)

Photodat2 <- photo_dat %>% select(TIME, GROUP.No)

plot(Photodat2)




a <- ggplot(photo_dat, aes(x=TIME(), y=TRAN.No, color= as.factor(GROUP.No))) +
  geom_point() +
  scale_y_log10() + 
  scale_x_continuous(trans='log10') +
  xlab("Episode length") +
  ylab("Variance of F variance") +
  scale_colour_manual(values = c("Blue", "green", "cyan"))

ggarrange(a,
          labels = c("A"),
          ncol = 1, nrow = 1)
