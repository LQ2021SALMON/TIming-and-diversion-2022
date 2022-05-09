# return timing data exploration

setwd("C:/Users/liuq/Desktop/salmon migration timing/timing_diversion_forecasting/timing_diversion_forecasting/DATAIN")
rm(list = ls(all=T))
library(tidyverse)
library(dplyr)
#return timing
sockeye.timing <- read.csv("ChilkoTimingForecastData.csv")

#overall timing

ggplot(data = sockeye.timing, mapping = aes(x = year, y = A20.day)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~ Stock) +
  theme_light() +
  labs(x = "year", y = "Timing Day")

#early stuart timing
stuart.timing <- filter(sockeye.timing, Stock == "earlystuart")

ggplot(data = stuart.timing, mapping = aes(x = year, y = A20.day)) + 
  geom_line() +
  geom_point() +
  theme_light() +
  labs(x = "year", y = "Timing Day", title = "Early Stuart return timing")
  
#early stuart timing cycle line all year

ggplot(data = stuart.timing, mapping = aes(x = Cycle, y = A20.day, group = Cycle)) + 
  geom_boxplot(mapping = aes(fill = Cycle),
               show.legend = FALSE) +
  theme_light() +
  labs(x = "Cycle line", y = "Timing Day", title = "Early Stuart return timing") +
  theme(axis.text.x = element_blank())+
  annotate("text", x=1:4, y=176, label=c("2021", "2018", "2019", "2020"))
  #scale_x_discrete(breaks = c(1, 2, 3, 4), labels= cycleyear)
  
#early stuart timing from 1977 cycle line 

ggplot(data = filter(stuart.timing, year >=1977), mapping = aes(x = Cycle, y = A20.day, group = Cycle)) + 
  geom_boxplot(mapping = aes(fill = Cycle),
               show.legend = FALSE) +
  theme_light() +
  labs(x = "Cycle line", y = "Timing Day", title = "Early Stuart return timing") +
  theme(axis.text.x = element_blank())+
  annotate("text", x=1:4, y=176, label=c("2021", "2018", "2019", "2020"))

#early stuart timing from 1992 cycle line 

ggplot(data = filter(stuart.timing, year >=1992), mapping = aes(x = Cycle, y = A20.day, group = Cycle)) + 
  geom_boxplot(mapping = aes(fill = Cycle),
               show.legend = FALSE) +
  theme_light() +
  labs(x = "Cycle line", y = "Timing Day", title = "Early Stuart return timing") +
  theme(axis.text.x = element_blank())+
  annotate("text", x=1:4, y=176, label=c("2021", "2018", "2019", "2020"))

#chilko timing
chilko.timing <- filter(sockeye.timing, Stock == "chilko")

ggplot(data = chilko.timing, mapping = aes(x = year, y = A20.day)) + 
  geom_line() +
  geom_point() +
  theme_light() +
  labs(x = "year", y = "Timing Day", title = "Chilko return timing")

#chilko timing cycle line all year

ggplot(data = chilko.timing, mapping = aes(x = Cycle, y = A20.day, group = Cycle)) + 
  geom_boxplot(
    mapping = aes(fill = Cycle),
    show.legend = FALSE) +
  theme_light() +
  labs(x = "Cycle line", y = "Timing Day", title = "Chilko return timing") +
  theme(axis.text.x = element_blank())+
  annotate("text", x=1:4, y=202, label=c("2021", "2018", "2019", "2020"))
#scale_x_discrete(breaks = c(1, 2, 3, 4), labels= cycleyear)

#chilko timing from 1977, cycle line 

ggplot(data = filter(chilko.timing, year >= 1977), mapping = aes(x = Cycle, y = A20.day, group = Cycle)) + 
  geom_boxplot(
    mapping = aes(fill = Cycle),
    show.legend = FALSE) +
  theme_light() +
  labs(x = "Cycle line", y = "Timing Day", title = "Chilko return timing") +
  theme(axis.text.x = element_blank())+
  annotate("text", x=1:4, y=202, label=c("2021", "2018", "2019", "2020"))
#scale_x_discrete(breaks = c(1, 2, 3, 4), labels= cycleyear)

#chilko timing from 1992, cycle line 

ggplot(data = filter(chilko.timing, year >= 1992), mapping = aes(x = Cycle, y = A20.day, group = Cycle)) + 
  geom_boxplot(
    mapping = aes(fill = Cycle),
    show.legend = FALSE) +
  theme_light() +
  labs(x = "Cycle line", y = "Timing Day", title = "Chilko return timing") +
  theme(axis.text.x = element_blank())+
  annotate("text", x=1:4, y=202, label=c("2021", "2018", "2019", "2020"))


#northern diversion
sockeye.ND <- read.csv("NorthernDiversion.csv")

ggplot(data = sockeye.ND, mapping = aes(x = year, y = nd)) + 
  geom_line() +
  geom_point() +
  theme_light() +
  labs(x = "year", y = "Northern diversion rate", title = "Northern diversion")

#northern diversion cycle line, all years

ggplot(data = sockeye.ND, mapping = aes(x = Cycle, y = nd, group = Cycle)) + 
  geom_boxplot(mapping = aes(fill = Cycle), show.legend = FALSE) +
  #geom_line(mapping = aes(color = Cycle)) +
  #geom_point() +
  theme_light() +
  labs(x = "Cycle line", y = "Northern diversion rate", title = "Northern diversion") +
  theme(axis.text.x = element_blank())+
  annotate("text", x=1:4, y=0, label=c("2021", "2018", "2019", "2020"))

#ND from 1977, cycle line
ggplot(data = filter(sockeye.ND, year >= 1977), mapping = aes(x = Cycle, y = nd, group = Cycle)) + 
  geom_boxplot(mapping = aes(fill = Cycle), show.legend = FALSE) +
  theme_light() +
  labs(x = "Cycle line", y = "Northern diversion rate", title = "Northern diversion") +
  theme(axis.text.x = element_blank())+
  annotate("text", x=1:4, y=0, label=c("2021", "2018", "2019", "2020"))

#ND from 1992, cycle line
ggplot(data = filter(sockeye.ND, year >= 1992), mapping = aes(x = Cycle, y = nd, group = Cycle)) + 
  geom_boxplot(mapping = aes(fill = Cycle), show.legend = FALSE) +
  #geom_line(mapping = aes(color = Cycle)) +
  #geom_point() +
  theme_light() +
  labs(x = "Cycle line", y = "Northern diversion rate", title = "Northern diversion") +
  theme(axis.text.x = element_blank())+
  annotate("text", x=1:4, y=0, label=c("2021", "2018", "2019", "2020"))
