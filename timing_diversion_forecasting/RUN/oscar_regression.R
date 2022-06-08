setwd("C:/Users/liuq/Desktop/Timing-Diversion-2022/Timing-and-Diversion-2022/timing_diversion_forecasting/DATAIN")
rm(list = ls(all=T))
library(tidyverse)
# data:grid means ---------------------------------------------------------
#these data are grid means (polygon groups 0:3) of monthly means 
data.gridmean <- readRDS("currentvelocity_oscar_gridmean_2022-05-03.rds")
#data.gridmean.long <- rbind(data.gridmean[[1]]$data.long, data.gridmean[[2]]$data.long)
data.gridmean.wide <- merge(data.gridmean[[1]]$data.wide, data.gridmean[[2]]$data.wide)

str(data.gridmean.wide)

##data for current u and v and sockeye timing/nd
oscar.data <- filter(data.gridmean.wide, group != 0, month <= 5, year <= 2021)
write.csv(oscar.data, file = "oscar.data.csv")

oscar.data.area1 <- filter(data.gridmean.wide, group == 1, month <= 5, year <= 2021)

sockeye.data <- read.csv("timing_nd.csv")

estuart.timing <- sockeye.data$estuart.day

for (i in seq_along())

