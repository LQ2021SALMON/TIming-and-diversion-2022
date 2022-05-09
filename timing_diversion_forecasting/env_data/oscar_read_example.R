

#a simple example showing the import of oscar current vectors u and v.
print(getwd())
setwd("C:/Users/liuq/Desktop/salmon migration timing/timing_diversion_forecasting/env_data")
rm(list = ls(all=T))
library(tidyverse)
# data:grid means ---------------------------------------------------------

#these data are grid means (polygon groups 0:3) of monthly means 
data.gridmean <- readRDS("currentvelocity_oscar_gridmean_2022-05-03.rds")

#data.gridmean.long <- rbind(data.gridmean[[1]]$data.long, data.gridmean[[2]]$data.long)
data.gridmean.wide <- merge(data.gridmean[[1]]$data.wide, data.gridmean[[2]]$data.wide)

str(data.gridmean.wide)



# data:monthly means ------------------------------------------------------

#these data are monthly means but still at full grid resolution (1/4deg lat by 1/4 deg lon)

data.currents.periodicmean <- readRDS("currentvelocity_oscar_periodicmean_2022-05-03.rds")

str(data.currents.periodicmean)

#gives a sense of the array dimensions (lat, lon, year:month)
attributes(data.currents.periodicmean$u)

period <- dimnames(data.currents.periodicmean$u)[[3]]
period

lat <- dimnames(data.currents.periodicmean$u)[[1]]
lon <- dimnames(data.currents.periodicmean$u)[[2]]

data.currents.periodicmean$u[1,1,1]
data.currents.periodicmean$u[lat==40,lon==180,period=="1993-01"]
data.currents.periodicmean$u[lat==40, lon==180,]

#pink data diversion
pink.diversion <- read.csv("../fraser_pink_forecast_diversion/data/fraser_pink_diversion.csv")

#pink data timing
pink.timing <- read.csv("../fraser_pink_forecast_timing/data/fraser_pink_timing.csv")

