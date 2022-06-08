

#a simple example showing the import of oscar current vectors u and v.

setwd("C:/Users/liuq/Desktop/Timing-Diversion-2022/Timing-and-Diversion-2022/timing_diversion_forecasting/DATAIN")
rm(list = ls(all=T))
library(tidyverse)
# data:grid means ---------------------------------------------------------

#these data are grid means (polygon groups 0:3) of monthly means 
data.gridmean <- readRDS("currentvelocity_oscar_gridmean_2022-05-03.rds")

#data.gridmean.long <- rbind(data.gridmean[[1]]$data.long, data.gridmean[[2]]$data.long)
data.gridmean.wide <- merge(data.gridmean[[1]]$data.wide, data.gridmean[[2]]$data.wide)

str(data.gridmean.wide)

gridmean.wide.area1_Jan <- filter(data.gridmean.wide, group == 1, month== 1, year <= 2021)
#write.csv(gridmean.wide.area1_Jan, file = "gridmean.wide.A1Jan.csv")
gridmean.wide.area2_Jan <- filter(data.gridmean.wide, group == 2, month== 1, year <= 2021)
#write.csv(gridmean.wide.area2_Jan, file = "gridmean.wide.A2Jan.csv")
gridmean.wide.area3_Jan <- filter(data.gridmean.wide, group == 3, month== 1, year <= 2021)
#write.csv(gridmean.wide.area3_Jan, file = "gridmean.wide.A3Jan.csv")
gridmean.wide.area0_Jan <- filter(data.gridmean.wide, group == 0, month== 1, year <= 2021)
#write.csv(gridmean.wide.area0_Jan, file = "gridmean.wide.A0Jan.csv")

# gridmean.wide.area1_Apr <- filter(data.gridmean.wide, group == 1, month== 4)
# gridmean.wide.area1_May <- filter(data.gridmean.wide, group == 1, month== 5)
# gridmean.wide.area1_Jun <- filter(data.gridmean.wide, group == 1, month== 6)
# gridmean.wide.area1_Jul <- filter(data.gridmean.wide, group == 1, month== 7)
# gridmean.wide.area1_Aug <- filter(data.gridmean.wide, group == 1, month== 8)
# gridmean.wide.area1_Sep <- filter(data.gridmean.wide, group == 1, month== 9)
# gridmean.wide.area1_Oct <- filter(data.gridmean.wide, group == 1, month== 10)
# gridmean.wide.area1_Nov <- filter(data.gridmean.wide, group == 1, month== 11)
# gridmean.wide.area1_Dec <- filter(data.gridmean.wide, group == 1, month== 12)

# timing/day for earlystuart and chilko 1993-2021
sockeye.timing <- read.csv("ChilkoTimingForecastData.csv")


early.stuart.df <- filter(sockeye.timing, Stock == "earlystuart", year >= 1993)
early.stuart.timing <- select(early.stuart.df, A20.day)
                       
                       
chilko.df <- filter(sockeye.timing, Stock == "chilko", year >= 1993)
chilko.timing <- select(chilko.df, A20.day)



#relationship between timing and current velocity in January of return year using 3 areas

df.test <- read.csv("timing_velocity_test.csv")

hist(df.test$estuart.day)
hist(df.test$chilko.day)
hist(df.test$nd)
# for early stuart
plot(estuart.day ~ gridmean.A1Jan.u, data = df.test)
plot(estuart.day ~ gridmean.A1Jan.v, data = df.test)
plot(estuart.day ~ gridmean.A2Jan.u, data = df.test)
plot(estuart.day ~ gridmean.A2Jan.v, data = df.test)
plot(estuart.day ~ gridmean.A3Jan.u, data = df.test)
plot(estuart.day ~ gridmean.A3Jan.v, data = df.test)
plot(estuart.day ~ gridmean.A0Jan.u, data = df.test)
plot(estuart.day ~ gridmean.A0Jan.v, data = df.test)

ES.A1Jan.u.reg <- lm(estuart.day ~ gridmean.A1Jan.u, df.test)
ES.A1Jan.v.reg <- lm(estuart.day ~ gridmean.A1Jan.v, df.test)
summary(ES.A1Jan.u.reg)
summary(ES.A1Jan.v.reg)
ES.A2Jan.u.reg <- lm(estuart.day ~ gridmean.A2Jan.u, df.test)
ES.A2Jan.v.reg <- lm(estuart.day ~ gridmean.A2Jan.v, df.test)
summary(ES.A2Jan.u.reg)
summary(ES.A2Jan.v.reg)
ES.A3Jan.u.reg <- lm(estuart.day ~ gridmean.A3Jan.u, df.test)
ES.A3Jan.v.reg <- lm(estuart.day ~ gridmean.A3Jan.v, df.test)
summary(ES.A3Jan.u.reg)
summary(ES.A3Jan.v.reg)
ES.A0Jan.u.reg <- lm(estuart.day ~ gridmean.A0Jan.u, df.test)
ES.A0Jan.v.reg <- lm(estuart.day ~ gridmean.A0Jan.v, df.test)
summary(ES.A0Jan.u.reg)
summary(ES.A0Jan.v.reg)

#for chilko
plot(chilko.day ~ gridmean.A1Jan.u, data = df.test)
plot(chilko.day ~ gridmean.A1Jan.v, data = df.test)
plot(chilko.day ~ gridmean.A2Jan.u, data = df.test)
plot(chilko.day ~ gridmean.A2Jan.v, data = df.test)
plot(chilko.day ~ gridmean.A3Jan.u, data = df.test)
plot(chilko.day ~ gridmean.A3Jan.v, data = df.test)
plot(chilko.day ~ gridmean.A0Jan.u, data = df.test)
plot(chilko.day ~ gridmean.A0Jan.v, data = df.test)

CK.A1Jan.u.reg <- lm(chilko.day ~ gridmean.A1Jan.u, df.test)
CK.A1Jan.v.reg <- lm(chilko.day ~ gridmean.A1Jan.v, df.test)
summary(CK.A1Jan.u.reg)
summary(CK.A1Jan.v.reg)
CK.A2Jan.u.reg <- lm(chilko.day ~ gridmean.A2Jan.u, df.test)
CK.A2Jan.v.reg <- lm(chilko.day ~ gridmean.A2Jan.v, df.test)
summary(CK.A2Jan.u.reg)
summary(CK.A2Jan.v.reg)
CK.A3Jan.u.reg <- lm(chilko.day ~ gridmean.A3Jan.u, df.test)
CK.A3Jan.v.reg <- lm(chilko.day ~ gridmean.A3Jan.v, df.test)
summary(CK.A3Jan.u.reg)
summary(CK.A3Jan.v.reg)
CK.A0Jan.u.reg <- lm(chilko.day ~ gridmean.A0Jan.u, df.test)
CK.A0Jan.v.reg <- lm(chilko.day ~ gridmean.A0Jan.v, df.test)
summary(CK.A0Jan.u.reg)
summary(CK.A0Jan.v.reg)

#for ND
plot(nd ~ gridmean.A1Jan.u, data = df.test)
plot(nd ~ gridmean.A1Jan.v, data = df.test)
plot(nd ~ gridmean.A2Jan.u, data = df.test)
plot(nd ~ gridmean.A2Jan.v, data = df.test)
plot(nd ~ gridmean.A3Jan.u, data = df.test)
plot(nd ~ gridmean.A3Jan.v, data = df.test)
plot(nd ~ gridmean.A0Jan.u, data = df.test)
plot(nd ~ gridmean.A0Jan.v, data = df.test)

ND.A1Jan.u.reg <- lm(nd ~ gridmean.A1Jan.u, df.test)
ND.A1Jan.v.reg <- lm(nd ~ gridmean.A1Jan.v, df.test)
summary(ND.A1Jan.u.reg)
summary(ND.A1Jan.v.reg)
ND.A2Jan.u.reg <- lm(nd ~ gridmean.A2Jan.u, df.test)
ND.A2Jan.v.reg <- lm(nd ~ gridmean.A2Jan.v, df.test)
summary(ND.A2Jan.u.reg)
summary(ND.A2Jan.v.reg)
ND.A3Jan.u.reg <- lm(nd ~ gridmean.A3Jan.u, df.test)
ND.A3Jan.v.reg <- lm(nd ~ gridmean.A3Jan.v, df.test)
summary(ND.A3Jan.u.reg)
summary(ND.A3Jan.v.reg)
ND.A0Jan.u.reg <- lm(nd ~ gridmean.A0Jan.u, df.test)
ND.A0Jan.v.reg <- lm(nd ~ gridmean.A0Jan.v, df.test)
summary(ND.A0Jan.u.reg)
summary(ND.A0Jan.v.reg)

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


