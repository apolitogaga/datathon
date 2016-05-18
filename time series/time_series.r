rm(list=ls())
data<-read.table("temps.csv",head=TRUE,sep="\t")
summary(dataset)
plot(data$temp, type = "l", bty = "n", ylab = "Temperature in °C")
data$temp <- ts(data = data$temp, frequency = 285)
?ts
plot(stl(data$temp, s.window = "periodic"))


#### begin
data<-read.table("monthly-total-number-of-pigs-sla.csv",head=TRUE,sep=",")
colnames(data)<-c("time","y")
plot(data$y, type="l")
### check for outlier
library(tsoutliers)
ts<-ts(data=data$y,frequency=1)

outlier <- tsoutliers::tso(ts,types = c("AO","LS","TC"),maxit.iloop=10)
plot(outlier)

fit<-arima(data$y)
plot(ts)


