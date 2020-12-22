rm(list=ls())
setwd(getwd())

library(xts) 
library(forecast) 

#importing data set.
rawData <- read.csv("M2weekly_corona.csv", header = TRUE, sep = ",")
data <- as.xts(x = rawData[,-1], order.by = as.Date(rawData$DATE, "%Y-%m-%d"))

#plotting M2
plot.xts(data,  main = "M2 Money Stock")
addLegend("topleft", on=1, 
          legend.names = "M2", 
          lty=c(1, 1),
          col="black")
#Plotting linear trend
modelM2Trend <- lm(log(data) ~ c(1:length(data)))
plot.xts(cbind(log(data),modelM2Trend$fitted.values), main = "M2 and linear Trend")

#Plotting deviations
plot.xts(as.xts(modelM2Trend$residuals),  main = "M2: deviations from linear trend")

#plotting growth
dlnData = diff.xts(as.xts(log(data)),1,1,na.pad=FALSE,log=TRUE)
plot.xts(dlnData,  main = "M2: weekly growth")

#checking for autocorrelation & partial autocorrelation
acfDlnGDP = acf(dlnData,na.action = na.contiguous,  main = "Autocorrelation of M2 growth")

pacfDlnGDP = pacf(dlnData,na.action = na.contiguous,  main = "Partial autocorrelation of M2 growth")

#using auto.arima with method bic to check what model we get.
forecast::auto.arima(dlnData,ic="bic")

#plotting growth of M2 forecast
modelM2Growth = forecast::auto.arima(dlnData)
forecastM2Growth = forecast::forecast(modelM2Growth)
plot(forecastM2Growth)

#Plotting again forecast, but this time just the data by itself for easier readability of the value of M2
modelM2Level = forecast::auto.arima(data,ic="bic")
forecastM2Level = forecast::forecast(modelM2Level)
plot(forecastM2Level)
