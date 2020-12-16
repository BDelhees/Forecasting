rm(list=ls())
library(knitr)
knitr::opts_knit$set(root.dir = "/Users/a.kotatis/Desktop/Studium/Master/1. Semester/Forecasting in Economics")
knitr::opts_chunk$set(message = FALSE)
setwd("/Users/a.kotatis/Desktop/Studium/Master/1. Semester/Forecasting in Economics")

rm(list = ls())
library(tidyverse)
library(tseries)  
library(xts)      # handling of time-series data (is it needed at all? check!!!)
library(forecast) 
library(tsbox)    
library(seasonal)
library(mFilter)

rawData <- read.csv("M2M_2008-2020.csv", header = TRUE, sep = ",")
rawData <- rawData[1:146,]
rawData$DATE <- as.Date(rawData$DATE)
rawData$qdate <- as.yearqtr(rawData$DATE)

rawData <- rawData %>%
  group_by(qdate) %>%
  summarise_all(mean)
rawData$DATE <- NULL
#rawData <- rawData[1:146,] #forM22008-2020 without corona dates
rawData$DATE <-  as.Date(rawData$DATE, format="%Y-%m-%d")
data <- as.xts(x = rawData[,-1], order.by = as.Date(rawData$DATE, "%Y Q%q"))



plot.xts(data,  main = "M2 Money Stock")
addLegend("topleft", on=1, 
          legend.names = "M2", 
          lty=c(1, 1),
          col="black")

modelM2Trend <- lm(log(data) ~ c(1:length(data)))
plot.xts(cbind(log(data),modelM2Trend$fitted.values), main = "M2 and linear Trend")

plot.xts(as.xts(modelM2Trend$residuals),  main = "M2: deviations from linear trend")

#colnames(data)[1] <- "Moneystock"
#quarterlyDummies <- forecast::seasonaldummy(ts_ts(data$Moneystock)) 
#modelM2NaSeason <- lm(log(data$SwissGdpNa) ~ quarterlyDummies)

dlnData = diff.xts(as.xts(log(data)),1,1,na.pad=FALSE,log=TRUE)
plot.xts(dlnData,  main = "M2: weekly growth")

#plot.xts(diff.xts(data$SwissGdpNa,4,1,na.pad=FALSE,log=TRUE),  main = "Swiss GDP: y/y growth rate")
#plot(hpfilter(as.xts(log(data)),freq = 1600))



acfDlnGDP = acf(dlnData,na.action = na.contiguous,  main = "Autocorrelation of M2 growth")

pacfDlnGDP = pacf(dlnData,na.action = na.contiguous,  main = "Partial autocorrelation of M2 growth")

arima(x = dlnData, order = c(1L, 0L, 1L))
arima(x = dlnData, order = c(1L, 0L, 1L), init = c(0.3, 1e-04))

forecast::auto.arima(dlnData,ic="bic")

nArOrder <- 5
nMaOrder <- 5
matrixAIC <- matrix(,nArOrder+1,nMaOrder+1)
matrixAICown <- matrix(,nArOrder+1,nMaOrder+1)
matrixBIC <- matrix(,nArOrder+1,nMaOrder+1)
for(iArOrder in 0:nArOrder){ 
  for(iMaOrder in 0:nMaOrder){
    temp = arima(x = dlnData, order = c(iArOrder,0,iMaOrder))
    #¥matrixAIC[iArOrder,iMaOrder] = temp$aic
    matrixAIC[iArOrder+1,iMaOrder+1] = -2*temp$loglik + (iArOrder+iMaOrder)*2
    matrixBIC[iArOrder+1,iMaOrder+1] = -2*temp$loglik + (iArOrder+iMaOrder)*log(length(data))
  }
}
which(matrixBIC==min(matrixBIC), arr.ind = TRUE)
which(matrixAIC==min(matrixAIC), arr.ind = TRUE)

modelM2Growth = forecast::auto.arima(dlnData[,1], order = c(0,0,1), init = c(0.3, 1e-04))
forecastM2Growth = forecast::forecast(modelM2Growth)
plot(forecastM2Growth)

#modelM2Level = forecast::auto.arima(data,ic="bic")

modelM2Level = forecast::auto.arima(data,ic="bic")
trendForecast <- c((length(data)+1):(length(data)+10))
forecastM2Level = forecast::forecast(modelM2Level, xreg=trendForecast)

plot(forecastM2Level)



