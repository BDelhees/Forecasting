getwd()
setwd("C:/Users/budde/OneDrive/UniLU MA/Semester 3/Forecasting In Economics")
rm(list=ls())
library(knitr)
knitr::opts_knit$set(root.dir = "C:/Users/budde/OneDrive/UniLU MA/Semester 3/Forecasting In Economics")
knitr::opts_chunk$set(message = FALSE)

rm(list = ls())

## Get Packages

library(tseries)  
library(xts)      # handling of time-series data (is it needed at all? check!!!)
library(forecast) 
library(tsbox)    
library(seasonal)
library(mFilter)

## Read Data

rawData <- read.csv("M2_monthly.csv", header = TRUE, sep = ",")
data <- as.xts(x = rawData[,-1], order.by = as.Date(rawData$DATE, format="%Y-%m-%d"))

plot.xts(data,  main = "M2 Money Stock")
addLegend("topleft", on=1, 
          legend.names = "M2", 
          lty=c(1, 1),
          col="black")

modelM2Trend <- lm(log(data) ~ c(1:length(data)))
plot.xts(cbind(log(data),modelM2Trend$fitted.values), main = "M2 and linear Trend")

plot.xts(as.xts(modelM2Trend$residuals),  main = "M2: deviations from linear trend")

dlnData = diff.xts(as.xts(log(data)),1,1,na.pad=FALSE,log=TRUE)
plot.xts(dlnData,  main = "M2: quarterly growth")

#plot(hpfilter(as.xts(log(data)),freq = 1600))

plot.xts(dlnData,  main = "Log-change in M2")

acfDlnGDP = acf(dlnData,na.action = na.contiguous,  main = "Autocorrelation of M2 growth")

pacfDlnGDP = pacf(dlnData,na.action = na.contiguous,  main = "Partial autocorrelation of M2 growth")

arima(x = dlnData["/2020-01-01"], order = c(1L, 0L, 0L))
arima(x = dlnData, order = c(1L, 0L, 0L), init = c(0.3, 1e-04))

forecast::auto.arima(dlnData,ic="bic")

nArOrder <- 5
nMaOrder <- 5
matrixAIC <- matrix(,nArOrder+1,nMaOrder+1)
matrixAICown <- matrix(,nArOrder+1,nMaOrder+1)
matrixBIC <- matrix(,nArOrder+1,nMaOrder+1)
for(iArOrder in 0:nArOrder){ 
  for(iMaOrder in 0:nMaOrder){
    temp = arima(x = dlnData["/2020-01-01"], order = c(iArOrder,0,iMaOrder)) #Change date here for other time horizon
    #¥matrixAIC[iArOrder,iMaOrder] = temp$aic
    matrixAIC[iArOrder+1,iMaOrder+1] = -2*temp$loglik + (iArOrder+iMaOrder)*2
    matrixBIC[iArOrder+1,iMaOrder+1] = -2*temp$loglik + (iArOrder+iMaOrder)*log(length(data))
  }
}
which(matrixBIC==min(matrixBIC), arr.ind = TRUE)
which(matrixAIC==min(matrixAIC), arr.ind = TRUE)

modelM2Growth = forecast::Arima(dlnData[,1], order = c(1,0,0), init = c(0.3, 1e-04))
forecastM2Growth = forecast::forecast(modelM2Growth)
plot(forecastM2Growth)

modelM2Level = forecast::auto.arima(log(data),xreg = c(1:length(data)),ic="bic")

modelM2Level = forecast::auto.arima(log(data),xreg = c(1:length(data)),ic="bic")
trendForecast <- c((length(data)+1):(length(data)+10))
forecastM2Level = forecast::forecast(modelM2Level,xreg = trendForecast)

plot(forecastM2Level)

