### Multivariate Timeseries Modelling A

## Load Packages

library(tseries)  
library(xts)     
library(forecast) 
library(tsbox)    
library(seasonal)
library(mFilter)
library(dplyr)
library(tidyverse)
library(forcats)

##Clean up workspace

rm(list = ls())

## set working directory

getwd()
setwd("C:/Users/budde/OneDrive/UniLU MA/Semester 3/Forecasting In Economics")

## Load Data

m2 <- read.csv("M2_monthly.csv", header = TRUE, sep = ",")
cci <- read.csv("cci_us.csv", header = TRUE, sep = ",")

#clean cci

cci <- as.data.frame(cci[-(1:163),-(1:5)])
cci <- (cci[,-3])
cci <- rename(cci, "DATE" = "TIME")
class(cci$DATE)
cci$DATE <- as.character(cci$DATE)
cci$DATE <- paste0(cci$DATE,"-01")

cci$DATE <- as.Date(cci$DATE, format="%Y-%m-%d")

class(m2$DATE)
m2$DATE <- as.character(m2$DATE)
m2$DATE <- as.Date(m2$DATE, format="%Y-%m-%d")

df <- inner_join(m2, cci)
df <- df[,-4]

## Transform to xts format

dataAll <- as.xts(x = df[,-1], order.by = as.Date(df$DATE, format="%Y-%m-%d"))
data <- dataAll["/2020-01-01"]
dlnData = diff.xts(as.xts(log(data)),1,1,na.pad=FALSE,log=TRUE) #calculating growth rate

# Correlation between M2 and CCI (consumer confidence index)

ccf(as.ts(dlnData$M2),as.ts(dlnData$Value))

# ARIMA Model, cci not significant but not a static model (else ARIMA (0,0,0))

forecast::auto.arima(dlnData$M2,xreg = dlnData$Value, ic="bic")

# Estimate ARMA model anyways, didnt work

forecast::auto.arima(dlnData$M2,xreg = cbind(dlnData$Value,lag(dlnData$Value)), ic="bic")

# Dynamic Model for Value

forecast::auto.arima(dlnData$Value,ic="bic")

forecast::auto.arima(dlnData$M2,ic="bic")

# Not seasonally adjusted Value

quarterlyDummies <- forecast::seasonaldummy(ts_ts(data$M2)) 
forecast::auto.arima(log(data$M2),xreg = cbind(log(data$Value),quarterlyDummies,c(1:length(data$M2))),ic="bic")




