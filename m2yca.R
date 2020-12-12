### Multivariate Timeseries Modelling A - M2 and Yield Curve

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

m2 <- read.csv("M2a.csv", header = TRUE, sep = ",")
yc <- read.csv("yc.csv", header = TRUE, sep = ",")

# Take a loot at the data


dim(yc)
dim(m2)


glimpse(yc)
glimpse(m2)


# Transform ...$DATE to date


class(yc$DATE)
yc$DATE <- as.character(yc$DATE)
yc$DATE <- as.Date(yc$DATE, format="%Y-%m-%d")



class(m2$DATE)
m2$DATE <- as.character(m2$DATE)
m2$DATE <- as.Date(m2$DATE, format="%Y-%m-%d")



df <- inner_join(m2, yc)
view(df)

## Transform to xts format

dataAll <- as.xts(x = df[,-1], order.by = as.Date(df$DATE, format="%Y-%m-%d"))
data <- dataAll["/2020-01-01"]
dlnData = diff.xts(as.xts(log(data)),1,1,na.pad=FALSE,log=TRUE) #calculating growth rate

# Correlation between M2 and Yield Curve

ccf(as.ts(dlnData$M2),as.ts(dlnData$GS10))

# No correlation found


# ARIMA Model, yc not significant but not a static model (else ARIMA (0,0,0))

forecast::auto.arima(dlnData$M2,xreg = dlnData$GS10, ic="bic")

# Estimate ARIMA model anyways, didnt work

forecast::auto.arima(dlnData$M2,xreg = cbind(dlnData$GS10,lag(dlnData$GS10)), ic="bic")


# Dynamic Model for GS10

forecast::auto.arima(dlnData$GS10,ic="bic")

forecast::auto.arima(dlnData$M2,ic="bic")

# Not seasonally adjusted Value

quarterlyDummies <- forecast::seasonaldummy(ts_ts(data$M2)) 
forecast::auto.arima(log(data$M2),xreg = cbind(log(data$GS10),quarterlyDummies,c(1:length(data$M2))),ic="bic")





