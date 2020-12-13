### Multivariate Timeseries Modelling A - SNP

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
snp <- read.csv("snp.csv", header = TRUE, sep = ",")

#clean snp


dim(snp)
view(snp)

glimpse(snp)

snp$Open = NULL
snp$High = NULL
snp$Low = NULL
snp$Adj.Close = NULL
snp$Volume = NULL

#cci <- as.data.frame(cci[-(1:163),-(1:5)])
#cci <- (cci[,-3])
#cci <- rename(cci, "DATE" = "TIME")

names(snp)[1] <- "DATE"
class(snp$DATE)
snp$DATE <- as.character(snp$DATE)
snp$DATE <- as.Date(snp$DATE, format="%Y-%m-%d")



glimpse(m2)

class(m2$DATE)
m2$DATE <- as.character(m2$DATE)
m2$DATE <- as.Date(m2$DATE, format="%Y-%m-%d")



df <- inner_join(m2, snp)
#df <- df[,-4]

## Transform to xts format

dataAll <- as.xts(x = df[,-1], order.by = as.Date(df$DATE, format="%Y-%m-%d"))
data <- dataAll["/2020-01-01"]
dlnData = diff.xts(as.xts(log(data)),1,1,na.pad=FALSE,log=TRUE) #calculating growth rate

# Correlation between M2 and SnP500

ccf(as.ts(dlnData$M2),as.ts(dlnData$Close))

# No correlation found


# ARIMA Model, snp not significant but not a static model (else ARIMA (0,0,0))


forecast::auto.arima(dlnData$M2,xreg = dlnData$Close, ic="bic")

# Estimate ARIMA model anyways, didnt work

forecast::auto.arima(dlnData$M2,xreg = cbind(dlnData$Close,lag(dlnData$Close)), ic="bic")


# Dynamic Model for Value

forecast::auto.arima(dlnData$Close,ic="bic")

forecast::auto.arima(dlnData$M2,ic="bic")

# Not seasonally adjusted Value

quarterlyDummies <- forecast::seasonaldummy(ts_ts(data$M2)) 
forecast::auto.arima(log(data$M2),xreg = cbind(log(data$Close),quarterlyDummies,c(1:length(data$M2))),ic="bic")




