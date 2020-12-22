### Multivariate Timeseries Modelling B

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
library(lmtest)

# Clean up workspace

rm(list = ls())

# Set working directory

getwd()
setwd("C:/Users/budde/OneDrive/UniLU MA/Semester 3/Forecasting In Economics")

# Load Data

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


# Create dataframe

data = ts(df[3:479,-1],start = c(1980,1), frequency = 12)

# plot growth

dlnData = diff(log(data),1,1)
plot(dlnData[,1], ylab = "Growth M2", main = "M2 Growth, 1980-2020") #Order comes from m2_cci_multivar_forecast_a file


# Forecasting with univariate model

modelM2Univariate = forecast::Arima(dlnData[,1], order = c(1,1,1))
forecastM2Univariate = forecast::forecast(modelM2Univariate)
plot(forecastM2Univariate)


# Forecasting multivariate Model

modelM2Exo = forecast::Arima(dlnData[,"M2"], order = c(1,1,1),xreg = dlnData[,"Value"]) #Order comes from m2_cci_multivar_forecast_a file
modelValue = forecast::Arima(dlnData[,"Value"],order = c(2,0,1)) # Order comes from m2_cci_multivar_forecast_a file
#modelExchangeRate = forecast::Arima(dlnData[,"ExchangeRateCHFEUR"],order = c(0,0,0))

#forecast exogenous factors

forecastValue = forecast::forecast(modelValue)

# Put them together

forecastM2Exo = forecast(modelM2Exo, xreg = forecastValue$mean)
plot(forecastM2Exo)
