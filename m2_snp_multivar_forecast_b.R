### Multivariate Timeseries Modelling B - M2 & S&P500

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

# Clean up workspace

rm(list = ls())

# Set working directory

getwd()
setwd("C:/Users/budde/OneDrive/UniLU MA/Semester 3/Forecasting In Economics")

# Load Data

m2 <- read.csv("M2a.csv", header = TRUE, sep = ",")
snp <- read.csv("snp.csv", header = TRUE, sep = ",")

glimpse(m2)
glimpse(snp)

#clean snp & m2

snp$Open = NULL
snp$High = NULL
snp$Low = NULL
snp$Adj.Close = NULL
snp$Volume = NULL

snp <- rename(snp, "DATE" = "Date")

snp$DATE <- as.Date(snp$DATE, format="%Y-%m-%d")

class(m2$DATE)
m2$DATE <- as.character(m2$DATE)
m2$DATE <- as.Date(m2$DATE, format="%Y-%m-%d")

df <- inner_join(m2, snp)



# Create dataframe

data = ts(df[3:479,-1],start = c(1980,1), frequency = 12)

# plot growth

dlnData = diff(log(data),1,1)
plot(dlnData[,1])

# Forecasting with univariate model

modelM2Univariate = forecast::Arima(dlnData[,1], order = c(1,1,1))
forecastM2Univariate = forecast::forecast(modelM2Univariate)
plot(forecastM2Univariate)

# Forecasting multivariate Model

modelM2Exo = forecast::Arima(dlnData[,"M2"], order = c(1,1,1),xreg = dlnData[,"Close"]) # Order comes from m2_snp_multivar_forecast_a file
modelValue = forecast::Arima(dlnData[,"Close"],order = c(0,0,0)) # Order comes from m2_snp_multivar_forecast_a file
#modelExchangeRate = forecast::Arima(dlnData[,"ExchangeRateCHFEUR"],order = c(0,0,0))

#forecast exogenous factors

forecastValue = forecast::forecast(modelValue)

# Put them together

forecastM2Exo = forecast(modelM2Exo, xreg = forecastValue$mean)
plot(forecastM2Exo)
