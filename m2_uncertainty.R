### Uncertainty in M2 forecasts

## Load Packages

library(fanplot)
library(tseries)  
library(xts)      
library(forecast) 
library(tsbox)
library(tidyverse)


# Set working directory

getwd()
setwd("C:/Users/budde/OneDrive/UniLU MA/Semester 3/Forecasting In Economics")


## Load data

m2 <- read.csv("M2a.csv", header = TRUE, sep = ",")
yc <- read.csv("yc.csv", header = TRUE, sep = ",")
snp <- read.csv("snp.csv", header = TRUE, sep = ",")
cci <- read.csv("cci_us.csv", header = TRUE, sep = ",")


cci <- as.data.frame(cci[-(1:163),-(1:5)])
cci <- (cci[,-3])
cci <- rename(cci, "DATE" = "TIME")
class(cci$DATE)
cci$DATE <- as.character(cci$DATE)
cci$DATE <- paste0(cci$DATE,"-01")

cci$DATE <- as.Date(cci$DATE, format="%Y-%m-%d")

snp$Open = NULL
snp$High = NULL
snp$Low = NULL
snp$Adj.Close = NULL
snp$Volume = NULL

names(snp)[1] <- "DATE"

df <- inner_join(m2,yc)

df <- inner_join(df, snp)

df <- inner_join(df, cci)


view(df)
datam2 = ts(df[1:480,-1],start = c(1980,11), frequency = 12)
view(datam2)




#Example Code to cross-check

#rawData <- read.csv("MacroDataQuarterly.csv", header = TRUE, sep = ";")
#view(rawData)
#data = ts(rawData[1:85,-1],start = c(1999,1), frequency = 4)
#view(data)

#dlnData = diff(log(data),1,1)
#plot(dlnData[,1])





## Create lagged differences and visualize them

dlnDatam2 = diff(log(datam2),1,1)
plot(dlnDatam2)
plot(dlnDatam2[,1])

plot(log(datam2), main = "Monthly Growth of M2, Yield Curve, S&P500, CCI", ylab = "")
## Forecasting uncertainty in univariate model - M2


modelM2Univariate = forecast::Arima(dlnDatam2[,1], order = c(1,1,1))
forecastM2Univariate = forecast::forecast(modelM2Univariate)
plot(forecastM2Univariate)

## Foresting uncertainty in Multivariate model - M2, S&p500, Yield Curve

# Estimate all models first

modelM2Exo = forecast::Arima(dlnDatam2[,"M2"], order = c(1,1,1),xreg = cbind(dlnDatam2[,"GS10"],dlnDatam2[,"Close"]))
modelyc = forecast::Arima(dlnDatam2[,"GS10"],order = c(0,0,1))
modelsnp = forecast::Arima(dlnDatam2[,"Close"],order = c(0,0,0)) 

# Forecast exogenous variables

forecastyc = forecast::forecast(modelyc)
plot(forecastyc)

forecastsnp = forecast::forecast(modelsnp)
plot(forecastsnp)


# Stick exogenous variables into M2 model

forecastM2Exo = forecast(modelM2Exo, xreg = cbind(forecastsnp$mean,forecastyc$mean)) #Warning message occured?
plot(forecastM2Exo)


## Create simulations and switch to Matrix

tHorizons = 12
nDraws = 100
forecastsnpMatrix <- rbind(kronecker(matrix(1,1,nDraws),data.matrix(dlnDatam2[,"Close"])),matrix(,tHorizons,nDraws))
forecastycMatrix <- rbind(kronecker(matrix(1,1,nDraws),data.matrix(dlnDatam2[,"GS10"])),matrix(,tHorizons,nDraws))
forecastM2Matrix <- rbind(kronecker(matrix(1,1,nDraws),data.matrix(dlnDatam2[,"M2"])),matrix(,tHorizons,nDraws))

T <- length(dlnDatam2[,"GS10"])
for (tauHorizon in 1:tHorizons) {
  
  for (iDraw in 1:nDraws) {
    # mean forecast for exgenous values here
    # the following command does not re-estimate the model, just replaces the data, see ?Arima
    modelsnp_temp <- Arima(forecastsnpMatrix[1:T+tauHorizon-1,iDraw],model = modelsnp) 
    modelyc_temp <- Arima(forecastycMatrix[1:T+tauHorizon-1,iDraw],model = modelyc)
    
    meanForecastsnp <- forecast::forecast(modelsnp_temp,h=1)$mean
    meanForecastycMatrix <- forecast::forecast(modelyc_temp,h=1)$mean
    
    forecastsnpMatrix[T+tauHorizon,iDraw] <- meanForecastsnp + rnorm(1, mean=0, sd=modelsnp$sigma2^0.5)
    forecastycMatrix[T+tauHorizon,iDraw] <- meanForecastycMatrix + rnorm(1, mean=0, sd=modelyc$sigma2^0.5)
    
    modelM2Exo_temp = forecast::Arima(forecastM2Matrix[1:T+tauHorizon-1,iDraw],model = modelM2Exo,xreg =     cbind(forecastsnpMatrix[1:T+tauHorizon-1,iDraw],forecastycMatrix[1:T+tauHorizon-1,iDraw]))
    
    meanForecastM2Matrix <- forecast::forecast(modelM2Exo_temp,h=1,xreg = cbind(forecastsnpMatrix[T+tauHorizon,iDraw],forecastycMatrix[T+tauHorizon,iDraw]))$mean
    forecastM2Matrix[T+tauHorizon,iDraw] <- meanForecastM2Matrix + rnorm(1, mean=0, sd=modelM2Exo$sigma2^0.5)
  }
  
} 


## Plot matrix with realized past data


plot(as.ts(data.matrix(dlnDatam2[,"M2"])), type = "l", lwd = 2, las = 1, ylab = "",xlim = c(1,T+tHorizons), main = "Uncertainty Simulation")
fan(data = t(forecastM2Matrix[(T+1):(T+tHorizons),]), type = "interval", start = 479)



## Bootstrap residuals to bypass normality assumption. Randomly draw from residual vector

forecastsnpMatrixBootstrap <- rbind(kronecker(matrix(1,1,nDraws),data.matrix(dlnDatam2[,"Close"])),matrix(,tHorizons,nDraws))
forecastycMatrixBootstrap <- rbind(kronecker(matrix(1,1,nDraws),data.matrix(dlnDatam2[,"GS10"])),matrix(,tHorizons,nDraws))
forecastM2MatrixBootstrap <- rbind(kronecker(matrix(1,1,nDraws),data.matrix(dlnDatam2[,"M2"])),matrix(,tHorizons,nDraws))

T <- length(dlnDatam2[,"GS10"])
for (tauHorizon in 1:tHorizons) {
  
  for (iDraw in 1:nDraws) {
    # mean forecast for exgenous values here
    # the following command does not re-estimate the model, just replaces the data, see ?Arima
    modelsnp_temp <- Arima(forecastsnpMatrix[1:T+tauHorizon-1,iDraw],model = modelsnp) 
    modelyc_temp <- Arima(forecastycMatrix[1:T+tauHorizon-1,iDraw],model = modelsnp)
    
    meanForecastsnp <- forecast::forecast(modelsnp_temp,h=1)$mean
    meanForecastycMatrix <- forecast::forecast(modelyc_temp,h=1)$mean
    
    forecastsnpMatrixBootstrap[T+tauHorizon,iDraw] <- meanForecastsnp + modelyc$residuals[ceiling(runif(1,0,length(modelM2Exo$residuals)))]
    forecastycMatrixBootstrap[T+tauHorizon,iDraw] <- meanForecastycMatrix + modelsnp$residuals[ceiling(runif(1,0,length(modelM2Exo$residuals)))]
    
    modelM2Exo_temp = forecast::Arima(forecastM2Matrix[1:T+tauHorizon-1,iDraw],model = modelM2Exo,xreg =     cbind(forecastsnpMatrixBootstrap[1:T+tauHorizon-1,iDraw],forecastycMatrixBootstrap[1:T+tauHorizon-1,iDraw]))
    
    meanForecastM2MatrixBootstrap <- forecast::forecast(modelM2Exo_temp,h=1,xreg = cbind(forecastsnpMatrix[T+tauHorizon,iDraw],forecastycMatrixBootstrap[T+tauHorizon,iDraw]))$mean
    forecastM2MatrixBootstrap[T+tauHorizon,iDraw] <- meanForecastM2MatrixBootstrap + modelM2Exo$residuals[ceiling(runif(1,0,length(modelM2Exo$residuals)))]
  }
  
} 


## Plot the results from the bootstrapped residuals

plot(as.ts(data.matrix(dlnDatam2[,"M2"])), type = "l", lwd = 2, las = 1, ylab = "",xlim = c(1,T+tHorizons), main = "Bootstrapped Uncertainty Simulation")
fan(data = t(forecastM2MatrixBootstrap[(T+1):(T+tHorizons),]), type = "interval", start = 479)
