### Uncertainty in M2 Forecasts

## Load packages

library(tseries)  
library(xts)      
library(forecast) 
library(tsbox) 



## Set working directory

getwd()
setwd("C:/Users/budde/OneDrive/UniLU MA/Semester 3/Forecasting In Economics")


## Clear Workspace

rm(list = ls())


## Load data

m2 <- read.csv("M2a.csv", header = TRUE, sep = ",")
yc <- read.csv("yc.csv", header = TRUE, sep = ",")
snp <- read.csv("snp.csv", header = TRUE, sep = ",")


snp$Open = NULL
snp$High = NULL
snp$Low = NULL
snp$Adj.Close = NULL
snp$Volume = NULL

names(snp)[1] <- "DATE"

df <- inner_join(m2,yc)

df <- inner_join(df, snp)


# Transform to ts (xts causes trouble)

view(df)
datam2 = ts(df[1:480,-1],start = c(1980,11), frequency = 12)
view(datam2)


# Create Lags

dlnData = diff(log(datam2),1,1)
plot(dlnData)
plot(dlnData[,1])

d4lnData = diff(log(datam2),4,1) #Not sure what the number 4 means here exactly
plot(d4lnData)
plot(d4lnData[,1])


### Professional forecasts to compare our forecasts to
# Source: https://www.forecasts.org/m2.htm


rawDataSPF <- read.csv("m2-forecast-data.csv", header = TRUE, sep = ",")
view(rawDataSPF)
rawDataSPF$Note = NULL
names(rawDataSPF)[2] <- "M2F"
rawDataSPF <- rawDataSPF[-70,]



forecastSPF = ts(rawDataSPF[,-1],start = c(2015,9), frequency = 12)
view(forecastSPF)


### Rolling Forecasts

tEvaluationPeriods <- 19
tPeriods = length(dlnData[,"M2"])
forecastHorizon = 4 #four steps ahead, if "1" then it would be 1 step ahead
nModels = 7 # we compare the univariate, the multivariate model (each q/q and y/y) and 5 professional forecasters. We also add a naive, no change forecast and the mean of all these forecasts
forecastRolling <- matrix(,tEvaluationPeriods,nModels)
for (tauEvaluationPeriod in 1:tEvaluationPeriods){
  
  lastPeriod <- tPeriods-tEvaluationPeriods+tauEvaluationPeriod-forecastHorizon # starts with 2014 Q4, loops through 2019 Q2 (forecast for y/y 2020 Q2) 
  
  # Univariate model, q/q
  modelM2Univariate_temp = forecast::Arima(dlnData[1:lastPeriod,"M2"], order = c(1,0,0))
  forecastM2Univariate_temp = forecast::forecast(modelM2Univariate_temp, h = forecastHorizon)
  forecastRolling[tauEvaluationPeriod,1] <- sum(forecastM2Univariate_temp$mean[1:forecastHorizon]) # y/y forecast is sum of q/q (with logs)
  
  # Univariate model, y/y
  modelM2Univariate_temp = forecast::Arima(d4lnData[1:(lastPeriod-3),"M2"], order = c(1,0,0))
  forecastM2Univariate_temp = forecast::forecast(modelM2Univariate_temp, h = forecastHorizon)
  forecastRolling[tauEvaluationPeriod,2] <- forecastM2Univariate_temp$mean[forecastHorizon]
  
  
  # Multivariate model
  modelM2Exo_temp = forecast::Arima(dlnData[1:lastPeriod,"M2"], order = c(0,0,0),xreg = cbind(dlnData[1:lastPeriod,"Close"],dlnData[1:lastPeriod,"GS10"]))
  modelsnp_temp = forecast::Arima(dlnData[1:lastPeriod,"Close"],order = c(1,0,0))
  modelyc_temp = forecast::Arima(dlnData[1:lastPeriod,"GS10"],order = c(0,0,0))
  
  
  forecastsnp_temp = forecast::forecast(modelsnp_temp, h = forecastHorizon)
  forecastyc_temp = forecast::forecast(modelyc_temp, h = forecastHorizon)
  forecastM2Exo_temp = forecast(modelM2Exo_temp, xreg = cbind(forecastsnp_temp$mean,forecastyc_temp$mean), h = forecastHorizon)
  
  forecastRolling[tauEvaluationPeriod,3] <- sum(forecastM2Exo_temp$mean[1:forecastHorizon])
  
  # Multivariate model, y/y
  modelM2Exo_temp = forecast::Arima(d4lnData[1:(lastPeriod-3),"M2"], order = c(0,0,0),xreg = cbind(d4lnData[1:(lastPeriod-3),"Close"],d4lnData[1:(lastPeriod-3),"GS10"]))
  modelsnp_temp = forecast::Arima(d4lnData[1:(lastPeriod-3),"Close"],order = c(1,0,0))
  modelyc_temp = forecast::Arima(d4lnData[1:(lastPeriod-3),"GS10"],order = c(0,0,0))
  
  forecastsnp_temp = forecast::forecast(modelsnp_temp, h = forecastHorizon)
  forecastyc_temp = forecast::forecast(modelyc_temp, h = forecastHorizon)
  forecastM2Exo_temp = forecast(modelM2Exo_temp, xreg = cbind(forecastsnp_temp$mean,forecastyc_temp$mean), h = forecastHorizon)
  
  forecastRolling[tauEvaluationPeriod,4] <- forecastM2Exo_temp$mean[forecastHorizon]
  
  # Multivariate model with forecaster  information, replace Euro Area GDP forecast with sPF forecast 
  #estimate swiss GPD model with euro area y/y rates)
  modelM2Exo_temp = forecast::Arima(d4lnData[1:(lastPeriod-3),"M2"], order = c(0,0,0),xreg = cbind(d4lnData[1:(lastPeriod-3),"Close"],d4lnData[1:(lastPeriod-3),"GS10"])) 
  
  # forecaster 1
  forecastM2Exo_temp = forecast(modelM2Exo_temp, xreg = cbind(rbind(0,0,0,forecastSPF[tauEvaluationPeriod,1]/100),forecastyc_temp$mean), h =   forecastHorizon)
  forecastRolling[tauEvaluationPeriod,5] <- forecastM2Exo_temp$mean[forecastHorizon]
  # 
  # # forecaster 2
  # forecastM2Exo_temp = forecast(modelM2Exo_temp, xreg = cbind(rbind(0,0,0,forecastSPF[tauEvaluationPeriod,2]/100),forecastyc_temp$mean), h =   forecastHorizon)
  # forecastRolling[tauEvaluationPeriod,6] <- forecastM2Exo_temp$mean[forecastHorizon]
  # 
  # # forecaster 3
  # forecastM2Exo_temp = forecast(modelM2Exo_temp, xreg = cbind(rbind(0,0,0,forecastSPF[tauEvaluationPeriod,3]/100),forecastyc_temp$mean), h =   forecastHorizon)
  # forecastRolling[tauEvaluationPeriod,7] <- forecastM2Exo_temp$mean[forecastHorizon]
  # 
  # # forecaster 4
  # forecastM2Exo_temp = forecast(modelM2Exo_temp, xreg = cbind(rbind(0,0,0,forecastSPF[tauEvaluationPeriod,4]/100),forecastyc_temp$mean), h =   forecastHorizon)
  # forecastRolling[tauEvaluationPeriod,8] <- forecastM2Exo_temp$mean[forecastHorizon]
  # 
  # # forecaster 5
  # forecastM2Exo_temp = forecast(modelM2Exo_temp, xreg = cbind(rbind(0,0,0,forecastSPF[tauEvaluationPeriod,5]/100),forecastyc_temp$mean), h =   forecastHorizon)
  # forecastRolling[tauEvaluationPeriod,9] <- forecastM2Exo_temp$mean[forecastHorizon]
  # 
  # "naive", no change forecast
  forecastRolling[tauEvaluationPeriod,10] <- d4lnData[(lastPeriod-3),"M2"]
  
  # average
  forecastRolling[tauEvaluationPeriod,11] <- mean(forecastRolling[tauEvaluationPeriod,1:9])
}


### Rolling Forecast error calculation

actualData <- d4lnData[(length(d4lnData[,"M2"])-tEvaluationPeriods+1):length(d4lnData[,"M2"]),"M2"]
actualDataMatrix = kronecker(matrix(1,1,11),actualData)
forecastErrors = actualDataMatrix - forecastRolling



### Calculate Loss of each forecast of each evaluation period


quadraticLoss = forecastErrors^2 # this is point wise multipliation (check!)
absoluteLoss = abs(forecastErrors)


### Look at mean

colMeans(quadraticLoss[1:18,])*100
colMeans(absoluteLoss[1:18,]) #look for lowest number, this is the best model


### Look at worst & best model with Diebold-Mariano-West regression (excluding the last observartion)

regressionDMW <- lm((quadraticLoss[1:18,9]-quadraticLoss[1:18,3]) ~ 1)
summary(regressionDMW)



### Trim by only including 5 best perfroming models

forecastTrimmesMean <- rowMeans(forecastRolling[,c(1,2,3,4,7)])
forecastErrorTrimmedMean <- actualData - forecastTrimmesMean
quadraticLossTrimmedMean <- mean((forecastErrorTrimmedMean[1:18])^2)
absoluteLossTrimmedMean <- mean(abs(forecastErrorTrimmedMean[1:18]))
