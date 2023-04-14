#### Source Code for Project 2 ECON 144
#### Andrew Grove and Thomas Santosa
#### 304785991 and 504797813

### We prepare all the libraries needed for this project
library(tseries)
library(dynlm)
library(fpp)
library(fma)
library(seasonal)
library(car)
library(lubridate)
library(strucchange)
library(vars)
library(lmtest)
library(dlnm)
library(forecast)
library(papeR)
library(mpe)
library(tsbox)
library(lubridate)
library(ForecastComb)
library(ggplot2)
library(rugarch)

#### Part II

#### a. 
data <- read.csv("master.csv")

chicken <- ts(data[,3], start=data[1,1], freq=12)
beef <- ts(data[,4], start=data[1,1], freq=12)
t <- seq(data[1,1] + (data[1, 2] - 1)/12, data[length(chicken), 1] + (data[length(chicken), 2] - 1) / 12, length=length(chicken))
t.b <- seq(data[1,1] + (data[1, 2] - 1)/12, data[length(beef), 1] + (data[length(beef), 2] - 1) / 12, length=length(beef))

plot(chicken, xlab="Years", ylab="Dollars per Pound", main="Chicken Legs Price per Pound")
plot(beef, xlab="Years", ylab="% of Beef Production", main="Beef Production in 2012 Base Units")

Acf(chicken, lag.max=36, main="ACF of Chicken Price")
Pacf(chicken, lag.max=36, main="PACF of Chicken Price")

Acf(beef, lag.max=36, main="ACF of Beef Production")
Pacf(beef, lag.max=36, main="PACF of Beef Production")


#### b. 

# Model for Chicken

#Trend
t2 = t^2
t3 = t^3
chicken.trend = tslm(chicken ~ t + t2)
summary(chicken.trend)
plot(chicken, xlab="Years", ylab="USD", main="Chicken Legs Price per Pound")
lines(t, chicken.trend$fit, col="blue")

plot(chicken.trend$residuals, xlab="Years", ylab="USD", main="Trend Residuals")
abline(h=0,y=1, col="red")
Acf(chicken.trend$residuals, lag.max=36, main="ACF of Trend Residuals")
Pacf(chicken.trend$residuals, lag.max=36, main="PACF of Trend Residuals")

#Trend + Cyclical
m = 5
aic <- matrix(NA, m, m)
bic <- matrix(NA, m, m)

for (i in 1:m){
  for (j in 1:m) {
    model <- arima(chicken,order=c(i,0,j), xreg=cbind(t.c, t2.c))
    aic[i, j] <- model$aic
    bic[i, j] <- BIC(model)
  }
}

aic
bic

chicken.tc = arima(chicken,order=c(3,0,1), xreg=cbind(t, t2))
summary(chicken.tc)
plot(chicken, xlab="Years", ylab="USD", main="Chicken Legs Price per Pound")
lines(t, fitted(chicken.tc), col="blue")

plot(chicken - fitted(chicken.tc), xlab="Years", ylab="USD", main="Trend + Cycles Residuals")
Acf(chicken - fitted(chicken.tc), lag.max=36, main="ACF of Trend + Cycles Residuals")
Pacf(chicken - fitted(chicken.tc), lag.max=36, main="PACF of Trend + Cycles Residuals")

#Trend + Cyclical + Seasonality
m = 2
aic <- matrix(NA, m, m)
bic <- matrix(NA, m, m)

for (i in 1:m){
  for (j in 1:m) {
    model <- arima(chicken,order=c(2,0,1), xreg=cbind(t.c, t2.c), seasonal=list(order=c(i-1, 0, j-1)))
    aic[i, j] <- model$aic
    bic[i, j] <- BIC(model)
  }
}

aic
bic

chicken.tsc = arima(chicken,order=c(2,0,1), xreg=cbind(t.c, t2.c), seasonal=list(order=c(1, 0, 1)))
summary(chicken.tsc)
plot(chicken, xlab="Years", ylab="USD", main="Chicken Legs Price per Pound")
lines(t.c, fitted(chicken.tsc), col="blue")

plot(chicken - fitted(chicken.tsc), xlab="Years", ylab="USD", main="TSC Residuals")
Acf(chicken - fitted(chicken.tsc), lag.max=36, main="ACF of TSC Residuals")
Pacf(chicken - fitted(chicken.tsc), lag.max=36, main="ACF of TSC Residuals")

# Model for Beef

#Trend
t2.b <- t.b^2

beef.trend <- tslm(beef~t.b + t2.b)
summary(beef.trend)
plot(beef, xlab="Years", ylab="USD", main="Beef Price per Pound")
lines(t.b, beef.trend$fit, col="blue")

plot(beef.trend$residuals, xlab="Years", ylab="USD", main="Trend Residuals")
abline(h=0,y=1, col="red")
Acf(beef.trend$residuals, lag.max=36, main="ACF of Trend Residuals")
Pacf(beef.trend$residuals, lag.max=36, main="PACF of Trend Residuals")

#Trend + Cyclical
n = 5
aic <- matrix(NA, n, n)
bic <- matrix(NA, n, n)

for (i in 1:n){
  for (j in 1:n) {
    model <- arima(beef,order=c(i,0,j))
    aic[i, j] <- model$aic
    bic[i, j] <- BIC(model)
  }
}

aic
bic

beef.tc = arima(beef,order=c(5,0,4))
summary(beef.tc)
plot(beef, xlab="Years", ylab="USD", main="Chicken Legs Price per Pound")
lines(t.b, fitted(beef.tc), col="blue")

plot(beef - fitted(beef.tc), xlab="Years", ylab="USD", main="Trend + Cycles Residuals")
Acf(chicken - fitted(beef.tc), lag.max=36, main="ACF of Trend + Cycles Residuals")
Pacf(beef - fitted(beef.tc), lag.max=36, main="PACF of Trend + Cycles Residuals")


#Trend + Cyclical + Seasonality
model <- arima(beef,order=c(5,0,4), seasonal=list(order=c(1, 0, 0)))

AIC(model)
BIC(model)

beef.tsc = arima(beef,order=c(2,0,1), seasonal=list(order=c(1, 0, 0)))
summary(beef.tsc)
plot(beef, xlab="Years", ylab="USD", main="Chicken Legs Price per Pound")
lines(t.b, fitted(beef.tsc), col="blue")

plot(beef - fitted(beef.tsc), xlab="Years", ylab="USD", main="TSC Residuals")
Acf(beef - fitted(beef.tsc), lag.max=36, main="ACF of TSC Residuals")
Pacf(beef - fitted(beef.tsc), lag.max=36, main="ACF of TSC Residuals")


#### c.

##Chicken Residuals vs. Fitted Values
plot(fitted(chicken.tsc),chicken.tsc$res, pch=20)
abline(h=0,lwd=2, col="red")

##Beef Residuals
plot(fitted(beef.tsc), beef.tsc$res, pch=20)
abline(h=0, lwd=2, col="red")


#### d. 

##Chicken ACF and PACF Residual plots
acf(chicken.tsc$res)
pacf(chicken.tsc$res)

##Beef ACF and PACF Residual plots
acf(beef.tsc$res)
pacf(beef.tsc$res)


#### e. 

## Recursive CUSUM plots of Chicken and Beef
plot(efp(chicken.tsc$res~1, type = "Rec-CUSUM"))
plot(efp(beef.tsc$res~1, type = "Rec-CUSUM"))


#### f. 

## Recursive Residuals plots of Chicken
plot(recresid(chicken.tsc$res~1), pch=20, main="Recursive Residual Plot of the Chicken Model")
abline(h=0, lwd=2, col="red")

## Recursive Residuals plots of Beef
plot(recresid(beef.tsc$res~1), pch=20, main="Recursive Residual Plot of the Beef Model")
abline(h=0, lwd=2, col="red")



#### g. 


#### h. 

## Beef Forecast
beef.forecast <- plot(forecast(beef.tsc, h=12))
accuracy(forecast(beef.tsc, h=12))


#### i. 

##Preparing the data
autoarima.c <- auto.arima(chicken)
autoarima.b <- auto.arima(beef)

ets.c <- ets(chicken)
ets.b <- ets(beef)

hw.c <- HoltWinters(chicken)
hw.b <- HoltWinters(beef)

##AutoArima Forecast
x1 <- forecast(autoarima.c, h=12)
x2 <- forecast(autoarima.b, h=12)

plot(x1, main="AutoArima Forecast for Chicken Data")
plot(x2, main="AutoArima Forecast for Beef Data")

accuracy(x1)
accuracy(x2)

##Holt-Winters Forecast
y1 <- forecast(hw.c, h=12)
y2 <- forecast(hw.b, h=12)

plot(y1, main="Holt-Winters Forecast for Chicken Data")
plot(y2, main="Holt-Winters Forecast for Beef Data")

accuracy(y1)
accuracy(y2)

##ETS Forecast
z1 <- forecast(ets.c, h=12)
z2 <- forecast(ets.b, h=12)

plot(z1, main="ETS Forecast for Chicken Data")
plot(z2, main="ETS Forecast for Beef Data")

accuracy(z1)
accuracy(z2)


#### j.

## Forecast Combination of Chicken Data
#fcomb.c <- (x1[["mean"]] + y1[["mean"]] + z1[["mean"]] + chicken.forecast[["mean"]])/4
#autoplot(chicken) + autolayer(fcomb.c) + ggtitle("Combination Forecast for Chicken Data")

## Forecast Combination of Beef Data
fcomb.b <- (x2[["mean"]] + y2[["mean"]] + z2[["mean"]] + beef.forecast[["mean"]])/4
autoplot(beef) + autolayer(fcomb.b) + ggtitle("Combination Forecast for Beef Data")


#### k. 

## VAR model
comb.data <- ts.union(chicken, beef)

comb.var <- VAR(comb.data, p=4)

summary(comb.var)


#### l. 

## IRF plot of the VAR model
plot(irf(comb.var))


#### m. 

## Granger Causality tests
grangertest(beef~chicken, order=4)
grangertest(chicken~beef, order=4)


#### n. 

fcast.var <- forecast(comb.var, h=12)

plot(fcast.var)


#### o. 



