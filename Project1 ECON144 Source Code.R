# We first run all the libraries that are needed 
# Thesee libraries are needed so that we can run all the functions e.g. anything with ...()
library(lattice)
library(foreign)
library(MASS)
library(car)
require(stats)
require(stats4)
library(KernSmooth)
library(fastICA)
library(cluster)
library(leaps)
library(mgcv)
library(rpart)
library(pan)
library(mgcv)
library(DAAG)
library(corrplot) # We'll use corrplot later on in this example too.
library(visreg) # This library will allow us to show multivariate graphs.
library(rgl)
library(knitr)
library(scatterplot3d)
library(mctest)
library(forecast)

#------------------- PART I ---------------------------
# We are reading the data from the file pork.csv
data <- read.csv(file = 'pork.csv') 

# We convert the data into a more accessible form for R and dividing it into three different columns 
# according to the data.
colnames(data) <- c("Year", "Month", "Pork") 

# Here we are only selecting the data from year 2000 onwards only for the sake of this project
# note that it is from row 337 in excel because we added the title rows on the first row. 
data <- data[337:nrow(data),] 

# We are attaching the data so that R can use it properly
attach(data) 

#------------------- PART II ---------------------------

#------------ SECTION 1 ---------------

#-------- a. ------------

#Here we are converting the data into a time-series friendly data for R
#Also, we are plotting our time-series friendly data
data.ts <- ts(data=data[,3], start=c(2000,1), end=c(2019, 11), freq=12) 
plot(data.ts, xlab="Years", ylab="% of Pork Production", main="Pork Production in 2012 Base Units")

#Here we are creating the same data but only from 2000 to 2005 to see the seasonality better, e.g. we are zooming in
data.ts_month <- ts(data=data[,3], start=c(2000,1), end=c(2004, 12), freq=12)
plot(data.ts_month, xlab="Years", ylab="% of Pork Production", main="Pork Production in 2012 Base Units")

#------- c. ------------

#Both ACF() and PACF() are functions that shows the ACF and PACF plot of the data
Acf(data.ts, main="ACF of Pork Production")
Pacf(data.ts, main="PACF of Pork Production")

#------ d. -------------

#Here we are converting years into a more accessible form for R
# Also, we are making a linear regression in the form of the data
years = Year + Month/12
y <- lm(data[,3]~years)

# Summary of the data and the plot of the data. 
summary(y)
plot(data.ts, xlab="Years", ylab="% of Pork Production", main="Linear Model of Pork Production")
abline(y, lty=1, lwd=1, col="red2") # plot of the linear regression model


time2 <- years^2

# We added two breaks in order to fit the data better. 
tbreak1 <- 2010 # A break in 2010
tbreak2 <- 2015 # A break in 2015
tb1 <- ts(pmax(0, years - tbreak1), start = 2000)
tb2 <- ts(pmax(0, years - tbreak2), start = 2000)

# Quadratic regression of the data
y_quad <- lm(data[,3]~time2+years+tb1+tb2)
summary(y_quad)
plot(data.ts, xlab="Years", ylab="% of Pork Production", main="Quadratic Model of Pork Production") #this is the plot of our data
legend(2000,135, "Quadratic model","red2",cex=1,bty="y")
lines(years,y_quad$fit,col="red2",lwd=1) # plot of the quadratic regression model

#Periodic regression of the data
sin.t<-sin(2*pi*years)
cos.t<-cos(2*pi*years)
# the function tslm() automatically turn the periodic regression into both a linear model and a time-series
y1 <- tslm(data.ts~years+I(sin(2*pi*years))+I(cos(2*pi*years)) + 
             I(ts(pmax(0, years - tbreak1), start = 2000)) + I(ts(pmax(0, years - tbreak2), start = 2000)))
plot(data.ts, xlab="Years", ylab="% of Pork Production", main="Periodic Model of Pork Production")
legend(2000,135, "Periodic model","red2",cex=1,bty="y")
lines(years,y1$fit,col="red2",lwd=1) #plot of the periodic regression model
summary(y1)

#plots of both the linear regression fit and the period fit into our data
plot(data.ts, xlab="Years", ylab="% of Pork Production", main="Linear and Periodic Models of Pork Production")
lines(years, y$fit, col="red2", lwd=1)
lines(years,y1$fit,col="blue",lwd=1)
legend(2000,135, c("Linear", "Periodic"), fill =c("red2", "blue"),cex=1,bty="y")

#------- e. ------------

#plots of the residuals vs. fitted values of the data for the linear regression fit
plot(y$fit,y$res, pch=20,col="skyblue4",lwd=4,xlab="Fitted", ylab="Residuals", main="Residuals vs. Fitted plot of the Linear Fit")
abline(h=0,lwd=2,col = "red3")#just a straight line y=0

#plots of the residuals vs. fitted values of the data for the quadratic regression fit
plot(y_quad$fit,y_quad$res, pch=20,col="skyblue4",lwd=4,xlab="Fitted", ylab="Residuals",, main="Residuals vs. Fitted plot of the Quadratic Fit")
abline(h=0,lwd=2,col = "red3")#just a straight line y=0

#plots of the residuals vs. fitted values of the data for the periodic regression fit
plot(y1$fit,y1$res, pch=20,col="skyblue4",lwd=4,xlab="Fitted", ylab="Residuals", , main="Residuals vs. Fitted plot of the Periodic Fit")
abline(h=0,lwd=2,col = "red3")#just a straight line y=0

#------- f. -------------

#histogram of the residuals of the linear regression fit
truehist(y$res,15,col="skyblue3",xlab="Residuals",ylab="Fraction",main="Histogram of Residuals")

#histogram of the residuals of the quadratic regression fit
truehist(y_quad$res,10,col="skyblue3",xlab="Residuals",ylab="Fraction",main="Histogram of Residuals")

#histogram of the residuals of the periodic regression fit
truehist(y1$res,10,col="skyblue3",xlab="Residuals",ylab="Fraction",main="Histogram of Residuals")

#------ g. ------------


#These are the codes to show the summary of each regression fit

summary(y)

summary(y_quad)

summary(y1)

#------- h. -----------

#Both AIC() and BIC() are functions to show the AIC and BIC of the model.

AIC(y,y_quad,y1) #AIC of linear, quadratic, and periodic regression fit
BIC(y,y_quad,y1) #BIC of linear, quadratic, and periodic regression fit

#------- i. ----------

# Forecasting the next two years of the periodic + linear model. Note that we need to create a new dataframe and feed it into
# the forecast function so that it knows on what time periods it should forecast on

#We are creating a new dataframe to include into the forecast() function in the form of vectors of the year
new <- data.frame(years = seq(2020, 2022, by=(1/12)))
#here we are plotting our forecast of the periodic regression fit using the function forecast()
plot(forecast(y1, newdata=new),xlab="Years", ylab="% of Pork Production", main="2-Year Forecast from Periodic Model")

#------------ SECTION 2 ---------------

#-------- a. ------------

# Creation of seasonal dummy array for seasonality regression
dummies <- seasonaldummy(data.ts)
y <- tslm(data.ts~dummies+0)
summary(y)

#-------- b. ------------

# Plot of the seasonal factors (regression coefficients associated with seasonal dummies)
plot(y$coefficients, type='l',ylab='Seasonal Factors', xlab="Month",lwd=2, main="Plot of Seasonal Factors")

#-------- c. ------------

# Regression on the data using trend components and seasonality components. The added complexity of the I() transformations
# is to make it so that we can predict on this model
y1 <- tslm(data.ts~years+ + I(ts(pmax(0, years - tbreak1), start = 2000)) + I(ts(pmax(0, years - tbreak2), start = 2000)) + season)
plot(data.ts, xlab="Years", ylab="% of Pork Production", main="Trend + Seasonal Model of Pork Production")
lines(years,y1$fit,col="red2",lwd=1)

plot(y1$fit,y1$res, pch=20,col="skyblue4",lwd=4,xlab="Fitted", ylab="Residuals", main="Residuals vs. Fitted Values")
abline(h=0,lwd=2,col = "red3")

#-------- d. ------------

# summary of the full model
summary(y1)

#-------- e. ------------

# forecasting the next two years of the full model. Note that we need to create a new dataframe and feed it into
# the forecast function so that it knows on what time periods it should forecast on

new <- data.frame(years = seq(2020, 2022, by=(1/12)))
plot(forecast(y1, newdata=new), xlab="Years", ylab="% of Pork Production", main="Forecasts from Full Model of Pork Production")
