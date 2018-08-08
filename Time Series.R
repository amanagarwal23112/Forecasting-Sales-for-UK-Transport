install.packages("rJava")
library(rJava)
library(xlsx)
setwd("C:/Users/DELL/Documents")
uk<- read.xlsx("UK Outward Passengers Movement.xls", sheetName = "New")
View(uk)

#Missing Value Treatment
is.na(uk)
new_uk <- na.omit(uk)
View(new_uk)

#Converting to time series
myuk<-ts(new_uk$Total, start = c(1996,1), end = c(2005,4), frequency = 4)
View(myuk)
plot(myuk)

#Seasonal Decompostion
plot(decompose(myuk, type = c("multiplicative")))
fit <- stl(myuk, s.window="period")
plot(fit)
ls(fit)
print(fit$win)

#Exponential Model
require(forecast)

#Simple Exponential
fit1 <- HoltWinters(myuk, beta=FALSE, gamma=FALSE)
accuracy(fit1$fitted, myuk)   # MAPE - 18.109

#Double Exponential
fit2 <- HoltWinters(myuk,gamma = FALSE)
accuracy(fit2$fitted, myuk)   # MAPE - 23.511

#Tripple Exponential
fit3 <- HoltWinters(myuk)
accuracy(fit3$fitted, myuk)   # MAPE - 2.917
plot(forecast(fit3,4))
forecast(fit3,4)

#ETS Model
fit4<-ets(myuk)
accuracy(fit4$fitted, myuk)
summary(fit4)    # MAPE - 1.75
forecast(myuk,4)
plot(forecast(myuk, 4))

#Arima Model
View(myuk)
acf(myuk)
acf(log(diff(myuk)))  # P = 4

pacf(log(diff(myuk)))  # Q = 0

ndiffs(myuk)         # D = 1
require(tseries)
adf.test(myuk)

fit5 <- arima(myuk, order=c(4, 1, 0), method =  "ML")
summary(fit5)             # MAPE = 3.32
plot(forecast(fit5,4))
forecast(fit5, 4)

#Auto ARIMA Model
fit6 <-auto.arima(myuk)
summary(fit6)           # MAPE - 2.42


# We take ETS as best fit model as it have a lowest moving average error percentage = 1.75
# Forecast of Next 4 Quarters : 
# 2006 Q1       18193.29
# 2006 Q2       24223.19
# 2006 Q3       29573.43
# 2006 Q4       21002.51
