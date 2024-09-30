###########################
###### Occupancy Rate ######
###########################

# Read data
thailand.data <- read.csv("thailand_tourism_data.csv")

# Data preparation: Filter to select variables of interest
thailand_occupancy.data <- subset(thailand.data, thailand.data$variable == "occupancy_rate")
thailand_net_profit_all.data <- subset(thailand.data, thailand.data$variable == "net_profit_all")
thailand_no_tourist_all.data <- subset(thailand.data, thailand.data$variable == "no_tourist_all")

library(forecast)
library(zoo)
library(tseries)
library(modeltime)

# Select representative provinces as examples
Bangkok_occupancy.data <- subset(thailand_occupancy.data, thailand_occupancy.data$province_eng == "Bangkok")
Bangkok_occupancy.data <- Bangkok_occupancy.data[, c("date", "value")] 

# Sort by date
Bangkok_occupancy.data <- Bangkok_occupancy.data[order(Bangkok_occupancy.data$date),]

# Time series conversion
Bangkok_occupancy.ts <- ts(Bangkok_occupancy.data$value, 
                           start = c(2019, 1), end = c(2023, 2), frequency = 12)

# Plot time series and autocorrelation graph
pic1 <- autoplot(Bangkok_occupancy.ts)

# White noise test
Box.test(Bangkok_occupancy.ts, type='Ljung-Box')

# Smoothing
opar <- par(no.readonly=TRUE)   
par(mfrow=c(2,2)) 

ylim <- c(min(Bangkok_occupancy.ts), max(Bangkok_occupancy.ts))    
plot(Bangkok_occupancy.ts, main="Raw time series")    
plot(ma(Bangkok_occupancy.ts, 2), main="Simple Moving Averages (k=2)", ylim=ylim) 
plot(ma(Bangkok_occupancy.ts, 5), main="Simple Moving Averages (k=5)", ylim=ylim)     
plot(ma(Bangkok_occupancy.ts, 10), main="Simple Moving Averages (k=10)", ylim=ylim)   
par(opar)

# ARIMA
# Stabilize time series and perform preliminary order determination
par(mfrow=c(2,1))
plot(Bangkok_occupancy.ts, type="l", xlab="Time", ylab="Bangkok Occupancy", main="Before Difference")
# Autocorrelation graph
acf(Bangkok_occupancy.ts, main="Autocorrelation", xlab="Lag")

ndiffs(Bangkok_occupancy.ts) # Result is 1, indicating one first difference is needed

nBangkok_occupancy.ts <- diff(Bangkok_occupancy.ts, 1)
ndiffs(nBangkok_occupancy.ts) # Result is 0, no further differencing needed

plot(nBangkok_occupancy.ts, type="l", main="After Difference")
acf(nBangkok_occupancy.ts, main="Autocorrelation", xlab="Lag")
pacf(nBangkok_occupancy.ts, main="Partial ACF", xlab="Lag")
# Perform unit root test on the differenced data
ADF <- adf.test(nBangkok_occupancy.ts) # p-value = 0.01043, reject the null hypothesis, indicating no unit root, the time series is stationary after first difference

# Automatic parameter tuning and comparison with previously determined orders
arima_occupancy <- auto.arima(Bangkok_occupancy.ts, lambda = "auto") # Parameters in ARIMA: autoregressive, differencing, moving average
summary(arima_occupancy) # (0,1,1)

arima2 <- arima(nBangkok_occupancy.ts, order = c(1, 1, 0)) # Parameters in ARIMA: autoregressive, differencing, moving average
summary(arima2)

# Significance test for confidence intervals of regression coefficients
confint(arima_occupancy) # Interval does not contain 0, significant

# Residual tests for the model
Box.test(arima_occupancy$residuals, type='Ljung-Box') # p-value = 0.6974, residual test not significant, stationary

# Fitting
Bangkok_occupancy.pred <- forecast(arima_occupancy)
plot(Bangkok_occupancy.pred$fitted)
lines(Bangkok_occupancy.ts, col="red3")

## Linear and Non-Linear Trend Models
# Determine if there is seasonality
Bangkok_occupancy.stl <- stl(Bangkok_occupancy.ts, s.window = "periodic")
plot(Bangkok_occupancy.stl)

# Train model
# Linear trend model
lm_seasonal_occupancy <- tslm(Bangkok_occupancy.ts ~ trend + season)
lm_pred_occupancy <- forecast(lm_seasonal_occupancy, level = 0)
plot(Bangkok_occupancy.ts, xlab = "Date", ylab = "Value") 
lines(lm_pred_occupancy$fitted, lwd = 2, col = "blue")
summary(lm_seasonal_occupancy)
accuracy(lm_seasonal_occupancy)
autoplot(lm_pred_occupancy)

# Non-linear trend model
nlm_seasonal_occupancy <- tslm(Bangkok_occupancy.ts ~ trend + I(trend^2) + season)
nlm_pred_occupancy <- forecast(nlm_seasonal_occupancy, level = 0)
plot(Bangkok_occupancy.ts, xlab = "Date", ylab = "Value") 
lines(nlm_pred_occupancy$fitted, lwd = 2, col = "green2")
summary(nlm_seasonal_occupancy)
accuracy(nlm_seasonal_occupancy)
autoplot(nlm_pred_occupancy)

# Model comparison
accuracy(arima_occupancy)
accuracy(lm_seasonal_occupancy)
accuracy(nlm_seasonal_occupancy)

# Split training and validation sets
nValid <- 10
nTrain <- length(Bangkok_occupancy.ts) - nValid
occupancy_train.ts <- window(nBangkok_occupancy.ts, start=c(2019,1), end=c(2019,nTrain))
occupancy_valid.ts <- window(nBangkok_occupancy.ts, start=c(2019,nTrain+1), end=c(2019,50))
# Forecasting and validation
arima_best_occupancy <- Arima(occupancy_train.ts, order=c(0,1,1))
arima_best_occupancy.pred <- forecast(arima_best_occupancy, h=nValid, level=0)
plot(arima_best_occupancy.pred)
lines(occupancy_valid.ts, col="green")


###########################
###### Net Profit ##########
###########################

# Read data
thailand.data <- read.csv("thailand.csv")

# Data preparation: Filter to select variables of interest
thailand_occupancy.data <- subset(thailand.data, thailand.data$variable == "occupancy_rate")
thailand_net_profit_all.data <- subset(thailand.data, thailand.data$variable == "net_profit_all")
thailand_no_tourist_all.data <- subset(thailand.data, thailand.data$variable == "no_tourist_all")

# Select representative provinces as examples
library(forecast)
library(zoo)
library(tseries)
library(modeltime)

Bangkok_net_profit_all.data <- subset(thailand_net_profit_all.data, thailand_net_profit_all.data$province_eng == "Bangkok")
Bangkok_net_profit_all.data <- Bangkok_net_profit_all.data[, c("date", "value")] 

# Sort by date
Bangkok_net_profit_all.data <- Bangkok_net_profit_all.data[order(Bangkok_net_profit_all.data$date),]

# Time series conversion
Bangkok_net_profit_all.ts <- ts(Bangkok_net_profit_all.data$value, 
                                 start = c(2019, 1), end = c(2023, 2), frequency = 12)

# Plot time series and autocorrelation graph to observe the data
pic1 <- plot.ts(Bangkok_net_profit_all.ts)
acf(Bangkok_net_profit_all.ts, main="Autocorrelation", xlab="Lag")
pacf(Bangkok_net_profit_all.ts, main="Partial ACF", xlab="Lag")

# ARIMA
# Stabilize time series and perform preliminary order determination
par(mfrow=c(2,1))
plot(Bangkok_net_profit_all.ts, type="l", xlab="Time", ylab="Bangkok Net Profit", main="Before Difference")
acf(Bangkok_net_profit_all.ts, main="Autocorrelation", xlab="Lag")

# Log transformation
Bangkok_net_profit_all.ts <- log(Bangkok_net_profit_all.ts)

# Differencing
ndiffs(Bangkok_net_profit_all.ts) # Result is 1, indicating one first difference is needed

nlogBangkok_net_profit_all.ts <- diff(Bangkok_net_profit_all.ts, 1)
ndiffs(nBangkok_net_profit_all.ts) # Result is 0, no further differencing needed

plot(nBangkok_net_profit_all.ts, type="l", main="After Difference")
acf(nBangkok_net_profit_all.ts, main="Autocorrelation", xlab="Lag")

# Perform unit root test on the differenced data
ADF <- adf.test(nlogBangkok_net_profit_all.ts) # p-value = 0.48, reject the null hypothesis, indicating no unit root, the time series is stationary after first difference

# Automatic parameter tuning and comparison with previously determined orders
arima_net_profit <- auto.arima(Bangkok_net_profit_all.ts) # Parameters in ARIMA: autoregressive, differencing, moving average
summary(arima_net_profit) # (1,1,1)

# Significance test for confidence intervals of regression coefficients
confint(arima_net_profit) # Interval does not contain 0, significant

# Residual tests for the model
Box.test(arima_net_profit$residuals, type='Ljung-Box') # p-value = 0.6974, residual test not significant, stationary

# Fitting
fBangkok_profit <- forecast(arima_net_profit)
plot(fBangkok_profit$fitted)
lines(Bangkok_net_profit_all.ts, col="red3")


## Linear and Non-Linear Trend Models
# Determine if there is seasonality
Bangkok_occupancy.stl <- stl(Bangkok_net_profit_all.ts, s.window = "periodic")
plot(Bangkok_occupancy.stl)

# Train model
# Linear trend model
lm_seasonal_net_profit <- tslm(Bangkok_net_profit_all.ts ~ trend + season)

# Forecast
lm_net_profit_forecast <- forecast(lm_seasonal_net_profit, h=10)
summary(lm_net_profit_forecast)
autoplot(lm_net_profit_forecast)

# Non-linear
nlm_seasonal_net_profit <- tslm(Bangkok_net_profit_all.ts ~ trend + I(trend^2) + season)

# Forecast
nlm_forecast_net_profit <- forecast(nlm_seasonal_net_profit, h=10)
summary(nlm_forecast_net_profit)
autoplot(nlm_forecast_net_profit)

# Model comparison
accuracy(arima_net_profit)
accuracy(lm_seasonal_net_profit)
accuracy(nlm_seasonal_net_profit)

# Split training and validation sets
nValid <- 10
nTrain <- length(Bangkok_net_profit_all.ts) - nValid
profit_train.ts <- window(nBangkok_net_profit_all.ts, start=c(2019,1), end=c(2019,nTrain))
profit_valid.ts <- window(nBangkok_net_profit_all.ts, start=c(2019,nTrain+1), end=c(2019,50))

# Forecasting and validation
arima_best_profit <- Arima(profit_train.ts, order=c(1,1,1))
arima_best_profit.pred <- forecast(arima_best_profit, h=nValid, level=0)
plot(arima_best_profit.pred)
lines(profit_valid.ts, col="green")


###########################
##### Number of Tourists ####
###########################

# Read data
thailand.data <- read.csv("thailand.csv")

# Data preparation: Filter to select variables of interest
thailand_occupancy.data <- subset(thailand.data, thailand.data$variable == "occupancy_rate")
thailand_net_profit_all.data <- subset(thailand.data, thailand.data$variable == "net_profit_all")
thailand_no_tourist_all.data <- subset(thailand.data, thailand.data$variable == "no_tourist_all")

# Select representative provinces as examples
library(forecast)
library(zoo)
library(tseries)
library(modeltime)

Bangkok_no_tourist_all.data <- subset(thailand_no_tourist_all.data, thailand_no_tourist_all.data$province_eng == "Bangkok")
Bangkok_no_tourist_all.data <- Bangkok_no_tourist_all.data[, c("date","value")] 

# Sort by date
Bangkok_no_tourist_all.data <- Bangkok_no_tourist_all.data[order(Bangkok_no_tourist_all.data$date),]

# Time series conversion
Bangkok_no_tourist_all.ts <- ts(Bangkok_no_tourist_all.data$value, 
                                start = c(2019,1), end = c(2023,2), frequency = 12)

# Plot time series and autocorrelation graph to observe the data
pic1 <- plot.ts(Bangkok_no_tourist_all.ts)
acf(Bangkok_no_tourist_all.ts, main="Autocorrelation", xlab="Lag")
pacf(Bangkok_no_tourist_all.ts, main="Partial ACF", xlab="Lag")

# ARIMA
# Stabilize time series and perform preliminary order determination
par(mfrow=c(2,1))
plot(Bangkok_no_tourist_all.ts, type="l", xlab="Time", ylab="Bangkok Tourists", main="Before Difference")
acf(Bangkok_no_tourist_all.ts, main="Autocorrelation", xlab="Lag")

# Log transformation
Bangkok_no_tourist_all.ts <- log(Bangkok_no_tourist_all.ts)
ndiffs(Bangkok_no_tourist_all.ts) # Result is 1, indicating one first difference is needed

nBangkok_no_tourist_all.ts <- diff(Bangkok_no_tourist_all.ts, 1)
ndiffs(nBangkok_no_tourist_all.ts) # Result is 0, no further differencing needed

plot(nBangkok_no_tourist_all.ts, type="l", main="After Difference")
acf(nBangkok_no_tourist_all.ts, main="Autocorrelation", xlab="Lag")

# Perform unit root test on the differenced data
ADF <- adf.test(nBangkok_no_tourist_all.ts) # p-value = 0.01043, reject the null hypothesis, indicating no unit root, the time series is stationary after first difference

# Automatic parameter tuning and comparison with previously determined orders
arima_tourist <- auto.arima(Bangkok_no_tourist_all.ts) # Parameters in ARIMA: autoregressive, differencing, moving average
summary(arima_tourist)

# Significance test for confidence intervals of regression coefficients
confint(arima_tourist) # Interval does not contain 0, significant

# Residual tests for the model
Box.test(arima_tourist$residuals, type='Ljung-Box') # p-value = 0.6974, residual test not significant, stationary

# Fitting
fBangkok_tourist <- forecast(arima_tourist)
plot(fBangkok_tourist$fitted)
lines(Bangkok_no_tourist_all.ts, col="red3")

## Linear and Non-Linear Trend Models
# Determine if there is seasonality
Bangkok_tourist.stl <- stl(Bangkok_no_tourist_all.ts, s.window = "periodic")
plot(Bangkok_tourist.stl)

# Train model
# Linear
lm_seasonal_tourist <- tslm(Bangkok_no_tourist_all.ts ~ trend + season)

# Forecast
lm_tourist_forecast <- forecast(lm_seasonal_tourist, h=10)
summary(lm_tourist_forecast)
autoplot(lm_tourist_forecast)

# Non-linear
nlm_tourist_seasonal <- tslm(Bangkok_no_tourist_all.ts ~ trend + I(trend^2) + season)

# Forecast
nlm_tourist_forecast <- forecast(nlm_tourist_seasonal, h=10)
summary(nlm_tourist_forecast)
autoplot(nlm_tourist_forecast)

# Model comparison
accuracy(arima_net_profit)
accuracy(lm_seasonal_net_profit)
accuracy(nlm_seasonal_net_profit)

# Split training and validation sets
nValid <- 10
nTrain <- length(Bangkok_no_tourist_all.ts) - nValid
tourist_train.ts <- window(nBangkok_no_tourist_all.ts, start=c(2019,1), end=c(2019,nTrain))
tourist_valid.ts <- window(nBangkok_no_tourist_all.ts, start=c(2019,nTrain+1), end=c(2019,50))

# Forecasting and validation
arima_best_tourist <- Arima(tourist_train.ts, order=c(0,1,1))
arima_best_tourist.pred <- forecast(arima_best_tourist, h=nValid, level=0)
plot(arima_best_tourist.pred)
lines(tourist_valid.ts, col="green")
