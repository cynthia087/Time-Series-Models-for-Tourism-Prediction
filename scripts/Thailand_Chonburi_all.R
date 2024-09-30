# Read data
thailand.data <- read.csv("thailand.csv")

# Data organization, filter out the variables of interest
thailand_occupancy.data <- subset(thailand.data, thailand.data$variable == "occupancy_rate")
thailand_net_profit_all.data <- subset(thailand.data, thailand.data$variable == "net_profit_all")
thailand_no_tourist_all.data <- subset(thailand.data, thailand.data$variable == "no_tourist_all")

# Import necessary functions
library(forecast)
library(zoo)
library(tseries)

###########################
###### Occupancy Rate ######
###########################

# Filter a representative province as an example
Chonburi_occupancy.data <- subset(thailand_occupancy.data, thailand_occupancy.data$province_eng == "Chonburi")
Chonburi_occupancy.data <- Chonburi_occupancy.data[, c("date", "value")] 

# Sort by date
Chonburi_occupancy.data <- Chonburi_occupancy.data[order(Chonburi_occupancy.data$date),]

# Time series conversion
Chonburi_occupancy.ts <- ts(Chonburi_occupancy.data$value, start = c(2019, 1), end = c(2023, 2), frequency = 12)

# Stationarity assessment and white noise test
autoplot(Chonburi_occupancy.ts)
options(scipen = 10)
Box.test(Chonburi_occupancy.ts, type='Ljung-Box')  # p-value < 0.01

# Time series smoothing and seasonal decomposition

# Moving average
opar <- par(no.readonly = TRUE)
par(mfrow=c(2, 2))
ylim <- c(min(Chonburi_occupancy.ts), max(Chonburi_occupancy.ts))
plot(Chonburi_occupancy.ts, main="Raw time series")
plot(ma(Chonburi_occupancy.ts, 2), main="Simple Moving Average (k=2)", ylim=ylim)
plot(ma(Chonburi_occupancy.ts, 5), main="Simple Moving Average (k=5)")
plot(ma(Chonburi_occupancy.ts, 10), main="Simple Moving Average (k=10)")
par(opar)

# Determine if there is seasonality 
Chonburi_occupancy.stl <- stl(Chonburi_occupancy.ts, s.window = "periodic")
plot(Chonburi_occupancy.stl)

# Model selection

# Linear
linear.lm.season1 <- tslm(Chonburi_occupancy.ts ~ trend + season)
linear.lm.pred.season1 <- forecast(linear.lm.season1)
plot(Chonburi_occupancy.ts, xlab="Date", ylab="Occupancy Rate")
lines(linear.lm.pred.season1$fitted, lwd=2, col="blue")
summary(linear.lm.pred.season1)

# Non-linear
quadratic.lm.season1 <- tslm(Chonburi_occupancy.ts ~ trend + I(trend^2) + season)
quadratic.lm.pred.season1 <- forecast(quadratic.lm.season1)
lines(quadratic.lm.pred.season1$fitted, lwd=2, col="green")
summary(quadratic.lm.pred.season1)

# ARIMA
install.packages("fBasics")
install.packages("fUnitRoots")
library(fBasics)
library(fUnitRoots)

# Differencing
ndiffs(Chonburi_occupancy.ts)
nChonburi_occupancy.ts <- diff(Chonburi_occupancy.ts, 1)
ADF1 <- adf.test(nChonburi_occupancy.ts)  # Passed the unit root test
adfTest(nChonburi_occupancy.ts, lag=1, type="nc")

# White noise test
Box.test(nChonburi_occupancy.ts, lag = 1, type='Ljung-Box') # p < 0.05

# Autocorrelation and partial autocorrelation plots
acf(nChonburi_occupancy.ts)
pacf(nChonburi_occupancy.ts)

# Modeling
arima1 <- auto.arima(nChonburi_occupancy.ts)
summary(arima1)  #(0,0,1)

# Significance test for the confidence intervals of regression coefficients
confint(arima1) # Significant

# Residual tests for the model
Box.test(arima1$residuals, type='Ljung-Box') # p > 0.05

# Plotting
arima.pred1 <- forecast(arima1)
plot(arima.pred1)

# Comparison
accuracy(linear.lm.pred.season1)  # 27.26454
accuracy(quadratic.lm.pred.season1) # 11.99539
accuracy(arima1)  # 9.507311

###########################
######## Net Profit #########
###########################

Chonburi_net_profit_all.data <- subset(thailand_net_profit_all.data, thailand_net_profit_all.data$province_eng == "Chonburi")
Chonburi_net_profit_all.data <- Chonburi_net_profit_all.data[, c("date", "value")] 
Chonburi_net_profit_all.data <- Chonburi_net_profit_all.data[order(Chonburi_net_profit_all.data$date),]
Chonburi_net_profit_all.ts <- ts(Chonburi_net_profit_all.data$value, start = c(2019, 1), end = c(2023, 2), frequency = 12)
Chonburi_net_profit_all.ts <- log(Chonburi_net_profit_all.ts)  # Natural logarithm transformation

# Stationarity assessment and white noise test
autoplot(Chonburi_net_profit_all.ts)
Box.test(Chonburi_net_profit_all.ts, type='Ljung-Box')  # p-value < 0.05

# Time series smoothing and seasonal decomposition

# Moving average
opar <- par(no.readonly = TRUE)
par(mfrow=c(2, 2))
ylim <- c(min(Chonburi_net_profit_all.ts), max(Chonburi_net_profit_all.ts))
plot(Chonburi_net_profit_all.ts, main="Raw time series")
plot(ma(Chonburi_net_profit_all.ts, 2), main="Simple Moving Average (k=2)", ylim=ylim)
plot(ma(Chonburi_net_profit_all.ts, 5), main="Simple Moving Average (k=5)")
plot(ma(Chonburi_net_profit_all.ts, 10), main="Simple Moving Average (k=10)")
par(opar)

# Season & trend
Chonburi_net_profit_all.stl <- stl(Chonburi_net_profit_all.ts, s.window = "periodic")
plot(Chonburi_net_profit_all.stl)

# Model selection

# Linear
par(mfrow=c(1, 1))
linear.lm.season2 <- tslm(Chonburi_net_profit_all.ts ~ trend + season)
linear.lm.pred.season2 <- forecast(linear.lm.season2)
plot(Chonburi_net_profit_all.ts, xlab="Date", ylab="Net Profit")
lines(linear.lm.pred.season2$fitted, lwd=2, col="blue")
summary(linear.lm.pred.season2)

# Non-linear
quadratic.lm.season2 <- tslm(Chonburi_net_profit_all.ts ~ trend + I(trend^2) + season)
quadratic.lm.pred.season2 <- forecast(quadratic.lm.season2)
lines(quadratic.lm.pred.season2$fitted, lwd=2, col="green")
summary(quadratic.lm.pred.season2)

# ARIMA
ndiffs(Chonburi_net_profit_all.ts)
nChonburi_net_profit_all.ts <- diff(diff(Chonburi_net_profit_all.ts), 1)
ndiffs(nChonburi_net_profit_all.ts)
adfTest(nChonburi_net_profit_all.ts, type="nc")  # Passed the unit root test
Box.test(nChonburi_net_profit_all.ts, type='Ljung-Box')  # p < 0.05

acf(nChonburi_net_profit_all.ts)
pacf(nChonburi_net_profit_all.ts)

arima2 <- auto.arima(Chonburi_net_profit_all.ts)
summary(arima2) #(0,1,0)
confint(arima2) # Significant
Box.test(arima2$residuals, type='Ljung-Box') # p > 0.05
arima.pred2 <- forecast(arima2)
plot(arima.pred2)

# Comparison
accuracy(linear.lm.pred.season2)  # 1.479454
accuracy(quadratic.lm.pred.season2) # 0.9034827
accuracy(arima2)  # 0.7556289


###########################
########## Tourist #########
###########################

Chonburi_no_tourist_all.data <- subset(thailand_no_tourist_all.data, thailand_no_tourist_all.data$province_eng == "Chonburi")
Chonburi_no_tourist_all.data <- Chonburi_no_tourist_all.data[, c("date", "value")] 
Chonburi_no_tourist_all.data <- Chonburi_no_tourist_all.data[order(Chonburi_no_tourist_all.data$date),]
Chonburi_no_tourist_all.ts <- ts(Chonburi_no_tourist_all.data$value, start = c(2019, 1), end = c(2023, 2), frequency = 12)
Chonburi_no_tourist_all.ts <- log(Chonburi_no_tourist_all.ts)  # Natural logarithm transformation

# Stationarity assessment and white noise test
autoplot(Chonburi_no_tourist_all.ts)
Box.test(Chonburi_no_tourist_all.ts, type='Ljung-Box')  # p < 0.01

# Time series smoothing and seasonal decomposition

# Moving average
opar <- par(no.readonly = TRUE)
par(mfrow=c(2, 2))
ylim <- c(min(Chonburi_no_tourist_all.ts), max(Chonburi_no_tourist_all.ts))
plot(Chonburi_no_tourist_all.ts, main="Raw time series")
plot(ma(Chonburi_no_tourist_all.ts, 2), main="Simple Moving Average (k=2)", ylim=ylim)
plot(ma(Chonburi_no_tourist_all.ts, 5), main="Simple Moving Average (k=5)")
plot(ma(Chonburi_no_tourist_all.ts, 10), main="Simple Moving Average (k=10)")
par(opar)

# Season & trend
Chonburi_no_tourist_all.stl <- stl(Chonburi_no_tourist_all.ts, s.window = "periodic")
plot(Chonburi_no_tourist_all.stl)

# Model selection

# Linear
linear.lm.season3 <- tslm(Chonburi_no_tourist_all.ts ~ trend + season)
linear.lm.pred.season3 <- forecast(linear.lm.season3)
plot(Chonburi_no_tourist_all.ts, xlab="Date", ylab="Tourist")
lines(linear.lm.pred.season3$fitted, lwd=2, col="blue")
summary(linear.lm.pred.season3)

# Non-linear
quadratic.lm.season3 <- tslm(Chonburi_no_tourist_all.ts ~ trend + I(trend^2) + season)
quadratic.lm.pred.season3 <- forecast(quadratic.lm.season3)
lines(quadratic.lm.pred.season3$fitted, lwd=2, col="green")
summary(quadratic.lm.pred.season3)

# ARIMA

# Differencing and testing
ndiffs(Chonburi_no_tourist_all.ts)  # No differencing required

# Autocorrelation and partial autocorrelation plots
acf(Chonburi_no_tourist_all.ts)
pacf(Chonburi_no_tourist_all.ts)

# Modeling
arima3 <- auto.arima(Chonburi_no_tourist_all.ts)
confint(arima3) # Significant
summary(arima3) #(1,0,0)
Box.test(arima3$residuals, type='Ljung-Box') # p > 0.05
arima.pred3 <- forecast(arima3)
plot(arima.pred3)

# Comparison
accuracy(linear.lm.pred.season3)  # 1.324548
accuracy(quadratic.lm.pred.season3) # 0.9490133
accuracy(arima3)  # 0.8926509

# Split the training and testing sets for prediction
nValid <- 20
nTrain <- length(Chonburi_occupancy.ts) - nValid
occupancy_train.ts <- window(nChonburi_occupancy.ts, start=c(2019, 1), end=c(2019, nTrain))
occupancy_valid.ts <- window(nChonburi_occupancy.ts, start=c(2019, nTrain + 1), end=c(2019, 50))
arima1 <- Arima(occupancy_train.ts, order=c(0, 0, 1))
arima1.pred <- forecast(arima1, h=nValid, level=0)
plot(arima1.pred)
lines(occupancy_valid.ts, col="green")

profit_train.ts <- window(nChonburi_net_profit_all.ts, start=c(2019, 1), end=c(2019, nTrain))
profit_valid.ts <- window(nChonburi_net_profit_all.ts, start=c(2019, nTrain + 1), end=c(2019, 50))
arima2 <- Arima(profit_train.ts, order=c(0, 1, 0))
arima2.pred <- forecast(arima2, h=nValid, level=0)
plot(arima2.pred)
lines(profit_valid.ts, col="green")

tourist_train.ts <- window(Chonburi_no_tourist_all.ts, start=c(2019, 1), end=c(2019, nTrain))
tourist_valid.ts <- window(Chonburi_no_tourist_all.ts, start=c(2019, nTrain + 1), end=c(2019, 50))
arima3 <- Arima(tourist_train.ts, order=c(1, 0, 0))
arima3.pred <- forecast(arima3, h=nValid, level=0)
plot(arima3.pred)
lines(tourist_valid.ts, col="green")
