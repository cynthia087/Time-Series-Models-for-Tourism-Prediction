# Load libraries and import data
library(forecast)
library(tseries)

thailand.data <- read.csv("thailand.csv")

thailand_occupancy.data <- subset(thailand.data, thailand.data$variable == "occupancy_rate")
thailand_no_tourist_all.data <- subset(thailand.data, thailand.data$variable == "no_tourist_all")
thailand_net_profit_all.data <- subset(thailand.data, thailand.data$variable == "net_profit_all")

####################### Occupancy ################################

# Select Chiang Mai as an example
ChiangMai_occupancy.data <- subset(thailand_occupancy.data,
                                   thailand_occupancy.data$province_eng == "Chiang Mai")
ChiangMai_occupancy.data <- ChiangMai_occupancy.data[, c("date", "value")]

# Sort by date
ChiangMai_occupancy.data <- ChiangMai_occupancy.data[order(ChiangMai_occupancy.data$date),]

# Time series conversion
ChiangMai_occupancy.ts <- ts(ChiangMai_occupancy.data$value,
                             start = c(2019, 1), end = c(2023, 2), frequency = 12)

# Plot time series and autocorrelation graph to observe data
pic1 <- plot.ts(ChiangMai_occupancy.ts)

# White noise test
Box.test(ChiangMai_occupancy.ts, type='Ljung-Box')

# Smoothing
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))

ylim <- c(min(ChiangMai_occupancy.ts), max(ChiangMai_occupancy.ts))
plot(ChiangMai_occupancy.ts, main="Raw time series")
plot(ma(ChiangMai_occupancy.ts, 2), main="Simple Moving Averages (k=2)", ylim=ylim)
plot(ma(ChiangMai_occupancy.ts, 5), main="Simple Moving Averages (k=5)", ylim=ylim)
plot(ma(ChiangMai_occupancy.ts, 10), main="Simple Moving Averages (k=10)", ylim=ylim)
par(opar)

# Determine if there is seasonality
ChiangMai_occupancy.stl <- stl(ChiangMai_occupancy.ts, s.window = "periodic")
plot(ChiangMai_occupancy.stl)

# Linear trend model
# Add seasonal term
linear_model_seasonal <- tslm(ChiangMai_occupancy.ts ~ trend + season)
linear_season_lm_pred <- forecast(linear_model_seasonal, level = 0)
plot(ChiangMai_occupancy.ts, xlab = "Date", ylab = "Value")
lines(linear_season_lm_pred$fitted, lwd = 2, col = "blue")
summary(linear_model_seasonal)

# Model prediction
predict_line_occupancy <- forecast(linear_model_seasonal, level = 0)

par(mfrow = c(1, 1))
plot(ChiangMai_occupancy.ts, ylim = c(0, 250), ylab = "Value", 
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(2019, 2024.25), main = "")
axis(1, at = seq(2019, 2024, 1), labels = format(seq(2019, 2024, 1)))

plot(predict_line_occupancy, lwd = 2, col = "blue4", ylab = "Value", 
     xlab = "Date")
abline(v=2023.15, lwd=2, col="red")
lines(predict_line_occupancy$fitted, col=2)

# Non-linear trend model 
nonlinear_model_seasonal <- tslm(ChiangMai_occupancy.ts ~ trend + I(trend^2) + season)
nonlinear_season_lm_pred <- forecast(nonlinear_model_seasonal, level = 0)
plot(ChiangMai_occupancy.ts, xlab = "Date", ylab = "Value")
lines(nonlinear_season_lm_pred$fitted, lwd = 2, col = "green2")
summary(nonlinear_model_seasonal)

# Non-linear model prediction
predict_nonlinear_occupancy <- forecast(nonlinear_model_seasonal, level = 0)

par(mfrow = c(1, 1))
plot(ChiangMai_occupancy.ts, ylim = c(0, 250), ylab = "Value", 
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(2019, 2024.25), main = "")
axis(1, at = seq(2019, 2024, 1), labels = format(seq(2019, 2024, 1)))
plot(predict_nonlinear_occupancy, lwd = 2, col = "green4", ylab = "Value", 
     xlab = "Date")
abline(v=2023.15, lwd=2, col="red")
lines(predict_nonlinear_occupancy$fitted, col=2)

## ARIMA model
# Differencing
ndiffs(ChiangMai_occupancy.ts) # Result is 0, but it did not pass the ADF unit root test
ADF1 <- adf.test(ChiangMai_occupancy.ts)

# Therefore, perform first differencing on the seasonal term
nChiangMai_occupancy.ts <- diff(ChiangMai_occupancy.ts, 12)
nChiangMai_occupancy.ts <- diff(nChiangMai_occupancy.ts, 1)
ndiffs(nChiangMai_occupancy.ts)

# Perform unit root test on the data again
ADF2 <- adf.test(nChiangMai_occupancy.ts)
# p=0.04<0.05, therefore reject the null hypothesis, no unit root exists, the first difference makes the time series stationary
# d = 1

# Plot autocorrelation graph
acf(ChiangMai_occupancy.ts, main="Autocorrelation", xlab="Lag")

# Plot partial autocorrelation graph
pacf(ChiangMai_occupancy.ts, main="Partial ACF", xlab="Lag")
# Based on the characteristics of the autocorrelation and partial autocorrelation graphs, it can be determined that p is approximately 1.

# Automatic parameter tuning and comparison with previously determined orders
arima1 <- auto.arima(ChiangMai_occupancy.ts)
summary(arima1)
confint(arima1)

# Residual tests for the model
Box.test(arima1$residuals, type='Ljung-Box')

# Fit using the ARIMA function
fit <- Arima(ChiangMai_occupancy.ts, order = c(0, 1, 0), seasonal = c(1, 1, 0))
# Forecast data for the next year
predict_arima_ChiangMai_occupancy <- forecast(fit, 12)
summary(fit)

par(mfrow = c(1, 1))
plot(ChiangMai_occupancy.ts, ylim = c(0, 250), ylab = "Value", 
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(2019, 2024.25), main = "")
axis(1, at = seq(2019, 2024, 1), labels = format(seq(2019, 2024, 1)))
plot(predict_arima_ChiangMai_occupancy, lwd = 2, col = "blue", ylab = "Value", 
     xlab = "Date")
abline(v=2023.15, lwd=2, col="red")
lines(predict_arima_ChiangMai_occupancy$fitted, col=2)

# Compare prediction results
linear.season <- as.numeric(accuracy(predict_line_occupancy)[2]) 
quadratic.season <- as.numeric(accuracy(predict_nonlinear_occupancy)[2])
arima.season <- as.numeric(accuracy(predict_arima_ChiangMai_occupancy)[2])

RMSE_all <- cbind(linear.season, quadratic.season, arima.season)
RMSE_all


######################### Tourist ##################################
thailand_no_tourist_all.data <- subset(thailand.data, thailand.data$variable == "no_tourist_all")
# Select Chiang Mai as an example
ChiangMai_tourist.data <- subset(thailand_no_tourist_all.data,
                                 thailand_no_tourist_all.data$province_eng == "Chiang Mai")
ChiangMai_tourist.data <- ChiangMai_tourist.data[, c("date", "value")]

# Sort by date
ChiangMai_tourist.data <- ChiangMai_tourist.data[order(ChiangMai_tourist.data$date),]

# Time series conversion
ChiangMai_tourist.ts <- ts(ChiangMai_tourist.data$value,
                             start = c(2019, 1), end = c(2023, 2), frequency = 12)
# Since the value is too large, log normalize the value in advance
ChiangMai_tourist.ts <- log(ChiangMai_tourist.ts)

# Plot time series and autocorrelation graph to observe data 
pic1 <- plot.ts(ChiangMai_tourist.ts)

# White noise test 
Box.test(ChiangMai_tourist.ts, type='Ljung-Box')

# Smoothing 
opar <- par(no.readonly=TRUE) 
par(mfrow=c(2,2))

ylim <- c(min(ChiangMai_tourist.ts), max(ChiangMai_tourist.ts)) 
plot(ChiangMai_tourist.ts, main="Raw time series") 
plot(ma(ChiangMai_tourist.ts, 2), main="Simple Moving Averages (k=2)", ylim=ylim)
plot(ma(ChiangMai_tourist.ts, 5), main="Simple Moving Averages (k=5)", ylim=ylim) 
plot(ma(ChiangMai_tourist.ts, 10), main="Simple Moving Averages (k=10)", ylim=ylim) 
par(opar)

# Determine if there is seasonality 
ChiangMai_tourist.stl <- stl(ChiangMai_tourist.ts, s.window = "periodic") 
plot(ChiangMai_tourist.stl)

# Linear trend model
# Add seasonal term
linear_model_seasonal <- tslm(ChiangMai_tourist.ts ~ trend + season) 
linear_season_lm_pred <- forecast(linear_model_seasonal, level = 0) 
plot(ChiangMai_tourist.ts, xlab = "Date", ylab = "Value") 
lines(linear_season_lm_pred$fitted, lwd = 2, col = "blue") 
summary(linear_model_seasonal)

# Model prediction
predict_line_tourist <- forecast(linear_model_seasonal, level = 0)

par(mfrow = c(1, 1))
plot(ChiangMai_tourist.ts, ylim = c(0, 250), ylab = "Value", 
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(2019, 2024.25), main = "")
axis(1, at = seq(2019, 2024, 1), labels = format(seq(2019, 2024, 1)))
plot(predict_line_tourist, lwd = 2, col = "blue4", ylab = "Value", 
     xlab = "Date")
abline(v=2023.15, lwd=2, col="red")
lines(predict_line_tourist$fitted, col=2)

# Non-linear trend model 
nonlinear_model_seasonal <- tslm(ChiangMai_tourist.ts ~ trend + I(trend^2) + season) 
nonlinear_season_lm_pred <- forecast(nonlinear_model_seasonal, level = 0) 
plot(ChiangMai_tourist.ts, xlab = "Date", ylab = "Value") 
lines(nonlinear_season_lm_pred$fitted, lwd = 2, col = "green2") 
summary(nonlinear_model_seasonal)

# Non-linear model prediction
predict_nonlinear_tourist <- forecast(nonlinear_model_seasonal, level = 0)

par(mfrow = c(1, 1))
plot(ChiangMai_tourist.ts, ylim = c(0, 250), ylab = "Value", 
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(2019, 2024.25), main = "")
axis(1, at = seq(2019, 2024, 1), labels = format(seq(2019, 2024, 1)))
plot(predict_nonlinear_tourist, lwd = 2, col = "green4", ylab = "Value", 
     xlab = "Date")
abline(v=2023.15, lwd=2, col="red")
lines(predict_nonlinear_tourist$fitted, col=2)

## ARIMA model
# Differencing 
ndiffs(ChiangMai_tourist.ts) # Result is 0, but it did not pass the ADF unit root test, so perform first differencing on the seasonal term
nChiangMai_tourist.ts <- diff(ChiangMai_tourist.ts, 12) 
nChiangMai_tourist.ts <- diff(nChiangMai_tourist.ts, 1) 
ndiffs(nChiangMai_tourist.ts)

# Perform unit root test on the data again 
ADF <- adf.test(nChiangMai_tourist.ts)
# p=0.01<0.05, therefore reject the null hypothesis, no unit root exists, the first difference makes the time series stationary

# Plot autocorrelation graph 
acf(ChiangMai_tourist.ts, main="Autocorrelation", xlab="Lag")

# Plot partial autocorrelation graph 
pacf(ChiangMai_tourist.ts, main="Partial ACF", xlab="Lag")
# Based on the characteristics of the autocorrelation and partial autocorrelation graphs, it can be determined that p is approximately 1.

# Automatic parameter tuning and comparison with previously determined orders 
arima1 <- auto.arima(ChiangMai_tourist.ts) 
summary(arima1)
confint(arima1)

# Residual tests for the model
Box.test(arima1$residuals, type='Ljung-Box')

# Fit using the ARIMA function
fit <- Arima(ChiangMai_tourist.ts, order = c(0, 1, 0), seasonal = c(1, 1, 0))
# Forecast data for the next year
predict_arima_ChiangMai_tourist <- forecast(fit, 12)
summary(fit)

par(mfrow = c(1, 1))
plot(ChiangMai_tourist.ts, ylim = c(0, 20), ylab = "Value", 
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(2019, 2024.25), main = "")
axis(1, at = seq(2019, 2024, 1), labels = format(seq(2019, 2024, 1)))
plot(predict_arima_ChiangMai_tourist, lwd = 2, col = "blue", ylab = "Value", 
     xlab = "Date")
abline(v=2023.15, lwd=2, col="red")
lines(predict_arima_ChiangMai_tourist$fitted, col=2)

# Compare prediction results
linear.season <- as.numeric(accuracy(predict_line_tourist)[2]) 
quadratic.season <- as.numeric(accuracy(predict_nonlinear_tourist)[2])
arima.season <- as.numeric(accuracy(predict_arima_ChiangMai_tourist)[2])

RMSE_all <- cbind(linear.season, quadratic.season, arima.season)
RMSE_all


########################### Profit #################################
thailand_net_profit_all.data <- subset(thailand.data, thailand.data$variable == "net_profit_all")
# Select Chiang Mai as an example
ChiangMai_profit.data <- subset(thailand_net_profit_all.data,
                                 thailand_net_profit_all.data$province_eng == "Chiang Mai")
ChiangMai_profit.data <- ChiangMai_profit.data[, c("date", "value")]

# Sort by date
ChiangMai_profit.data <- ChiangMai_profit.data[order(ChiangMai_profit.data$date),]

# Time series conversion
ChiangMai_profit.ts <- ts(ChiangMai_profit.data$value,
                           start = c(2019, 1), end = c(2023, 2), frequency = 12)
# Since the value is too large, log normalize the value in advance
ChiangMai_profit.ts <- log(ChiangMai_profit.ts)

# Plot time series and autocorrelation graph to observe data 
pic1 <- plot.ts(ChiangMai_profit.ts)

# White noise test 
Box.test(ChiangMai_profit.ts, type='Ljung-Box')

# Smoothing 
opar <- par(no.readonly=TRUE) 
par(mfrow=c(2,2))

ylim <- c(min(ChiangMai_profit.ts), max(ChiangMai_profit.ts)) 
plot(ChiangMai_profit.ts, main="Raw time series") 
plot(ma(ChiangMai_profit.ts, 2), main="Simple Moving Averages (k=2)", ylim=ylim)
plot(ma(ChiangMai_profit.ts, 5), main="Simple Moving Averages (k=5)", ylim=ylim) 
plot(ma(ChiangMai_profit.ts, 10), main="Simple Moving Averages (k=10)", ylim=ylim) 
par(opar)

# Determine if there is seasonality 
ChiangMai_profit.stl <- stl(ChiangMai_profit.ts, s.window = "periodic") 
plot(ChiangMai_profit.stl)

# Linear trend model
# Add seasonal term
linear_model_seasonal <- tslm(ChiangMai_profit.ts ~ trend + season) 
linear_season_lm_pred <- forecast(linear_model_seasonal, level = 0) 
plot(ChiangMai_profit.ts, xlab = "Date", ylab = "Value") 
lines(linear_season_lm_pred$fitted, lwd = 2, col = "blue") 
summary(linear_model_seasonal)

# Model prediction
predict_line_profit <- forecast(linear_model_seasonal, level = 0)

par(mfrow = c(1, 1))
plot(ChiangMai_profit.ts, ylim = c(0, 13), ylab = "Value", 
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(2019, 2024.25), main = "")
axis(1, at = seq(2019, 2024, 1), labels = format(seq(2019, 2024, 1)))
plot(predict_line_profit, lwd = 2, col = "blue4", ylab = "Value", 
     xlab = "Date")
abline(v=2023.15, lwd=2, col="red")
lines(predict_line_profit$fitted, col=2)

# Non-linear trend model 
nonlinear_model_seasonal <- tslm(ChiangMai_profit.ts ~ trend + I(trend^2) + season) 
nonlinear_season_lm_pred <- forecast(nonlinear_model_seasonal, level = 0) 
plot(ChiangMai_profit.ts, xlab = "Date", ylab = "Value") 
lines(nonlinear_season_lm_pred$fitted, lwd = 2, col = "green2", ylab = "Value", 
      xlab = "Date") 
summary(nonlinear_model_seasonal)

# Non-linear model prediction
predict_nonlinear_profit <- forecast(nonlinear_model_seasonal, level = 0)

par(mfrow = c(1, 1))
plot(ChiangMai_profit.ts, ylim = c(0, 13), ylab = "Value", 
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(2019, 2024.25), main = "")
axis(1, at = seq(2019, 2024, 1), labels = format(seq(2019, 2024, 1)))
plot(predict_nonlinear_profit, lwd = 2, col = "green4", ylab = "Value", 
     xlab = "Date")
abline(v=2023.15, lwd=2, col="red")
lines(predict_nonlinear_profit$fitted, col=2)

## ARIMA model
# Differencing 
ndiffs(ChiangMai_profit.ts) # Result is 0, but it did not pass the ADF unit root test, so perform first differencing on the seasonal term
nChiangMai_profit.ts <- diff(ChiangMai_profit.ts, 12) 
ndiffs(nChiangMai_profit.ts)

# Perform unit root test on the data again 
ADF <- adf.test(nChiangMai_profit.ts)
# p=0.01<0.05, therefore reject the null hypothesis, no unit root exists, the first difference makes the time series stationary

# Plot autocorrelation graph 
acf(ChiangMai_profit.ts, main="Autocorrelation", xlab="Lag")

# Plot partial autocorrelation graph 
pacf(ChiangMai_profit.ts, main="Partial ACF", xlab="Lag")
# Based on the characteristics of the autocorrelation and partial autocorrelation graphs, it can be determined that p is approximately 1.

# Automatic parameter tuning and comparison with previously determined orders 
arima1 <- auto.arima(ChiangMai_profit.ts) 
summary(arima1)
confint(arima1)

# Residual tests for the model
Box.test(arima1$residuals, type='Ljung-Box')

# Fit using the ARIMA function
fit <- Arima(ChiangMai_profit.ts, order = c(0, 0, 0), seasonal = c(0, 0, 1))
# Forecast data for the next year
predict_arima_ChiangMai_profit <- forecast(fit, 12)
summary(fit)

par(mfrow = c(1, 1))
plot(ChiangMai_profit.ts, ylim = c(0, 20), ylab = "Value", 
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(2019, 2024.25), main = "")
axis(1, at = seq(2019, 2024, 1), labels = format(seq(2019, 2024, 1)))
plot(predict_arima_ChiangMai_profit, lwd = 2, col = "blue", ylab = "Value", 
     xlab = "Date")
abline(v=2023.15, lwd=2, col="red")
lines(predict_arima_ChiangMai_profit$fitted, col=2)

# Compare prediction results
linear.season <- as.numeric(accuracy(predict_line_profit)[2]) 
quadratic.season <- as.numeric(accuracy(predict_nonlinear_profit)[2])
arima.season <- as.numeric(accuracy(predict_arima_ChiangMai_profit)[2])

RMSE_all <- cbind(linear.season, quadratic.season, arima.season)
RMSE_all

# Split training and testing sets and recheck the optimal model — non-linear model with seasonal terms
nValid <- 20
nTrain <- length(ChiangMai_occupancy.ts) - nValid

###### Occupancy #######
occupancy_train.ts <- window(ChiangMai_occupancy.ts, start=c(2019, 1), end=c(2019, nTrain))
occupancy_valid.ts <- window(ChiangMai_occupancy.ts, start=c(2019, nTrain + 1), end=c(2019, 50))
# Add seasonal term
nonlinear_model_seasonal <- tslm(occupancy_train.ts ~ trend + I(trend^2) + season) 
nonlinear_season_lm_pred <- forecast(nonlinear_model_seasonal, level = 0) 
# Forecasting
predict_nonlinear_occupancy <- forecast(nonlinear_model_seasonal, h=nValid, level = 0)
plot(predict_nonlinear_occupancy, lwd = 2, col = "green4", ylab = "Value", 
     xlab = "Date")
lines(occupancy_valid.ts, lwd = 2, col="red")

###### Tourist #######
tourist_train.ts <- window(ChiangMai_tourist.ts, start=c(2019, 1), end=c(2019, nTrain))
tourist_valid.ts <- window(ChiangMai_tourist.ts, start=c(2019, nTrain + 1), end=c(2019, 50))
# Add seasonal term
nonlinear_model_seasonal <- tslm(tourist_train.ts ~ trend + I(trend^2) + season) 
nonlinear_season_lm_pred <- forecast(nonlinear_model_seasonal, level = 0) 
# Forecasting
predict_nonlinear_tourist <- forecast(nonlinear_model_seasonal, h=nValid, level = 0)
plot(predict_nonlinear_tourist, lwd = 2, col = "green4", ylab = "Value", 
     xlab = "Date")
lines(tourist_valid.ts, lwd = 2, col="red")

###### Profit #######
profit_train.ts <- window(ChiangMai_profit.ts, start=c(2019, 1), end=c(2019, nTrain))
profit_valid.ts <- window(ChiangMai_profit.ts, start=c(2019, nTrain + 1), end=c(2019, 50))
# Add seasonal term
nonlinear_model_seasonal <- tslm(profit_train.ts ~ trend + I(trend^2) + season) 
nonlinear_season_lm_pred <- forecast(nonlinear_model_seasonal, level = 0) 
# Forecasting
predict_nonlinear_profit <- forecast(nonlinear_model_seasonal, h=nValid, level = 0)
plot(predict_nonlinear_profit, lwd = 2, col = "green4", ylab = "Value", 
     xlab = "Date")
lines(profit_valid.ts, lwd = 2, col="red")


