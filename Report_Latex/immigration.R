############################################################
### Time Series Analysis
# Load data
app <- read.csv('immigration_apprehended.csv')
dep <- read.csv('immigration_deported.csv')

##############################
## Time series plot of immigrants apprehended
# Dataset is backwards, need to reverse it
year.num <- rev(app[,1])
num <- rev(app[,2])

# Plot time series
plot(year.num, num, type='l', xlab='Year', ylab='Apprehended', lwd=2,
     cex.axis=1.2, cex.lab=1.2,
     main='Undocumented Immigrants Apprehended in the U.S.')

# Check stationarity
par(mfrow=c(1,2))
acf(num)
pacf(num)

# Check for constant variance
# Load library for Box-Cox lambda
library(forecast)
# Check lambda value. If lambda=0, a log transformation is needed
# Log transformation  needed
BoxCox.lambda(num) # -0.139
# Apply log transformation
lognum <- log(num)

# Check for for constant mean
# Load library for Augmented Dickey-Fuller unit-root test
library(tseries)
adf.test(lognum) # Data is not stationary (has a unit root)
dlognum <- diff(lognum) # Take the difference
par(mfrow=c(1,1))
plot(year.num[-1], dlognum, type='l', xlab='Year', ylab='Diff. Log. Apprehended',
     main='Undocumented Immigrants Apprehended in the U.S.')
par(mfrow=c(1,2))
acf(dlognum)
pacf(dlognum)
adf.test(dlognum) # Data is stationary

##############################
### Time series plot of immigrants deported
year.dep <- rev(dep[,1])
rem <- rev(dep[,2])
ret <- rev(dep[,3])
# Remove NA in return column
ret <- na.omit(ret)

## Plot time series
par(mfrow=c(1,1))
plot(year.dep, rem, type='l', xlab='Year', ylab='Deported', lwd=2,
     cex.axis=1.2, cex.lab=1.2, xlim=range(year.dep), ylim=range(c(rem, ret)),
     main='Undocumented Immigrants Removed from the U.S.')
lines(year.dep[36:128], ret, type='l', col='blue', lwd=2)
legend("topleft", legend=c("Removed", "Retured"), col=c('black', 'blue'),
       lty=1:1)

## Check stationarity
par(mfrow=c(1,2))
# Removed data series
acf(rem)
pacf(rem)
# Returned data series
acf(ret)
pacf(ret)

# Check for for constant variance
BoxCox.lambda(rem) # 0.172
BoxCox.lambda(ret) # -0.042 - Need to apply log
# Apply log transformation
logrem <- log(rem)
logret <- log(ret)

# Check for constant mean
# Augmented Dickey-Fuller unit root test
adf.test(logrem) # Data is not stationary (has a unit root)
adf.test(logret) # Data is not stationary (has a unit root)

# Difference the data
dlogrem <- diff(logrem)
dlogret <- diff(logret)
par(mfrow=c(1,2))
acf(dlogrem)
pacf(dlogrem)
acf(dlogret)
pacf(dlogret)

# Check again
adf.test(dlogrem) # Data is stationary
adf.test(dlogret) # Data is stationary

##############################
### Determine Model Orders
# Load TSA library
library(TSA)
eacf(dlognum) # ARMA(1,11), MA(12)
eacf(dlogrem) # ARMA(3,3)
eacf(dlogret) # AR(1)

##############################
## Build models
# Apprehended dataset
num.out1 <- arima(dlognum, order = c(1, 0, 11))
num.out1
num.out2 <- arima(dlognum, order = c(0, 0, 12))
num.out2
# Removed dataset
rem.out <- arima(dlogrem, order = c(3, 0, 3))
rem.out
# Returned dataset
ret.out <- arima(dlogret, order = c(1, 0, 0))
ret.out

##############################
## Residual analysis
Box.test(num.out1$residuals, lag=12, type="Ljung") # Residual series is white noise
Box.test(num.out2$residuals, lag=12, type="Ljung") # Residual series is white noise
Box.test(rem.out$residuals, lag=12, type="Ljung") # Residual series is white noise
Box.test(ret.out$residuals, lag=12, type="Ljung") # Residual series is white noise

##############################
## Check significance
# Load lmtest library
library(lmtest)
# Immigrants apprehended
# ARMA(1, 11)
coeftest(num.out1) # Model selected
num.out1.1 <- arima(dlognum, order = c(1, 0, 11),
                    fixed = c(0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, NA, NA))
coeftest(num.out1.1)
# MA(12)
coeftest(num.out2)
num.out2.1 <- arima(dlognum, order = c(0, 0, 12),
                    fixed = c(NA, 0, 0, 0, 0, NA, 0, 0, 0, 0, NA, 0, NA))
coeftest(num.out2.1)
# Check AIC
num.out1$aic
num.out1.1$aic
num.out2$aic
num.out2.1$aic # Model selected

# Immigrants removed
coeftest(rem.out)
rem.out1 <- arima(dlogrem, order = c(3, 0, 3),
                  fixed = c(0, NA, 0, 0, NA, 0, 0))
coeftest(rem.out1) # Variables aren't significant now
rem.out2 <- arima(dlogrem, order = c(3, 1, 3))
coeftest(rem.out2)
rem.out3 <- arima(dlogrem, order = c(3, 1, 3),
                  fixed = c(0, 0, 0, 0, NA, 0))
coeftest(rem.out3)
# Check AIC
rem.out$aic # Model selected
rem.out1$aic
# Second check after root test
rem.out2$aic
rem.out3$aic
# AIC values are higher after differencing again
# Original model will still be used

# Immigrants returned
coeftest(ret.out)
ret.out1 <- arima(dlogret, order = c(1, 0, 0), fixed = c(NA, 0))
coeftest(ret.out1)
# Check AIC
ret.out$aic
ret.out1$aic # Model selected

############################################################
### Model Diagnostics
##############################
## Stationarity & Model Redundancy Check
# MA(12) model for apprehended immigrants is automatically stationary

# ARMA(3,3) model for removed immigrants
rem.out$coef
polyroot(c(1, -rem.out$coef[1:3]))
abs(polyroot(c(1, -rem.out$coef[1:3]))) # Model is close to one. Needs to be differenced.
# Original model still considered
polyroot(c(1, rem.out$coef[4:6])) # No root matches

# AR(1) model for returned immigrants
polyroot(c(1, -ret.out1$coef[1]))
abs(polyroot(c(1, -ret.out1$coef[1]))) # Model is stationary

##############################
## Residual Analysis Check
# MA(12) model for apprehended immigrants
par(mfrow=c(1,1))
plot(year.num[-95], num.out2.1$residuals, type='l',
     main="Residual Analysis of Apprehended Immigrants")
num.se <- sqrt(num.out2.1$sigma2) # Standard deviation of error
cf.num <- 2*num.se # Confidence interval
abline(h=cf.num, lty=2) # Upper confidence interval
abline(h=-cf.num, lty=2) # Lower confidence interval
identify(year.num[-95], num.out2.1$residuals) # 9, 19, 30 selected
# Check again outliers
library(tsoutliers)
locate.outliers(num.out2.1$residuals, par=coefs2poly(num.out2.1)) # No significant outliers identified.
# Box-Ljung test
Box.test(num.out2.1$residuals, lag=12, type="Ljung") # Residuals are not white noise. Outliers present.
# ACF and PACF
par(mfrow=c(1,2))
acf(num.out2.1$residuals)
pacf(num.out2.1$residuals)

# ARMA(3,3) model for removed immigrants
par(mfrow=c(1,1))
plot(year.dep[-128], rem.out$residuals, type='l',
     main="Residual Analysis of Removed Immigrants")
rem.se <- sqrt(rem.out$sigma2) # Standard deviation of error
cf.rem <- 2*rem.se # Confidence interval
abline(h=cf.rem, lty=2) # Upper confidence interval
abline(h=-cf.rem, lty=2) # Lower confidence interval
identify(year.dep[-128], rem.out$residuals) # 26, 58, 63 selected
# Check again for outliers
locate.outliers(rem.out$residuals, par=coefs2poly(rem.out))
# Box-Ljung test
Box.test(rem.out$residuals, lag=12, type="Ljung") # Residuals are not white noise. Outliers present.
# ACF and PACF
par(mfrow=c(1,2))
acf(rem.out$residuals)
pacf(rem.out$residuals)

# AR(1) model for returned immigrants
par(mfrow=c(1,1))
plot(year.dep[36:127], ret.out1$residuals, type='l',
     main="Residual Analysis of Returned Immigrants")
ret.se <- sqrt(ret.out1$sigma2) # Standard deviation of error
cf.ret <- 2*ret.se # Confidence interval
abline(h=cf.ret, lty=2) # Upper confidence interval
abline(h=-cf.ret, lty=2) # Lower confidence interval
identify(year.dep[36:127], ret.out1$residuals) # 3, 16, 17, 27, 28 selected
# Check again for outliers
locate.outliers(ret.out1$residuals, par=coefs2poly(ret.out1))
# Box-Ljung test
Box.test(ret.out1$residuals, lag=12, type="Ljung") # Residuals are not white noise. Outliers present.
# ACF and PACF
par(mfrow=c(1,2))
acf(ret.out1$residuals)
pacf(ret.out1$residuals)


############################################################
### Forecasting - 5 Years
##############################
## APPREHENDED IMMIGRANTS
# Add back the difference order
# Check ARIMA(1,1,11)
num.out2.2 <- arima(lognum, order = c(1, 1, 11))
coeftest(num.out2.2)
# Check ARIMA(0,1,12)
num.out2.3 <- arima(lognum, order = c(0, 1, 12))
coeftest(num.out2.3) # No significant variables
# Check ARIMA(1,1,11) with only significant variables
num.out2.4 <- arima(lognum, order = c(1, 1, 11),
                    fixed = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA))
coeftest(num.out2.4)
# Check AIC
num.out2.2$aic
num.out2.3$aic
num.out2.4$aic # Model selected

# Format the model to not predict the last 5
lognum.mod <- lognum[c(-5, -4, -3, -2, -1)]
num.out2.5 <- arima(lognum.mod, order = c(1, 1, 11),
                    fixed = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA))

# Variable setup
pp <- predict(num.out2.5, 5) # Predict the last 5 known years
nn <- length(lognum.mod) # Length of your data
nt <- 5	# Forecast horizon
nb <- 20 # Number of data points you want to plot
tt <- (nn-nb):length(num) # Indexes of data points you want to plot
xxx <- num[tt] # Data you want to plot
pred <- exp(pp$pred)
pred.upper <- exp(pp$pred+2*pp$se) # Upper bound for prediction
pred.lower <- exp(pp$pred-2*pp$se) # Lower bound for prediction
rr <- range(c(xxx, pp$pred, pred.upper,
              pred.lower)) # Find the minimum and maximum y values in your plot

# Plot the data and prediction
par(mfrow=c(1,1))
plot(year.num[tt], xxx, pch=1, xlim=c(year.num[nn-nb], year.num[nn+nt]),
     ylim=rr, main='Five Year Apprehended Forecast',
     ylab='Number Apprehended', xlab='Year')
lines(year.num[tt], xxx) # Observed values
points(year.num[nn+1:nt], pred, pch=2, col='red', type='o') # Predicted values
lines(year.num[nn+1:nt], pred.upper, lty=2, col='red') # Upper bound of predicted interval
lines(year.num[nn+1:nt], pred.lower, lty=2, col='red')	# Lower bound of predicted interval
points(year.num[nn+1:nt], num[91:95], pch=1, col='black', type='o') # Add the last 5 points
legend.text = c("Actual Value", "Prediction")
legend("topleft", legend.text, col=1:2, pch=1:2, lty=rep(1,2))

##############################
## REMOVED IMMIGRANTS
# Add back the difference order
# Check ARIMA(3,1,3)
rem.out2 <- arima(logrem, order = c(3, 1, 3))
coeftest(rem.out2)
# Check ARIMA(3,1,3) with only significant variables
rem.out3 <- arima(logrem, order = c(3, 1, 3),
                  fixed = c(0, NA, 0, 0, NA, 0)) # No significant variables
coeftest(rem.out3)
# Check AIC
rem.out2$aic # Model selected
rem.out3$aic

# Format the model to not predict the last 5
logrem.mod <- logrem[c(-5, -4, -3, -2, -1)]
rem.out4 <- arima(logrem.mod, order = c(3, 1, 3),
                  fixed = c(0, NA, 0, 0, NA, 0))

# Variable setup
pp <- predict(rem.out4, 5) # Predict the last 5 known years
nn <- length(logrem.mod) # Length of your data
nt <- 5	# Forecast horizon
nb <- 20 # Number of data points you want to plot
tt <- (nn-nb):length(rem) # Indexes of data points you want to plot
xxx <- rem[tt]	# Data you want to plot
pred <- exp(pp$pred)
pred.upper <- exp(pp$pred+2*pp$se) # Upper bound for prediction
pred.lower <- exp(pp$pred-2*pp$se) # Lower bound for prediction
rr <- range(c(xxx, pred, pred.upper,
              pred.lower)) # Find the minimum and maximum y values in your plot

# Plot the data and prediction
plot(year.dep[tt], xxx, pch=1, xlim=c(year.dep[nn-nb], year.dep[nn+nt]),
     ylim=rr, main='Five Year Deported (Removed) Forecast',
     ylab='Number Removed', xlab='Year')
lines(year.dep[tt], xxx) # Observed values
points(year.dep[nn+1:nt], pred, pch=2, col='red', type='o') # Predicted values
lines(year.dep[nn+1:nt], pred.upper, lty=2, col='red') # Upper bound of predicted interval
lines(year.dep[nn+1:nt], pred.lower, lty=2, col='red') # Lower bound of predicted interval

##############################
## RETURNED IMMIGRANTS
# Add back the difference order
# Check ARIMA(3,1,3)
ret.out2 <- arima(logret, order = c(1, 1, 0))
coeftest(ret.out2) # Already has all the significant variables

# Format the model to not predict the last 5
logret.mod <- logret[c(-5, -4, -3, -2, -1)]
ret.out3 <- arima(logret.mod, order = c(1, 1, 0))

# Variable setup
pp <- predict(ret.out3, 5) # Predict the last 5 known years
nn <- length(logret.mod) # Length of your data
nt <- 5	# Forecast horizon
nb <- 20 # Number of data points you want to plot
tt <- (nn-nb):length(ret) # Indexes of data points you want to plot
xxx <- ret[tt]	# Data you want to plot
pred <- exp(pp$pred) # Prediction
pred.upp <- exp(pp$pred+2*pp$se)  # Upper bound for prediction
pred.low <- exp(pp$pred-2*pp$se)  # Lower bound for prediction
rr <- range(c(xxx, pred, pred.upp, pred.low)) # Find the minimum and maximum y values in your plot

# Plot the data and prediction
plot(year.dep[tt], xxx, pch=1, xlim=c(year.dep[nn-nb], year.dep[nn+nt]),
     ylim=rr, main='Five Year Deported (Returned) Forecast',
     ylab='Number Returned', xlab='Year')
lines(year.dep[tt], xxx)	# Observed values
points(year.dep[nn+1:nt], pred, pch=2, col='red', type='o')	# Predicted values
lines(year.dep[nn+1:nt], pred.upp, lty=2, col='red')	# Upper bound of predicted interval
lines(year.dep[nn+1:nt], pred.low, lty=2, col='red')	# Lower bound of predicted interval

############################################################
### Intervention Events
## Apprehended immigrants - No outliers to observe intervention events

##############################
## Removed Immigrants - Operation Wetback
# Prepare variables
rem1 <- 63 # Location point of outlier
x <- ts(rem, start=1892, frequency=1)
x1 <- ts(rem[1:rem1], start=1892, frequency=1)
lx <- log(x) # With outlier
n <- length(x)

# Temporary intervention
rem.tmp <- ts(c(rep(0, rem1), 1, rep(0, n-rem1-1)), start=1892, frequency=1)
# Permanent intervention
rem.perm <- ts(c(rep(0, rem1), rep(1, n-rem1)), start=1892, frequency=1)
# ARIMA(3,1,3) order
rem.out # Model
order <- c(3,1,3)

# No intervention
rem00 <- arimax(lx, order=order, method="ML")
rem00
# Permanent intervention
rem01 <- arimax(lx, order=order, xtransf=data.frame(rem.perm),
                transfer=list(c(0,0)), method="ML")
rem01
# Temporary intervention
rem10 <- arimax(lx, order=order, xtransf=data.frame(rem.tmp),
                transfer=list(c(1,0)), method="ML")
rem10
# Temporary & permanent intervention
rem11 <- arimax(lx, order=order, xtransf=data.frame(rem.tmp, rem.perm),
                transfer=list(c(1,0), c(0,0)), method="ML")
rem11 # Intervention selected

# Select the lowest AIC for the intervention event
aic <- c(rem00$aic, rem01$aic, rem10$aic, rem11$aic)
model <- list(rem00, rem01, rem10, rem11)
which.min(aic) # 4
model[which.min(aic)] # rem11

# Total impact
tc <- filter(rem.tmp, filter=rem11$coef[7], method='recursive', side=1) * rem11$coef[8] # Temporary change
ls <- ts(c(rep(0,rem1), rep(rem11$coef[9], n-rem1)), start=c(1892), frequency=1) # Level shift
plot(tc+ls, xlim=c(1952, 1960), type='l', main="Intervention Effect of Removed Immigrants")

##############################
## Returned immigrants
# Prepare variables
ret1 <- 28 # Location point of outlier
x <- ts(ret, start=1927, frequency=1)
x1 <- ts(ret[1:ret1], start=1927, frequency=1)
lx <- log(x) # With outlier
n <- length(x)

# Temporary intervention
ret.tmp <- ts(c(rep(0, ret1), 1, rep(0, n-ret1-1)), start=1927, frequency=1)
# Permanent intervention
ret.perm <- ts(c(rep(0, ret1), rep(1, n-ret1)), start=1927, frequency=1)
# ARIMA(3,1,3) order
ret.out1 # Model
order <- c(1,1,0)

# No intervention
ret00 <- arimax(lx, order=order, method="ML")
ret00
# Permanent intervention
ret01 <- arimax(lx, order=order, xtransf=data.frame(ret.perm),
                transfer=list(c(0,0)), method="ML")
ret01 # Intervention selected
# Temporary intervention
ret10 <- arimax(lx, order=order, xtransf=data.frame(ret.tmp),
                transfer=list(c(1,0)), method="ML")
ret10
# Temporary & permanent intervention
ret11 <- arimax(lx, order=order, xtransf=data.frame(ret.tmp, ret.perm),
                transfer=list(c(1,0), c(0,0)), method="ML")
ret11

# Select the lowest AIC for the intervention event
aic <- c(ret00$aic, ret01$aic, ret10$aic, ret11$aic)
model <- list(ret00, ret01, ret10, ret11)
which.min(aic) # 2
model[which.min(aic)] # ret01

# Total impact
tc <- 0
ls <- ts(c(rep(0,ret1), rep(ret01$coef[2], n-ret1)), start=c(1927), frequency=1) # Level shift
plot(tc+ls, xlim=c(1952, 1957), type='l', main="Intervention Effect of Returned Immigrants")

############################################################
### Multivariate Time Series
library(vars)
# Adjust variables and remove NA rows
# This data subset should have a value for each of the variable at a year
app <- app[1:93,]
num <- rev(app[,2])
dep <- dep[1:93,]
rem <- rev(dep[,2])
ret <- rev(dep[,3])
x <- cbind(num, rem, ret)

# Vector Autoregressive Model - VAR
var <- VAR(x, p=1)
summary(var)
# Plot the impulse response from each variable
imp <- irf(var)
plot(imp)
# Determine p
VARselect(x, lag.max=6) # p = 1 selected
# Portmanteau test
serial.test(var) # Residuals are white noise
