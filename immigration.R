#Install and Load packages
install.packages(c("forecast", "tseries", "TSA", "lmtest", "tsoutliers", "vars", "ggplot2"))

library(forecast)
library(tseries)
library(TSA)
library(lmtest)
library(tsoutliers)
library(vars)
library(ggplot2)

############################################################
### Time Series Analysis
# Load data
app <- read.csv('immigration_apprehended.csv')
dep <- read.csv('immigration_deported.csv')

#######################################
###Presidential Analysis

#Add President column to data frame
app['President'] <- NA
app['President'][app['Year'] >= 2017] <- "Trump"
app['President'][app['Year'] >= 2009 & app['Year'] <= 2016] <- "Obama"
app['President'][app['Year'] >= 2001 & app['Year'] <= 2008] <- "Bush Jr"
app['President'][app['Year'] >= 1993 & app['Year'] <= 2000] <- "Clinton"
app['President'][app['Year'] >= 1989 & app['Year'] <= 1992] <- "Bush Sr"
app['President'][app['Year'] >= 1981 & app['Year'] <= 1988] <- "Reagan"
app['President'][app['Year'] >= 1977 & app['Year'] <= 1980] <- "Carter"
app['President'][app['Year'] >= 1974 & app['Year'] <= 1976] <- "Ford"
app['President'][app['Year'] >= 1969 & app['Year'] <= 1973] <- "Nixon"
app['President'][app['Year'] >= 1963 & app['Year'] <= 1968] <- "Johnson"
app['President'][app['Year'] >= 1961 & app['Year'] <= 1962] <- "Kennedy"
app['President'][app['Year'] >= 1953 & app['Year'] <= 1960] <- "Eisenhower"
app['President'][app['Year'] >= 1945 & app['Year'] <= 1952] <- "Truman"
app['President'][app['Year'] >= 1933 & app['Year'] <= 1944] <- "Roosevelt"
app['President'][app['Year'] >= 1929 & app['Year'] <= 1932] <- "Hoover"
app['President'][app['Year'] >= 1923 & app['Year'] <= 1928] <- "Coolidge"

app$President <- as.factor(app$President)

#Apprehended plot colored by president
ggplot(data = app, aes(x =Year, y= Number)) +
        aes(group=NA) +
        geom_line(aes(color=President)) +
        xlab('Year') +
        ylab('Apprehended') +
        ggtitle('Undocumented Immigrants Apprehended in the U.S.') +
        scale_color_discrete(breaks=c("Coolidge","Hoover","Roosevelt","Truman","Eisenhower","Kennedy","Johnson",
                                      "Nixon","Ford","Carter","Reagan","Bush Sr","Clinton","Bush Jr","Obama","Trump"))

#Add President Party Column to data frame
app$PresidentParty <- NA
republicanPresidents <- c("Trump", "Bush Jr", "Bush Sr", "Reagan", "Ford", "Nixon","Eisenhower","Hoover","Coolidge")
app$PresidentParty[app$President %in% republicanPresidents] = "Republican"
app$PresidentParty[!(app$President %in% republicanPresidents)] = "Democrat"

app$PresidentParty <- as.factor(app$PresidentParty)

#Apprehended plot colored by President Party
ggplot(data = app, aes(x =Year, y= Number)) +
        aes(group=NA) +
        geom_line(aes(color=PresidentParty)) +
        scale_color_manual(name="President Party",breaks = c("Republican", "Democrat"), 
                           values=c("red", "blue")) +
        xlab('Year') +
        ylab('Apprehended') +
        ggtitle('Undocumented Immigrants Apprehended in the U.S. by Political Party')

#Basic statistics of apprehended, removed, and returned
mean(app$Number)
mean(dep$Removals)
mean(dep$Returns, na.rm=TRUE)

median(app$Number)
median(dep$Removals)
median(dep$Returns, na.rm=TRUE)


sd(app$Number)
sd(dep$Removals)
sd(dep$Returns, na.rm=TRUE)

mean(app$Number[app$PresidentParty=="Republican"])
mean(app$Number[app$PresidentParty=="Democrat"])

before2000 = dep[dep$Year<2000,]
after2000 = dep[dep$Year>=2000,]

mean(before2000$Removals)
mean(after2000$Removals)


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
BoxCox.lambda(num) # -0.139 - Need to apply log
# Apply log transformation
lognum <- log(num)

# Check for for constant mean
# Load library for Augmented Dickey-Fuller unit-root test
library(tseries)
adf.test(lognum) # Data is not stationary (has a unit root)
dlognum <- diff(lognum) # Take the difference
par(mfrow=c(1,1))
plot(year.num[-1], dlognum, type='l', xlab='Year', lwd=2,
     cex.axis=1.2, cex.lab=1.2, ylab='Diff. Log. Apprehended',
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
legend("topleft", legend=c("Removed", "Returned"), col=c('black', 'blue'),
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
BoxCox.lambda(rem) # 0.172 - Need to apply log
BoxCox.lambda(ret) # -0.042 - Need to apply log
# Apply log transformation
logrem <- log(rem)
logret <- log(ret)

# Difference the data
dlogrem <- diff(logrem)
dlogret <- diff(logret)
par(mfrow=c(1,1))

# Check for constant mean
# Augmented Dickey-Fuller unit root test
adf.test(logrem) # Data is not stationary (has a unit root)
adf.test(logret) # Data is not stationary (has a unit root)

# Difference the data
dlogrem <- diff(logrem)
dlogret <- diff(logret)

# Time series plot
par(mfrow=c(1,1))
plot(year.dep[-1], dlogrem, type='l', xlab='Year', lwd=2,
     cex.axis=1.2, cex.lab=1.2, xlim=range(year.dep),
     ylim=range(c(dlogrem, dlogret)),
     ylab='Diff. Log. Apprehended',
     main='Undocumented Immigrants Apprehended in the U.S.')
lines(year.dep[36:128][-1], dlogret, type='l', col='blue', lwd=2)
legend("bottomleft", legend=c("Removed", "Retured"), col=c('black', 'blue'),
       lty=1:1)
# ACF & PACF
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
coeftest(num.out1)
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
# Get rid of the last 5 years
new_lognum = head(lognum,-5) #length: 90
new_logret = head(logret,-5)#length: 88
new_logrem = head(logrem,-5) #length: 123

## APPREHENDED IMMIGRANTS
# Add back the difference order
# Check ARIMA(1,1,11)
num.out2.2 <- arima(new_lognum, order = c(1, 1, 11)) #aic 44.25
coeftest(num.out2.2) #ma 6 and ma11 are significant
# Check ARIMA(0,1,12)
num.out2.3 <- arima(new_lognum, order = c(0, 1, 12)) #aic 44.58
coeftest(num.out2.3) # ma1, ma6, ma11 are significant 
# Check ARIMA(1,1,11) with only significant variables
#num.out2.4 <- arima(lognum, order = c(1, 1, 11),
# fixed = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA))
num.out.2.4.1 <- arima(new_lognum, order = c(1, 1, 11),
                       fixed = c(0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, NA)) #aic 45.11
# check ARIMA(0,1,12) with only significant variables
num.out.2.5 <- arima(new_lognum, order = c(0, 1, 12),
                     fixed = c(NA, 0, 0, 0, 0, NA, 0, 0, 0, 0, NA, 0)) #aic 32.6

#num.out2.6 <- arima(new_lognum, order = c(12, 1, 0),
# fixed = c(NA, 0, 0, 0, 0, NA, 0, 0, 0, 0, NA, 0)) #aic 32.6

coeftest(num.out2.4)
coeftest(num.out2.6)
# Check AIC
num.out2.2$aic
num.out2.3$aic
num.out2.4.1$aic 
num.out.2.5$aic # Smallest aic, Model selected

# Variable setup 
pp_a <- predict(num.out.2.5, 5) # Predict the last 5 known years
nn_a <- length(new_lognum)
nt <- 5	# Forecast horizon
nb <- 20 # Number of data points you want to plot
tt_a <- (nn_a-nb):length(num) # Indexes of data points you want to plot
xxx_a <- num[tt_a] # Data you wannt to plot
pred_a <- exp(pp_a$pred)
pred.upper_a <- exp(pp_a$pred+2*pp_a$se) # Upper bound for prediction
pred.lower_a <- exp(pp_a$pred-2*pp_a$se) # Lower bound for prediction
rr_a <- range(c(xxx_a, pp_a$pred, pred.upper_a,
                pred.lower_a)) # Find the minimum and maximum y values in your plot

# Plot the data and prediction
plot(year.num[tt_a], xxx_a, pch=1, xlim=c(year.num[nn_a-nb], year.num[nn_a+nt]),
     ylim=rr_a, main='Five Year Apprehended Forecast',
     ylab='Number Apprehended', xlab='Year')
lines(year.num[tt_a], xxx_a) # Observed values_
points(year.num[nn_a+1:nt], pred_a, pch=2, col='red', type='o') # Predicted values
lines(year.num[nn_a+1:nt], pred.upper_a, lty=2, col='red') # Upper bound of predicted interval
lines(year.num[nn_a+1:nt], pred.lower_a, lty=2, col='red')	# Lower bound of predicted interval
points(year.num[nn_a+1:nt], num[91:95], pch=1, col='black', type='o') # Add the last 5 points
legend.text = c("Actual Value", "Prediction")
legend("topleft", legend.text, col=1:2, pch=1:2, lty=rep(1,2))

## REMOVED IMMIGRANTS
# Add back the difference order
# Check ARIMA(3,1,3)
rem.out2 <-arima(new_logrem, order = c(3,1,3), fixed = c(0,NA, NA, NA, NA, 0))
coeftest(rem.out2) #ar2 and ma2 are significant
# Check ARIMA(3,1,3) with only significant variables
rem.out3 <- arima(new_logrem, order = c(3, 1, 3),
                  fixed = c(0, NA, 0, 0, NA, 0)) # No significant variables
coeftest(rem.out3)
# Check AIC
rem.out2$aic # Model selected
rem.out3$aic

# Variable setup
pp <- predict(rem.out2, 5)
nn <- length(new_logrem) # Length of your data
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
legend.text = c("Actual Value", "Prediction")
legend("bottomleft", legend.text, col=1:2, pch=1:2, lty=rep(1,2))

## RETURNED IMMIGRANTS
# Add back the difference order
# Check ARIMA(1,1,0)
ret.out2 <- arima(new_logret, order = c(1,1,0))
coeftest(ret.out2) # Already has all the significant variables

# Variable setup
pp <- predict(ret.out2,5)
nn <- length(new_logret)
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
legend.text = c("Actual Value", "Prediction")
legend("bottomleft", legend.text, col=1:2, pch=1:2, lty=rep(1,2))

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

