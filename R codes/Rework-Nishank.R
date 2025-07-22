library(readxl)
library(fpp)
library(forecast)
mu <- read_excel("Documents/IPBA/capstone/Formatted Data.xlsx", 
                 sheet = "Monthly ")
mu<-mu[-c(61:72),]
###load dataset -  monthly in 'mu'and follow the codes below.
View(mu)
muts<-mu[,5]
muts<-ts(muts,frequency = 12, start = c(2015,4))

plot.ts(muts)

dm<-decompose(muts, type = "multiplicative")
plot(dm)

##############################
#Simple Exopnential Smoothing
es <- ses(muts, h=6)
plot(es) #plotting the forecast

summary(es) #summary of SES
#alpha is = 0.9999 (higher alpha means recent observations are given more weightage while forecasting)
#therefore data exhibits serial correlation


#forecast accuracy
#MAE: 1982809 - Scale Dependant - i.e. it's impossible to compare TS on different scales
#RMSE: 2514603 - Scale Dependant - i.e. it's impossible to compare TS on different scales
#MAPE: 12.4126 - Ideal should be less than 7 or 7.3
#MASE: 1.95847 - around 1 means the actual forecast does ok with out of sample data. 
                  # Here it means the forecast wasn't good

accuracy(es)
#MAPE: 12.4126 - Ideal should be less than 7 or 7.3

#Residuals check
#residuals should be uncorrelated - to test accuracy of the model
#residuals mean should be close to 0, else forecast is biased - to test accuracy of the model
#residuals should have a constant variance - for testing prediction intervals
#residuals are normally distributed - for testing prediction intervals

plot.ts(es$residuals) #the residuals maybe showing a seasonality


#Ljung-Box Test: is the p value is small, i.e. <0.05, data is not white noise)
#here H0 - Data is IID
#H1 - Data is not IID and that it exhibits serial correlation

Box.test(es$residuals, lag=20, type="Ljung-Box")
# here X-squared = 79.775, df = 20, p-value = 6.373e-06
#p value definitely smaller than 0.05

#autocorrelation function (acf plot)
acf(es$residuals)
#less than 95% spikes are inside blue line, hence data seems correlated


#plotting forecast Errors as a Histogra+Bell Curve
#to run this command, select from 'plotforecasterrors' below all the way to last bracket

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


plotForecastErrors(es$residuals)

#here data seems slightly skewed, i.e. trend & seasonality were not captured
#so we move to holt's model


##############################
#Holt 's Model - Used when there is Trend######

hol <- holt(muts, h=10)
plot(hol)   
#image/graph seems same as SES in this step

summary(hol)
##      AIC     AICc      BIC     #AIC,BIC denote goodness of fit - show how much of info is lost
##  2025.019  2026.130  2035.491  # model with least AIC or BIC value is preferred
### Error measures:
###                  ME    RMSE     MAE       MPE     MAPE     MASE      ACF1
###Training set -117889.4 2532333 1992272 -2.039415 12.50441 1.967817 0.1219838

checkresiduals(hol)
#p value is 5.402e-07 therefore data exhibits serial correlation

#Ljung-Box test

#data:  Residuals from Holt's method
#Q* = 44.119, df = 8, p-value = 5.402e-07

#Model df: 4.   Total lags used: 12
#here p-value is less than 0.05, therefore the data exhibits serial autocorrelation
#histogram not symmetric, 
#ACF curve - seems close to ok, but some values too close to blue line or cross it
#ACF curve - shows seasonality


###### HOLT WINTERS MODEL ######

muhw <- HoltWinters(muts, beta = FALSE, gamma = FALSE) #single exp smoo. HW

muhw
## Smoothing parameters:
##alpha: 0.9999339
##beta : FALSE
##gamma: FALSE

plot(muhw)
#the red line is the fitted values after using the holt-winters function

#check forecast for next 6 months:
muhw2<-forecast:::forecast.HoltWinters(muhw,h=6)
forecast:::plot.forecast(muhw2)

Box.test(muhw2$residuals, lag = 20, type="Ljung-Box")
#Box-Ljung test

#data:  muhw2$residuals
#X-squared = 70.657, df = 20, p-value = 1.422e-07

Acf(muhw2$residuals, lag.max = 20)
#correlation coefficients are still outside the range

plot.ts(muhw2$residuals)
#pattern seen, model did not do a good job

hist((muhw2$residuals), col = "red", frequency = FALSE)


##########
#Holt Winters - Double

muhw3<- HoltWinters(muts, gamma = FALSE)
muhw3
#alpha: 1 - therefore the data has not been smoothen at all

plot(muhw3)
#in this plot, the fitted spikes are higher and the dips are lower than observed data

muhw4 <- forecast:::forecast.HoltWinters(muhw3,h=6)
forecast:::plot.forecast(muhw4)
#this seems to have really picked up from the downward trend

Box.test(muhw4$residuals, lag = 20, type="Ljung-Box")
#Box-Ljung test

#data:  muhw4$residuals
#X-squared = 44.021, df = 20, p-value = 0.001495

Acf(muhw4$residuals, lag.max = 20)
#correlation coefficients are still outside the range

plot.ts(muhw4$residuals)
#pattern seen, closer to noise but still, model did not do a good job

hist((muhw4$residuals), col = "red", frequency = FALSE)

###
#HoltWinters - Triple

muhwt<-HoltWinters(muts)
plot(muhwt)

muhwt
#Alpha here is 0, Beta is 0 - older time points given importance, 
#Gamma is 0.63 i.e. importance given to both old and new seasonality components

muhwt1 <- hw (muts, h=6, seasonal = "multiplicative")
muhwt1
plot(muhwt1)

muhwf<-forecast:::forecast.HoltWinters(muhwt, h=6)
plot(muhwf)

Box.test(muhwf$residuals, lag = 20, type="Ljung-Box")
#Box-Ljung test

#data:  muhwf$residuals
#X-squared = 27.474, df = 20, p-value = 0.1225
#p-value is not significant therefore the residuals do not exhibit any serial autocorrelation

Acf(muhwf$residuals, lag.max = 20)
#only 1 correlation coefficient is outside the range

plot.ts(muhwf$residuals)
#no pattern seen, model seems to do a better job than previous models

hist((muhwf$residuals), col = "red", frequency = FALSE)

accuracy(muhwf)
#                   ME    RMSE      MAE        MPE     MAPE      MASE        ACF1
#Training set 62736.71 1223188 841961.7 -0.4147942 4.959681 0.8316266 -0.03859394
#good accuracy as MAPE is under 7 at 4.959681


####
#ETS 

etstrain<-window(muts, start=c(2015,4), end = c(2018,4), frequency = 12)
etstest<-window(muts, start=c(2018,4), end = c(2020,4), frequency = 12)

tr <- ets(etstrain)
Test <- ets(etstest, model=tr)
accuracy(Test)
#                  ME     RMSE      MAE        MPE     MAPE     MASE       ACF1
#Training set -78065.55 934083.3 618233.5 -0.7876467 3.906534 0.433474 -0.1573769

accuracy(forecast(tr,30),etstest)
#                    ME      RMSE       MAE        MPE     MAPE     MASE       ACF1 Theil's U
#Training set -140993.2  727153.7  592558.5 -1.023266 3.712373 0.5945795 0.03198602        NA
#Test set     -764144.2 1683383.6 1243467.3 -5.822847 7.954044 1.2477083 0.14097841 0.7040632

#############################
########### ARIMA ###########NEEDS SOME WORK########
#############################

# Check for stationarity using ADF Test
#in ADF Test, Fickey Fuller Test Statistic should be around -12.513 and p value around 0.01
adf.test(muts)
#Dickey-Fuller = -4.8008, Lag order = 3, p-value = 0.01

mutsdiff1 <- diff(muts, differences = 1)
plot.ts(mutsdiff1)
adf.test(mutsdiff1)
#Dickey-Fuller = -4.956, Lag order = 3, p-value = 0.01
# the plot doesn't look completely white noise
    # and DF value is far from -12, so we take one more difference

mutsdiff2 <- diff(muts, differences = 2)
plot.ts(mutsdiff2)
adf.test(mutsdiff2)
#Dickey-Fuller = -6.1392, Lag order = 3, p-value = 0.01
# the plot looks like white noise
  # and DF value is closer to -12, , so we will try to take one more difference

mutsdiff3 <- diff(muts, differences = 3)
plot.ts(mutsdiff3)
adf.test(mutsdiff3)
#Dickey-Fuller = -7.7565, Lag order = 3, p-value = 0.01
# The plot looks like white noise,
  # and the DF value is closer to -12 but more close to previous value
  # so we'll stick to d=2 for now for ARIMA model candidate

# so let's look at ACF Plot
acf(diff(mutsdiff2, lag.max = 20))
acf(diff(mutsdiff3, lag.max = 20))
#there is a sinewave form and reduction in spikes is after lag 2, hence p=2

# so let's look at PACF Plot
pacf(diff(mutsdiff2, lag.max = 20))
pacf(diff(mutsdiff3, lag.max = 20))
#in both plots there's no sine wave, hence q=0

#therefore it seems we can select either order (2,3,0) or (2,2,0)

muarima <- Arima(y = muts, order = c(2,3,0))
muarima
accuracy(muarima)

## seems like we will have to use auto.arima here as we did not get a good accuracy
auto.arima(muts)

## turns out the suggestions are:
#ARIMA(0,0,0)(0,1,0)[12] with drift 
#Coefficients:
  #drift
#51756.80
#s.e.  16228.21

#sigma^2 estimated as 1.859e+12:  log likelihood=-745.63
#AIC=1495.26   AICc=1495.53   BIC=1499

######