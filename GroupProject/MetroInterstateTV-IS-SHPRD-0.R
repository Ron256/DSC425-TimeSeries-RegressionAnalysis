# load the necessary packages
#library(ggplot2)
#library(ggfortify)  # for autoplot
#library(forecast)   # for the "ma" moving average function

library(fBasics)    # for basic time series statistics

library(tidyverse)
library(lubridate) # for mdy date conversion
#library(dplyr)
#library(TSA)
#library(fUnitRoots)
library(ggplot2)
library(ggfortify)
library(forecast)
library(lmtest)
library(fUnitRoots)
library(tseries)
# library(fpp2)
library(TSA)



################################################################################
#
# load the data set
################################################################################

# set working directory
# setwd("C:/Users/REJALU/OneDrive - DePaul University/DSC425/GroupProject")
setwd("C:/Users/REJALU/OneDrive - DePaul University/DSC425/GroupProject")
source("eacf.R")
source("Backtest.R")

metroIS = read.csv("Metro_Interstate_Traffic_Volume.csv")
count(metroIS)
head(metroIS, 1200)
tail(metroIS)

# checking for null values
colSums(is.na(metroIS))

# check the datatype of the date field
class(metroIS$date_time)

# convert the date_time to the date class object
metroIS$tDate_time <- as.POSIXct(metroIS$date_time, TZ=Sys.timezone())
class(metroIS$tDate_time)

range(metroIS$tDate_time)

################################################################################
# Data processing
#################################################################################


# Checking for duplicates:

metroIS_nodups <- 
  metroIS %>%
    distinct() # remove duplicate rows based on all columns. 

# derive the weekly dates
metroIS_nodups <- metroIS_nodups %>%  mutate(weekdate = as.Date(cut(tDate_time, "week"), "%Y-%m-%d"))
                          
# 2016-12-06 18:00:00, light snow, 90
###############################################################################################################

# Using the separate function from the tidyr package
metroIS_monthly <- tidyr::separate(metroIS_nodups, date_time, c("date", "time"), sep = " ")
monthly_metroIS_traffic <- tidyr::separate(metroIS_monthly, date, c("year", "month", "day"), sep = "-")


# Deriving the daily traffic volume and daily average temperature. 
##############################################################################################################
options(scipen=999) # turn off the exponential format
summarized_metroIS_traffic_weekly <- monthly_metroIS_traffic %>% 
  group_by(weekdate) %>%  # here, I am using year_week
  summarise(weeklytrafficVolume = sum(traffic_volume), average_temp=mean(temp), .groups = 'drop')

#summarized_metroIS_traffic_weekly <- summarized_metroIS_traffic_weekly %>%
#  mutate(weeklydate=floor_date(year_w, week, 1)) #%>%
 # select(-year) %>% # remove year column
  #select(-month) %>%# remove month column from the data set.
  #select (-day)


range(summarized_metroIS_traffic_weekly$weekdate)
class(summarized_metroIS_traffic_weekly$weekdate)

# create a weekly ts object
# dailytrafficVolumeTs = ts(summarized_metroIS_traffic_daily$date, start=2012, end=2018, frequency =365)
weeklytrafficVolumeTs = ts(summarized_metroIS_traffic_weekly$weeklytrafficVolume, start=2012, end=2018, frequency =52)


# Daily Average temperature
weeklyAvgTempTs = ts(summarized_metroIS_traffic_weekly$average_temp, start=2012, end=2018, frequency =52)

# Step 1 plot the time series data vs time:
######################################################################################################################################


# daily traffic volume
autoplot(weeklytrafficVolumeTs, 
         xlab="Time", 
         ylab="traffic volume", 
         main = "Metro InterState weekly Traffic Volume Time Series Plot") + 
  theme_bw() +
  theme(text=element_text(size=15)) + 
  theme(plot.title = element_text(hjust = 0.5))

mean(weeklytrafficVolumeTs)

# weekly average temperature
autoplot(weeklyAvgTempTs, 
         xlab="Time", 
         ylab="average temp", 
         main = "Metro InterState weekly Average Temperature Time Series Plot") + 
  theme_bw() +
  theme(text=element_text(size=15)) + 
  theme(plot.title = element_text(hjust = 0.5))

###################################################################################################################################
# Step 2 Check normality 

###################################################################################################################################
# Determining normality of the two series
###################################################################################################################################

# Determining the normality of weeklytrafficVolumeTs
## qq plot
par(mfrow=c(1,1))
qqnorm(weeklytrafficVolumeTs, pch = 1, frame = FALSE) + theme_classic() +
  qqline(weeklytrafficVolumeTs, col = "#d95f02", lwd = 2) 

# We can also plot the histogram with a normal distribution overlain
# compute the mean
m = mean(summarized_metroIS_traffic_weekly$weeklytrafficVolume)
# compute the standard deviation
s = sd(summarized_metroIS_traffic_weekly$weeklytrafficVolume)

ggplot(summarized_metroIS_traffic_weekly, aes(weeklytrafficVolume)) + 
  geom_histogram(aes(y = ..density..), col="black", fill="#2c7fb8", bins=25) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = s), col="#d95f02", size=1.2) + 
  theme_bw() +
  xlab("rate") + ggtitle("Distribution of weekly traffic volume") + 
  theme(text = element_text(size=20)) + 
  theme(plot.title = element_text(size=20, hjust = 0.5))


skewness(weeklytrafficVolumeTs) 
# So, here we would not reject the null hypothesis that traffic volume is normally distributed.
#We have sufficient evidence to show that traffic volume is not normally distributed
kurtosis(weeklytrafficVolumeTs)
# Kurtosis is less than three, which explains why traffic volume is not normally distributed.
# Using the jarque-Bera test
normalTest(weeklytrafficVolumeTs, method="jb", na.rm=FALSE)
# The test statistic is 53.83 and the p-value is less 0.05 so we would reject 
# the null hypothesis that traffic volume is normally distributed.

######################################################################################################################################################
# Determining the normality of daily average temperature.
#####################################################################################################################################################

qqnorm(weeklyAvgTempTs, pch = 1, frame = FALSE) + theme_classic() +
  qqline(weeklyAvgTempTs, col = "#d95f02", lwd = 2) 

# We can also plot the histogram with a normal distribution overlain
# compute the mean
m = mean(summarized_metroIS_traffic_weekly$average_temp)
# compute the standard deviation
s = sd(summarized_metroIS_traffic_weekly$average_temp)

ggplot(summarized_metroIS_traffic_weekly, aes(average_temp)) + 
  geom_histogram(aes(y = ..density..), col="black", fill="#2c7fb8", bins=25) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = s), col="#d95f02", size=1.2) + 
  theme_bw() +
  xlab("rate") + ggtitle("Distribution of weekly average temperature") + 
  theme(text = element_text(size=20)) + 
  theme(plot.title = element_text(size=20, hjust = 0.5))



skewness(weeklyAvgTempTs) 
# So, here we would not reject the null hypothesis that traffic volume is normally distributed.
#We have sufficient evidence to show that traffic volume is not normally distributed
kurtosis(weeklytrafficVolumeTs)
# Kurtosis is less than three, which explains why traffic volume is not normally distributed.
# Using the jarque-Bera test
normalTest(weeklytrafficVolumeTs, method="jb", na.rm=FALSE)
# The test statistic is 53.83 and the p-value is less 0.05 so we would reject 
# the null hypothesis that weekly average temperature is normally distributed.

# transform the data
####################################################################################################################################
# lets try to transform the data to increase normality

summarized_metroIS_traffic_weekly <- summarized_metroIS_traffic_weekly %>% 
  select (weeklytrafficVolume, average_temp, weekdate) %>%
  mutate(log_weeklytrafficVolume=log(weeklytrafficVolume), log_avgtemp= log(average_temp))

# create ts objects for the different transformed data
log_weeklytrafficVolume_ts = ts(summarized_metroIS_traffic_weekly$log_weeklytrafficVolume, start=2012, end=2018, frequency =52)


# weekly Average temperature
log_weeklyAvgTemp_ts = ts(summarized_metroIS_traffic_weekly$log_avgtemp, start=2012, end=2018, frequency =52)

# log of weekly traffic volume
autoplot(log_weeklytrafficVolume_ts, 
         xlab="Time", 
         ylab="log of dailytrafficvolume", 
         main = "Metro InterState log of weekly traffic volume Time Series Plot") + 
  theme_bw() +
  theme(text=element_text(size=15)) + 
  theme(plot.title = element_text(hjust = 0.5))

# log of weekly average temperature
autoplot(log_weeklyAvgTemp_ts, 
         xlab="Time", 
         ylab="log of daily average temp", 
         main = "Metro InterState log of weekly Average Temperature Time Series Plot") + 
  theme_bw() +
  theme(text=element_text(size=15)) + 
  theme(plot.title = element_text(hjust = 0.5))


# check the normality of the transformed variable of log_weeklytrafficvolume, log_dailytrafficvolume.

qqnorm(log_weeklytrafficVolume_ts, pch = 1, frame = FALSE) + theme_classic() +
  qqline(log_weeklytrafficVolume_ts, col = "#d95f02", lwd = 2) 

# We can also plot the histogram with a normal distribution overlain
# compute the mean
m = mean(summarized_metroIS_traffic_weekly$log_weeklytrafficVolume)
# compute the standard deviation
s = sd(summarized_metroIS_traffic_weekly$log_weeklytrafficVolume)

ggplot(summarized_metroIS_traffic_weekly, aes(log_weeklytrafficVolume)) + 
  geom_histogram(aes(y = ..density..), col="black", fill="#2c7fb8", bins=25) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = s), col="#d95f02", size=1.2) + 
  theme_bw() +
  xlab("rate") + ggtitle("Distribution of log weekly traffic volume") + 
  theme(text = element_text(size=20)) + 
  theme(plot.title = element_text(size=20, hjust = 0.5))


skewness(log_weeklytrafficVolume_ts) 
# So, here we would not reject the null hypothesis that traffic volume is normally distributed.
#We have sufficient evidence to show that traffic volume is not normally distributed
kurtosis(log_weeklytrafficVolume_ts)
# Kurtosis is less than three, which explains why traffic volume is not normally distributed.
# Using the jarque-Bera test
normalTest(log_weeklytrafficVolume_ts, method="jb", na.rm=FALSE)
# The test statistic is 31887 and the p-value is less 0.05 so we would reject 
# the null hypothesis that traffic volume is normally distributed.

# check the normality of the transformed variable of the daily average temperature, log_weeklyAvgTemp.

qqnorm(log_weeklyAvgTemp_ts, pch = 1, frame = FALSE) + theme_classic() +
  qqline(log_weeklyAvgTemp_ts, col = "#d95f02", lwd = 2) 

# We can also plot the histogram with a normal distribution overlain
# compute the mean
m = mean(summarized_metroIS_traffic_weekly$log_avgtemp)
# compute the standard deviation
s = sd(summarized_metroIS_traffic_weekly$log_avgtemp)

ggplot(summarized_metroIS_traffic_weekly, aes(log_avgtemp)) + 
  geom_histogram(aes(y = ..density..), col="black", fill="#2c7fb8", bins=25) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = s), col="#d95f02", size=1.2) + 
  theme_bw() +
  xlab("rate") + ggtitle("Distribution of log weekly average temperature") + 
  theme(text = element_text(size=20)) + 
  theme(plot.title = element_text(size=20, hjust = 0.5))


skewness(log_weeklyAvgTemp_ts) 
# So, here we would not reject the null hypothesis that traffic volume is normally distributed.
#We have sufficient evidence to show that traffic volume is not normally distributed
kurtosis(log_weeklyAvgTemp_ts)
# Kurtosis is less than three, which explains why traffic volume is not normally distributed.
# Using the jarque-Bera test
normalTest(log_weeklyAvgTemp_ts, method="jb", na.rm=FALSE)
# The test statistic is 15.82 and the p-value is less 0.05 so we would reject 
# the null hypothesis that traffic colume is normally distributed.

# transform the weeklytraffic volume using the box cox transformation.

# apply the Box cox transformation to the daily traffic volume
weeklytrafficVolTs.transform <- TSA::BoxCox.ar(weeklytrafficVolumeTs, method="yule-walker")
weeklytrafficVolTs.transform$ci
par(mfrow=c(1,2))
lambda=1.15
weeklytrafficVolTs.box = (weeklytrafficVolumeTs^lambda - 1)/lambda
plot(weeklytrafficVolumeTs, type='l', ylab ='count for Original time series object')
plot(weeklytrafficVolTs.box, type='l', ylab ="count of transformed ts series")
mtext("Before and After applying the Box-Cox transformation to the time series object of the weekly traffic volume", at=par("usr")[1]+0.05*diff(par("usr")[1:2]), cex=1.3, line=1.5) 



# check the normality of the transformed variable, weeklytrafficVolTs.box
qqnorm(weeklytrafficVolTs.box, pch = 1, frame = FALSE) + theme_classic() +
  qqline(weeklytrafficVolTs.box, col = "#d95f02", lwd = 2) 

## we need to create a data frame
m = mean(weeklytrafficVolTs.box)
# compute the standard deviation
s = sd(weeklytrafficVolTs.box)

df <- data.frame(weeklytrafficVol_box=weeklytrafficVolTs.box)

ggplot(df, aes(weeklytrafficVol_box)) + 
  geom_histogram(aes(y = ..density..), col="black", fill="#2c7fb8", bins=25) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = s), col="#d95f02", size=1.2) + 
  theme_bw() +
  xlab("rate") + ggtitle("Distribution of log average temperature") + 
  theme(text = element_text(size=20)) + 
  theme(plot.title = element_text(size=20, hjust = 0.5))


skewness(weeklytrafficVolTs.box)
kurtosis(weeklytrafficVolTs.box)

normalTest(weeklytrafficVolTs.box, method="jb", na.rm=FALSE)

#######################################################################################################################################
# Step 3 : Check Stationarity

# daily traffic volume
par(mfrow=c(2,3))
acf(weeklytrafficVolTs.box)
Acf(weeklytrafficVolTs.box, lag.max=30)
pacf(weeklytrafficVolTs.box) # either have two or three patterns of short tall short spikes, which indicates a seasonal AR2 term
eacf(weeklytrafficVolTs.box) # there is some seasonality so we a difference

autoplot(weeklytrafficVolTs.box)
adfTest(weeklytrafficVolTs.box, type="nc")  # fail to reject non-stationarity (zero-mean)
adfTest(weeklytrafficVolTs.box, type="c") # regression with a constant, no time trend: reject non stationarity,
adfTest(weeklytrafficVolTs.box, type="ct") # reject non stationarity, Is this random walk with drift? , does this mean trend-stationary?
kpss.test(weeklytrafficVolTs.box, null="Trend")# reject the null hypothesis of stationarity, so first differencing is required. 
#kpss.test(weeklytrafficVolTs.box) # we reject stationarity, this means that the series are non-stationary.
kpss.test(weeklytrafficVolTs.box, null="Level")
aTSA::adf.test(weeklytrafficVolTs.box)
autoplot(diff(weeklytrafficVolTs.box))

par(mfrow=c(1,2))
Acf(diff(weeklytrafficVolTs.box)) # Sign of over differencing
pacf(diff(weeklytrafficVolTs.box))


eacf(diff(weeklytrafficVolTs.box)) # tRY MA1, MA2, AR1MA1| AR1, MA2
aTSA::adf.test(diff(weeklytrafficVolTs.box)) # we reject non-stationarity
kpss.test(diff(weeklytrafficVolTs.box), null="Trend") # we fail to reject the null hypothesis of stationarity implying that the series are stationary. 
# testing for the existance of a drift
t.test(diff(weeklytrafficVolTs.box))

# determining MA behavior
Acf(diff(diff(weeklytrafficVolTs.box), 52)) # Sign of over-differencing but there is an MA behavior going on
pacf(diff(diff(weeklytrafficVolTs.box), 52))
eacf(diff(diff(weeklytrafficVolTs.box), 52)) # we shall MA2, MA1

###############################################################################################

# Running only the seasonal
# try MA2 behavior
fit1 = Arima(weeklytrafficVolTs.box, order=c(0, 0, 0), seasonal=list(order=c(2, 0, 0), seasonal=52), include.drift = TRUE)
fit1
lmtest::coeftest(fit1)

Acf(fit1$residuals, lag.max=30)
Box.test(fit1$residuals, type='Ljung') # we  reject white noise implying that there is more to capture. 

############################################################################################

fit1 = Arima(weeklytrafficVolTs.box, order=c(0, 1, 1), seasonal=list(order=c(2, 1, 0), seasonal=52), include.drift = TRUE)
fit1
lmtest::coeftest(fit1)

Acf(fit1$residuals, lag.max=30)
Box.test(fit1$residuals, type='Ljung') # we reject the null hypothesis, there is still more information to capture

fit1 = Arima(weeklytrafficVolTs.box, order=c(0, 1, 1), seasonal=list(order=c(2, 0, 0), seasonal=52), include.drift = TRUE)
fit1
lmtest::coeftest(fit1)

Acf(fit1$residuals, lag.max=30)
Box.test(fit1$residuals, type='Ljung')

# try MA2 behavior
fit1 = Arima(weeklytrafficVolTs.box, order=c(0, 1, 1), seasonal=list(order=c(0, 0, 2), seasonal=52), include.drift = TRUE)
fit1
lmtest::coeftest(fit1)

Acf(fit1$residuals, lag.max=30)
Box.test(fit1$residuals, type='Ljung') # we fail to reject white noise



fit1_noDrift = Arima(weeklytrafficVolTs.box, order=c(0, 1, 1), seasonal=list(order=c(1, 0, 0), seasonal=52))
fit1_noDrift

lmtest::coeftest(fit1_noDrift)
Acf(fit1_noDrift$residuals, lag.max = 30)

fit1 = Arima(weeklytrafficVolTs.box, order=c(0, 1, 2), seasonal=list(order=c(2, 1, 0), seasonal=52), include.drift = TRUE)
fit1
lmtest::coeftest(fit1)

Acf(fit1$residuals, lag.max=30)
pacf(fit1$residuals, lag.max=30)
Box.test(fit1$residuals, type='Ljung') # we fail to reject but we have some outside the confidence interval.
###############################################################################

fit1 = Arima(weeklytrafficVolTs.box, order=c(0, 1, 1), seasonal=list(order=c(0, 0, 3), seasonal=52))
fit1
lmtest::coeftest(fit1)

Acf(fit1$residuals, lag.max=30)
pacf(fit1$residuals, lag.max=30)
Box.test(fit1$residuals, type='Ljung') # we fail to reject but we have some outside the confidence interval.

###############################################################################
fitA = auto.arima(weeklytrafficVolTs.box)
fitA

fit1 = Arima(weeklytrafficVolTs.box, order=c(0, 1, 2), seasonal=list(order=c(1, 0, 0), seasonal=52))
fit1
lmtest::coeftest(fit1)

Acf(fit1$residuals, lag.max=30)
pacf(fit1$residuals, lag.max=30)
Box.test(fit1$residuals, type='Ljung')


fit_auto = auto.arima(weeklytrafficVolTs.box)
fit_auto
lmtest::coeftest(fit_auto)


fit1Lm = lm(weeklytrafficVolTs.box~ time(weeklytrafficVolumeTs))
summary(fit1Lm)

Acf(fit1Lm$residuals)
pacf(fit1Lm$residuals)


#fit1a= Arima(weeklytrafficVolTs.box, xreg=time(weeklytrafficVolTs.box), order=c(2,0,0))
#fit1b
#coeftest(fit1b)

par(mfrow=c(1,1))
Acf(fit1$residuals)
Box.test(fit1$residuals, type="Ljung")



##################################################################

# Weekly Traffic volume
plot(decompose(weeklytrafficVolumeTs))
autoplot(weeklytrafficVolumeTs)
par(mfrow=c(2,2))
# Acf(weeklytrafficVolumeTs)
par(mfrow=c(2,2))
Acf(weeklytrafficVolumeTs, lag.max = 100)
pacf(weeklytrafficVolumeTs)


eacf(weeklytrafficVolumeTs)
adfTest(weeklytrafficVolumeTs, type="ct") # we reject non stationary; this is trend - stationary
kpss.test(weeklytrafficVolumeTs, null="Trend") # we reject stationarity, implying that first differencing is required.

autoplot(diff(weeklytrafficVolumeTs))
par(mfrow=c(2,2))
autoplot(diff(weeklytrafficVolumeTs))
Acf(diff(weeklytrafficVolumeTs)) # sign of over-differencing
Acf(diff(weeklytrafficVolumeTs), lag.max=30)
pacf(diff(weeklytrafficVolumeTs)) # MA decays to zero, may be MA4 or 5 in the seasonal term

eacf(diff(weeklytrafficVolumeTs)) #Try  MA1, MA2, MA4, MA5, 
eacf(diff(weeklytrafficVolumeTs), ma.max = 52) # we can see that our seasonanility is week.
#auto.arima(weeklytrafficVolumeTs)
adfTest(diff(weeklytrafficVolumeTs), type="ct") # we reject non-stationarity.
kpss.test(diff(weeklytrafficVolumeTs), null="Trend") # we fail to reject stationarity, then the series are stationary.
eacf(diff(weeklytrafficVolumeTs))

par(mfrow=c(1,2))
# determine the seasonal order
Acf(diff(diff(weeklytrafficVolumeTs), 52))
pacf(diff(diff(weeklytrafficVolumeTs), 52)) # We can try AR1, AR2 on the seasonal side. 
eacf(diff(diff(weeklytrafficVolumeTs), 52))

t.test(diff(weeklytrafficVolumeTs)) # there is a drift.

# non-seasonal order ARIMA(0,1,1) AND seasonal order AR1
fitA = Arima(weeklytrafficVolumeTs, order=c(0,1,1), seasonal = list(order=c(1, 0, 0), seasonal=52), include.drift = TRUE)
fitA

lmtest::coeftest(fitA)
Acf(fitA$residuals)
Box.test(fitA$residuals, type="Ljung") # we  reject NULL hypothesis: there is more infomartion to capture

# non-seasonal order ARIMA(0,1,2) AND seasonal order AR2
fitB = Arima(weeklytrafficVolumeTs, order=c(0,1,2), seasonal = list(order=c(2, 0, 0), seasonal=52), include.drift = TRUE)
fitB
lmtest::coeftest(fitB)
Box.test(fitB$residuals, type="Ljung", lag=50) # we fail to reject white noise

# non-seasonal order ARIMA(0,1,2) AND seasonal order AR2
fitB = Arima(weeklytrafficVolumeTs, order=c(0,1,2), seasonal = list(order=c(2, 0, 0), seasonal=52), xreg=time(weeklytrafficVolumeTs))
fitB
lmtest::coeftest(fitB)
Box.test(fitB$residuals, type="Ljung", lag=50)


# non-seasonal order ARIMA(0,1,4) AND seasonal order AR2
fitC = arima(weeklytrafficVolumeTs, order=c(0,1,4),  seasonal = list(order=c(2, 0, 0), seasonal=52))
fitC
lmtest::coeftest(fitC)
par(mfrow=c(1,2))
Acf(fitC$residuals)



Box.test(fitC$residuals, type="Ljung", lag=30) # we fail to reject white noise

# non-seasonal order ARIMA(0,1,5) AND seasonal order AR2
fitD = Arima(weeklytrafficVolumeTs, order=c(0,1,5), seasonal = list(order=c(2, 0, 0), seasonal=52), include.drift = TRUE)
fitD
lmtest::coeftest(fitD)
Box.test(fitD$residuals, type="Ljung", lag=50) # we fail to reject white noise

par(mfrow=c(2,2))
Acf(fitB$residuals)
Acf(fitA$residuals)
Acf(fitC$residuals)
Acf(fitD$residuals)

# fitting the model with auto arima.
fit_AIC = auto.arima(weeklytrafficVolumeTs, ic="aic")
fit_AIC
coeftest(fit_AIC)


# fitting the model with auto arima.
fit2_AIC = auto.arima(weeklytrafficVolumeTs, ic="aic", allowdrift=F)
fit2_AIC
coeftest(fit2_AIC)



fit_BIC = auto.arima(weeklytrafficVolumeTs, ic="bic")
fit_BIC

par(mfrow=c(2,2))
Acf(fit_AIC$residuals, main="fit_AIC residuals with drift")
Acf(fit2_AIC$residuals)
Acf(fit_BIC$residuals)

# evaluating model performance:
# we choose a number of elements to test and as a window size. 
# So, here we take the length of the series and multiply it by point eight and that
# gives us 65%
ntest=0.65 *length(weeklytrafficVolumeTs) # validation set is 65% of entire series
# let's do a back test and see what we will get.
pmA1 = backtest(fitA, weeklytrafficVolumeTs, orig=ntest, h=1)
pmB1 = backtest(fitB, weeklytrafficVolumeTs,  h=1)
pmC1 = backtest(fitC, weeklytrafficVolumeTs, h=1, orig=0.65*length(weeklytrafficVolumeTs))
pmD1 = backtest(fitD, weeklytrafficVolumeTs, orig=0.65*length(weeklytrafficVolumeTs, h=1))
pm_AIC =  backtest(fit_AIC, weeklytrafficVolumeTs, orig=0.65*length(weeklytrafficVolumeTs), h=1)
pm2_AIC = backtest(fit2_AIC, weeklytrafficVolumeTs, orig=0.65*length(weeklytrafficVolumeTs), h=1)
pm_BIC = backtest(fit_BIC, weeklytrafficVolumeTs,orig=0.65*length(weeklytrafficVolumeTs, h=1))

print(paste("M1's model Mean Absolute Percentage Error: ", round(pm1$mape, 3), sep=" "))
print(paste("M2's model Mean Absolute Percentage Error: ", round(pm2$mape, 3), sep=" "))

# deriving predictions 
par(mfrow=c(1,1))
plot(forecast(fit_BIC, h=50))
mean(weeklytrafficVolumeTs)


###############################################################################################################
# The effect of average temperature on traffic volume
data=cbind(trafficVolume=weeklytrafficVolumeTs, AvgTemp=weeklyAvgTempTs)
head(data)
df.data=data.frame(time(weeklytrafficVolumeTs),weeklytrafficVolumeTs, weeklyAvgTempTs)
colnames(df.data) <- c('weeklydates', 'trafficVolume', 'avgTemp')
autoplot(data, facets=T)



# Now, let's try a regression
fitLm = lm(weeklytrafficVolumeTs ~ time(weeklytrafficVolumeTs))
summary(fitLm)
par(mfrow=c(2,2))
acf(fitLm$residuals)
Acf(fitLm$residuals, lag.max = 30) # Looks like descending behaviour ... maybe not much MA 
# (not much in the way of strong single ma behavior) 
# maybe an AR
pacf(fitLM$residuals)# likely to be an AR1 or ARMA process for the residuals
fit1 = Arima(weeklytrafficVolumeTs, xreg=time(weeklytrafficVolumeTs), order=c(0,0,0))
fit1
coeftest(fit1)

par(mfrow=c(1,1))
Acf(fit1$residuals)
Box.test(fit1$residuals, type="Ljung")


fit2 = Arima(weeklytrafficVolumeTs, xreg=time(weeklytrafficVolumeTs), order=c(2, 0, 0))
fit2

coeftest(fit2)
par(mfrow=c(1,1))
Acf(fit1$residuals)
Box.test(fit1$residuals, type="Ljung")

adfTest(fitLM$residuals, type="nc")  # Rejects unit root
kpss.test(fitLM$residuals, null="Level")  # Cannot reject stationarity

# So, fit an arima with an x-regressor
fit4 = auto.arima(weeklytrafficVolumeTs, xreg=time(weeklytrafficVolumeTs))
fit4

# forecast 
autoplot(forecast(fit4, xreg=2015:2018))

###################################################################################################
dev.off()
# Trend- Staionarity
fit1=lm(weeklytrafficVolumeTs ~ weeklyAvgTempTs)
fit1

plot(fit1$residuals, main="residual plot")
autoplot(ts(fit1$residuals),
         main = "Residuals Time plot") + 
  theme_bw() +
  theme(text=element_text(size=15)) + 
  theme(plot.title = element_text(hjust = 0.5))

s = ts(cbind(weeklytrafficVolumeTs, weeklyAvgTempTs), class="mts")
autoplot(s)
# OLS regression
fit2 = lm(formula= weeklytrafficVolumeTs ~ weeklyAvgTempTs + time(weeklyAvgTempTs))
coeftest(fit2)
summary(fit2)


##################################################################################
library(astsa)
s= ts(cbind(weeklyAvgTempTs, weeklytrafficVolumeTs), class="mts")
autoplot(s, facets = T)


lag2.plot(weeklyAvgTempTs, weeklytrafficVolumeTs,8) # no correlation
lag2.plot(weeklytrafficVolumeTs, weeklyAvgTempTs,8) # no correlation

# cross-correlation plot
ccf(weeklyAvgTempTs, weeklytrafficVolumeTs)

library(dynlm)
fit_dynlm = dynlm(weeklytrafficVolumeTs ~ time(weeklyAvgTempTs) + lag(weeklyAvgTempTs, -5))
