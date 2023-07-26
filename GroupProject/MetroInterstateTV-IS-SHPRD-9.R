# load the necessary packages

library(dplyr)

library(fBasics)
library(ggplot2)
#library(ggfortify)
library(forecast)
#
library(forecast)
library(fGarch)
library(rugarch)
library(tseries)
library(fUnitRoots)
library(lmtest)
library(fpp3)
library(TSA)
library(astsa)
library(dynlm)
library(vars)



################################################################################
#
# load the data set
################################################################################

# set working directory
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

metroIS %>% dplyr::select(holiday) %>% dplyr::distinct()

################################################################################
# Data processing
#################################################################################


# Checking for duplicates:

metroIS_nodups <- 
  metroIS %>%
  distinct() # remove duplicate rows based on all columns. 
# holiday when  x == 'None then False else True
# derive the weekly dates
metroIS_nodups <- metroIS_nodups %>%  mutate(weekdate = as.Date(cut(tDate_time, "week"), "%Y-%m-%d"), 
                                             holiday_type= case_when(
                                               holiday == "None" ~ FALSE,
                                               holiday != "None" ~ TRUE)
)

head(metroIS_nodups)
metroIS_nodups %>% dplyr::filter(holiday_type == FALSE) %>% summarise(n=n()) # 48126
metroIS_nodups %>% dplyr::filter(holiday_type == TRUE) %>% summarise(n=n()) # 61 

# qa
# check if the function above works
#metroIS_nodups %>% dplyr::select(holiday, holiday_type) %>% filter(holiday != "None")
metroIS_nodups %>% dplyr::filter(holiday != "None") %>%
  dplyr::select(holiday, holiday_type) 

# 2016-12-06 18:00:00, light snow, 90
###############################################################################################################

# Using the separate function from the tidyr package
metroIS_monthly <- tidyr::separate(metroIS_nodups, date_time, c("date", "time"), sep = " ")
monthly_metroIS_traffic <- tidyr::separate(metroIS_monthly, date, c("year", "month", "day"), sep = "-")
#dev.off()
# bar chart
holiday.agg <- monthly_metroIS_traffic %>%
  dplyr::group_by(holiday_type) %>%
  summarise(weeklytrafficVolume = mean(traffic_volume), .groups = 'drop') %>%  # derive weekly average traffic volume
  mutate(HolidayIndicator = as.factor(holiday_type))

barplot <- ggplot(holiday.agg, aes(x = reorder(factor(holiday_type), weeklytrafficVolume), y = weeklytrafficVolume, fill = HolidayIndicator)) +
  geom_col(position = "dodge", colour = "black") +  # use the position parameter to create the grouped bar charts
  
  # This is for color blind people
  scale_fill_manual(values = c("#a6cee3", "#b2df8a")) #+ scale_fill_brewer(palette = "Pastel1")


barplot <- barplot + theme(axis.text.x = element_text(face="bold.italic", 
                                                      size=10, 
                                                      angle=10),  
                           axis.text.y = element_text(face="bold.italic", 
                                                      
                                                      size=10
                           ),  
                           panel.grid.major.x = element_blank(), # Hide the horizontal grid lines
                           panel.grid.minor.x = element_blank(),
                           plot.title = element_text(hjust = 0.5, 
                                                     size = 14, 
                                                     face = "bold.italic"
                           ) # format the title
) + labs(title = "A bar graph showing Weekly Average traffic Volume on Holiday Vs Non-Holidays", 
         x = "Holiday Status", y="traffic volume")

barplot

# adding holiday as part of the aggregation
weekly_df <- monthly_metroIS_traffic %>% 
  group_by(weekdate, holiday) %>%  # here, I am using year_week
  summarise(weeklytrafficVolume = sum(traffic_volume), average_temp=mean(temp), .groups = 'drop')

# create time series objects
trafficVolumeTs = ts(weekly_df$weeklytrafficVolume, start=2012, end=2018, frequency =52)

AvgTempTs =    ts(weekly_df$average_temp, start=2012, end=2018, frequency =52)
Holiday = ts(weekly_df$holiday, start=2012, end=2018, frequency =52)
#Holiday = as.factor(Holiday)
Holiday[Holiday == "None"] = FALSE
Holiday[Holiday != FALSE] = TRUE
Holiday = as.factor(Holiday)
# creating the variable into binary
holiday_type_num = as.numeric(Holiday)
holiday_type_num

qplot(as.numeric(log(AvgTempTs)), as.numeric(log(trafficVolumeTs)), color=as.factor(Holiday), geom="point", xlab="Avg Temp", ylab="weekly traffic volume", main="Relationship between weekly traffic volume on Holidays Vs Non-Holidays") + theme_bw() + labs(colour = 'Holiday')





# Deriving the daily traffic volume and daily average temperature. 
##############################################################################################################
options(scipen=999) # turn off the exponential format
summarized_metroIS_traffic_weekly <- monthly_metroIS_traffic %>% 
  group_by(weekdate) %>%  # here, I am using year_week
  summarise(weeklytrafficVolume = sum(traffic_volume), average_temp=mean(temp), .groups = 'drop')

#summarized_metroIS_traffic_weekly <- summarized_metroIS_traffic_weekly %>%
#  mutate(weeklydate=floor_date(year_w, week, 1)) #%>%
# select(-year) %>% # remove year column
# select(-month) %>%# remove month column from the data set.
#  select (-day)


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
par(mfrow=c(1,2))
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

# transform the data
####################################################################################################################################
# lets try to transform the data to increase normality

summarized_metroIS_traffic_weekly <- summarized_metroIS_traffic_weekly %>% 
  dplyr::select (weeklytrafficVolume, average_temp, weekdate) %>%
  dplyr::mutate(log_weeklytrafficVolume=log(weeklytrafficVolume), log_avgtemp= log(average_temp))

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

par(mfrow=c(2,2))
# Acf(weeklytrafficVolumeTs)
par(mfrow=c(2,2))
Acf(weeklytrafficVolumeTs, lag.max = 100)
pacf(weeklytrafficVolumeTs)


eacf(weeklytrafficVolumeTs)
adfTest(weeklytrafficVolumeTs, type="ct") # we reject non stationary; this is trend - stationary
kpss.test(weeklytrafficVolumeTs, null="Trend") # we reject stationarity, implying that first differencing is required.

autoplot(diff(weeklytrafficVolumeTs))
mean(diff(weeklytrafficVolumeTs))
par(mfrow=c(2,2))
autoplot(diff(weeklytrafficVolumeTs))
par(mfrow=c(2,2))
Acf(diff(weeklytrafficVolumeTs)) # sign of over-differencing
#Acf(diff(weeklytrafficVolumeTs), lag.max=30)
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
fitA = Arima(weeklytrafficVolumeTs, order=c(0,1,1), seasonal = list(order=c(1, 0, 0), seasonal=52), include.drift = FALSE)
fitA

lmtest::coeftest(fitA)
Acf(fitA$residuals)
Box.test(fitA$residuals, type="Ljung") # we  reject NULL hypothesis: there is more infomartion to capture

# non-seasonal order ARIMA(0,1,2) AND seasonal order AR2
fitB = Arima(weeklytrafficVolumeTs, order=c(0,1,2), seasonal = list(order=c(2, 0, 0), seasonal=52), include.drift = FALSE)
fitB
lmtest::coeftest(fitB)
Box.test(fitB$residuals, type="Ljung", lag=50) # we fail to reject white noise


# non-seasonal order ARIMA(0,1,4) AND seasonal order AR2
fitC = arima(weeklytrafficVolumeTs, order=c(0,1,4),  seasonal = list(order=c(2, 0, 0), seasonal=52), fixed=c(NA, 0, NA, NA, NA, 0))
fitC
lmtest::coeftest(fitC)
par(mfrow=c(1,2))
Acf(fitC$residuals)
Box.test(fitC$residuals, lag=10, type="Ljung")



Box.test(fitC$residuals, type="Ljung", lag=30) # we fail to reject white noise

# non-seasonal order ARIMA(0,1,5) AND seasonal order AR2
fitD = Arima(weeklytrafficVolumeTs, order=c(0,1,5), seasonal = list(order=c(2, 0, 0), seasonal=52), include.drift = FALSE)
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
fit2_AIC = auto.arima(weeklytrafficVolumeTs, ic="aic", allowdrift=FALSE)
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
pmA1 = backtest(fitA, weeklytrafficVolumeTs, h=1, orig=0.65*length(weeklytrafficVolumeTs))
pmB1 = backtest(fitB, weeklytrafficVolumeTs,  h=1, orig=0.90*length(weeklytrafficVolumeTs))
pmC1 = backtest(fitC, weeklytrafficVolumeTs, h=1, orig=0.90*length(weeklytrafficVolumeTs))
pmD1 = backtest(fitD, weeklytrafficVolumeTs, h=1, orig=0.90*length(weeklytrafficVolumeTs))
pm_AIC =  backtest(fit_AIC, weeklytrafficVolumeTs, orig=0.90*length(weeklytrafficVolumeTs), h=1)
pm2_AIC = backtest(fit2_AIC, weeklytrafficVolumeTs, orig=0.90*length(weeklytrafficVolumeTs), h=1)
pm_BIC = backtest(fit_BIC, weeklytrafficVolumeTs,orig=0.90*length(weeklytrafficVolumeTs, h=1))

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
# we model the autocorrelation
fitLm = lm(weeklytrafficVolumeTs ~ time(weeklytrafficVolumeTs))
summary(fitLm)
par(mfrow=c(2,2))
acf(fitLm$residuals)
Acf(fitLm$residuals, lag.max = 30) # Looks like descending behaviour ... maybe not much MA 
# (not much in the way of strong single ma behavior) 
# maybe an AR
pacf(fitLM$residuals)# likely to be an AR1 or ARMA process for the residuals
eacf(fitLm$residuals) # Maybe AR1 MA1, AR1 MA2 , AR2, MA2

fit1 = Arima(weeklytrafficVolumeTs, xreg=cbind(time(weeklytrafficVolumeTs), holiday_type_num), order=c(1,0,1))
fit1
coeftest(fit1)

par(mfrow=c(1,1))
Acf(fit1$residuals)
Box.test(fit1$residuals, type="Ljung")




fit2 = Arima(weeklytrafficVolumeTs, xreg=cbind(time(weeklytrafficVolumeTs), holiday_type_num), order=c(1, 0, 2))
summary(fit2)

coeftest(fit2)
par(mfrow=c(1,1))
Acf(fit2$residuals)
Box.test(fit2$residuals, type="Ljung", lag=10)


adfTest(fitLM$residuals, type="nc")  # Rejects unit root
kpss.test(fitLM$residuals, null="Level")  # Cannot reject stationarity


fit3 = Arima(log(weeklytrafficVolumeTs), xreg=time(log(weeklytrafficVolumeTs)), order=c(2, 0, 2))
summary(fit3)

coeftest(fit3)
par(mfrow=c(1,1))
Acf(fit3$residuals)
Box.test(fit3$residuals, type="Ljung", lag=10)


# So, fit an arima with an x-regressor
fit4 = auto.arima(weeklytrafficVolumeTs, xreg=cbind(time(weeklytrafficVolumeTs), holiday_type_num), ic="bic")
summary(fit4)
coeftest(fit4)
Box.test(fit4$residuals, type="Ljung")

bkt_fit2 = backtest(fit2, weeklytrafficVolumeTs/10000, h=1, orig=0.90*length(weeklytrafficVolumeTs/10000))
bkt_fit3 = backtest(fit3, weeklytrafficVolumeTs, h=1, orig=0.90*length(weeklytrafficVolumeTs))
bkt_fit4 = backtest(fit4, weeklytrafficVolumeTs, h=1, orig=0.90*length(weeklytrafficVolumeTs))
bkt_fit_f1 = backtest(fit_f1, weeklytrafficVolumeTs, h=1, orig=0.90*length(weeklytrafficVolumeTs))



# forecast 
autoplot(forecast(fit4, xreg=2015:2018))

###################################################################################################

# Trend- Staionarity
fit1=lm(weeklytrafficVolumeTs ~ weeklyAvgTempTs)
summary(fit1)

plot(fit1$residuals, main="residual plot")
autoplot(ts(fit1$residuals),
         main = "Residuals Time plot", xlab="Time", ylab="Residuals") + 
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
#library(astsa)

lag2.plot(weeklyAvgTempTs, weeklytrafficVolumeTs,8) # no correlation
lag2.plot(weeklytrafficVolumeTs, weeklyAvgTempTs, 8)

# cross-correlation plot
par(mfrow=c(1,1))
ccf(weeklyAvgTempTs, weeklytrafficVolumeTs)
par(mfrow=c(1,1))
ccf(weeklytrafficVolumeTs, weeklyAvgTempTs)

#library(dynlm)
class(weeklytrafficVolumeTs)
fit_dynlm = dynlm(weeklytrafficVolumeTs ~  stats::lag(weeklyAvgTempTs, -5))
summary(fit_dynlm)
Acf(fit_dynlm$residuals)
Acf(fit_dynlm$residuals^2)
Box.test(fit_dynlm$residuals, lag = 10, type="Ljung") # reject the null hypothesis of white noise. 
Acf(weeklytrafficVolumeTs)

fit_dynlm_b = dynlm(weeklyAvgTempTs  ~  stats::lag(weeklytrafficVolumeTs, -15))
summary(fit_dynlm_b)
Acf(fit_dynlm_b$residuals)
Acf(fit_dynlm_b$residuals^2)
Box.test(fit_dynlm_b$residuals, lag = 10, type="Ljung") # reject the null hypothesis 


# Arima with lag
s = as.zoo(ts.intersect(weeklytrafficVolumeTs, weekly_average_temp_lag_5= stats::lag(weeklyAvgTempTs, 5)))
autoplot(s)

#fit_arima = Arima(s$weeklytrafficVolumeTs, xreg=cbind(s$weekly_average_temp_lag_5, time(s$weekly_average_temp_lag_5)*s$weekly_average_temp_lag_5), order=c(0,0,0))
#coeftest(fit_arima)
#Acf(fit_arima$residuals)
#Box.test(fit_arima$residuals, lag = 10, type="Ljung")

# Arima with lag
fit_arima_lag = Arima(subset(weeklytrafficVolumeTs, end=308), xreg=subset(weeklyAvgTempTs, start=6), order=c(1, 0, 5), fixed=c(NA, NA, 0, 0, NA, 0, 0, NA)) # 1, 0, 4
fit_arima_lag
coeftest(fit_arima_lag)
par(mfrow=c(1,1))
Acf(fit_arima_lag$residuals)
Box.test(fit_arima_lag$residuals, lag = 10,  type="Ljung")


###########################################################################################################
# 'thunderstorm','mist','fog','haze' - other
# holiday when  x == 'None then False else True
# Holiday Vs Non Holiday
# Do we need to scale our data

metroIS_monthly %>% 
  select (weather_main) %>% 
  unique()

metroIS_monthly <- metroIS_monthly %>% mutate(weather_main = tolower(weather_main))



##############################################################################################################
# PREWHITENING
###############################################################################################################
# ccf plot
par(mfrow=c(1,1))
ccf(weeklyAvgTempTs, weeklytrafficVolumeTs)
lag2.plot(weeklyAvgTempTs, weeklytrafficVolumeTs)

autoplot(weeklyAvgTempTs)

# let's look at the ACF of weeklyAvgTemp
Acf(weeklyAvgTempTs) # sinusoidal behavior
Acf(weeklyAvgTempTs, lag.max=10)
eacf(weeklyAvgTempTs) # it has some bit of seasonality though it's week

adfTest(weeklyAvgTempTs, type="ct") 
kpss.test(weeklyAvgTempTs, null="Trend")

Acf(diff(weeklyAvgTempTs), lag.max=50)
eacf(diff(weeklyAvgTempTs))
Acf(diff(diff(weeklyAvgTempTs)))
eacf(diff(diff(weeklyAvgTempTs)))

fit_wkly_avg_tmp = Arima(weeklyAvgTempTs, order=c(1, 0, 1), seasonal=list(period=52, order=c(0, 1, 1)))
fit_wkly_avg_tmp
coeftest(fit_wkly_avg_tmp)
Acf(fit_wkly_avg_tmp$residuals)
Box.test(fit_wkly_avg_tmp$residuals, type="Ljung")

#weeklyAvgTempTs_t <- na.omit(weeklyAvgTempTs)
#weeklytrafficVolumeTs_t <- na.omit(weeklytrafficVolumeTs)
#prewhiten(weeklyAvgTempTs, weeklytrafficVolumeTs, x.model=fit_wkly_avg_tmp)

prewhiten(weeklyAvgTempTs, weeklytrafficVolumeTs, x.model=fit_wkly_avg_tmp) # this one is failing 
# manual method 
#fitweeklyAvgTemp <- fitted(Arima(weeklyAvgTempTs, model=fit_wkly_avg_tmp))fitweeklytrafficVolume <- fitted(Arima(weeklytrafficVolumeTs, model=fit_wkly_avg_tmp))
ccf(weeklyAvgTempTs, fitweeklytrafficVolume) # Another of doing it. 


# This is to be continued

#########################################################################################################################
ccf(weeklyAvgTempTs, weeklytrafficVolumeTs) # the highest lag is at 15
fit_dynlm_1 = dynlm(weeklytrafficVolumeTs ~ stats::lag(weeklyAvgTempTs))
summary(fit_dynlm_1) # we get a weak multiple R-squared
sqrt(mean(fit_dynlm_1$residuals^2))
eacf(diff(weeklyAvgTempTs))
Acf(fit_dynlm_1$residuals)
fit_a = Arima(weeklyAvgTempTs, order=c(2, 1, 0))
prewhiten(weeklyAvgTempTs, weeklytrafficVolumeTs, fit_a)

Acf(weeklytrafficVolumeTs)

fit_dynlm_13 =  dynlm(weeklytrafficVolumeTs ~ stats::lag(weeklyAvgTempTs, -13))
summary(fit_dynlm_13)
Acf(fit_dynlm_13$residuals)
Box.test(fit_dynlm_13$residuals, lag=10, type="Ljung")

########################################################################################################################################
# VARS
s= ts(cbind(weeklyAvgTempTs, weeklytrafficVolumeTs ), class="mts")
autoplot(s, facets = T)

ccf(weeklyAvgTempTs, weeklytrafficVolumeTs) # lag - 15 is the higest correlation here
v_s = VARselect(cbind(weeklyAvgTempTs, weeklytrafficVolumeTs), lag.max=8, type="const")
v_s

# let's look like at the AIC errs on the side of avery sparse model.
fit1 = VAR(cbind(weeklyAvgTempTs, weeklytrafficVolumeTs), p=3, type="const")
coeftest(fit1)
serial.test(fit1, lags.pt=10, type="PT.asymptotic") # we are rejecting the null hypothesis of residual autocorrelation.

# BIC 
fit1 = VAR(cbind(weeklyAvgTempTs, weeklytrafficVolumeTs), p=2, type="const")
coeftest(fit1)
serial.test(fit1, lags.pt=10, type="PT.asymptotic") # what does this mean?
# AIC
fit1_aic = VAR(cbind(weeklyAvgTempTs, weeklytrafficVolumeTs), p=8, type="const")
coeftest(fit1_aic)
serial.test(fit1_aic, lags.pt=10, type="PT.asymptotic")


# AIC
fit1_aic = VAR(cbind(weeklyAvgTempTs, weeklytrafficVolumeTs), p=6, type="const")
coeftest(fit1_aic)
serial.test(fit1_aic, lags.pt=10, type="PT.asymptotic")
##########################################################################################################################################

class(holiday_type_num)
# ARCH EFFECTS

par(mfrow=c(1,2))
Acf(weeklytrafficVolumeTs^2)
Acf(abs(weeklytrafficVolumeTs))
eacf(weeklytrafficVolumeTs)

par(mfrow=c(2,2))
Acf(fit_BIC$residuals)
Acf(fit_BIC$residuals^2)
Acf(abs(fit_BIC$residuals))

#autoplot(weeklytrafficVolumeTs)
#autoplot(weeklytrafficVolumeTs) + xlim(c(2012, 2015.058))
#time(weeklytrafficVolumeTs)

Box.test(fit_BIC$residuals^2, lag=20, type="Ljung")
Box.test(abs(fit_BIC$residuals), lag=10, type="Ljung")

gFit = garchFit( ~ arma(0, 1) + garch(1, 1), data=weeklytrafficVolumeTs, trace=F)
gFit

library(rugarch)
s = ugarchspec(variance.model=list(garchOrder=c(1, 1)),
               mean.model=list(armaOrder=c(0, 1)))

gFit3 = ugarchfit(s, diff(weeklytrafficVolumeTs))
gFit3






#################################################################################################################################################
# Time series regression
# check the linear relationship
#qplot(weeklyAvgTempTs, weeklytrafficVolumeTs, geom="point")

# check the correlations
cor(weeklyAvgTempTs, weeklytrafficVolumeTs) # there is no correlation. 
ccf(weeklyAvgTempTs, weeklytrafficVolumeTs) 

fit_d = dynlm(weeklytrafficVolumeTs ~ stats::lag(weeklyAvgTempTs, -15))
coeftest(fit_d)
Acf(fit_d$residuals) # we do have residuals to models
eacf(fit_d$residuals)
Box.test(fit_d$residuals, lag=10, type="Ljung") # we reject white noise

Acf(diff(weeklyAvgTempTs))
eacf(diff(weeklyAvgTempTs))

fit_e = Arima(weeklytrafficVolumeTs, order=c(0, 1, 2), seasonal=c(2, 0, 0))
coeftest(fit_e)
Acf(fit_e$residuals)
Box.test(fit_e$residuals, lag=10, type="Ljung")

fit_f = Arima(weeklytrafficVolumeTs, order=c(0, 1, 4), seasonal=c(2, 0, 0), fixed=c(NA, 0, NA, NA, NA, 0))
coeftest(fit_f)
Acf(fit_f$residuals)
Box.test(fit_f$residuals, lag=10, type="Ljung")

fit_f1 = Arima(weeklytrafficVolumeTs, order=c(0, 1, 3), seasonal=c(2, 0, 0), xreg=weeklyAvgTempTs, fixed=c(NA, 0, NA, NA, 0, 0))
coeftest(fit_f1)
Acf(fit_f1$residuals)
Box.test(fit_f1$residuals, lag=10, type="Ljung")

fit_auto_arima_xreg = auto.arima(weeklytrafficVolumeTs, xreg=weeklyAvgTempTs, ic="bic")
coeftest(fit_auto_arima_xreg)
Acf(fit_auto_arima_xreg$residuals)
Box.test(fit_auto_arima_xreg$residuals,  type="Ljung")

# Pick the best model and forecast


####################################################################################################################################################
# Intervention variable
autoplot(weeklytrafficVolumeTs)
intervention = rep(0, length(weeklytrafficVolumeTs))
intervention[time(weeklytrafficVolumeTs) < 2015] = 1
intervention
fit_2_I <- auto.arima(weeklytrafficVolumeTs, xreg=cbind(time(weeklytrafficVolumeTs), intervention,   time(weeklytrafficVolumeTs) * intervention))
coeftest(fit_2_I)
Box.test(fit_2_I$residuals)


# Adding Holiday Status
##############################################################################################################################


weekly_df <- monthly_metroIS_traffic %>% 
  group_by(weekdate, holiday) %>%  # here, I am using year_week
  summarise(weeklytrafficVolume = sum(traffic_volume), average_temp=mean(temp), .groups = 'drop')

# create time series objects
trafficVolumeTs = ts(weekly_df$weeklytrafficVolume, start=2012, end=2018, frequency =52)

AvgTempTs =    ts(weekly_df$average_temp, start=2012, end=2018, frequency =52)
Holiday = ts(weekly_df$holiday, start=2012, end=2018, frequency =52)
#Holiday = as.factor(Holiday)
Holiday[Holiday == "None"] = FALSE
Holiday[Holiday != FALSE] = TRUE
Holiday = as.factor(Holiday)
# creating the variable into binary
holiday_type_num = as.numeric(Holiday)
holiday_type_num

qplot(as.numeric(log(AvgTempTs)), as.numeric(log(trafficVolumeTs)), color=as.factor(Holiday), geom="point", xlab="Avg Temp", ylab="weekly traffic volume") + theme_bw() + labs(colour = 'Holiday')

autoplot(trafficVolumeTs)
autoplot(log(trafficVolumeTs))

Acf(trafficVolumeTs)
eacf(trafficVolumeTs)
t.test(diff(trafficVolumeTs)) # we fail to reject
# Run basic unit root tests
adfTest(trafficVolumeTs, type="nc")
kpss.test(trafficVolumeTs, null="Level")

adfTest(trafficVolumeTs, type="ct")
kpss.test(trafficVolumeTs, null="Trend")
auto.arima(trafficVolumeTs)
auto.arima(trafficVolumeTs, xreg=cbind(holiday_type_num, xreg=time(trafficVolumeTs), holiday_type_num * time(trafficVolumeTs)), ic="bic")

fit_qa = auto.arima(trafficVolumeTs, xreg=cbind(holiday_type_num, xreg=time(trafficVolumeTs), holiday_type_num * time(trafficVolumeTs)), ic="bic")
coeftest(fit_qa)
Acf(abs(fit_qa$residuals))

Box.test(fit_qa$residuals, lag=10, type="Ljung")

fit_qb = auto.arima(trafficVolumeTs, xreg=holiday_type_num)
coeftest(fit_qb)
Acf(abs(fit_qb$residuals))

Box.test(fit_qb$residuals, lag=10, type="Ljung")




