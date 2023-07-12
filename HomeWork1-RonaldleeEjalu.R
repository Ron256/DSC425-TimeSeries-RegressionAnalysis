
###############################################################
# Ronaldlee Ejalu
# DSC 425
##############################################################
library(ggplot2)
library(forecast)   # for the "ma" moving average function
library(ggfortify)  # for autoplot
library(fBasics)    # for basic time series statistics
# library(lubridate) # for mdy date conversion

# set working directory
setwd("C:/Users/REJALU/OneDrive - DePaul University/DSC425/Week1/ClassProjects/data")

# read the crude oil data set file
crudeoil = read.csv("crudeoil.csv") 

# look at the top five records of the file
head(crudeoil)

# look at the last 5 records.
tail(crudeoil)


class(crudeoil$date)
# we also see that date is in the wrong format.

# extract out the Date column and convert it into date type
date = as.Date(crudeoil$date, "%d-%b-%y")

crudeoil$Date = as.Date(crudeoil$date, "%d-%b-%y")

# determine the type of date
class(date) 

# extract out the price
price = crudeoil$price

# basic plot
qqplot(date, price, geom="line")

length(price) # number of observations i the price column

# the basic stats function gives us the number of observations, which is in scientific format
# you get the min, max and quartiles about it the price vector
basicStats(price) 

range(date)

# create a ts object
coTs = ts(crudeoil$price, start=c(2004, 01), frequency=52)

# create the autoplot of the ts object.
autoplot(coTs)

# create 30 day moving average
coSmooth = ma(coTs, order=30, centre = T)


# Using a qplot and ggplot plotting the the series of the 30-day moving average along with
# the series plot
# We can also use qplot
qplot(time(coTs), coTs, geom="line"
      , ylab = "ts", xlab="Date converted into numeric", main="30-day moving average of the Spot prices") + 
  geom_line(aes(x=time(coSmooth), y=coSmooth), col="red") 

# create a data frame
codf <- data.frame(time(coTs), coTs)
names(codf)

names(codf)[1] = 'date'
#########################################################################
# Part 1 (a)
#################################################
# using ggplot to plot the 30-day moving average of the spot prices 
# along with the series plot
ggplot(codf, aes(date, coTs)) + 
  geom_line(aes(color='ts'), col='black') +
  geom_line(aes(x=time(coSmooth), y=coSmooth), color='red') + 
  theme_bw() +
  labs(title = "30-Day Moving Average graphed with ts"
       , x = "date"
       , y = "price") +
    theme(text=element_text(size=15)) + 
  theme(plot.title = element_text(hjust=0.5))


######################################################################
# b). plotting the series of the spot prices along with a LOESS smoothing of the 
# series
#
##############################################################################



# so we take price and date and convert it into a data frame
# since the loess function works with a data frame
priceDate_df = data.frame(price, date)


# Remember, it LOESS is the default smoothing for geom_smooth!
ggplot(data=priceDate_df, aes(x=date, y=price)) + 
  geom_line(col="#1f78b4") + 
  geom_smooth(formula= y ~ x, method="loess", col="#b2df8a") +
  theme(
        axis.text.x = element_text(face="bold.italic", 
                                   size=12),  
        axis.text.y = element_text(face="bold.italic", 
                                   
                                   size=12
        ),  
         # panel.grid.major.x = element_blank(), # Hide the horizontal grid lines
         # panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  size = 14, 
                                  face = "bold.italic"
        ) # format the title
  ) + 
  theme_bw() +
  labs(title = "series of spot prices along with a LOESS smoothing of the series"
       ) + 
  theme(text=element_text(size=15)) + 
  theme(plot.title = element_text(hjust=0.5))

########################################################################################
# (c) % change rate of spot prices rate = (pt - pt-1) /pt-1
#
##########################################################################################

# plotting the rate series vs. time
#qplot(time(rate), rate, geom="line") # produce a basic plot.

#ggplot(data=rate_df, aes(date, rate)) + 
#  geom_line(col="#1f78b4") + 
#  theme_bw() + 
#  labs(title = "% change of spot prices vs time", 
#       y='% change of\n spot prices', 
#       x='time')

# Now, let's calculate the return.  We can do this with the "diff" function which
# takes the difference between neighboring values and produces a series with one 
# less sample.  That is why we remove the last sample from the series values in 
# the denominator.
n = nrow(crudeoil)
priceRate = diff(crudeoil$price) / crudeoil$price[-n]

# Now, let's plot the returns.  To do this in ggplot, it is more convenient to 
# have these in a data.frame.
pct_change_rate <- data.frame(crudeoil$Date[-n], priceRate)
head(pct_change_rate)
# Rename the columns to something more readable.
names(pct_change_rate) = c("Date", "rate")

# Now, plot the rate
ggplot(pct_change_rate, aes(Date, rate)) + 
  geom_line() + 
  theme_bw() +
  labs(title = "% change of spot prices vs time", 
       y='% change of\n spot prices', x='time') +
theme(text=element_text(size=15)) + 
  theme(plot.title = element_text(hjust=0.5))



################################################################################
#
# (d) Analyze the distribution of rate using the normal quartile plot
# Computing Symmetry and Kurtosis of the rate distribution
# Testing for formality for the distrribution rate using the Jarque-Bera test at 
# at 95% level
#
################################################################################


# analyzing the distribution of rate using a normal quartile plot.
ggplot(pct_change_rate, aes(sample=rate)) + 
  stat_qq() +
  # adding a line for comparison against perfect fit
  stat_qq_line() + 
  theme_bw() +
  labs(title = "Exploring the distribution of rate using normal quantile plot") + 
  theme(text=element_text(size=15)) + 
  theme(plot.title = element_text(hjust = 0.5)) 



library(fBasics)
# Compute Symmetry
skewness(pct_change_rate$rate)  # Not much in the way of skewness!

library(fBasics)
kurtosis(pct_change_rate$rate)   # Note, this is "excess" kurtosis, so normal = 0

# If you want regular kurtosis where normal = 3, then set a method
# kurtosis(pct_change_rate$rate, method="moment")

# Using jarque.bera.test 
library(tseries)
jarque.bera.test(pct_change_rate$rate)  # Likely does not come from a normal distribution p < .01

library(fBasics)
# using the normalTest function
normalTest(pct_change_rate$rate, method="jb", na.rm = FALSE)



################################################################################
#
#(e) log-rate of change of the series (log return)
# computed as difference of the logs of the prices
################################################################################



# compute the difference of the logs of the prices
logRateOfChange <- diff(log(coTs))
head(logRateOfChange, 10)

# derive a data set 
logRofC_ds = data.frame(time(coTs)[-nrow(crudeoil)], logRateOfChange)
# rename the columns
names(logRofC_ds) = c("time", "log_return")
head(logRofC_ds, 10)


# analyzing the distribution of rate using a normal quartile plot.
ggplot(logRofC_ds, aes(sample=log_return)) + 
  stat_qq() +
  # adding a line for comparison against perfect fit
  stat_qq_line() + 
  theme_bw() +
  labs(title = "Exploring the distribution of rate using normal qq-plot") +
  theme(text=element_text(size=15)) +
  theme(plot.title = element_text(hjust = 0.5)) 



# We can also plot the histogram with a normal distribution overlain
# compute the mean
m = mean(logRofC_ds$log_return)
# compute the standard deviation
s = sd(logRofC_ds$log_return)

ggplot(logRofC_ds, aes(log_return)) + 
  geom_histogram(aes(y = ..density..), col="black", fill="#2c7fb8", bins=25) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = s), col="#d95f02", size=1.2) + 
  theme_bw() +
  xlab("Return") + ggtitle("Distribution of log-returns") + 
  theme(text = element_text(size=15)) + 
  theme(plot.title = element_text(hjust = 0.5)) 


# Using jarque.bera.test 
library(tseries)
jarque.bera.test(logRofC_ds$log_return)  # Likely does not come from a normal distribution p < .01

library(fBasics)
# using the normalTest function
normalTest(logRofC_ds$log_return, method="jb", na.rm = FALSE)

################################################################################
#
# (2) Analzing the weekly groceries.csv sales data 
#
#
################################################################################

# load the data set.

groceries = read.csv("groceries.csv") 

# look at the top 10 records of the data set
head(groceries, 10)

# date type
class(groceries$Date)

# extract out the Date column and convert it into date type
groceries$convertedDate = as.Date(groceries$Date, "%d-%b-%y")

# look at the top 10 records of the data set
head(groceries)

# determine the type of the converted date
class(groceries$convertedDate)

# the last 5 records
tail(groceries)

################################################################################
#
#2(a) create a ts object 
#
################################################################################

# creating a ts for the object 
toothPaste_ts <- ts(groceries$ToothPaste, start=c(2008, 1), frequency = 52)
toothPaste_ts

################################################################################
#
#2(b) create a timeplot for the time series of ToothPaste weekly sales
#
################################################################################

autoplot(toothPaste_ts, 
         xlab="Time", 
         ylab="Sales in units", 
         main = "2008 ToothPaste weekly sales units") + 
  theme_bw() +
  theme(text=element_text(size=15)) + 
  theme(plot.title = element_text(hjust = 0.5))



################################################################################
#
# 2(d) Using the decompose function with the proper series type to plot the series
# in such away that highlights any seasonality
#
################################################################################



# create a time series object
toothPaste_ts_w=ts(groceries$ToothPaste, start=c(2008, 1), frequency = 52)

# examine any seasonality, trend an randomness
decompose(toothPaste_ts_w)


# we need to try some lower values, for example 2
toothPaste_ts_w2=ts(groceries$ToothPaste, start=c(2008, 1), frequency = 2)
# examine any seasonality, trend an randomness
decompose(na.omit(toothPaste_ts_w2))


d = decompose(toothPaste_ts_w2)
# you get some na before and at the back
d$trend 
sd(na.omit(d$random))


# omitting missing values
plot(decompose(na.omit(d$random)))


# we need to try some lower values, for example 3
toothPaste_ts_w3=ts(groceries$ToothPaste, start=c(2008, 1), frequency = 3)
# examine any seasonality, trend an randomness
decompose(na.omit(toothPaste_ts_w3))


d = decompose(toothPaste_ts_w3)
# you get some na before and at the back
d$trend 
sd(na.omit(d$random))


# omitting missing values
plot(decompose(na.omit(d$random)))



# we need to try some lower values, for example 4
toothPaste_ts_w4=ts(groceries$ToothPaste, start=c(2008, 1), frequency = 4)
# examine any seasonality, trend an randomness
decompose(na.omit(toothPaste_ts_w4))


d = decompose(toothPaste_ts_w4)
# you get some na before and at the back
d$trend 
sd(na.omit(d$random))


# omitting missing values
plot(decompose(na.omit(d$random)))


# we need to try some lower values, for example 13
toothPaste_ts_w13=ts(groceries$ToothPaste, start=c(2008, 1), frequency = 13)
# examine any seasonality, trend an randomness
decompose(na.omit(toothPaste_ts_w13))

d = decompose(toothPaste_ts_w13)
# you get some na before and at the back
d$trend 
sd(na.omit(d$random))
sd(na.omit(d$seasonal))

# omitting missing values
plot(decompose(na.omit(d$random)))

################################################################################
#
# Using the zoo library
###############################################################################
library(zoo)
groceriesZoo <- zoo(toothPaste_ts_w13, groceries$convertedDate)
plot(decompose(groceriesZoo))

################################################################################
#
# 3 Using the total monthly expenditures on cafes, restaurants and takeout foods
# services in Australia
#
################################################################################


# load the fpp2 library
library(fpp2)

# determine the object 
class(auscafe)

# printing the first 20 samples of the ts object
head(auscafe, 21)

# determine the frequency of the ts object
frequency(auscafe)


################################################################################
#
# (b) Creating a time plot for the series
#
################################################################################


autoplot(auscafe, 
         xlab="Time", 
         ylab="billions of Australian dollars", 
         main = "Total monthly expenditures on cafes, restaurants and takeout food services in Australia") +
  theme_bw() +
  theme(text=element_text(size=15)) + 
  theme(plot.title = element_text(hjust = 0.5))


################################################################################
#
#(c) Compute and analyze the distribution of log returns
#
################################################################################

# Compute the log returns
log_returns = diff(log(auscafe))
head(log_returns, 10)



# create a data frame comprising of time and log_returns
monthly_exp <- data.frame(time(log_returns), log_returns)

# rename the first column
names(monthly_exp)[1] = "time"
head(monthly_exp)


# We can also plot the histogram with a normal distribution overlain
# compute the mean
m = mean(monthly_exp$log_returns)
# compute the standard deviation
s = sd(monthly_exp$log_returns)

ggplot(monthly_exp, aes(log_returns)) + 
  geom_histogram(aes(y = ..density..), col="black", fill="#2c7fb8", bins=25) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = s), col="#d95f02", size=1.2) + 
  theme_bw() +
  xlab("log_returns") + ggtitle("Distribution of log-returns") + 
  theme(text = element_text(size=20)) + 
  theme(plot.title = element_text(size=20, hjust = 0.5)) 




# analyzing the distribution of log_returns using a normal quartile plot.
ggplot(monthly_exp, aes(sample=log_returns)) + 
  stat_qq() +
  # adding a line for comparison against perfect fit
  stat_qq_line() +
  labs(title = "normal quartile plot of the log returns") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


library(fBasics)

# Compute Kurtosis
kurtosis(monthly_exp$log_returns)

################################################################################
#
# (d) testing the hypothesis of normality for the distribution of rate 
#
################################################################################

library(tseries)
jarque.bera.test(monthly_exp$log_returns)  # Likely does not come from a normal distribution p < .01

library(fBasics)
normalTest(monthly_exp$log_returns, method = "jb", na.rm = FALSE)


################################################################################
#
# (e) using the appropriate (additive or multiplicative) decompose function 
#
#
################################################################################

plot(decompose(auscafe, type = "multiplicative"))






