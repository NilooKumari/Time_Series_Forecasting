                        #### ARIMA Model ####

# ARIMA: Auto regressive integrated moving average
# It leverages auto co-relation within  data points. We need the data to be stationary
# In exponential smoothning we did not take care of the co-relation between 2 consecutive time point


# Conditions to run ARIMA:
#    1. Time series is stationary
#    2. Time series do not have co-relations
#    3. Error is conpletely white noise
#    4. No correlation between the error terms

# ARIMA(p,d,q)
# p: Order of autoregressive part
# d: Degree of first differencing involved
# q: Order of moving average part

# Autocorrelation(ACF): helps in deciding 'q'
# Partial Autocorrelation (PACF):helps in deciding 'p'


# Steps in ARIMA
#    1. Make the time series stationary by differencing 
#    2. Build and select a candidate ARIMA model
#    3. Use PACF and ACF plots to determine order of p and q respectively
#    4. Use the condidate ARIMA model to forecast
#    5. Validate the model

# Why do we need time series to be stationary?
#    1. To replicate past patterns in future
#    2. To get a time series that has constant mean and variance over time

# To get a time series like a Pure White Noise, it needs to be differenced 
# and made stationary

#### Import the required libraries

library(forecast)
library(tseries)
data("WWWusage") # Time series data of the no.of users connected to internet through a server every minute

intusage<- WWWusage

# Step 1: Plot the time series data

plot(intusage)

# Chek if TS is stationary before running ARIMA
# TS does not look stationary 
# Differencing needs to be done

intusagediff1<- diff(intusage,differences = 1)
plot.ts(intusagediff1)

# Still does not look like white noise
# Second order differencing required

intusagediff2<- diff(intusage,differences = 2)
plot.ts(intusagediff2)

# Statistical way of checking stationarity: Augmented Dickey Fuller Test
# ADF Unit Root Test
# If data looks like pure white noise: Test stat= -12.51350108 & p-value=0.01

adf.test(intusage)
adf.test(intusagediff1)
adf.test(intusagediff2) # closest to ADF test stats, hence d=2 is giving stationary time series

# Select the order for p from PACF plot
Acf(diff(intusagediff2,lag.max=20))

# If TS is close to stationary, correlations exponentially decay with time and 
# correlation values decrease over a period of time
# To select the candidate ARIMA for p
#    - Check if the ACF plot is in the form of a sine wave
#    - If it does,check the PACF plot
pacf(diff(intusagediff2,lag.max=20))
#    - Observe the spikes in the PACF plot
#    - Check if spikes  crosses the blue line 
# To determine the p value
#    - Observe that spike that crosses the blue line beyond which autocorrelation begins to decay
#    - p is generally between 0-3
# so order of p=3

# No sign wave pattern in the PACF plot 
# so order of q=0
# Order of candidate ARIMA model is (3,2,0)

# Step 2: Selecting the candidate ARIMA model

intusagearima<- Arima(y=intusage,order=c(3,2,0))
summary(intusagearima)
# Model that gives the least corrected AIC values is the preffered model

# Forecast 
intusageforecasts<- forecast(intusagearima,h=5)
intusageforecasts


# Validate the model
#    - check residuals for autocorrelation

Acf(intusageforecasts$residuals, lag.max=20)
# no autocorrelation as all the lines are within the blue line

plot.ts(intusageforecasts$residuals)
# Residuals look random

Box.test(intusageforecasts$residuals,lag=20,type="Ljung-Box")
# Box-Ljung test also show p is insignificant

### ARIMA forecasts for future time-points are good ###

# Short cut for ARIMA in R programming language
# auto.arima model is very useful

auto.arima(intusage)

# auto.arima helps in selecting the right order for ARIMA and build the model









