
#### Import the required libraries

library(forecast)
library(tseries)

#### Steps in time series analysis

# 1. Read the data in R 
Sales<- scan('http://robjhyndman.com/tsdldata/data/sales.dat')

# 2. Store the data as the time series object
Salestimeseries<- ts(Sales)

# 3. Pass frequency of the time series (12: data collected every month, 
#    4: data collected every quarter 1: data collected annually)
Salestimeseries<-ts(Sales,frequency=12)

# 4. Specify the start of the time period along with sub period
Salestimeseries<-ts(Sales,frequency=12,start = c(1987,1))

# 6. Plot the time series 
plot.ts(Salestimeseries)

# 7. Transform the time series if needed

# 8. Decompose function to decompose various component of a time series
decompose(Salestimeseries,type = "multiplicative")

# 9. Check for the presence of Trend, Seasonality, Random Component
# 10. Select the modelling technique
# 11. Build the model
# 12. Forecast
# 13. Validate

#### Forecasting techniques

# 1.Exponential Smoothing: More weights give to most recent observations,does not take into account trend or seasonality.
# 2.ARIMA: Auto regressive integrated moving average.It does not take into account Seasonality
# 3.X-12 ARIMA:Takes into account monthly and quarterly Seasonality
# 4.STL: Seasonal and Trend decomposition using Lewis


## Simple Exponential Smoothing

# Using cafe data (quarterly expenditure of eating out in Australia) from fpp package
library(fpp)
data(cafe)
cafe
es<-ses(cafe,h=10) # simple exponential smoothing
plot(es) # plotting the data to get the forcast for next 10 time period
         # shaded area represents the predicted intervals of 80% & 95% confidence interval 

summary(es) # alpha: smoothing parameter- higher value of alpha signifies more recent observations
            # are given more weightage for forecasting
# SES is the estimated mean and renders same forecast across future time period

## Validation

# Forecast Error= Observed value - Point Forecast
#    1. Mean absolute error
#    2. Root mean squared error
#    3. Precentage error
#    4. Mean absolute percentage error(MAPE)
#    5. Mean absolute scaled error

accuracy(es) 

# MAPE is the most commonly used value. Ideal MAPE should be less that 7

## Residual Check 

# Residual= Observed value- Fitted value
# Assumptions in residual forecasting
#    1. Residuals should be uncorrelated
#    2. Residual mean is close to zero
#    3. Rediduals have a constant variance
#    4. Residuals are normally distributed

plot(es$residuals)
# Can see a pattern in the data 

## Check for Autocorrelation 

# In a time series showing trend or seasonality, data at each time point might be
# influencing the data at the next time point. Data across the time point 
# are correlated to each other.
# When we are smoothing the time series we are trying to get rid of autocorrelation
# and noise so that we are able to capture the actual model,actual trend,
# seasonal pattern and actual irregular component
# When we smoothen the data we are trying to get rid of autocorrelation between the data
# across various time points

# ACF plot is the autocorrelation at different lag points.It starts from 0th observation
# and can go on to how many lags we want to 

#Any spike within blue line should be ignored

acf(es$residuals)

# Test for autocorrelation- Ljung Box test
# If p value is <0.05 then the data is not White Noise

## Check if residuals are normally distributed

hist(es$residuals)
# Positive errors are not actually cancelling the negative errors

# Trend and seasonality is not fully captured using Simple Exponential Smoothing method


#### Explore other methods

# Automatic function to do  Exponential Smoothing on a time series
# ETS - Error, Trend and Seasonality

fit<- ets(cafe) # gives in time forecast
fcast<- forecast(fit,h=12) # forecast for next 12 time period
fcast
summary(fit)
# ETS spits the model with the least AIC value 
# MAPE < 7, good model


# Residuals of the forecast
Box.test(fcast$residuals,lag=12,type="Ljung-Box")

# p-value>0.05 so residuals are not autocorrelated 


### Can also validate the model using data partition
# Use windows partitioning
str(cafe)
training <- window(cafe,start=c(1982,4),end=c(1999,4),frequency=4)
testing<- window(cafe,start=c(2000,4),end=c(2010,4),frequency=4)

# There should be atleast 50 data points in the training data set for a good model
# Build the model on the training data set
# Fit the model
# Test the model on the testing data set
# Check for accuracy

tr<- ets(training)
Test<- ets(testing, model=tr)
accuracy(Test)

### Data Transformations in Time Series
# We transform the data to reduce the variance within the data
# Can use lambda parameter from the family of Box Cox transformation

data(a10) # sample data from fpp package
lam <- BoxCox.lambda(a10)
fit<- ets(a10,lambda = lam)
fcast<- forecast(fit,h=12) # forecast for next 12 time period
# automatically transforms time series back to original before forecasting
fcast
summary(fit)
accuracy(fit)
# MAPE < 7, good model
