# Script Name: Regression Model Project 
#   Purpose:
#   Author(s):
#   Date Created: 
#   
#   Notes:
#   
#
#
#-----------------------------------------------------------------------
# Working Directory 

getwd()
setwd("C:/Users/vanes/OneDrive/Desktop/QMBE_3730_Vanessa_Pham_2025/QMBE_3730_Vanessa_Pham_2025")

#-----------------------------------------------------------------------
#Libraries and Packages

install.packages("tidyverse")
install.packages("stargazer")
install.packages("caTools")
library(tidyverse)
library(stargazer)
install.packages("wooldridge")
library(wooldridge)
library(modelr)
library(broom)
library(caTools)

install.packages("ggplot2")
library(ggplot2)

install.packages("geomtextpath")
library(geomtextpath)

library(zoo)

install.packages("forecast")
library(forecast)

install.packages("tseries")
library(tseries)

#-----------------------------------------------------------------------
#Load Data

data <- read.csv('monthly_production.csv')
view(data)

#-----------------------------------------------------------------------
#Data Analysis

#1. What is a time series? Give two real-world examples

#Time series is a series where the data is placed over time. Its goal is
#to uncover patterns over time. There are many examples of time series such as
#collecting and analyzing data over time like the weather temperature or
#financial stocks. 

#2. What is the difference between a time series and cross-sectional data?

#The difference between time series and cross-sectional data is that 
#cross-sectional data observes data with many variables but is only over one
#period of time where time-series is data observed over many time periods. 

#3. Sort the data in ascending order based on the month column

#4. Plot the provided time series dataset. What patterns do you observe?

data$month <- as.Date(paste0(data$month, "-01"))

ggplot(data, aes(x = month, y = production)) + 
  geom_line(main = "Production Line Graph 1956-1995") 

#Ggplot is to plot the graph and first we need to tell R what dataset we are 
#using and after tell which variables are the x and y axis. Afterwards the 
#geom_line() is the line graph. Before that, I had to convert the month variable
#to a date form because it kept saying we needed to adjust the group aesthetic

#From the data we were provided, I can say that it looks like a curvilinear   
#trend, specifically, a quadratic. I can see it is also a seasonal pattern also. 

#5. Decompose the time series into trend, seasonality, and residuals.

# We started with converting to time series object
ts_data <- ts(data$production, start = c(1956, 1), frequency = 12)
view(ts_data)

# Decompose the time series
decomposed <- decompose(ts_data)

# Plot the components
plot(decomposed)

#adding seasonality to the datatable 
seasonality <- data.frame(
  month = time(decomposed$seasonal)
)

seasonality <- seasonality %>% 
  mutate(season = rep(1:12, length.out =nrow(seasonality)))

#adding trend regression 

trend <- lm(production ~ month, data = data)
trend

# By doing so, we are able to break down the parts and look at their patterns 
#individually. We are able to see their frequencies and focus on trends that we 
#might not see with the lineplot data. For example, we were able to look at the 
#trend more clearly and see more specifically and clearly the little ups and downs

#6. What is autocorrelation? Plot the autocorrelation function (ACF) and 
#partial autocorrelation function (PACF) for the dataset.

# Autocorrelation is a correlation of the time series and its lagged version over time
#If there is a strong correlation between time x subscript t and x subscript t-k,
#then it should be included in the autogressive model. A partial autocorrelation 
#function are what tease out the influence of the sequence of x subscript t-1
#and so fourth when calculating the correlation between x subscript t and x 
#subscript t-k

#plot ACF 
acf(ts_data, main = "ACF")

#plot PACF
pacf(ts_data, main = "PACF")

#7. Split the time series into a training and test set. Why is this important?

# Split Data into training and test sets
dim_data<-dim(data)

#Use Ceiling function to round up to the nearest whole number and splitting it 70/30
n <- length(ts_data)
train<-ceiling((0.7*n)) 
train

#select sets sequentially

training_set=window(ts_data, end = time(ts_data)[train])
test_set=window(ts_data, start = time(ts_data)[train])

dim(data.frame(training_set))
dim(data.frame(test_set))

#it is important because we are able to use it to help predict the future by 
#using past data, help the model get more data or get more pattern, prevent 
#overfitting, and it can be more accurate

#8. Perform a moving average smoothing on the data. What window size did you choose and why?

#use the function rollmean which is for moving averages
mas_prod <- zoo::rollmean(data$production, k = 12, fill = NA, align = "center")

#We choose 12 month window because this dataset is througout around 50 years 
#which is a long time and we thought it was best to use 12 months so it can 
#be used for long term trends 

#9. Fit a simple exponential smoothing model to the series. Evaluate its performance.

#making the ses 
ses_data <- ses(training_set, h = length(test_set))
ses_data

#making sure that the test set is a time series 
test_set_2 <- ts(test_set, start = (test_set), frequency = 12)

#fitting the ses to the series 
autoplot(ses_data) +
  autolayer(test_set_2, series = "Actual", color = "red") +
  labs(title = "Simple Exponential Smoothing Forecast vs Actual",
       x = "Year", y = "Production")

# Determine the start time of the test set
test_start_index <- time(ts_data)[length(training_set) + 1]

# Recreate test_set_2 as a time series
test_set_2 <- window(ts_data, start = time(ts_data)[train], frequency = 12)

#evaluate it's performance 
accuracy(ses_data, test_set)

#after evaluating it's performance, we found that the ME for the training set is 
#0.2791117 and test set is -3.6761339. The RMSE training set is 17.47546 and 
#test set is 21.88610. MAE training set is 13.75565 and testing set is 18.36319. 
#And finally their MAPE training set is 10.72187 and test set is 11.96169 

#Forecast the next 5 periods. Plot the forecast along with confidence intervals.

#Forecasting the next 5 periods 
ses_5_per <- ses(ts_data, h = 5) #Sep: 119.4463, Oct: 114.4003, Nov: 110.1038
#Dec: 106.2976, Jan: 102.8444

#plotting the forecast 
autoplot(ses_5_per) +
  labs(title = "Simple Exponential Smoothing Forecast",
       x = "Time",
       y = "Production") +
  theme_minimal()

#11. Compare the forecasting accuracy using metrics like MAE, RMSE, and MAPE.

#MAE training set is 13.75565 and testing set is 18.36319 their MAPE training
#set is 10.72187 and test set is 11.96169. Their RMSE training set is 17.47546
#and test is 21.88610

#12. How would you deal with missing values in a time series?

#In general for time series, I woudld first see if it is a lot of missing values
#which if it isn't then I would drop the missing values. If it was a lot of 
#missing values than I would have to replace it with the average or median

#13. Perform a stationarity test (e.g., Augmented Dickey-Fuller test) on the data. Interpret the results.

kpss_set <- kpss.test(training_set)
adf_set <- adf.test(training_set)

#14. Ensemble the output of the two models and find the MAE, RMSE, and MAPE of the ensemble model

forecast_set <- (kpss_set+adf_set)/2
accuracy(forecast_set, test_set)