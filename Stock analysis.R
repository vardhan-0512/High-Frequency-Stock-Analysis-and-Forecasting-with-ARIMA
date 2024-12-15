#load Libraries
library(quantmod)#quantmod is an R package that provides a framework for quantitative financial modeling and trading. It provides a rapid prototyping environment that makes modeling easier by removing the repetitive workflow issues surrounding data management and visualization.
library(MASS)#It stands for Modern Applied Statistics with S. It is a valuable resource for statistical modeling and analysis, providing functions and data sets for a wide range of statistical methods. 
library(tseries)#It stand for "Time Series Analysis and Computational Finance," provides a wide range of functions and tools for analyzing and modeling time series data.It provides a comprehensive set of tools for analyzing and modeling time series data
library(forecast)#It is a powerful package that provides a wide range of functions and tools for time series forecasting. It offers functionalities for exploring time series data, fitting various forecasting models, evaluating model performance, and generating forecasts.
loadSymbols(c("AMZN","GOOG","MSFT","AAPL"))

#---------Stock Analysis of Amazon(2007-2023)--------

#str()command displays the contents of a list
str(AMZN)

#head() command returns the first 6 rows by default
head(AMZN)

#tail() command returns the last 6 rows by default
tail(AMZN)

#summary() command returns the minimum,maximum,mean,median,and 1st and 3rd quartiles for an numerical vector
summary(AMZN)

#Here the chart Series function cannot run because there is character column in this data. So first we have to delete this column so that we can execute chartSeries function
#To remove character column, we are assigning the value name column  to NULL
AMZN$Name<-NULL
head(AMZN)

#Data Pre-processing 
#first we have to check whether there is missing data in the data set.
#We can do this by introducing a function called sum and write the condition is.na(dataset) which tells us whether the count of empty(or)NA fields in data set
#If the sum is 0, then there is no missing data in the given data set.
sum(is.na(AMZN))

#plotting the graphs for open,high,low,close and a graph for the present year
plot(AMZN$AMZN.Open)
barChart(AMZN, bar.type = 'hlc', TA = NULL)
candleChart(AMZN, TA=NULL, subset = '2023')

#Using the chartSeries function to display the time vs price analysis
chartSeries(AMZN)
addMACD()# MACD is a trend-following momentum indicator that helps identify potential buying and selling opportunities in financial markets.
addBBands() # Adds Bollinger bands to the stock price.
addCCI() # Add Commodity channel index.
addADX() #Add Directional Movement Indicator
addCMF() #Add Money Flow chart

#To check the highest and lowest price of the stockfh
seriesHi(AMZN) # To check the highest point of price. 
seriesLo(AMZN) # To check the lowest point of price.

# Returns by day
Returns_by_day<-dailyReturn(AMZN)
head(Returns_by_day);tail(Returns_by_day)

# Returns by week
Returns_by_week<-weeklyReturn(AMZN)
head(Returns_by_week);tail(Returns_by_week)

#Returns by month
Returns_by_Month<-monthlyReturn(AMZN)
head(Returns_by_Month);tail(Returns_by_Month)

#Returns by year
Returns_by_year<-yearlyReturn(AMZN)
head(Returns_by_year);tail(Returns_by_year)

#---------Stock Analysis of Google(2007-2023)--------

#str()command displays the contents of a list
str(GOOG)

#head() command returns the first 6 rows by default
head(GOOG)

#tail() command returns the last 6 rows by default
tail(GOOG)

#summary() command returns the minimum,maximum,mean,median,and 1st and 3rd quartiles for an numerical vector
summary(GOOG)

#Here the chart Series funtction cannot run because there is character column in this data. So first we have to delete this column so that we can execute chartSeries function
#To remove character column, we are assigning the value name column  to NULL
GOOG$Name<-NULL
head(GOOG)

#Data Pre-processing 
#first we have to check whether there is missing data in the data set.
#We can do this by introducing a function called sum and write the condition is.na(dataset) which tells us whether the count of empty(or)NA fields in data set
#If the sum is 0, then there is no missing data in the given data set.
sum(is.na(GOOG))

#plotting the graphs for open,high,low,close and a graph for the present year
plot(GOOG$GOOG.Open)
barChart(GOOG, bar.type = 'hlc', TA = NULL)
candleChart(GOOG, TA=NULL, subset = '2023')

#Using the chartSeries function to display the time vs price analysis
chartSeries(GOOG)
addMACD()# MACD is a trend-following momentum indicator that helps identify potential buying and selling opportunities in financial markets.
addBBands() # Adds Bollinger bands to the stock price.
addCCI() # Add Commodity channel index.
addADX() #Add Directional Movement Indicator
addCMF() #Add Money Flow chart

#To check the highest and lowest price of the stockfh
seriesHi(GOOG) # To check the highest point of price. 
seriesLo(GOOG) # To check the lowest point of price.

# Returns by day
Returns_by_day<-dailyReturn(GOOG)
head(Returns_by_day);tail(Returns_by_day)

# Returns by week
Returns_by_week<-weeklyReturn(GOOG)
head(Returns_by_week);tail(Returns_by_week)

#Returns by month
Returns_by_Month<-monthlyReturn(GOOG)
head(Returns_by_Month);tail(Returns_by_Month)

#Returns by year
Returns_by_year<-yearlyReturn(GOOG)
head(Returns_by_year);tail(Returns_by_year)

#---------Stock Analysis of Microsoft(2007-2023)--------

#str()command displays the contents of a list
str(MSFT)

#head() command returns the first 6 rows by default
head(MSFT)

#tail() command returns the last 6 rows by default
tail(MSFT)

#summary() command returns the minimum,maximum,mean,median,and 1st and 3rd quartiles for an numerical vector
summary(MSFT)

#Here the chart Series funtction cannot run because there is character column in this data. So first we have to delete this column so that we can execute chartSeries function
#To remove character column, we are assigning the value name column  to NULL
MSFT$Name<-NULL
head(MSFT)

#Data Pre-processing 
#first we have to check whether there is missing data in the data set.
#We can do this by introducing a function called sum and write the condition is.na(dataset) which tells us whether the count of empty(or)NA fields in data set
#If the sum is 0, then there is no missing data in the given data set.
sum(is.na(MSFT))

#plotting the graphs for open,high,low,close and a graph for the present year
plot(MSFT$MSFT.Open)
barChart(MSFT, bar.type = 'hlc', TA = NULL)
candleChart(MSFT, TA=NULL, subset = '2023')

#Using the chartSeries function to display the time vs price analysis
chartSeries(MSFT)
addMACD()# MACD is a trend-following momentum indicator that helps identify potential buying and selling opportunities in financial markets.
addBBands() # Adds Bollinger bands to the stock price.
addCCI() # Add Commodity channel index.
addADX() #Add Directional Movement Indicator
addCMF() #Add Money Flow chart

#To check the highest and lowest price of the stockfh
seriesHi(MSFT) # To check the highest point of price. 
seriesLo(MSFT) # To check the lowest point of price.

# Returns by day
Returns_by_day<-dailyReturn(MSFT)
head(Returns_by_day);tail(Returns_by_day)

# Returns by week
Returns_by_week<-weeklyReturn(MSFT)
head(Returns_by_week);tail(Returns_by_week)

#Returns by month
Returns_by_Month<-monthlyReturn(MSFT)
head(Returns_by_Month);tail(Returns_by_Month)

#Returns by year
Returns_by_year<-yearlyReturn(MSFT)
head(Returns_by_year);tail(Returns_by_year)

#---------Stock Analysis of APPLE(2007-2023)--------

#str()command displays the contents of a list
str(AAPL)

#head() command returns the first 6 rows by default
head(AAPL)

#tail() command returns the last 6 rows by default
tail(AAPL)

#summary() command returns the minimum,maximum,mean,median,and 1st and 3rd quartiles for an numerical vector
summary(AAPL)

#Here the chart Series funtction cannot run because there is character column in this data. So first we have to delete this column so that we can execute chartSeries function
#To remove character column, we are assigning the value name column  to NULL
AAPL$Name<-NULL
head(AAPL)

#Data Pre-processing 
#first we have to check whether there is missing data in the data set.
#We can do this by introducing a function called sum and write the condition is.na(dataset) which tells us whether the count of empty(or)NA fields in data set
#If the sum is 0, then there is no missing data in the given data set.
sum(is.na(AAPL))

#plotting the graphs for open,high,low,close and a graph for the present year
plot(AAPL$AAPL.Open)
barChart(AAPL, bar.type = 'hlc', TA = NULL)
candleChart(AAPL, TA=NULL, subset = '2023')

#Using the chartSeries function to display the time vs price analysis
chartSeries(AAPL)
addMACD()# MACD is a trend-following momentum indicator that helps identify potential buying and selling opportunities in financial markets.
addBBands() # Adds Bollinger bands to the stock price.
addCCI() # Add Commodity channel index.
addADX() #Add Directional Movement Indicator
addCMF() #Add Money Flow chart

#To check the highest and lowest price of the stockfh
seriesHi(AAPL) # To check the highest point of price. 
seriesLo(AAPL) # To check the lowest point of price.

# Returns by day
Returns_by_day<-dailyReturn(AAPL)
head(Returns_by_day);tail(Returns_by_day)

# Returns by week
Returns_by_week<-weeklyReturn(AAPL)
head(Returns_by_week);tail(Returns_by_week)

#Returns by month
Returns_by_Month<-monthlyReturn(AAPL)
head(Returns_by_Month);tail(Returns_by_Month)

#Returns by year
Returns_by_year<-yearlyReturn(AAPL)
head(Returns_by_year);tail(Returns_by_year)

#For data which is constantly enlarges, there is a possibility that it may contain missing values.So we have to preprocess this data and replace the missing values with the column(or)row's mean(or)median so that we can apply ML algorithms.
#We can also add missing values by this function and try the belwo process by this function
#[na.gen <- function(data,n) {
#  i <- 1
#  while (i < n+1) {
#    idx1 <- sample(1:nrow(data), 1)
#    idx2 <- sample(1:ncol(data), 1)
#    data[idx1,idx2] <- NA
#    i = i+1
# }
#  return(data)
#}]
#dataset <- na.gen(dataset,50)
#sum(is.na(dataset))->This will now display the sum as 50(missing values)

#The below function replaces the missing values with mean(or)median of that column.
#Mean
#[dataset.impute <- dataset
#for (i in which(sapply(dataset.impute, is.numeric))) { 
#  dataset.impute[is.na(dataset.impute[, i]), i] <- mean(dataset.impute[, i],  na.rm = TRUE) 
#}]
#Median
#[dataset.impute <- dataset
#for (i in which(sapply(dataset.impute, is.numeric))) { 
#  dataset.impute[is.na(dataset.impute[, i]), i] <- median(dataset.impute[, i],  na.rm = TRUE) 
#}]

#--------------Stock analysis of Amazon using machine learning-----------

#plot and convert to ln format
lnstock <- log(AMZN$close[1:700])
lnstock

#ACF,PACF and Dickey-Fuller Test
acf(lnstock, lag.max=20)
pacf(lnstock, lag.max=20)
difflnstock=diff(lnstock,1)
difflnstock
adf.test(lnstock)
adf.test(difflnstock)

#time series and auto.arima
pricearima<-ts(lnstock, start=c(2007,23), frequency = 12)
fitlnstock<-auto.arima(pricearima)
fitlnstock
plot(pricearima,type="l")
title('Amazon Price')
exp(lnstock)

#Forecasted Values From ARIMA
forecastedvalues_ln=forecast(fitlnstock,h=560)
forecastedvalues_ln
plot(forecastedvalues_ln)

forecastedvaluesextracted=as.numeric(forecastedvalues_ln$mean)
finalforecastvalues=exp(forecastedvaluesextracted)
finalforecastvalues

#Percentage Error
amzn <- data.frame(AMZN$close[700:1259], finalforecastvalues)
col_headings <- c("Actual Price", "Forecasted Price")
names(amzn) <- col_headings
percentage_error <- (amzn$`Actual Price` - amzn$`Forecasted Price`) / amzn$`Actual Price`
percentage_error
accuracy_measures <- accuracy(amzn$`Forecasted Price`, amzn$`Actual Price`)
accuracy_measures
mean(percentage_error)

#Ljung_Box
Box.test(fitlnstock$resid, lag=5, type="Ljung-Box")
Box.test(fitlnstock$resid, lag=10, type="Ljung-Box")
Box.test(fitlnstock$resid, lag=15, type="Ljung-Box")

#--------------Stock analysis of Gooogle using machine learning-----------

#plot and convert to ln format
lnstock=log(GOOG$close[1:500])
lnstock

#ACF,PACF and Dickey-Fuller Test
acf(lnstock, lag.max=20)
pacf(lnstock, lag.max=20)
difflnstock=diff(lnstock,1)
difflnstock
adf.test(lnstock)
adf.test(difflnstock)

#time series and auto.arima
pricearima<-ts(lnstock, start=c(2007,23), frequency = 12)
fitlnstock<-auto.arima(pricearima)
fitlnstock
plot(pricearima,type="l")
title('Google Price')
exp(lnstock)

#Forecasted Values From ARIMA
forecastedvalues_ln=forecast(fitlnstock,h=476)
forecastedvalues_ln
plot(forecastedvalues_ln)

forecastedvaluesextracted=as.numeric(forecastedvalues_ln$mean)
finalforecastvalues=exp(forecastedvaluesextracted)
finalforecastvalues

#Percentage Error
goog <- data.frame(AAPL$close[500:975], finalforecastvalues)
col_headings <- c("Actual Price", "Forecasted Price")
names(goog) <- col_headings
percentage_error <- (goog$`Actual Price` - goog$`Forecasted Price`) / goog$`Actual Price`
percentage_error
accuracy_measures <- accuracy(goog$`Forecasted Price`, goog$`Actual Price`)
accuracy_measures
mean(percentage_error)

#Ljung_Box
Box.test(fitlnstock$resid, lag=5, type="Ljung-Box")
Box.test(fitlnstock$resid, lag=10, type="Ljung-Box")
Box.test(fitlnstock$resid, lag=15, type="Ljung-Box")

#--------------Stock analysis of Microsoft using machine learning-----------

#plot and convert to ln format
lnstock=log(MSFT$close[1:700])
lnstock

#ACF,PACF and Dickey-Fuller Test
acf(lnstock, lag.max=20)
pacf(lnstock, lag.max=20)
difflnstock=diff(lnstock,1)
difflnstock
adf.test(lnstock)
adf.test(difflnstock)

#time series and auto.arima
pricearima<-ts(lnstock, start=c(2007,23), frequency = 12)
fitlnstock<-auto.arima(pricearima)
fitlnstock
plot(pricearima,type="l")
title('Microsoft Price')
exp(lnstock)

#Forecasted Values From ARIMA
forecastedvalues_ln=forecast(fitlnstock,h=560)
forecastedvalues_ln
plot(forecastedvalues_ln)

forecastedvaluesextracted=as.numeric(forecastedvalues_ln$mean)
finalforecastvalues=exp(forecastedvaluesextracted)
finalforecastvalues

#Percentage Error
msft <- data.frame(MSFT$close[700:1259], finalforecastvalues)
col_headings <- c("Actual Price", "Forecasted Price")
names(msft) <- col_headings
percentage_error <- (msft$`Actual Price` - msft$`Forecasted Price`) / msft$`Actual Price`
percentage_error
accuracy_measures <- accuracy(msft$`Forecasted Price`, msft$`Actual Price`)
accuracy_measures
mean(percentage_error)

#Ljung_Box
Box.test(fitlnstock$resid, lag=5, type="Ljung-Box")
Box.test(fitlnstock$resid, lag=10, type="Ljung-Box")
Box.test(fitlnstock$resid, lag=15, type="Ljung-Box")

#--------------Stock analysis of Apple using machine learning-----------

#plot and convert to ln format
lnstock=log(AAPL$close[1:700])
lnstock

#ACF,PACF and Dickey-Fuller Test
acf(lnstock, lag.max=20)
pacf(lnstock, lag.max=20)
difflnstock=diff(lnstock,1)
difflnstock
adf.test(lnstock)
adf.test(difflnstock)

#time series and auto.arima
pricearima<-ts(lnstock, start=c(2007,23), frequency = 12)
fitlnstock<-auto.arima(pricearima)
fitlnstock
plot(pricearima, type="l")
title('APPLE Price')
exp(lnstock)

#Forecasted Values From ARIMA
forecastedvalues_ln=forecast(fitlnstock,h=560)
forecastedvalues_ln
plot(forecastedvalues_ln)

forecastedvaluesextracted=as.numeric(forecastedvalues_ln$mean)
finalforecastvalues=exp(forecastedvaluesextracted)
finalforecastvalues

#Percentage Error
aapl <- data.frame(AAPL$close[700:1259], finalforecastvalues)
col_headings <- c("Actual Price", "Forecasted Price")
names(aapl) <- col_headings
percentage_error <- (aapl$`Actual Price` - aapl$`Forecasted Price`) / aapl$`Actual Price`
percentage_error
accuracy_measures <- accuracy(aapl$`Forecasted Price`, aapl$`Actual Price`)
accuracy_measures
mean(percentage_error)


#Ljung_Box
Box.test(fitlnstock$resid, lag=5, type="Ljung-Box")
Box.test(fitlnstock$resid, lag=10, type="Ljung-Box")
Box.test(fitlnstock$resid, lag=15, type="Ljung-Box")

#-----------------Definitions-------------------------

#MACD is a stock technical indicator
#Stock technical indicators are calculated by applying certain formula to stock prices and volume data. They are used to alert on the need to study stock price action with greater detail, confirm other technical indicator's signal or predict future stock prices direction
#MACD stands for Moving averages convergence/divergence. It consists of centered oscillator that measures a stock's price momentum and identifies trends.

#Bollinger Bands
#Bollinger Bands are a popular technical analysis tool used to visualize and analyze price volatility in financial markets. Bollinger Bands consist of three lines plotted on a price chart: a middle band, an upper band, and a lower band.
#The middle band is typically a simple moving average (SMA) of the price over a specified period. The upper and lower bands are calculated by adding and subtracting a specified number of standard deviations from the middle band.

#Commodity Channel Index
#Commodity Channel Index (CCI) is a technical analysis indicator used to identify overbought or oversold conditions in financial markets.
#The CCI measures the relationship between an asset's current price, its average price, and its standard deviation over a specified period.

#Directional Movement Indicator(DMI)
#The Directional Movement Indicator (DMI), also known as the Average Directional Index (ADX).
#It is a technical analysis indicator used to measure the strength and direction of a trend in financial markets. The DMI consists of three lines: the Positive Directional Indicator (+DI), the Negative Directional Indicator (-DI), and the Average Directional Index (ADX).

#Chaikin Money Flow
#The Chaikin Money Flow (CMF) is a technical analysis indicator used to measure the flow of money into or out of a financial instrument over a specified period.
#It combines price and volume data to determine whether a stock or asset is being accumulated or distributed by traders.

#ARIMA
#It stands for Autoregressive Integrated Moving Average is a widely used time series analysis and forecasting model. 
#It combines autoregressive (AR), moving average (MA), and differencing components to capture the underlying patterns and dynamics of a time series data.

#Autocorrelation Function (ACF):
#The ACF measures the correlation between a time series and its lagged values.
#It helps identify the relationship between an observation and its previous observations at different lags.
#The ACF can provide insights into the presence of seasonality or trend in the data.

#Partial Autocorrelation Function (PACF):
#The PACF measures the correlation between a time series and its lagged values while removing the effects of intermediate lags.
#It helps identify the direct relationship between an observation and its previous observations at specific lags, independent of the influence of other lags.

#Dickey-Fuller Test
#The Dickey-Fuller test, also known as the augmented Dickey-Fuller (ADF) test, is a statistical test used to determine whether a time series data has a unit root, which indicates non-stationary. 
#The test is commonly employed in econometrics and time series analysis to assess the stationarity of a variable.

#Ljung Box Test
#The Ljung-Box test, also known as the Ljung-Box Q test, is a statistical test used to assess the presence of autocorrelation in a time series data.
#It is commonly employed in econometrics and time series analysis to evaluate the randomness or lack of serial correlation in a variable.
