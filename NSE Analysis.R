# Lab Assignment 5+6
# Topic: Modelling, Evaluation and Visualization- Part 1: Data Collection
# Date: 11-11-2021

# Name: Sonali Shanbhag
# Reg No: 19BDS0114

# About the data source: I will be fetching data related to stocks, quote and index from the 'NSE (National Stock Exchange, India)', using the 
# 'nse2r' package. This package is community maintained and is not officially supported by 'NSE'. The accuracy of data is only as correct as
# provided on the official website (https://www.nseindia.com)

# For the level 1 of the dataset, it will contain data obtained at an interval of 5 mins from 9:00am-3:15pm, from dates 11th November to 16th November. 
# For level 2 of the dataset, it will be a continuation of level 1, with the data collection continuing until 28th November.

# Attributes:
# Symbol: An arrangement of characters, often related to the company name representing publicly-traded securities on an exchange. 
# Series: Categories for a stock under which trading can be done. EQ stands for equity and only permits intra-day transactions and equity delivery.
# Last_corp_announcement_date: The date on which a company announces important details about a decision that has a high impact on business, or the company's shareholders. 
# Last_corp_announcement: Important details about a decision that has a high impact on business, or the company's shareholders announced by the company.
# Open_price: The price of a security at the beginning of the trading day.
# High_price: The maximum price of a security in a given time period.
# Low_price: The minimum price of a security in a given time period.
# Last_traded_price: The last price that a trade occurred in a futures contract.
# Prev_close_price: A security's closing price on the preceding time period of the one being referenced.
# Percent_change: The absolute percentage price change of the security's price since the previous day's close.
# Traded_quantity: The total number of shares or contracts transacted for a specified security during a specified time period
# Turnover_in_lakhs: How much trading activity took place on a given business day in the market as a whole or individual stock, in lakhs.

#import the required packages
library(graphics)
library(utils)
library(base)
library(stats)
library(dplyr)
library(caTools)
library(ggplot2)

#read the dataset
df <- read.csv("https://raw.githubusercontent.com/anthoniraj/dsr_datasets_final/main/19BDS0114.csv")

#Data Cleaning and Pre-processing

#select only required columns
data <- select(df,c('timestamp', 'symbol', 'open_price', 'high_price', 'low_price',	'last_traded_price','prev_close_price',	'percent_change',	'traded_quantity',	'turnover_in_lakhs'))

#omit null values
data <- na.omit(data)

#Convert time stamp to Unix Time
data$timestamp<-as.numeric(as.POSIXct(data$timestamp, format="%d-%m-%Y %H:%M"))
data$timestamp

#Label Encoding Categorical Column: Symbol
symbols <- c(data$symbol)
symbols <- sort(symbols)
symbols <- unique(symbols)
factors <- factor(symbols)
length(symbols)
labels <- c(1:50)
data$symbol<- factor(data$symbol, levels=symbols,labels=labels)
head(data$symbol)

#define the linear model- in the syntax:
#lm(dependent_var ~ independent_var1 + independent_var2 +....., data=dataset)
model <- lm(high_price~open_price+low_price+last_traded_price+prev_close_price+percent_change+timestamp+symbol, data=data)
summary(model)

#define values for prediction- change for different predictions
op<-746.25
lp<-741.1
ltp<-771.5
pcp<-746.35
pc<-3.37
t<-1636618500
#write the name of the stock here
stock<-"M&M"
sym<-toString(which(symbols==stock)) #find the index in the categories array

#predict dependent variable using the predictors
pred<- predict(model, newdata=data.frame(open_price=op, low_price=lp, last_traded_price=ltp, prev_close_price=pcp, percent_change=pc, timestamp=t,symbol=sym))

#Display Results

#reverse the label encoding for meaningful output
print(paste0("Symbol: ", (symbols[strtoi(sym)]))) 

print(paste0("Open Price: ", op))
print(paste0("Low Price: ", lp))
print(paste0("Last Traded Price: ", ltp))
print(paste0("Previous Close Price: ", pcp))
print(paste0("Percent Change: ", pc))

#convert UNIX time stamp back to datetime for meaningful output
print(paste0("Timestamp: ", as.POSIXct(t, origin="1970-01-01"))) 

#display predicted value
print(paste0("Predicted High Price: ", pred))

# MODEL EVALUATION
#Encoding for dataset
symbols <- c(data$symbol)
symbols <- sort(symbols)
symbols <- unique(symbols)
factors <- factor(symbols)
labels <- c(1:50)
data$symbol<- factor(data$symbol, levels=symbols,labels=labels)
head(data$symbol)

#Split into Train and Test Set into a 80-20 ratio
split<-sample.split(data, SplitRatio=0.8)
train<-subset(data, split==TRUE)
test<-subset(data, split==FALSE)

#train the training data 
model_train <- lm(high_price~open_price+low_price+last_traded_price+prev_close_price+percent_change+timestamp+symbol, data=train)
summary(model_train)
pred<- predict(model_train, newdata=test)
price<-test$high_price

#calculate evaluation metrics
d = price-pred
mse = mean((d)^2)
mae = mean(abs(d))
rmse = sqrt(mse)
rsq = 1-(sum((d)^2)/sum((price-mean(price))^2))

print(paste0("Mean Square Error: ", mse))
print(paste0("Mean Absolute Error: Rs. ", mae))
print(paste0("Root Mean Square Error: Rs. ", rmse))
print(paste0("R-Squared Error: ", rsq))

print("CONCLUSION: Accuracy Measures are quite high (>>>>1) and definitely not appreciable for prediction of something as precise and detail-oriented as Stock Data. Thus, in my opinion using Multi-Variate Linear Regression would not be the best model for prediction, since the model might be overfitting. Using Random Forest might be a better option for the same.")

# Inference: Accuracy Measures are quite high (>>>>1) and definitely not appreciable for prediction of something 
#            as precise and detail-oriented as Stock Data. Thus, in my opinion using Multi-Variate Linear Regression 
#            would not be the best model for prediction, since the model might be overfitting. Using Random Forest 
#            might be a better option for the same.

#VISUALIZATION
dataset <- read.csv("https://raw.githubusercontent.com/anthoniraj/dsr_datasets_final/main/19BDS0114.csv")
#Plotting a line plot
head(dataset)
dataset <- filter(dataset, symbol=='M&M')

#Line Plot for high_price and low_price 
ggplot(dataset, aes(x=timestamp, group=1))+
geom_line(aes(y=high_price, color="green"))+
ggtitle("M&M High and Low Prices")+
geom_line(aes(y=low_price, color="red"))+
theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

#Box Plot for M&M Share
close<-dataset$prev_close_price
open<-data$open_price
high<-data$high_price
low<-data$low_price
boxplot(open, close, high, low, names=c("Close Price", "Open Price", "High Price", "Low Price"), main="Multiple Box Plots for M&M Share")

#Actual Prices vs Predicted Prices

#Note: library lubridate not available on VPL, so the prices are
#displayed as UNIX timestamps instead of datetime objects
#library(lubridate)
#test$timestamp <- as.datetime(test$timestamp)

eval <- data.frame(test$symbol, test$timestamp, pred, price)
eval<- filter(eval, test.symbol=="21")
head(eval)
ggplot(eval, aes(x=test.timestamp, group=1))+
geom_line(aes(y=pred, color="predicted"))+
ggtitle("Actual Prices vs Predicted Prices")+
geom_line(aes(y=price, color="actual"))+
theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
