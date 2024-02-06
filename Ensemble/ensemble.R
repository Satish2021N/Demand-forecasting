if(!require(ggplot2)) install.packages("ggplot2")
if(!require(forecast)) install.packages("forecast")
if(!require(dplyr)) install.packages("dplyr")
if(!require(prophet)) install.packages("prophet")
library(forecast)
library(dplyr)
library(ggplot2)
library(tidyr)

#LOADING DATASET
df = read.csv("F:/Demand Forecasting Thesis/Seasonality/Daily_Report_2020.csv")

#Changing the date format to POSTXLT format
df$Date = strptime(x=df$Date,
                   format = "%m/%d/%Y")

#Changing the POSTXlt format to date format and date type
df$Date = as.Date(df$Date)

#PICKING THE SPECIFIC VARIABLES
df = df %>% select(Date,
                   Jackets)

#Changing the name of the columns
colnames(df)[2]="y"

#Create Bubble plot
ggplot(df, aes(x=Date, y=y))+
  geom_line()+
  xlab("Time")+
  ylab("Adult Jackets")+
  theme(text = element_text(size=20))+
  scale_x_date(date_labels = "%Y %b")

#Loading the forecasts
sarimax = read.csv("F:/Demand Forecasting Thesis/Ensemble/sarimax.csv")
prophet = read.csv("F:/Demand Forecasting Thesis/Ensemble/prophet.csv")
neural = read.csv("F:/Demand Forecasting Thesis/Ensemble/neural.csv")

#Create a test data
test = df %>% filter(Date >= "2020-12-01")

#Ensemble DataFrame
ensemble = cbind(test, sarimax, prophet)

#Ensemble FOrecast
ensemble = transform(ensemble, 
                     ensemble_forecast = rowMeans(ensemble[,3:ncol(ensemble)]))

#Accuracy
accuracy(ensemble$ensemble_forecast, ensemble$y)


#Plotting
#Structuring out data first
ensemble %>% 
  pivot_longer(-c(Date),
               names_to = "forecast",
               values_to = "value") %>% 
  ggplot(aes(x=Date, y= value))+
  geom_line(aes(color=forecast), size=1)+
  theme_minimal()


