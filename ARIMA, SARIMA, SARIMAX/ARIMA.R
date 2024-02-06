if(!require("ggplot2")) install.packages("ggplot2")
if(!require("dplyr")) install.packages("dplyr")
if(!require("forecast")) install.packages("forecast")
install.packages("DT")

#Loading dataset
df = read.csv("F:/Demand Forecasting Thesis/Seasonality/Daily_Report_2020.csv")

#Transformation of Date variable into POSTXLT format and type format
df$Date = strptime(df$Date,
                   format = "%m/%d/%Y")

#Conversion of above POSTXLT format into date format
df$Date = as.Date(df$Date)

#Selecting specific columns from the data frame
df = df %>% select(Date,
                   Adults.Jackets,
                   Easter,
                   Thanksgiving,
                   Christmas,
                   Temperature)

#Regressors -- > Easter, Thanksgiving, Christmas, Temperature

#Changing the colname
colnames(df)[2] = "y"

#Plotting ggplot
p <- ggplot(df, aes(x=df$Date, y=y))+
  geom_line()+
  xlab("Time")+
  ylab("Shelter Demand")+
  theme(text = element_text(size = 20))+
  scale_x_date(date_labels = "%Y %b")
p
ggplotly(p)

#Training Set and Test Set Division
#Since we have regressors as well and we want to include it as well, we will be doing in differnt format
training_set = df %>% filter(Date < "2020-12-01")
test_set = df %>% filter(Date >= "2020-12-01")

training_set

#Time Series object conversion
#7 or 365 if daily, if weekly 52, if monthly 12 months, if quarterly 4
training_y = ts(data=training_set$y,
                frequency = 7)
#Encourage to use 7 because empirically it works best,
#when you have periods of seven, it means that you have so much more periods in order to define the seasonality.
#If you have 365, you don't really have a lot of complete periods to define it.

#Auto-correlation plot
acf(training_y)
#Auto correlation plot justifies
#There is a strong relationship in the past
#Correlation is almost close to 1.
#Our time series is a simple one
#we would see that the information decreases over time
#powerful plot because it really does tell you in advance what is the difficulty of your problem

#Stationarity
#To find out how many times we have to do in order to make our data stationary
#ndiffs function stands for number of differences
#We don't have to do anything to make the model stationary the automated function of the ARIMA family will do it for us.
ndiffs(x=training_y, test = "adf")

#Storing or Isolating the regressors inside of a matrix
training_set_reg = as.matrix(training_set[,3:6])
test_set_reg = as.matrix(test_set[,3:6])

#SARIMAX model
model = auto.arima(y= training_y,
                   stepwise = FALSE,
                   approximation = FALSE,
                   xreg = training_set_reg)
summary(model)

#Forecasting 
predictions_sarimax = forecast(model, xreg = test_set_reg)

#Plotting
autoplot(predictions_sarimax)

#Accuracy
accuracy(predictions_sarimax$mean, test_set$y)
FA#MAPE is 0.2% which is absolutely brilliant

#Saving our forecast
sarimax = as.data.frame(predictions_sarimax$mean)
colnames(sarimax)[1] = "SARIMAX"
write.csv(sarimax,
          file = "Saving Forecast/Sarima.csv",
          row.names = FALSE ) #rownames set to false since we dont want any row name

#For removing out object, variables stored in current R session
rm(list = ls())

#To clear plot
dev.off()

#For clearing the console
cat("\014")
