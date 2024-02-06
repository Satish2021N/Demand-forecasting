#libraries
library(dplyr)
library(ggplot2)
library(forecast)

#load the dataset
df = read.csv("F:/Demand Forecasting Thesis/Seasonality/Daily_Report_2020.csv")

#Transform the Date Variable
df$Date = strptime(x = df$Date,
                   format = "%m/%d/%Y")
df$Date = as.Date(df$Date)

#Pick the variables
df = df %>% select(Date,
                   Adults.Jackets,
                   Easter,
                   Thanksgiving,
                   Christmas,
                   Temperature)

#Change variable name
colnames(df)[2] = "y"

#Create bubble plot
ggplot(df, aes(x = Date, y = y)) +
  geom_line() +
  xlab("Time") +
  ylab("Jackets Demand") +
  theme(text = element_text(size = 20)) +
  scale_x_date(date_labels = "%Y %b")

##################### Model Preparation #################

#Training and test set
training = df %>% filter(Date < '2020-12-01')
test = df %>% filter(Date >= '2020-12-01')

#Transform the time series
#7 for daily, 52 for weekly, 12 for monthly, 4 for quarterly
#5 for weekdays
training_y = ts(training$y, frequency = 7)

#Extract the regressors
training_reg = as.matrix(training[, 3:ncol(training)])
test_reg = as.matrix(test[, 3:ncol(test)])

#Model
model = nnetar(y = training_y,
               p = 1,
               P = 1, 
               size = 3,
               xreg = training_reg,
               decay = 0.1)

#Predictions
predictions = forecast(model, xreg = test_reg)
plot(predictions)

#accuracy
accuracy(predictions$mean, test$y)


################## Save forecast ##########################

#Storing
neural = as.data.frame(predictions$mean)
colnames(neural)[1] = 'neural_networks'
write.csv(neural,
          file = "F:/Demand Forecasting Thesis/Ensemble/neural.csv",
          row.names = FALSE)























