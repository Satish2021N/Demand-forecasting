if(!require(ggplot2)) install.packages("ggplot2")
if(!require(forecast)) install.packages("forecast")
if(!require(dplyr)) install.packages("dplyr")
if(!require(prophet)) install.packages("prophet")
library(forecast)
library(dplyr)
library(ggplot2)

#LOADING DATASET
df = read.csv("F:/Demand Forecasting Thesis/Seasonality/Daily_Report_2020.csv")

#Changing the date format to POSTXLT format
df$Date = strptime(x=df$Date,
                   format = "%m/%d/%Y")

#Changing the POSTXlt format to date format and date type
df$Date = as.Date(df$Date)

#PICKING THE SPECIFIC VARIABLES
df = df %>% select(Date,
                   Adults.Jackets,
                   Easter,
                   Thanksgiving,
                   Christmas,
                   Temperature)

#Changing the name of the columns
colnames(df)[1]="ds"
colnames(df)[2]="y"

#Create Bubble plot
ggplot(df, aes(x=ds, y=y))+
  geom_line()+
  xlab("Time")+
  ylab("Jackets Demand")+
  theme(text = element_text(size=20))+
  scale_x_date(date_labels = "%Y %b")

#Isolate Easter Holiday
easter_dates = subset(df, df$Easter==1) #function subset, (data_set, condition
easter_dates = easter_dates$ds
easter = tibble(holiday='easter', 
                ds=easter_dates,
                lower_window=-4,
                upper_window=+2) #Using function tibble that has 3 things , we need to call exactlty the same holiday, ds, lower and upper windows

#Isolate Thanksgiving holiday
thanksgiving_dates = subset(df, df$Thanksgiving==1)
thanksgiving_dates = thanksgiving_dates$ds
thanksgiving = tibble(holiday="thanksgiving",
                      ds=thanksgiving_dates,
                      lower_window=-3,
                      upper_window=1) #tibble is a function that creates a data frame, and we have to specify what goes inside of it, specify the columns

#Merge Holidays
holidays = bind_rows(easter, thanksgiving) #Multiple dataframes by row

#Training and Test Set
training = df %>% 
  filter(ds < "2020-12-01") %>%
  select(ds,y, Christmas, Temperature)

test = df %>%
  filter(ds >= "2020-12-01") %>%
  select(ds, y, Christmas, Temperature)

#Facebook Prophet Model
#Since we are dealing with daily data, we have daily data as well so daily = False
m = prophet(holidays=holidays,
            yearly.seasonality = TRUE,
            weekly.seasonality = TRUE,
            daily.seasonality = FALSE,
            seasonality.mode = 'multiplicative',
            seasonality.prior.scale = 10, #Stength of seasonality 
            holidays.prior.scale = 10, #How holidays events can affect our seasonality curve
            changepoint.prior.scale = 0.05) #How flexible is our seasonlaity curve

#Adding Regressor
m = add_regressor(m, "Christmas")
m = add_regressor(m, "Temperature")

#Fit the model
m = fit.prophet(m, training)

#Regressor Coefficients How much the regressors affect our forecast
regressor_coefficients(m)
#Since we are using regressor_mode as multiplicative which is by default we read the coefficients in percentage, we need to multiply the coeffcient by 100
#Christmas doesnt have much impact on shelter whereas temperature plays vital role in shelter demand, as temperature decreases we have demand for shelter also decrease.

#Creating future dataframe which consists of future value and future values for regressors
future = make_future_dataframe(m,
                               periods = nrow(test)) #Since we have daily data we dont need to take care about frequency

future[,2:3] = df %>% select(Christmas, Temperature)


#Forecasting
forecast = predict(m, future)
#yhat --> actual prediction of the forecast

#Select specific values from the forecast dataset
forecast %>%
  select(ds, easter) %>%
  filter(abs(easter) >0) %>% #Selecting absolute value of easter, converts the negative values into positive and only shows the value other than zero
  filter(ds > "2020-01-01")
print(easter_dates)

forecast %>%
  select(ds, thanksgiving) %>%
  filter(abs(thanksgiving) >0) %>%
  filter(ds > "2020-01-01")
print(thanksgiving_dates)

#Visualization
plot(m,forecast) #General Foreast black lines is the observed value, Blue line is the acutal prediction,
prophet_plot_components(m, forecast) #For viewing components of time series
#Holidays chart represent the data of easter and thanksgiving, thanksgiving was sometime positive
#extra regressor represent the christmas, how it fluctuates, and explains in defining the error
plot(m, forecast) + add_changepoints_to_plot(m)
#The reason why trend doesnt change in the last few months of the year,  is because a rule in prophet that says a few month before the time series the trend doesnt change and continues to extrapolate the trend


#Accuracy
#Splitting the forecast yhat, predicted value to match that of test set
predictions = tail(forecast$yhat, nrow(test))
accuracy(predictions, test$y)

#Saving the forecast
prophet  = as.data.frame(predictions)
colnames(prophet)[1] = "Prophet"
write.csv(prophet,
          file = "F:/Demand Forecasting Thesis/Ensemble/prophet.csv",
          row.names = FALSE)


#To clear object stored in the current R session
rm(list = ls())

#To clear plot
dev.off()

#To clear console
cat("\014")
