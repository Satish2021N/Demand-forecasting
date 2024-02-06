#Importing necessary packages
if(!require("forecast")) install.packages("forecast")
if(!require("dplyr")) install.packages("dplyr")
if(!require("ggplot2")) install.packages("dplyr2")


#Importing the dataset
dataFrame = read.csv("F:/Demand Forecasting Thesis/Seasonality/Daily_Report_2020.csv")

#Transforming the date variable
#Converting the date variable type to POSIXlt format
dataFrame$Date = strptime(x = dataFrame$Date, format = "%m/%d/%Y")

#Converting the POSIXlt format into date format
dataFrame$Date = as.Date(dataFrame$Date)

#Selecting the variblaes we want to work with
dataFrame = dataFrame %>% select (Date,
                           Adults.Jackets,
                           Thanksgiving,
                           Christmas,
                           Temperature)

#Changing the variable name
colnames(dataFrame)[2]= "y"

#Creating a bubble plot
ggplot(dataFrame, aes(x=Date, y =y ))+
  geom_line()+
  xlab("Time")+
  ylab("Jacket Demand")+
  theme(text = element_text(size=20))+
  scale_x_date(date_labels = "%Y %b")


#Transform times series object
#The function ts is used to create time-series objects.
y = ts(data = dataFrame$y,
       start= c(2014,1), #In the dataframe the starting date if 1st of January 2014
       frequency = 365.25)
plot.ts(y)

#Plotting Seasonality Plot
#This is like a time plot except that the data are plotted against the seasons in separate years.
#Plots a seasonal plot 
ggseasonplot(x= y,
             main="Seasonality Graph") 

#Additive Decomposition
decomposition_additive = decompose(x = y,
                                 type="additive") #Decomposes our times series data into trend, seasonality and error
plot(decomposition_additive)

#Seasonal Decomposition
#Removing the trend plot from out observed plot, showing both the seasonality and error
detrend_additive = decomposition_additive$x - decomposition_additive$trend
plot(detrend_additive)

#Removing the seasonality plot
desasonal_additive = detrend_additive - decomposition_additive$seasonal
plot(desasonal_additive)

#Multiplicative Decomposition
decompostion_mulitplicate = decompose(x=y,
                                      type="multiplicative")
plot(decompostion_mulitplicate)

#Seasonal Decomposition -- Mulitplicative
#In mulitplicative decomposition we either multiply or divide not subtract
#Removing the trend plot from our observed plot to observe seasonal cycles
detrend_multiplicative = decompostion_mulitplicate$x / decompostion_mulitplicate$trend
plot(detrend_multiplicative)

#Removing both the trend and seasonal cycles to understand error
deseasonal_multiplicative = detrend_multiplicative / decompostion_mulitplicate$seasonal
plot(deseasonal_multiplicative)


# Clear packages

detach("package:datasets", unload = TRUE) # For base

# Clear plots
dev.off()  # But only if there IS a plot

#To remove the objects, variables from the current r session
rm(list = ls())

# Clear console
cat("\014")  # ctrl+L







