forecast.data <- read_excel('C:/Users/610696/Desktop/Data/R WWI Data//Time_Series_Data.xlsx')

vc <- '108' #insert stock item key here


#used to filter the data by the value in vc
forecast.proto <- filter(forecast.data, forecast.data$`Stock Item Key` %in% vc)

#used this to order the data by year 
forecast.proto <- forecast.proto[
  order( forecast.proto[,4] ),
]

#removing to create Time Series data
forecast.proto$`Stock Item` <- NULL
forecast.proto$`Stock Item Key` <- NULL
####################
###################







###### Time Series Forecast Modeling ######
library(fpp2)

#declare ts variable 
y <- ts(forecast.proto[,1], start = c(2013,1), frequency = 12)
DY <- diff(y)

fit <- snaive(DY) #Naive model
print(summary(fit))
checkresiduals(fit)

####fit ets method 
fit_ets <- ets(y) #Exponential Smoothing model
print(summary(fit_ets))
checkresiduals(fit_ets)

####fit ARIMA 
fit_arima <- auto.arima(y, d=1, D=1, stepwise = F, approximation = F, trace = TRUE) #ARIMA model
print(summary(fit_arima))
checkresiduals(fit_arima)








#####Time Series Predictions#####
frcst <- forecast(fit_arima, h = 24)
autoplot(frcst, include = 12)+
  ylab("Total Quantity Sold")+
  theme_classic()
print(summary(frcst)) 