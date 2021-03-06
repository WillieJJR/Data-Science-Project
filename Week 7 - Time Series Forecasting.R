library(readxl)

#read in WWI customer data
sales <- read_excel('C:/Users/610696/Desktop/Data/R WWI Data//Item_Quantity_Sales_by_Year.xlsx')
head(sales)
View(sales)

#Understanding the trending items by year
top2013_sales <- sales %>% 
  slice_max(order_by = sales$`2013`, n = 10)
top2013_sales
  #Top 10 Product sales for 2013 are Stock Item 29, 28, 42, 22, 32, 36, 41, 31, 34, 35

top2014_sales <- sales %>% 
  slice_max(order_by = sales$`2014`, n = 10)
top2014_sales
  #Top 10 Product Sales for 2014 are Stock Items 28, 29, 31, 35, 33, 41, 26, 21, 34, 37

top2015_sales <- sales %>% 
  slice_max(order_by = sales$`2015`, n = 10)
top2015_sales
  #Top 10 Product Sales for 2015 are Stock Items 29, 28, 32, 20, 40, 31, 33, 43, 39, 21

top2016_sales <- sales %>% 
  slice_max(order_by = sales$`2016`, n = 10)
top2016_sales
#Top 10 Product Sales for 2015 are Stock Items 29, 28, 31, 35, 223, 43, 40, 39, 41, 34


#read in WWI customer data
forecast.data <- read_excel('C:/Users/610696/Desktop/Data/R WWI Data//Time_Series_Data.xlsx')
head(forecast.data)

#Model data for forecasting & analysis

#subset data by stock item key
vc <- '41' #insert stock item key here
forecast.proto <- filter(forecast.data, forecast.data$`Stock Item Key` %in% vc)
#used this to order the data by year 
forecast.proto <- forecast.proto[
  order( forecast.proto[,4] ),
]

forecast.proto$`Stock Item` <- NULL
forecast.proto$`Stock Item Key` <- NULL


#Time Series Analysis 
library(fpp2)

#declare time series variable 
y <- ts(forecast.proto[,1], start = c(2013,1), frequency = 12)
plot(y)

#preliminary analysis

#time plot 
autoplot(y) + 
  ggtitle("Time Plot: Item #28 sales per month")+
  ylab("# of Sales per month")+
  theme_classic() #slight positive trend with very sporadic sale surges


#change of sales from month to month to highlight peaks and divots
DY <- diff(y)

autoplot(DY) + 
  ggtitle("Time Plot: Item #28 sales per month")+
  ylab("# of Sales per month")+
  theme_classic() #high variability of peaks and divots

#check seasonality
ggseasonplot(DY) +
  ggtitle("Seasonal: Change in Monthly Sales")+
  ylab("# of Sales per Month") # doesn't seem to be any pattern for sales variability 

#check seasonality by months
ggsubseriesplot(DY)+
  ggtitle("Seasonal: Change in Monthly Sales")+
  ylab("# of Sales per Month") #noticing that every other month has an increase in sales which is why the graph shows sparatic changes


#forecasting time series

#seasonal naive method 
fit <- snaive(DY) #residual standard deveiation is 2900.26
print(summary(fit))
checkresiduals(fit)

#fit ets method 
fit_ets <- ets(y) #residual standard deviation = 0.27
print(summary(fit_ets))
checkresiduals(fit_ets)

#fit ARIMA 
fit_arima <- auto.arima(y, d=1, D=1, stepwise = F, approximation = F, trace = TRUE) #residual standard deviation = 1310.149
print(summary(fit_arima))
checkresiduals(fit_arima)

#forecast predictions
frcst <- forecast(fit_arima, h = 1)
autoplot(frcst, include = 6)
print(summary(frcst)) 
