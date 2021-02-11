library(readxl)
library(shiny)
library(forecast)
library(shinydashboard)
library(ggplot2)
library(gridExtra)
library(shinydashboard)

#read in dataset
forecast.data <- read_excel('C:/Users/610696/Desktop/Data/R WWI Data//Time_Series_Data.xlsx')


#create function to build timeseries dataframe 
create_ts_data <- function(k) {
  
vc <- k #k #input$vs

#filter data by vc
#forecast.proto <- as.data.frame(filter(forecast.data, forecast.data$SIK %in% vc))
forecast.proto <- subset(forecast.data, forecast.data$`Stock Item Key` == vc)

#used this to order the data by year 
forecast.proto <- forecast.proto[
  order( forecast.proto[,4] ),
]

#removing to create Time Series data
forecast.proto$`Stock Item` <- NULL
forecast.proto$`Stock Item Key` <- NULL

#formatting time series data
timeseries_data <- ts(forecast.proto[,1], start = c(2013,1), frequency = 12)
print(timeseries_data)
}


ui <- shinyUI(fluidPage(
  titlePanel("Forecasting Items Dashboard"),
  sidebarLayout( position = "left",
          sidebarPanel( "sidebar panel",
                        selectInput(inputId = "k_stock_item",
                                    label = "Stock Item Number",
                                    choices = forecast.data$`Stock Item Key`,
                                    ),
                        sliderInput(inputId = "length_forecast",
                                    label = "Select length to Forecast",
                                    min = 1,
                                    max = 36,
                                    value = 12),
                        sliderInput(inputId = "previous_months",
                                    label = "Select previous Months to Include",
                                    min = 6,
                                    max = 24,
                                    value = 12)),
          mainPanel(
                    column(9, plotOutput("Forecasting_Plot", width="800px",height="400px")),
                    column(9, DT::dataTableOutput("forecast_pred")))
  ))
)                              



server <- function(input, output) {
  
  
  
  output$Forecasting_Plot <- renderPlot({
    
    
    
    timeSeriesdata <- create_ts_data(input$k_stock_item)
    
    #arima model
    fit_arima <- auto.arima(create_ts_data(input$k_stock_item), d=1, D=1, stepwise = F, approximation = F, trace = TRUE) #ARIMA model

    
    #forecast visualization
    frcst <- forecast(fit_arima, h = input$length_forecast) #use input variables to decide the amount of months to forecast
    
    
    autoplot(frcst, include = input$previous_months)+ #use input variables to decide the amount of previous months to include
      ylab("Total Quantity Sold")+
      ggtitle("ARIMA Forecasting")+
      theme_classic()
    
  })

  
  output$forecast_pred <- DT::renderDataTable({
    
    
    timeSeriesdata <- create_ts_data(input$k_stock_item)
    
    #arima model
    fit_arima <- auto.arima(create_ts_data(input$k_stock_item), d=1, D=1, stepwise = F, approximation = F, trace = TRUE) #ARIMA model
    
    
    #forecast visualization
    frcst <- forecast(fit_arima, h = input$length_forecast) #use input variables to decide the amount of months to forecast
    
    table_df <- summary(frcst)
    
    #table_df_2 <- cbind(Dates = rownames(table_df), table_df)
   #rownames(table_df) <- 1:nrow(table_df)
    
    print(table_df)
    
  })

}


shinyApp(ui, server)
