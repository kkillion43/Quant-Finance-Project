TestModelPicked = function(Portfolio) {

  SetupTimeSeries(Portfolio)
  
  starting.year <- 2000
  ending.year <- 2015
  
  Sum.Returns.Training <- window(Sum.Returns.ts, start = c(starting.year, 1), end = c(ending.year, 12))
  Sum.Returns.Testing <- window(Sum.Returns.ts, start = c(ending.year+1, 1), end = c(ending.year+1, 12))
  
  # ARIMA (0,0,0) (0,0,1) selected for MonthlyHigh Portfolio

  if(attr(Portfolio, "metadata") == "Monthly High") {
    arima.model <- arima(Sum.Returns.Training, order = c(0, 0, 0), seasonal = c(0, 0, 1), include.mean = TRUE)
    model.metadata <- "ARIMA (0,0,0) (0,0,1)"
  }
  else if(attr(Portfolio, "metadata") == "Monthly Low") {
    # ARIMA (2,0,2) (1,0,0) selected for MonthlyLow Portfolio
    arima.model <- arima(Sum.Returns.Training, order = c(2, 0, 2), seasonal = c(1, 0, 0), include.mean = FALSE)
    model.metadata <- "ARIMA (2,0,2) (1,0,0)"
  }
  else if(attr(Portfolio, "metadata") == "Quarterly High") {
    # ARIMA (0,0,0) (0,0,1) selected for QuarterlyHigh Portfolio
    arima.model <- arima(Sum.Returns.Training, order = c(0, 0, 0), seasonal = c(0, 0, 1), include.mean = FALSE)
    model.metadata <- "ARIMA (0,0,0) (0,0,1)"
  }  
  else if(attr(Portfolio, "metadata") == "Quarterly Low") {
    # ARIMA (2,0,4) (0,0,1) selected for QuarterlyLow Portfolio
    arima.model <- arima(Sum.Returns.Training, order = c(2, 0, 4), seasonal = c(0, 0, 1), include.mean = TRUE)
    model.metadata <- "ARIMA (2,0,4) (0,0,1)"
  }  
  else if(attr(Portfolio, "metadata") == "Yearly High") {
    # ARIMA (0,0,0) (0,0,1) selected for YearlyHigh Portfolio
    arima.model <- arima(Sum.Returns.Training, order = c(0, 0, 0), seasonal = c(0, 0, 1), include.mean = FALSE)
    model.metadata <- "ARIMA (0,0,0) (0,0,1)"
  }  
  else {
    # ARIMA (2,0,0) (2,0,2) selected for YearlyLow Portfolio
    arima.model <- arima(Sum.Returns.Training, order = c(2, 0, 0), seasonal = c(2, 0, 2), include.mean = TRUE)
    model.metadata <- "ARIMA (2,0,0) (2,0,2)"
  }  
  
  arima.forecast <- forecast(arima.model, h=12)
  testacc <- data.frame(accuracy(arima.forecast, Sum.Returns.Testing))
  # RMSE and MAE are much lower for the test set than they were for the training set
  
  attr(arima.forecast, "metadata") <- model.metadata
  
  #http://r.789695.n4.nabble.com/How-to-get-last-day-of-a-month-td890694.html
  df.Forecast <- as.data.frame(cbind( Portfolio = attr(Portfolio, "metadata"),
                                      Model = attr(arima.forecast, "metadata"),
                                      Date = as.Date(as.yearmon(time(arima.forecast$mean), "%b%Y"), frac = 1),
                                      Return = exp(arima.forecast$mean)), 
                               stringsAsFactors = FALSE)
  
  # had to update these fields as they go to character for some reason
  df.Forecast$Return <- as.numeric(df.Forecast$Return)
  df.Forecast$Date <- as.Date(as.Date(as.yearmon(time(arima.forecast$mean), "%b%Y"), frac = 1)) 
  
  if(testModel == 1) {
    write.table(df.Forecast, "Forecasts.csv", sep = ",", row.names = FALSE)
  }
  else {
    write.table(df.Forecast, "Forecasts.csv", sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  
  testModel <<- testModel + 1
  
  if(attr(Portfolio, "metadata") == "Monthly High" | 
     attr(Portfolio, "metadata") == "Monthly Low" | 
     attr(Portfolio, "metadata") == "Quarterly High" |
     attr(Portfolio, "metadata") == "Yearly High") {
        Model_holt_2 <- holt(Sum.Returns.Training, h=12, damped = TRUE)
        attr(Model_holt_2, "metadata") <- "Holt Additive (Damped)"
        testacc <- as.data.frame(accuracy(Model_holt_2, Sum.Returns.Testing))
        df.Forecast <- as.data.frame(cbind(Portfolio = attr(Portfolio, "metadata"),
                                             Model = attr(Model_holt_2, "metadata"),
                                             Date = as.Date(as.yearmon(time(Model_holt_2$mean), "%b%Y"), frac = 1),
                                             Return = exp(Model_holt_2$mean)), 
                                      stringsAsFactors = FALSE)
        
        # had to update these fields as they go to character for some reason
        df.Forecast$Return <- as.numeric(df.Forecast$Return)
        df.Forecast$Date <- as.Date(as.Date(as.yearmon(time(Model_holt_2$mean), "%b%Y"), frac = 1)) 
  }
  else {
    if(attr(Portfolio, "metadata") == "Quarterly High") {
    ets_model <- ets(Sum.Returns.Training, model = "AAA", damped = TRUE)
    attr(ets_model, "metadata") <- "ETS(A, Ad, A)"
    # forecast and calculate errors
    ets_fc <- forecast(ets_model, h = 12)
    ets_MAE <- mean(abs(Sum.Returns.Testing - ets_fc$mean))
    ets_RMSE <- sqrt(mean((Sum.Returns.Testing - ets_fc$mean)^2))
    }
    
    else {
      ets_model <- ets(Sum.Returns.Training, model = "AAN", damped = TRUE)
      attr(ets_model, "metadata") <- "ETS(A, Ad, N)"
    }
  
    # forecast and calculate errors
    ets_fc <- forecast(ets_model, h = 12)
    ets_MAE <- mean(abs(Sum.Returns.Testing - ets_fc$mean))
    ets_RMSE <- sqrt(mean((Sum.Returns.Testing - ets_fc$mean)^2))
    df.Forecast <- as.data.frame(cbind(Portfolio = attr(Portfolio, "metadata"),
                                       Model = attr(ets_model, "metadata"),
                                       Date = as.Date(as.yearmon(time(ets_fc$mean), "%b%Y"), frac = 1),
                                       Return = exp(ets_fc$mean)), 
                                 stringsAsFactors = FALSE)
    # had to update these fields as they go to character for some reason
    df.Forecast$Return <- as.numeric(df.Forecast$Return)
    df.Forecast$Date <- as.Date(as.Date(as.yearmon(time(ets_fc$mean), "%b%Y"), frac = 1)) 
  }
  
  write.table(df.Forecast, "Forecasts.csv", sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
}
