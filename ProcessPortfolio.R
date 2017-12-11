ProcessPortfolio = function(Portfolio) {
  # setup time series for rolling time series method
  SetupTimeSeries(Portfolio)
  
  ## Calculate beta 
  CalculateBeta(Portfolio)
  
  # Basic data analysis
  
  par(mfrow=c(3,2))
  
  ## Use STL function for decomposition
  # The time series can be analysed using the stl function in order to seperate the trend, seasonality and remainder (remaining coincidential) 
  #   components from one another.
  starting.year <- 2000
  ending.year <- 2004
  
  while (ending.year <= 2015)
  {
    Sum.Returns.Training <- window(Sum.Returns.ts, start = c(starting.year, 1), end = c(ending.year, 12))
    Sum.Returns.Testing <- window(Sum.Returns.ts, start = c(ending.year+1, 1), end = c(ending.year+1, 12))
    Sum.Returns_stl <- stl(Sum.Returns.Training, s.window=7)

    # check Holt additive model (not damped)
    ProcessHolt1(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, Sum.Returns_stl, ForecastingPeriods = 12, starting.year, 
                 ending.year, model.metadata = "Holt Additive (Not Damped)")    

    # check Holt additive damped model
    ProcessHolt2(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, ForecastingPeriods = 12, starting.year, 
                 ending.year, model.metadata = "Holt Additive (Damped)")    
    
    # check ets model
    ProcessEts(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, ForecastingPeriods = 12, starting.year, 
               ending.year, model.metadata = "ets")    

    #check ARIMA Different Model Combinations....
    ProcessArima(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, ForecastingPeriods = 12, starting.year, ending.year,
                 xlimits = c(ending.year-1, ending.year+2))    

    # increment ending year for looping
    ending.year <- ending.year + 1
  }
  
  # setup time series for 80/20 time series method
  SetupTimeSeries(Portfolio)

  # 80 % would go roughly from beginning of 2000 to the 10th month of 2012
  # which leaves the remaining 20% for testing starting in the 11th month of 2012 and ending in the 12th month of 2015
  starting.year <- 2000
  ending.year <- "2012_10"
  
  Sum.Returns.Training <- window(Sum.Returns.ts, start = c(starting.year, 1), end = c(2012, 10))
  Sum.Returns.Testing <- window(Sum.Returns.ts, start = c(2012, 11), end = c(2015, 12))
  Sum.Returns_stl <- stl(Sum.Returns.Training, s.window=7)
  
  # forecast 38 periods since testing set is 38 periods
  
  # check Holt additive model (not damped)
  ProcessHolt1(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, Sum.Returns_stl, ForecastingPeriods = 38, starting.year, ending.year,
               model.metadata = "Holt Additive (80_20 Not Damped)")    
  
  # check Holt additive damped model
  ProcessHolt2(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, ForecastingPeriods = 38, starting.year, ending.year, 
               model.metadata = "Holt Additive (80_20 Damped)")    
  
  # check ets model
  ProcessEts(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, ForecastingPeriods = 38, starting.year, ending.year,
             model.metadata = "ets (80_20)")    
  
  #check ARIMA Different Model Combinations....
  ProcessArima(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, ForecastingPeriods = 38, starting.year, ending.year,
               xlimits = c(2011, 2017), TestingMethod = "80_20_TimeSeries")    
  
}
