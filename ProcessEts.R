ProcessEts = function(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, ForecastingPeriods, starting.year, ending.year,
                      model.metadata) {
  # check ets model (test best error and seasonal components. We defaulted the trend component is additive)
  ets_model <- ets(Sum.Returns.Training, model = "ZAZ")
  attr(ets_model, "metadata") <- model.metadata

  # forecast and calculate errors
  ets_fc <- forecast(ets_model, h = ForecastingPeriods)
  ets_MAE <- mean(abs(Sum.Returns.Testing - ets_fc$mean))
  ets_RMSE <- sqrt(mean((Sum.Returns.Testing - ets_fc$mean)^2))
  
  
  PlotModel(Portfolio, Decomposed.Returns = NULL, ets_model, starting.year, ending.year, ets_model, ForecastingPeriods)
  
  accuracies[df.num,] <<- list(portfolio = attr(Portfolio, "metadata"), model.name = attr(ets_model, "metadata"),
                               starting.year = starting.year, ending.year = ending.year, 
                               MAE = ets_MAE, RMSE = ets_RMSE)
  
  df.num <<- df.num + 1
}
