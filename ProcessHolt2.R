ProcessHolt2 = function(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, ForecastingPeriods, starting.year, ending.year, model.metadata) {
  # check Holt additive damped model 
  Model_holt_2 <- holt(Sum.Returns.Training, h=ForecastingPeriods, damped = TRUE)
  attr(Model_holt_2, "metadata") <- model.metadata
  
  PlotModel(Portfolio, Decomposed.Returns = NULL, Model_holt_2, starting.year, ending.year)
  
  testacc <- as.data.frame(accuracy(Model_holt_2, Sum.Returns.Testing))
  
  accuracies[df.num,] <<- list(portfolio = attr(Portfolio, "metadata"), model.name = attr(Model_holt_2, "metadata"),
                               starting.year = starting.year, ending.year = ending.year, 
                               MAE = testacc$MAE[2], RMSE = testacc$RMSE[2])
  
  df.num <<- df.num + 1
}
