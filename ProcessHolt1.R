ProcessHolt1 = function(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, Sum.Returns_stl, ForecastingPeriods, starting.year, ending.year,
                        model.metadata) {
  
  # check Holt additive model (not damped) 
  Model_holt_1 <- holt(Sum.Returns.Training, h=ForecastingPeriods)
  attr(Model_holt_1, "metadata") <- model.metadata
  
  PlotModel(Portfolio, Sum.Returns_stl, Model_holt_1, starting.year, ending.year)
  
  testacc <- as.data.frame(accuracy(Model_holt_1, Sum.Returns.Testing))
  
  
  accuracies[df.num,] <<- list(portfolio = attr(Portfolio, "metadata"), model.name = attr(Model_holt_1, "metadata"),
                               starting.year = starting.year, ending.year = ending.year, 
                               MAE = testacc$MAE[2], RMSE = testacc$RMSE[2])
  
  df.num <<- df.num + 1
}
