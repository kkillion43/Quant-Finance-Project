ProcessArima = function(Portfolio, Sum.Returns.Training, Sum.Returns.Testing, ForecastingPeriods, starting.year, ending.year, xlimits,
                        TestingMethod = c("Rolling_TimeSeries", "80_20_TimeSeries")) {
  # ARIMA code adapted from Chad's code
  #*****************
  #ARIMA Different Model Combinations....
  
  #********
  #Begin Test Area
  lData <- list("Sum.Returns.Training")
  lTest <- list("kpss","adf","pp")
  lIc <- list("aic","bic","aicc")
  lSeasonalTest <- list("ocsb","ch")
  
  for (eachTest in lTest){
    for (eachIC in lIc){
      for (eachlSeasonalTest in lSeasonalTest){
        for (eachlData in lData){
          
          Amodel <-  auto.arima(Sum.Returns.Training, test=eachTest, seasonal.test = eachlSeasonalTest, stepwise = TRUE, ic=eachIC)
          
          arima1.forecast <- forecast(Amodel, h=ForecastingPeriods) 
          testacc <- data.frame(accuracy(arima1.forecast, Sum.Returns.Testing))
          accuracies[nrow(accuracies)+1,] <<- list(portfolio = attr(Portfolio, "metadata"), 
                                                   model.name = paste0(Amodel),
                                                   starting.year = starting.year, ending.year = ending.year, 
                                                   MAE = testacc$MAE[2], RMSE = testacc$RMSE[2])
          
          #plot to pdf
          tryCatch (
            {
              if (TestingMethod == "Rolling_TimeSeries") {
                pdf.filename <- paste0(attr(Portfolio, "metadata"), " ", Amodel, " ", 
                                       eachTest, " ", eachIC," ", eachlSeasonalTest, "start", starting.year, "end", ending.year, " .pdf")
                pdf( pdf.filename, width=7,height=5)
                #pdf( paste0(attr(Portfolio, "metadata"), " ", Amodel, " start ", 
                #          starting.year, " end ", ending.year, ".pdf"),width=7,height=5)
              plotarimapred(Sum.Returns.Testing, Amodel, xlim=xlimits, range.percent = 0.05)
              }
              else {
                pdf( paste0(attr(Portfolio, "metadata"), " 80_20_TimeSeries ", Amodel, " start ", 
                            starting.year, " end ", ending.year, ".pdf"),width=7,height=5)
                plotarimapred(Sum.Returns.Testing, Amodel, xlim=xlimits, range.percent = 0.05)
              }
            },
            finally = {
              dev.off()
            }
          )
        }
      }
    }
  }
}
