PlotModel = function(Portfolio, Decomposed.Returns, Returns.Model, starting.year, ending.year, ets.Model = NULL, ForecastingPeriods = NULL){
  if(!is.null(Decomposed.Returns)) {
    par(mfrow=c(3,2))
    pdf.filename <- paste(attr(Portfolio, "metadata"), "decomp_start", starting.year, "end", ending.year, ".pdf", sep = "_")
    
    tryCatch ({
      pdf(pdf.filename, width = 7, height = 5)
      plot(Decomposed.Returns, col="black", main=paste(attr(Portfolio, "metadata"), "_decomp_start_", starting.year, "_end_", ending.year))
      },
      finally = {
        dev.off()
      }
    )
  }

  # if not ets forecast, plot normal forecast
  if(is.null(ets.Model)) {
    par(mfrow=c(1,1))
    pdf.filename <- paste(attr(Portfolio, "metadata"), attr(Returns.Model, "metadata"), "start", starting.year, "end", ending.year, ".pdf", sep = "_")
#    pdf.filename <- paste0(attr(Portfolio, "metadata"), attr(Returns.Model, "metadata"), " ", 
#                           eachTest, " ", eachIC," ", eachlSeasonalTest, "start", starting.year, "end", ending.year, " .pdf")
    tryCatch (
      {
        pdf(pdf.filename, width = 7, height = 5)
        plot(Returns.Model, main = paste(attr(Portfolio, "metadata"), attr(Returns.Model, "metadata"), "start", starting.year, "end", ending.year, sep = "_"))
      },
      finally = {
        dev.off()
      }
    )
  }
    # otherwise, plot ets forecast
  else {
    par(mfrow=c(1,1))
    pdf.filename <- paste(attr(Portfolio, "metadata"), attr(Returns.Model, "metadata"), "start", starting.year, "end", ending.year, ".pdf", sep = "_")
    tryCatch (
      {
        pdf(pdf.filename, width = 7, height = 5)
        plot(forecast(ets.Model, h=ForecastingPeriods), main = paste(attr(Portfolio, "metadata"), attr(Returns.Model, "metadata"), "start", starting.year, "end", ending.year, sep = "_"))
      },
      finally = {
        dev.off()
      }
    )
  }
}