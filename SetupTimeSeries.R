SetupTimeSeries = function(Portfolio) {
  Sum.Returns <<- group_by(Portfolio, Date) %>% summarise(LogReturns = sum(LogReturns))
  # convert to numeric
  Sum.Returns$LogReturns <<- as.numeric(Sum.Returns$LogReturns)
  
  # setup time series
  Sum.Returns.ts <<- ts(Sum.Returns$LogReturns, start = c(2000, 1), frequency = 12)  
}
