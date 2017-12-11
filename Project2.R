rm(list = ls())
gc()
par(mfrow=c(1,1))
dev.off()

# working directory for importing data is Project folder
setwd("C:/Users/igota/OneDrive - Southern Methodist University/Documents/8310/Project")

library(dplyr)

#install.packages("fpp")
require(fpp)
#install.packages("forecast")
require(forecast)

library(TSPred)

# load monthly returns file
system.time(dfMonthly <- read.csv("dfMonthly.csv"))

MonthlyHighSymbols <- read.csv("MonthHighList_Symbols.csv",
                               header = FALSE)

MonthlyLowSymbols <- read.csv("MonthLowList_Symbols.csv",
                               header = FALSE)

QuarterlyHighSymbols <- read.csv("QuartHighList_Symbols.csv",
                               header = FALSE)

QuarterlyLowSymbols <- read.csv("QuartLowList_Symbols.csv",
                              header = FALSE)

YearlyHighSymbols <- read.csv("YearHighList_Symbols.csv",
                               header = FALSE)

YearlyLowSymbols <- read.csv("YearLowList_Symbols.csv",
                              header = FALSE)

# check out first few rows of dfMonthly
head(dfMonthly)

# check out structure of dfMonthly
str(dfMonthly)

# remove columns not needed
dfMonthly[, c("Close", "High", "Low", "Open", "Volume", "LogVolume", "Volatility")] <- NULL

# Log Adjusted Close to prevent issues with Negative Returns Months
dfMonthly$LogAdj.Close <- log(dfMonthly$Adj.Close)

#http://www.quantmod.com/documentation/Delt.html
library(quantmod)
dfMonthly$LogReturns <- Delt(dfMonthly$LogAdj.Close, k=1)

# need to convert dfMonthly Date field to Date type
dfMonthly$Date = as.Date(dfMonthly$Date, format = "%Y-%m-%d")

# recheck structure of dfMonthly
str(dfMonthly)

# Subset SPY (S&P 500) 
SPY <- dfMonthly[which(dfMonthly$Ticker == "SPY"),c(1, 2, 4, 5, 6)]

# check out first few rows of SPY
head(SPY)

# Subset into different portfolios based on Symbols
MonthlyHighVolatility <- dfMonthly[which(dfMonthly$Ticker %in% (MonthlyHighSymbols$V2)),c(1, 2, 4, 5, 6)]
metadata <- "Monthly High"
attr(MonthlyHighVolatility, "metadata") <- metadata

MonthlyLowVolatility <- dfMonthly[which(dfMonthly$Ticker %in% (MonthlyLowSymbols$V2)),c(1, 2, 4, 5, 6)]
metadata <- "Monthly Low"
attr(MonthlyLowVolatility, "metadata") <- metadata

QuarterlyHighVolatility <- dfMonthly[which(dfMonthly$Ticker %in% (QuarterlyHighSymbols$V2)),c(1, 2, 4, 5, 6)]
metadata <- "Quarterly High"
attr(QuarterlyHighVolatility, "metadata") <- metadata

QuarterlyLowVolatility <- dfMonthly[which(dfMonthly$Ticker %in% (QuarterlyLowSymbols$V2)),c(1, 2, 4, 5, 6)]
metadata <- "Quarterly Low"
attr(QuarterlyLowVolatility, "metadata") <- metadata

YearlyHighVolatility <- dfMonthly[which(dfMonthly$Ticker %in% (YearlyHighSymbols$V2)),c(1, 2, 4, 5, 6)]
metadata <- "Yearly High"
attr(YearlyHighVolatility, "metadata") <- metadata

YearlyLowVolatility <- dfMonthly[which(dfMonthly$Ticker %in% (YearlyLowSymbols$V2)),c(1, 2, 4, 5, 6)]
metadata <- "Yearly Low"
attr(YearlyLowVolatility, "metadata") <- metadata

# get header of Monthly High Volatility
head(MonthlyHighVolatility)

PortfolioList <- list(MonthlyHighVolatility, MonthlyLowVolatility, QuarterlyHighVolatility, QuarterlyLowVolatility,
                      YearlyHighVolatility, YearlyLowVolatility)

# working directory for writing any data is Project2 folder
setwd("C:/Users/igota/OneDrive - Southern Methodist University/Documents/8310/Project2")

# Rebuild function files in case of changes
source("ProcessPortfolio.R")
source("PlotModel.R")
source("ProcessArima.R")
source("ProcessEts.R")
source("ProcessHolt1.R")
source("ProcessHolt2.R")
source("SetupTimeSeries.R")
source("CalculateBeta.R")
source("TestModelPicked.R")

number.obs <- 72*3

# Initialize accuracies
accuracies <<- data.frame(portfolio = rep("X", number.obs), "model.name" = rep("X", number.obs), starting.year = rep.int(0, number.obs), 
                          ending.year = rep.int(0, number.obs), MAE = rep.int(0, number.obs), RMSE = rep.int(0, number.obs), 
                          stringsAsFactors = FALSE)

# Initialize betas
betas <<- data.frame(portfolio = character(), portfolio.beta = numeric(), 
                          stringsAsFactors = FALSE)
df.num <<- 1

system.time(lapply(PortfolioList, ProcessPortfolio))

write.csv(accuracies, "Accuracies.csv")
write.csv(betas, "Betas.csv")

Avg.Accuracies <- group_by_at(accuracies, vars(portfolio, model.name)) %>% summarise(AvgMAE = mean(MAE), AvgRMSE = mean(RMSE))
write.csv(Avg.Accuracies, "Average_Accuracies.csv")

# write actuals
WritePortfolioActuals = function(Portfolio) {
  # write portfolio actuals
  tmpActuals <- group_by(Portfolio, Date) %>% summarise(LogReturns = sum(LogReturns), Returns = sum(Returns))
  # convert to numeric
  tmpActuals$LogReturns <- as.numeric(tmpActuals$LogReturns)
  write.csv(tmpActuals, paste0(attr(Portfolio, "metadata"), ".csv"))
  rm(tmpActuals)
}

lapply(PortfolioList, WritePortfolioActuals)

# get forecasts for 2016 for models chosen (ARIMA and non-ARIMA)
testModel <<- 1
lapply(PortfolioList, TestModelPicked)
