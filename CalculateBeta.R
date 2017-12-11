CalculateBeta = function(Portfolio) {
## Calculate beta for use in modeling
## http://danialk.github.io/blog/2015/12/19/calculating-stocks-beta-using-r/
  betafit <- lm(Sum.Returns$LogReturns ~ SPY$LogReturns)
  betaresult <- summary(betafit)
  betas[nrow(betas)+1,] <<- list(portfolio = attr(Portfolio, "metadata"), portfolio.beta = betaresult$coefficients[2,1])
}

