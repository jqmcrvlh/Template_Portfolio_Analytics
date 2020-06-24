####https://www.marketwatch.com/story/these-nine-companies-are-working-on-coronavirus-treatments-or-vaccines-heres-where-things-stand-2020-03-06?mod=home-page

library(quantmod)
library(PerformanceAnalytics)
####Inform  only the Start date to study****************************************************
start <- "2018-01-01"
end <- Sys.Date()
#*****************************************************************************

####Fill the list of stocks using code as in Yahoo Finance********************
stocks <- c("GILD", "GSK", "INO", "JNJ", "MRNA", "REGN", "SNY", "TAK")
#*****************************************************************************

####Inform the name of namePortfolio, ComparPortfolio and titlePortfolio******
namePortfolio <- "COVID"
ComparPortfolio <- "SPY"
titlePortfolio <- "  8 stocks Coronavirus Treatments vs SPY"
#*****************************************************************************

####Inform the interest rate if you want, in a decimal base*******************
rf <- 0.0125
#*****************************************************************************

####The program will organize the vectors your work is finished***************
symbols <- append(stocks, "SPY", after = length(stocks)) 
print(symbols)
namecols <- c(namePortfolio, ComparPortfolio)

####get free data from yahoo and Select Adjuste values************************
rets <- list()
getSymbols(symbols, src = 'yahoo', from = start, to=end)
for(i in 1:length(symbols)) {
  returns <- Return.calculate(Ad(get(symbols[i])))
  colnames(returns) <- symbols[i]
  rets[[i]] <- returns
}

####Return Portfolio**********************************************************
rets <- na.omit(do.call(cbind, rets))
head(rets)

####Adjust the weight of portfolio*******************************************
por_weights <-c()
for(i in 1:length(stocks)){
  parDiv <- length(stocks)
  por_weights[i] <- 1/parDiv
}
print(por_weights)

port_mil <- rets[,1:8]
head(port_mil)
port_value = port_mil*por_weights
head(port_value)
port_value$Group <- rowSums(port_value)
head(port_value)

####Select a portfolio grouped*************************************************
port_total <- port_value$Group
head(port_total)

####Add return index***********************************************************
port_total$idex <- rets[,ncol(rets)]
colnames(port_total) <- namecols
head(port_total)
tail((port_total))

####Performance Summary compare with Major indicator**************************

Double_next.com <- c(1)
idex.col <- c(2)

####Chart Portfolio************************************************************
charts.PerformanceSummary(port_total[, c(Double_next.com , idex.col )], main = titlePortfolio, colorset=redfocus, lwd=4, ylog=TRUE )


####Chart Analytics************************************************************
chart.Correlation(port_total[, c(Double_next.com , idex.col )], method = 'pearson')

chart.Histogram(port_total[, c(Double_next.com)],methods = c('add.density', 'add.normal', 'add.risk'))

chart.Histogram(port_total[, c(idex.col)],methods = c('add.density', 'add.normal', 'add.risk'))

chart.RelativePerformance(Ra = port_total[, c(Double_next.com)], Rb = port_total[, c(idex.col)], main = "Relative Performance", xaxis = TRUE, ylog=TRUE)

####Sharpe Ratio**************************************************************
SharpeRatio(port_total[, c(Double_next.com , idex.col )], Rf= rf)

SortinoRatio(port_total[, c(Double_next.com , idex.col )], MAR = rf)

#####Returns******************************************************************
Return.annualized(port_total[, c(Double_next.com , idex.col )])

Return.annualized.excess(Rp = port_total[, c(Double_next.com)], Rb = port_total[, c(idex.col)])


## Return.relative(Ra = port_total[, c(Double_next.com)], Rb = port_total[, c(idex.col)], scale = 12 )

####Maximum Drawdown***********************************************************
maxDrawdown(port_total[, c(Double_next.com , idex.col )])


####downside risk (deviation, variance) of the return distribution*************
DownsideDeviation(port_total[, c(Double_next.com , idex.col )], method = "full", potential = FALSE)

DownsidePotential(port_total[, c(Double_next.com , idex.col )])


####Calculate the value at risk***********************************************
VaR(port_total[, c(Double_next.com , idex.col )], p=0.95)



###############xxxxxxxxxxxxxxxxxxxx#####################xxxxxxxxxxxxxxxx#################

# ## rebal_port1 <- Return.portfolio(R = ret_port1, weights = por_weights, rebalance_on = "weeks")
# 
# 
# rebalance <- Return.portfolio(R = ret_port1, weights = por_weights, rebalance_on = "weeks", verbose = TRUE)
# head(rebalance$EOP.Weight)
# head(rebalance$BOP.Weight)
# head(rebalance$contribution)
# head(rebalance$returns)
# 
# 
# ###Table of Annualized Return, Annualized Std Dev, and Annualized Sharpe###
# table.AnnualizedReturns(rebal_port1, scale = 12, Rf= rf/12)
# 
# ### Anualized comands and charts
# an_ret <- rebalance$returns
# head(an_ret)
# plot(cumsum(an_ret))
# 
# # Calculate the mean, volatility, and Sharpe ratio of returns
# returns_ann <- Return.annualized(an_ret)
# returns_ann
# sd_ann <- StdDev.annualized(an_ret)
# sd_ann
# sharpe_ann <- SharpeRatio.annualized(an_ret, Rf=(rf/12)/30)
# sharpe_ann
# 
# # Plotting the 12-month rolling annualized mean
# chart.RollingPerformance(R = an_ret, width = 30, FUN = "Return.annualized")
# 
# # Plotting the 12-month rolling annualized standard deviation
# chart.RollingPerformance(R = an_ret, width = 30, FUN = "StdDev.annualized")
# 
# # Plotting the 12-month rolling annualized Sharpe ratio
# chart.RollingPerformance(R = an_ret, width = 30, FUN = "SharpeRatio.annualized", Rf=.035/30)
# 
# 
# # Fill in window for 2008
# port2016 <- window(an_ret, start = "2016-01-01", end = "2016-12-31")
# 
# # Create window for 2014
# port2018 <- window(an_ret, start = "2018-01-01", end = "2018-12-31")
# 
# # Plotting settings
# par(mfrow = c(1, 2) , mar=c(3, 2, 2, 2))
# names(port2016) <- "port2016"
# names(port2018) <- "port2018"
# 
# # Plot histogram of 2008
# chart.Histogram(port2016, methods = c("add.density", "add.normal"))
# 
# # Plot histogram of 2014
# chart.Histogram(port2018, methods = c("add.density", "add.normal"))
# 
# #  Compute the skewness 
# skewness(an_ret)  
#   
# 
# # Compute the excess kurtosis 
# kurtosis(an_ret)
# 
# # Calculate the SemiDeviation
# SemiDeviation(an_ret)
# 
# # Calculate the value at risk
# VaR(an_ret,p = 0.05)
# VaR(an_ret,p = 0.025)
# 
# # Calculate the expected shortfall
# ES(an_ret,p = 0.05)
# ES(an_ret,p = 0.025)
# 
# # Plot histogram of returns
# chart.Histogram(an_ret, methods = c("add.density", "add.normal"))
# 
# 
# # Table of drawdowns
# table.Drawdowns(an_ret)
# 
# # Plot of drawdowns
# chart.Drawdown(an_ret)
