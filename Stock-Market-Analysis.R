# Loading of the library
library(xts)
library(zoo)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(grid)
library(tseries)
library(forecast)
library(rugarch)
library(quantmod)

# Preprocessing of the data
start <- as.Date("2014-01-01")
end <- as.Date("2019-10-01")
#get the data for Apple/Microsoft/Google
getSymbols("AAPL", src = "yahoo", from = start, to = end)

head(AAPL)

AAPL=data.frame(date=index(AAPL), coredata(AAPL))
i_stock=AAPL

# Set the size of the plots
options(repr.plot.width=12, repr.plot.height=6) 
# Get the distribution of the open/close/high low prices of Apple
p1 = ggplot(i_stock, aes(AAPL.Open)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
p2 = ggplot(i_stock, aes(AAPL.High)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
p3 = ggplot(i_stock, aes(AAPL.Low)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
p4 = ggplot(i_stock, aes(AAPL.Close)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))
grid.arrange(p1,p2, nrow=1,ncol=2)

## Create a daily Date object
inds <- seq(as.Date("2014-01-04"), as.Date("2018-10-01"), by = "day")

# For ts function, freq: 365.5=daily1=annual, 4=quartly, 12=monthly
create_ts <- function(col_idx){
  ## Create a time series object
  i_ts <- as.numeric(i_stock[,col_idx]) %>%
    tsclean(replace.missing = TRUE, lambda = NULL) %>%
    ts(start = c(2014, as.numeric(format(inds[1], "%j"))),
       frequency = 365.5)
  return(i_ts)
}

# Check the High price of Apple stock at a daily basis
i_ts = create_ts(which(colnames(i_stock) == "AAPL.High"))
plot.ts(i_ts, xlab = "Time", ylab = "High value", main = "Time Series", col = "red")

# Augmented Dickey-Fuller test to check if the high price curve is stationary
adf.test(i_stock[,which(colnames(i_stock) == "AAPL.High")], alternative = "stationary", k = 0)

# Decomposing of the time series
i_tscomponents <- decompose(i_ts)
plot(i_tscomponents, col = "red")

i_tsdiff1 <- diff(i_ts, differences=1)
plot.ts(i_tsdiff1, col = "red")
# The time series (above) appears to be stationary.

acf(i_ts, lag.max=60)             # plot a correlogram

acf(i_tsdiff1, lag.max=60)             # plot a correlogram
acf(i_tsdiff1, lag.max=60, plot=FALSE) # get the autocorrelation values

pacf(i_tsdiff1, lag.max=60)             # plot a partial correlogram
pacf(i_tsdiff1, lag.max=60, plot=FALSE) # get the partial autocorrelation values

i_tsarima <- auto.arima(i_ts, max.p = 3, max.q = 3, max.d = 3)
i_tsarima

i_tsforecasts <- forecast(i_tsarima, h = 60)
plot(i_tsforecasts, col = "red")

garch11.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
#estimate model 
garch11.fit=ugarchfit(spec=garch11.spec, data=i_ts)

f=ugarchforecast(garch11.fit, n.ahead=20)
f

# remove all the history from the previous part
rm(list=ls())


#load packages, be careful multiple dependency between packages
# better to install packages in order
#install.packages("devtools")
library(devtools)
# Install from github directly
#require(devtools)
# will be asked to install Rtools during the process
#install_github("braverock/blotter") 
#install_github("braverock/quantstrat")
library(blotter)
library(quantstrat)
#install.packages("remotes")
#require(remotes)
#remotes::install_github("pdrano/IKTrading")
library(IKTrading)
#install.packages("quantmod")
library(quantmod)
library(TTR)
library(dplyr)


start <- as.Date("2016-01-01")
end <- as.Date("2016-10-01")
#get the data for Apple
getSymbols("AAPL", src = "yahoo", from = start, to = end, getSymbols.warning4.0=FALSE)
# Let's get data for Microsoft (MSFT) and Google (GOOG) 
getSymbols(c("MSFT", "GOOG"), src = "yahoo", from = start, to = end,getSymbols.warning4.0=FALSE)
head(AAPL)

#visualize the data
# lineplot for close price
plot(AAPL[, "AAPL.Close"], main = "Close Price of AAPL")

#Bollinger Band chart, % Bollinger change,Volume Traded and Moving Average Convergence Diverence
chartSeries(AAPL,TA='addBBands();addVo();addMACD()')

#candlestick plot
#candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")
#We not just need to look into the Apple, but also need to compare it with other stocks.
# Create an xts object (xts is loaded with quantmod) that contains closing
# prices for AAPL, MSFT, and GOOG
stocks <- as.xts(data.frame(AAPL = AAPL[, "AAPL.Close"], MSFT = MSFT[, "MSFT.Close"], 
                            GOOG = GOOG[, "GOOG.Close"]))
head(stocks)

par(mfrow=c(1,2))
# transformation 1: return since the beginning of period
stock_return = apply(stocks, 1, function(x) {x / stocks[1,]}) %>% t %>% as.xts
plot(as.zoo(stock_return), screens=1,lty = 1:3, xlab = "Date", ylab = "Return",main="Return since the beginning")
legend("topleft", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.6)

# transformation 2 :log difference
stock_change = stocks %>% log %>% diff
plot(as.zoo(stock_change), screens=1, lty = 1:3, xlab = "Date", ylab = "Log Difference", main="Log difference in price")
legend("topleft", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.6)

# in order to have the complete moving average in a year, we plan to extend the data
start = as.Date("2010-01-01")
getSymbols(c("AAPL", "MSFT", "GOOG"), src = "yahoo", from = start, to = end)
#candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")
#the 20-day, 50-day, and 200-day moving averages
#addSMA(n = c(20, 50, 200))
# using addSMA() after candleChart() will produces two charts
# to produce one chart with the two indicators, we use chartSeries()
chartSeries(AAPL, type="candlesticks",up.col = "black", dn.col = "red", theme = "white",
            subset = "2016-01-04/",TA='addSMA(n = c(20, 50, 200));addVo()')

# get more data
start <- as.Date("2010-01-01")
end <- as.Date("2016-10-01")
getSymbols("AAPL", src="yahoo", from = start, to = end)
AAPL_sma_20 <- SMA(Cl(AAPL),  # The closing price of AAPL, obtained by quantmod's Cl() function
                   n = 20)     # The number of days in the moving average window

AAPL_sma_50 <- SMA(Cl(AAPL),n = 50)
AAPL_sma_200 <- SMA(Cl(AAPL),n = 200)
AAPL_trade <- AAPL
AAPL_trade$`20d` <- AAPL_sma_20
AAPL_trade$`50d` <- AAPL_sma_50

regime_val <- sigComparison("", data = AAPL_trade,
                            columns = c("20d", "50d"), relationship = "gt") -
  sigComparison("", data = AAPL_trade,
                columns = c("20d", "50d"), relationship = "lt")

plot(regime_val, main = "Regime", ylim = c(-2, 2))

# put the identified regimes with the other indicators together
# again, we can use the command step by step to add each indicator
#candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")
#addSMA(n = c(20, 50), on = 1, col = c("red", "blue"))
#addLines(h = 0, col = "black", on = 3)
#addTA(regime_val, col = "blue", yrange = c(-2, 2))

# we also can add them together with one long command
chartSeries(AAPL, type="candlesticks",up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/",
            TA='addVo();addSMA(n = c(20, 50), on = 1, col = c("red", "blue"));
                addLines(h = 0, col = "black", on = 3);
                addTA(regime_val, col = "blue", yrange = c(-2, 2))')

# see bullish on Apple stock for 1034 days, and bearish for 616 days
table(as.vector(regime_val))

sig <- diff(regime_val) / 2
plot(sig, main = "Signal", ylim = c(-2, 2))

table(sig)

## The Cl function from quantmod pulls the closing price from the object
Cl(AAPL)[which(sig == 1)]#1 indicating "buy"
Cl(AAPL)[which(sig == -1)] #-1 indicating "sell"
# the profit is calculate as
as.vector(Cl(AAPL)[sig == 1])[-1] - Cl(AAPL)[sig == -1][-table(sig)[["1"]]]

rm(list = ls(.blotter), envir = .blotter)  # Clear blotter environment
currency("USD")  # Currency being used
Sys.setenv(TZ = "MDT")  # Allows quantstrat to use timestamps
AAPL_adj <- adjustOHLC(AAPL,use.Adjusted = TRUE)
stock("AAPL_adj", currency = "USD", multiplier = 1)
initDate <- "1990-01-01"  # A date prior to first close price, used in function initAcct()

strategy_st <- portfolio_st <- account_st <- "SMAC-20-50"  # Names of objects
rm.strat(portfolio_st)  # Need to remove portfolio from blotter env
rm.strat(strategy_st)   # Ensure no strategy by this name exists either
initPortf(portfolio_st, symbols = "AAPL_adj",  # This is a simple portfolio
          # trading AAPL only
          initDate = initDate, currency = "USD")

initAcct(account_st, portfolios = portfolio_st,  # Uses only one portfolio
         initDate = initDate, currency = "USD",
         initEq = 1000000)  # Start with a million dollars
# Initialize the order container; will
# contain all orders made by strategy
initOrders(portfolio_st, store = TRUE)  

# define the strategy.
# store = TRUE tells function to store in the .strategy environment
strategy(strategy_st, store = TRUE)  

# Now define trading rules
# Indicators are used to construct signals
add.indicator(strategy = strategy_st, name = "SMA",     # SMA is a function
              arguments = list(x = quote(Cl(mktdata)),  # args of SMA
                               n = 20),
              label = "fastMA")
add.indicator(strategy = strategy_st, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = 50),
              label = "slowMA")

# Next comes trading signals
add.signal(strategy = strategy_st, name = "sigComparison",
           arguments = list(columns = c("fastMA", "slowMA"),
                            relationship = "gt"),
           label = "bull")
add.signal(strategy = strategy_st, name = "sigComparison",
           arguments = list(columns = c("fastMA", "slowMA"),
                            relationship = "lt"),
           label = "bear")

#rules that generate trades of buy or sell
add.rule(strategy = strategy_st, name = "ruleSignal",  # Almost always this one
         arguments = list(sigcol = "bull",  # Signal (see above) that triggers
                          sigval = TRUE,
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open",
                          osFUN = osMaxDollar,
                          # The next parameter, which is a parameter passed to
                          # osMaxDollar, will ensure that trades are about 10%
                          # of portfolio equity
                          maxSize = quote(floor(getEndEq(account_st,
                                                         Date = timestamp) * .1)),
                          tradeSize = quote(floor(getEndEq(account_st,
                                                           Date = timestamp) * .1))),
         type = "enter", path.dep = TRUE, label = "buy")

add.rule(strategy = strategy_st, name = "ruleSignal",
         arguments = list(sigcol = "bear",
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open"),
         type = "exit", path.dep = TRUE, label = "sell")

# apply trade strategy now
applyStrategy(strategy_st, portfolios = portfolio_st)

# analyze trade strategy result
updatePortf(portfolio_st)
dateRange <- time(getPortfolio(portfolio_st)$summary)[-1]
updateAcct(portfolio_st, dateRange)
updateEndEq(account_st)
tStats <- tradeStats(Portfolios = portfolio_st, use="trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
print(data.frame(t(tStats[, -c(1,2)])))

final_acct <- getAccount(account_st)
plot(final_acct$summary$End.Eq["2010/2016"], main = "Portfolio Equity")
