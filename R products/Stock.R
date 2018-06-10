# This experiment is based on stock data as the background of the analysis.
# How stock data is obtained from the Yahoo Finance section. The daily price and trading volume data of the stock are observed.
# Then, the daily rate of return in a stock data is calculated. 
# Then through a variety of stock charts for technical analysis, on the basis of a stock at the same time analyze the trading volume of multiple stocks
# Finally get the correlation between them and other data characteristics.


# Download Apple's stock data from early 2018 to the current ¡®getSymbols¡¯ function in the ¡®quantmod¡¯ package. 
# The data source chosen 'yahoo'
library(quantmod)
getSymbols("AAPL",from = "2018-01-01",to = Sys.Date(),src = "yahoo")
head(AAPL)

# Tseries package is mainly used for time series analysis and computational finance
install.packages("tseries")
library(tseries)

# Calling the get.hist.quote function to get Google's stock data
goog<-get.hist.quote(instrument = "GOOG", start="2018-01-01", end="2018-05-25",quote = "AdjClose")
head(goog)

# Take chartSeries as an example to make a K line chart
chartSeries(AAPL,up.col='green', dn.col='red',theme="white")
chartSeries(AAPL,name = "AAPLBARCHART",subset='2018-01-01::2018-05-24',type="bars")
chartSeries(AAPL,name = "AAPLLINECHART",subset="2018-01-01::2018-05-24",type="line")
chartSeries(AAPL,name = "AAPLCANDCHART",subset="2018-01-01::2018-05-24",type="candlesticks")

#(1) addBBands() Bollinger indicator
chartSeries(AAPL,up.col='green', dn.col='red',theme="white")
addBBands(n=14,sd=2,draw='bands')
#(2) addADX() average trend indicator
addADX()
#(3) addMACD() Exponential Smoothness Convergence/Divergence Moving Average 
#This is a commonly used oscillator. Using the sequence's fast moving average minus the slow moving average can be used to identify market trends.
addMACD()
chartSeries(AAPL,up.col='red', dn.col='green',theme="white",TA=c(addBBands(),addMACD(),addADX(),addVo()))


# Simple yield
close <- AAPL[,4]
close1 <- lag(close,1)
head(close1)

calclose <- merge(close,close1)
simplerate <- (close-close1)/close1
names(simplerate)="simplerate"
calrate=merge(calclose,simplerate)
head(calrate)

# Logarithmic rate of return
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
rate=periodReturn(close,period="daily",type="log")
head(rate)

# Use the ETL function in the quantmod package to download the stock market data of Apple, Microsoft, Oracle, and Google, and conduct a brief analysis
library(quantmod)
new.environment <- new.env()
getSymbols(c("AAPL", "ORCL", "MSFT", "GOOG"), src = "yahoo", env = new.environment)
str(get("AAPL", env = new.environment))
str(get("ORCL", env = new.environment))
str(get("MSFT", env = new.environment))
str(get("GOOG", env = new.environment))

# Find out when these stocks have skyrocketed and plunged (for example, the opening or closing price is more than 2% higher than the previous day)
AAPL <- Delt(Cl(get("AAPL", env = new.environment)))
length(AAPL[which(AAPL > 0.02), ])
plot(AAPL[which(AAPL > 0.02), ])

ORCL <- Delt(Cl(get("ORCL", env =new.environment)))
length(ORCL[which(ORCL > 0.02), ])
plot(ORCL[which(ORCL > 0.02), ])

MSFT <- Delt(Cl(get("MSFT", env = new.environment)))
length(MSFT[which(MSFT > 0.02), ])
plot(MSFT[which(MSFT > 0.02), ])

GOOG <- Delt(Cl(get("GOOG", env = new.environment)))
length(GOOG[which(GOOG > 0.02), ])
plot(GOOG[which(GOOG > 0.02), ])


getSymbols(c("AAPL", "ORCL", "MSFT", "GOOG"), src = "yahoo", env = new.environment, from = "2018-01-01", to = "2018-05-24")
# Daily price adjustment of four company stocks in one data frame
m <- cbind(Ad(get("AAPL", env = new.environment)), 
           Ad(get("ORCL", env = new.environment)), 
           Ad(get("MSFT", env = new.environment)), 
           Ad(get("GOOG", env = new.environment)))
# Analyze and judge relevance and plot
library(psych)
corr.test(as.data.frame(m))
library(corrplot)
corrplot.mixed(cor(m), lower = "square", upper = "circle")

