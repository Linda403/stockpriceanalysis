# lab 3. Stock price analysis

library(quantmod)
library(forecast)
#install.packages("zoo")
library(zoo)

getSymbols("AMZN", from="2020-01-02")
chartSeries(AMZN)

getSymbols("FB", from="2010-01-01")
chartSeries(FB)

getSymbols("NDAQ", from="2020-01-01")
chartSeries(NDAQ)

#---------------------------
getSymbols("AAPL", from="2015-01-02")
chartSeries(AAPL)
head(AAPL)
tail(AAPL)
chartSeries(AAPL)
?reChart
reChart(subset="last 2 years")

last(AAPL$AAPL.Close,"2 years")
data<- last(AAPL$AAPL.Close, "2 years")
class(data)

head(data, 3)
tail(data, 3)
plot(data)
#lm(y~time(y))
#yt= b0+b1t, et
#xts slightly modified time series object

data.lm<-lm(data~time(data))
          
plot(data)
lines(data.lm$fitted.values, col = "red")
summary(data)

t=as.vector(time(data))
data.lm2<-lm(data~t+I(t^2))

data.lm<-(data~time(data))
plot(data)
lines(data.lm2$fitted.values, col = "red")
#abline- draw the line from 0
#fitted lines- more universal, general 
#apply to wide variety of situations

plot(data.lm)
summary(data.lm)
checkresiduals(data.lm)
checkresiduals(data.lm)
#how do we know it will grow by 14 cents?

## Trailing Moving Average
## k: the window width for the moving average. w=21 here
ma.trailing= rollmean(data, k=21, 
                      align = "right")

plot(data,
     ylab = "Closing price", # label for y-axis
     xlab = "Time", #label for x-axis
     bty = "l", #border of the plot (2 axis only)
     main = "") #no title is needed)
lines(ma.trailing, col="red")

plot(data)
lines(ma.trailing, col = "red")

#9/16 prediction 150
#9/17 prediction 150

#9/17 146
