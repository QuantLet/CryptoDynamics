[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **CryptoDynamics_Scraping** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : CryptoDynamics_Scraping

Published in : 'On Cointegration and Cryptocurrency Dynamics'

Description : 'Scrape and merge the prices of the N largest cryptocurrencies over a specified time period'

Keywords : Cryptocurrencies, CRIX, Cointegration, Scraping

See also : 'CryptoDynamics_Series, Crypto_Dynamics_Wachter, CryptoDynamics_Estimation'

Author : Georg Keilbar, Yanfen Zhang

Submitted : October 5 2019 by Georg Keilbar

```

### R Code
```r

rm(list = ls())

library(readxl)
library(vars)
library(MTS)
library(crypto)
library(reshape2)

setwd("~/Dropbox/Cointegration and CC Dynamics/New Code")

#List of all cryptocurrencies from coinmarketcap
list = crypto_list(coin = NULL, start_date = '20190131', end_date = '20190131',
                   coin_list = NULL)

#Scrape the largest N currencies
N = 17
start = '20170101'
end = FALSE
data = crypto_history(limit = N, start_date = start, end_date = end, coin_list = NULL, sleep = NULL)
write.csv(data,file="cryptodata.csv")

#Merge the largest 10 cryptocurrency with at lease 2 1/2 year of history which are not tied to USD (manual selection)
price = na.omit(reshape(data[,c("date","symbol","close")],idvar="date",timevar="symbol",direction="wide")[,c(1,2,3,4,5,8,9,10,13,14,17)])

#Take logs
date = price[,1]
logprice = log(as.matrix(price[,-1]))

#First differences
logreturn = diff(logprice)

#Export to csv
write.csv(file="logprice.csv",cbind(date,as.data.frame(logprice)))
write.csv(file="logreturn.csv",cbind(date[-1],as.data.frame(logreturn)))

```

automatically created on 2020-03-24