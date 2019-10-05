[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **CryptoDynamics_Series** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : CryptoDynamics_Series

Published in : 'Cointegration and Cryptocurrency Dynamics in High Dimensions'

Description : 'Plots the log-prices of the 20 largest cryptocurrencies over a period from October 2017 - October 2019.'

Keywords : Cryptocurrencies, CRIX, Cointegration, Time series

See also : 'CryptoDynamics_Estimation, Crypto_Dynamics_Wachter, CryptoDynamics_Scraping'

Author : Georg Keilbar, Yanfen Zhang

Submitted : October 5 2019 by Georg Keilbar

```

![Picture1](CryptoDynamics_Series.png)

### R Code
```r

rm(list = ls())

setwd("~/Dropbox/Cointegration Test/Code")

logprice = as.matrix(read.csv("logprice.csv")[,-c(1,2)])
date = as.Date(read.csv("logprice.csv")[,2])

plot(logprice[,1]~date,type="l",ylim=c(-7,10),xlab="",ylab="",lwd=2)
lines(logprice[,2]~date,col="red",lwd=2)
lines(logprice[,3]~date,col="blue",lwd=2)
lines(logprice[,4]~date,col="green",lwd=2)
lines(logprice[,5]~date,col="purple",lwd=2)
for(i in 5:20){lines(logprice[,i]~date,col="grey")}

```

automatically created on 2019-10-05