rm(list = ls())

###############################
# Set directory and load data #
###############################

#setwd("~/...")
data = read.csv("logprice.csv",header=T)
date = as.Date(read.csv("logprice.csv")[,2])


########################################
# Joint time series plot of log prices #
########################################

pdf(file = "CryptoDynamics_Series.pdf", width = 7, height = 5, family = "Helvetica") # defaults to 7 x 7 inches
par(mar=c(2,2,1,1))
plot(data[,3]~date,type="l",ylim=c(-5,10),xlab="",ylab="",lwd=2)
lines(data[,4]~date,col="blue",lwd=2)
lines(data[,5]~date,col="red",lwd=2)
lines(data[,6]~date,col="green",lwd=2)
for(i in 6:12){lines(data[,i]~date,col="grey")}
dev.off()
