rm(list=ls())
library("tsDyn")
library("tseries")
library("xtable")
library("lattice")
library("igraph")


###############################
# Set directory and load data #
###############################

setwd("~/Dropbox/Cointegration and CC Dynamics/New Code")
source("wachterplot1009.R")
data = read.csv("logprice.csv",header=T)
date = as.Date(read.csv("logprice.csv")[,2])

#Transform to weekly data
logprice_weekly <- matrix(0, nrow(data)/7,10)
for (i in 1:nrow(logprice_weekly))
{
  logprice_weekly[i,] <-apply(as.matrix(data[(7 * (i - 1) + 1):(7 * i),3:12]),2,mean)
}


#########################
# Test for stationarity #
#########################

#Select whether you want to use daily or weekly data

#daily data:
logprice = as.matrix(data[,-c(1,2)]) # X_t
#weekly data:
#logprice = logprice_weekly[,-(7:10)]

T = nrow(logprice)
p = ncol(logprice)
dlogprice = logprice[-1,]-logprice[-T,] # Delta_X_t

#ADF Test for level and first differences
for (i in 1:p){
  print(adf.test(logprice[,i]))
}

for (i in 1:p){
  print(adf.test(dlogprice[,i]))
}

#KPSS Test for level and first differences
for (i in 1:p){
  print(kpss.test(logprice[,i]))
}
for (i in 1:p){
  print(kpss.test(dlogprice[,i]))
}


####################
# Wachter Q-Q plot #
####################

R_0t <- t(dlogprice)
R_1t <- t(logprice[-T,])

S_00 <- R_0t %*% t(R_0t)/(T-1)
S_01 <- R_0t %*% t(R_1t)/(T-1)
S_11 <- R_1t %*% t(R_1t)/(T-1)

E<-eigen(solve(S_11)%*%t(S_01)%*%solve(S_00)%*%S_01)

lambda = E$values

wachterplot(lambda,T-1)
r = 4


##############################
# Estimation of coefficients #
##############################

fit = VECM(logprice,1,r=r,estim="ML",include="none")
summary(fit)

#Estimation of matrix of cointegration vectors, \beta
beta = summary(fit)$model.specific$beta
xtable(t(round(beta,digits=2)))
write.csv(beta,file="beta.csv")

#Plot long-run stochastic trend
longrun = t(beta)%*%t(logprice)

pdf(file = "longrun.pdf", width = 7, height = 5, family = "Helvetica") # defaults to 7 x 7 inches
par(mar=c(2,2,1,1))
plot(longrun[1,]~date,type="l",xlab="",ylab="",lwd=1.5,ylim=c(-2,5))
lines(longrun[2,]~date,col="red",lwd=1.5)
lines(longrun[3,]~date,col="blue",lwd=1.5)
lines(longrun[4,]~date,col="green",lwd=1.5)
dev.off()

#Test for stationarity
adf.test(longrun[1,])
adf.test(longrun[2,])
adf.test(longrun[3,])
adf.test(longrun[4,])

#Test for nonlinearity (smooth transition autoregression effects)
star(longrun[1,])
star(longrun[2,])
star(longrun[3,])
star(longrun[4,])

#Estimation of loading matrix \alpha
alpha = summary(fit)$coefficients[,1:r]
xtable(round(alpha,digits=2))

#Estimation of lag autoregressive parameters \gamma
gamma = summary(fit)$coefficients[,(r+1):(r+p)]
gamma1 = gamma
rownames(gamma1) = colnames(gamma1) = rownames(logprice)
pdf(file = "gamma.pdf", width = 8, height = 7.5, family = "Helvetica") # defaults to 7 x 7 inches
par(mar=c(3,4,2,2))
levelplot(gamma1, xlab="",ylab="", at=unique(c(seq(-0.3, 0, length=100), 
          seq(0,0.3,length=100))),
          col.regions = colorRampPalette(c("red","white","blue"))(1e3))
dev.off()

#Network Plot
network = graph_from_adjacency_matrix(as.matrix(gamma))
E(network)$width <- ((E(network)$weight))*100
E(network)$color = "black"
V(network)$name = rownames(dlogprice)
V(network)$color = "yellow"
V(network)$label.cex = 1

network = delete.edges(network, which(E(network)$weight<0.1))

pdf(file = "gamma.pdf", width = 5, height = 5, family = "Helvetica") # defaults to 7 x 7 inches
par(mar=c(0,0,0,0))
plot.igraph(network,vertex.size=30,edge.arrow.size=10, edge.curved=.2,layout=layout.circle(network))
dev.off()

