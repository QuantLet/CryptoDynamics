rm(list = ls())

library(xtable)

#################
# Set directory #
#################

setwd("~/Dropbox/Cointegration and CC Dynamics/New Code")

##############
# Simulation #
##############

simulation = function(T,M,S,p=2,k=1,r=1){

  alpha0 = c(-0.2,0.2)
  Gamma = matrix(0.05,p,p)
  beta = c(1,-1)
  gamma0 = 0.1  
  
  gammahat=rep(0,S)
  alphahat=matrix(0,p,S)

  result = matrix(0,p+1,2)

  for (s in 1:S){
    
    set.seed(s)
    x0 = rnorm(p)

    dx = x = matrix(0,T,p)
    
    set.seed(s)
    dx[1,] = alpha0 %*% t(beta) %*% x0 + rnorm(p,sd=0.05)
    x[1,] = x0 + dx[1,]
    for (t in 2:T){
      set.seed(t*s)
      dx[t,] = alpha0 %*% t(beta) %*% x[t-1,] * as.numeric((1+tanh(gamma0*t(beta)%*%x[t-1,]))) + t(dx[t-1,] %*% Gamma) + rnorm(p,sd=0.05)
      x[t,]  = x[t-1,] + dx[t,]
    }

    dx_1 = rbind(rep(0,p),dx[-T,])
    z = beta%*% t(rbind(x0,x[-T,]))

    gamma = matrix(seq(-1,1,length.out=M),r,M)
    MSE = rep(0, M)
  
    for (m in 1:M){
      for (j in 1:p){
        G = 1 + tanh(gamma[,m] %*% z)
        zG = as.numeric(z*G)
        fit = lm(dx[,j]~cbind(zG,dx_1))
        MSE[m] = mean(lm(dx[,j]~cbind(zG,dx_1))$residuals^2)/p + MSE[m]
        alphahat[j,s] = lm(dx[,j]~cbind(zG,dx_1))$coeff[2]
      }
    }

    gammahat[s] = gamma[,which(MSE==min(MSE))]
  }

result[1,1] = mean(gammahat-gamma0)
result[1,2] = sqrt(mean((gammahat-gamma0)^2))

result[2,1] = mean(alphahat[1,]-alpha0[1])
result[2,2] = sqrt(mean((alphahat[1,]-alpha0[1])^2))

result[3,1] = mean(alphahat[2,]-alpha0[2])
result[3,2] = sqrt(mean((alphahat[2,]-alpha0[2])^2))

return(result)
}

RMSE250  = simulation(250,41,1000)
RMSE500  = simulation(500,41,1000)
RMSE1000 = simulation(1000,41,1000)
RMSE2000 = simulation(2000,41,1000)

RMSE = cbind(RMSE250[,2],RMSE500[,2],RMSE1000[,2],RMSE2000[,2])
xtable(RMSE,digits=4)
