#This function constructs the Wachter q-q plot for
#the empirical distribution of the squared sample canonical 
#correlations (entries of vector lambda) as discussed in
#Onatski and Wang (2018) "Alternative Asymptotics for cointegration
#tests in large VARs".
#The inputs are: 
#1) a p by 1 vector lambda, whose entries are the squared sample 
#canonical correlations.
#2) the sample size T, (the sample refers to the data from which 
                         #the squared sample canonical correlations were computed)

#The output is a p by 2 matrix, with the first column containing the
#abscissas of the points on the Wachter plot, and the second column 
#containing corresponding ordinates. Running the function also produces
#a figure with the plot.


wachterplot=function(lambda,N,q_95,q_05)
{
p = length(lambda)
abscissas = matrix(0, p, 1)
grid = 10000

#parameters of the Wachter distribution
c = p / N
gamma1 = c / (1 + c)
gamma2 = 2 * c / (1 + c)
bplus1 = (sqrt(gamma1 * (1 - gamma2)) + sqrt(gamma2 * (1 - gamma1)))^2
bminus1 = (sqrt(gamma1 * (1 - gamma2)) - sqrt(gamma2 * (1 - gamma1)))^2
xf1 = seq(from = bminus1, to = bplus1, length.out = grid)

#Wachter density on the grid
f1 = sqrt((bplus1 - xf1)*(xf1 - bminus1)) / ((2*pi)*gamma1*xf1*(1-xf1))

#Computing Wachter 100(t-1/2)/p quantiles 

cusum <- matrix(0, 1, length(xf1))

cusum[1] <- f1[1]

for (i in 2:length(xf1))
{
  cusum[i] <- cusum[i-1]+f1[i]
}

A<-cusum*(bplus1 - bminus1) / grid*p+1/2

index=rep(0,p)

for(k in 1:p)
{
index[k]=which(abs(A-k)==min(abs(A-k)))
}

abscissas=xf1[index]

# Sorting lambdas to obtain corresponding ordinates
ordinates=sort(lambda)

LIM=max(lambda)

# making the Wachter plot
png("Wachter_10.png", width=20, height=20, units="cm", res=800, bg="transparent")
plot(
  abscissas,
  ordinates,
  type = 'o',
  col = "red",
  xlab = "Quantiles of Wachter",
  ylab = "Empirical quantiles",
  main = "The Wachter q-q plot",
  xlim=c(0,LIM),
  ylim=c(0,LIM)
)
lines(seq(0,LIM,0.0001), seq(0,LIM,0.0001), col="blue",type = 'l')
#lines(abscissas, sort(q_95),col="orange",lty=2)
#lines(abscissas, sort(q_05),col="purple",lty=2)

dev.off()
}

