library(quantmod)
library(Ecdat)
library (tseries) 
library ( fGarch ) 
library(forecast)
library(PerformanceAnalytics)

data(SP500 , package ="Ecdat") 
returnBM = SP500$r500[1805] 
x= SP500$r500[(1804 -2*253+1):1804] 
plot(c(x,returnBM),type ='l') 
results = garchFit (~arma (1 ,0) + garch (1 ,1) ,data =x, cond.dist ="std")
hist(x)
dfhat =as.numeric( results@fit$par [6]) 
forecast = predict ( results ,n.ahead =1)
x_1<-rnorm(length(x),mean=mean(x),sd=sd(x))
hist(x_1)
VaR(x,0.99,method="historical")# -2.73%
# so -22.8% is highly unusual
#VaR (alternative)
b=quantile(x,1-0.99)
b # -2.73%, coincideix

# so -22.8% is highly unusual

# why doesn't this work ?
# (returnBM-forecast$meanForecast)/forecast$standardDeviation 
# és l'estadistic de contrast ~ t_dfhat
# (with probability = 1-alpha) returnBM <=
# VaR = forecast$meanForecast+forecast$standardDeviation*qt(1-0.99,dfhat)
# = -6.6% (?)

###############
###2
payoff_function<-function(S,K){
  return(max(K-S,0))
}
N=352 # (?)
M=500
t0=0
tn=1
S0=90
r=0.05
sigma=0.40
K=75
centerpremium = monte_carlo(M,N,t0,tn,S0,r,sigma,payoff_function,K)

# a.
r = rnorm(1000,0.05,0.01)
sigma = rnorm(1000,0.40,0.10)
# b.
prices<-c() 
for(i in 1:1000){
  prices[i]<-monte_carlo(M,N,t0,tn,S0,r[i],sigma[i],payoff_function,K)
}
prices<-prices/centerpremium-1 # returns !!!!!!!!!!!!!!!
#c.
hist(prices)
epdfPlot(prices,type="l",xlab="prices",ylab="dens") # no cal
#VaR
a=quantile(prices,1-0.99)
a # -89.5%
# VaR (alternative)
VaR(prices,0.99,method="modified") # -89.2%, close enough
#Tail VaR
mean(prices[prices<=a])

