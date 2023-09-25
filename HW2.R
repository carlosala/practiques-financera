#HW2 Financial Engineering, Temporal Series 02
#Júlia Albero, 1566550
#Sergi Cucala, 1570044
##Marc Luque, 1567604
#Carlo Sala, 1570775



###1
install.packages("Ecdat")
install.packages("quantmod")
install.packages("tseries")
install.packages("fGarch")
install.packages("forecast")
library(quantmod)
library(Ecdat)
data ( Tbrate) 
library (tseries) 
library ( fGarch ) 
library(forecast)
# r = the 91 - day treasury bill rate 4
# y = the log of real GDP 5
# pi = the inflation rate 6
7
Tbill = Tbrate [ ,1] 
Del.Tbill = diff( Tbill ) 
plot(Tbill)
plot(Del.Tbill)
acf(Tbill) # non stationary
acf(Del.Tbill) # stationary

adf.test(Tbill) # non stationary
adf.test(Del.Tbill) # stationary

kpss.test(Tbill)# non stationary (alternative hypothessys)
kpss.test(Del.Tbill)#stationary 
# heteroskedasticity we dont know
checkresiduals(Del.Tbill)
# we observe pure heteroskedasticity
# b
garch.model = garchFit( formula =~ arma (1 ,0) + garch (1 ,0) ,Del.Tbill ) 
summary ( garch.model ) 
garch.model@fit$matcoef 

# c d e
res = residuals ( garch.model ) 
res_std =res / garch.model@sigma.t 
par ( mfrow =c(2 ,3) ) 
plot ( res) 
acf (res ) 
acf (res ^2)
plot ( res_std )
acf ( res_std )
acf ( res_std ^2)


#f g
x<-diff(log(Tbill))
plot(x)
acf(x)#MA(1) d=0
pacf(x)#Solo MA  
# cal estudfiar la part garch ...
acf(x^2)#0.5
pacf(x^2)#1.5
mod1<-garchFit(formula = ~arma(0,1)+garch(1,1),x)
mod2<-garchFit(formula = ~arma(0,1)+garch(1,2),x)
checkresiduals(x)
checkresiduals(x^2)

eacf(x)
auto.arima(x)
auto.arima(x^2)


coef<-mod1@fit$matcoef
coef



##2
library(fGarch)
data(SP500 , package ="Ecdat") 
returnBM = SP500$r500[1805] 
x= SP500$r500[(1804 -2*253+1):1804] 
plot.ts(c(x,returnBM),type ='l',main="Index S&P500",ylab="Index",col="red") 
results=garchFit(~arma (1 ,0) + garch (1 ,1) ,data =x, cond.dist ="std")
dfhat =as.numeric( results@fit$par [6]) 
forecast = predict ( results ,n.ahead =1) 
summary(results)
# a.
# we are asked to find conditional 
# P(yt < returnBM) = P(yt - ythat < returnBM - ythat)
# = P(sigmat*deltat < returnBM - ythat) 
# = P(deltat < (returnBM - ythat)/sigmat) and deltat ~ t_dfhat
pt((returnBM-forecast$meanForecast)/forecast$standardDeviation,dfhat)
# = 8.132691e-05
?pt
#b
library(forecast)
residuals<-residuals(results)
stan_duals<-residuals/sd(residuals)
plot.ts(stan_duals,type="l",main="Plot residuals",ylab="Index",col="red")
acf(residuals,main="ACF residuals")
pacf(residuals,main="PACF residuals")
Box.test(residuals, lag=5, type="Ljung", fitdf=1)
#p-value>0.05... accept H0, residuals are WN

residuals2<-residuals^2
plot.ts(residuals2/sd(residuals2),type="l",main="Plot squared residuals",ylab="Index",col="green")
acf(residuals2,main="ACF residuals squares")#ma(2)
pacf(residuals2,main="PACF residuals squares")#ar(2)
Box.test(residuals2, lag=5, type="Ljung", fitdf=1)
#The residuals have a p-value<0.05 we need to accept H1->not being WN
#model arma(2,2)...

#c
require(fGarch)
garch.model = garchFit ( formula =~ arma (1 ,0) + garch (1 ,0) ,x ) 
resid<-residuals(garch.model)
resid2<-resid^2
st_du<-resid2/sd(resid2)
plot.ts(st_du,type="l",main="Plot residuals of ARCH(1)",ylab="Index",col="red")
acf(resid2,main="ACF residuals squares ARCH(1)")
pacf(resid2,main="PACF residuals squares ARCH(1)")
Box.test(resid2, lag=5, type="Ljung", fitdf=1)


###AIXO HO HAVIEU ESCRIT VOSALTRES...HO VOLEU TREURE?
#yes, but gamma1, delta, skew and shape aren´t included to the model
summary(garch.model)#ar1 no es significativo
garch.model = garchFit ( formula =~ arma (0 ,0) + arch (1 ,0) ,x ) 
#yes, but gamma1, delta, skew and shape aren´t included to the model
summary(garch.model)#ar1 no es significativo

#d
fit1 = arima ( x , order =c(1,0,0) ) 
acf( residuals ( fit1), main="ACF residuals from AR(1)")
pacf(residuals(fit1), main=" PACF residuals from AR(1)")
library(forecast)
checkresiduals(residuals(fit1))

Box.test ( residuals ( fit1 ) ,lag =10 , type ="Ljung")
#Los datos se distribuyen de forma independiente, el test dice que se el residuo se comporta como un WN ...?


#3
datahw3 <- get(load('data_HW_3.RData'))
plot(datahw3,type="l")
acf(datahw3)
pacf(datahw3)
#arma(2,4)

squares<-datahw3^2
acf(squares)
pacf(squares)
#garch(1,0)
library(tseries)
require(fGarch)
fit1<-garchFit ( formula =~ arma (1 ,0) + garch (1,0) ,datahw3 ) 
fit2<-garchFit ( formula =~ arma (1 ,1) + garch (1,1) ,datahw3 ) 
fit3<-garchFit ( formula =~ arma (1 ,2) + garch (2,0) ,datahw3 ) 
fit4<-garchFit ( formula =~ arma (2 ,0) + garch (2,1) ,datahw3 ) 
fit5<-garchFit ( formula =~ arma (2 ,1) + garch (3,0) ,datahw3 ) 

logLik.fGARCH <- function(x){
  x@fit$value
}

N<-length(datahw3)
loglink1<-logLik.fGARCH(fit1)
loglink2<-logLik.fGARCH(fit2)
loglink3<-logLik.fGARCH(fit3)
loglink4<-logLik.fGARCH(fit4)
loglink5<-logLik.fGARCH(fit5)

loglink<-c(loglink1,loglink2,loglink3,loglink4,loglink5)
q<-c(1:5)
k<-q+1
aicc<-(-2)*loglink + (2*k*N)/(N-k-1)
print(data.frame(q,loglink,aicc))
#escollim garch(2,2)

fit.tseries<-garchFit ( formula =~ arma (2,0) + garch (1,0) ,datahw3 )
summary(fit.tseries)

