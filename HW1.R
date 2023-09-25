#####  Introduction to Financial Engineering HW1: Temporal Series  #####

#1566550 (Júlia Albero)
#1567604(Marc Luque)
#1570044(Sergi Cucala)
#1570775(Carlo Sala)


#1

library(quantmod)
library(Ecdat)
data ( Tbrate) 
library ( tseries ) 
# r = the 91 - day treasury bill rate 
# y = the log of real GDP 
# pi = the inflation rate 

###########################

# Questions a ) and b ) 
plot ( Tbrate ) 
#y té tendencia positiva, 
#r presenta una tendencia positiva fins 1981 i dps, té tendencia negativa
#pi té una volatilitat molt alta

acf ( Tbrate ) 
#tots els lags dels acf son més grans que 0.05 no es poden modelar ni amb AR 
#ni MA, haurem d'utilitzar un model més complicat

#########################

# Consider only the diagonal plots which correspond to the ACF plots of r , y an pi
adf.test ( Tbrate [,1]) 
adf.test ( Tbrate [,2]) 
adf.test ( Tbrate [,3]) 
#si efectivament, en els acfs de les tres variables que tenim, en cap d'ells acceptarem la hipotesis alternativa
#per tant,  La hipótesis nula daquesta prueba es que existe una raíz unitaria en la serie, 
#esto significa que el modelo no seria estacionario

#########################

# Qeustions c ) and d ) 
diff_rate = diff ( Tbrate ) 
adf.test ( diff_rate [ ,1]) 
adf.test ( diff_rate [ ,2]) 
adf.test ( diff_rate [ ,3]) 

plot ( diff_rate ) 
acf(diff_rate)

#ara que hem diferenciat, les proves de dickey fuller ens diuen que hem d'acceptar la hipotesis altyernativa, és a dir, 
#no tenim arrel unitaria, llavors la serie si que serà estacionaria per a totes les variables
#podem veure que ens els plots de les series, els grafics no presenten tendencia, i 
#els acfs es fan més petits de 0.05 desde els primers lags
#

####################

# Questions e ) , f ) , g ) and h ) 
#e
#r<-ar()
library ( forecast ) 
auto.arima( Tbrate [ ,1] , max.P=0 , max.Q=0 , ic="aic") 
#ARIMA(0,1,1), els acf tambe diuen el mateix. 

#f
#ma(1)

#g
#menor aic

#h
auto.arima( Tbrate [ ,1] , max.P=0 , max.Q=0 , ic="bic") 
#no canvia el model

# Questions i )
fit1 = arima ( Tbrate [ ,1] , order =c(0,1,1) ) 
acf( residuals ( fit1) ) 
checkresiduals(residuals(fit1))

Box.test ( residuals ( fit1 ) ,lag =10 , type ="Ljung")
#Los datos se distribuyen de forma independiente

#########################
#2
# a.
# Yes (it's an AR model)

# b.
# We can rewrite the model as Y_t-mu = phi*(Y_{t-1}-mu) + eps
phi = -0.55
# Therefore, as 5 = mu*(1-phi), we obtain 
mu = 5/(1-phi)
mu
sigmaeps<-1.2
# c. 
# Variance is 
sigmaY2 = sigmaeps/(1-phi^2)
sigmaY2

# d. 
# Covariance function is
cov<-function(h){
  return(sigmaeps^2*(phi^abs(h))/(1-phi^2))
}

####################3
#3
mu<-104
fi1<-0.4
fi2<-0.25
fi3<-0.1
yn_3<-105
yn_2<-102
yn_1<-103
yn<-99
yn1<-mu+fi1*(yn-mu)+fi2*(yn_1-mu)+fi3*(yn_2-mu)
yn2<-mu+fi1*(yn1-mu)+fi2*(yn-mu)+fi3*(yn_1-mu)
yn1
yn2

########################
#4
library (Ecdat) 
data ( Mishkin ) 
tb1 =log ( Mishkin [ ,3]) 
plot(tb1, main="Mishkin PDF plot")
acf(tb1, main="Mishkin ACF plot")
tbr1<-diff(tb1)
plot(tbr1, main="Diff Mishkin PDF plot")
acf(tbr1, main="Diff Mishkin ACF plot")
#diferenciem un cop per treure tendencia

#b
auto.arima( tb1 , max.P=0 , max.Q=0 , ic="bic")
auto.arima( tb1 , max.P=0 , max.Q=0 , ic="aic")
#en el bic, nos dice (0,1,1) de la serie normal
#en el aic, nos dice (3,1,5) de la serie normal


#c
checkresiduals(tbr1)
#veiem diferents coses: no es completament white noise, 
#té volatilitat alta just abans del 1960, s'hauria de modelar
#el acf es fa més petit de 0.05 en els ultims lags
#i els residus no son normals

