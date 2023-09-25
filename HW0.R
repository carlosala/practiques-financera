### AUTHORS
# Júlia Albero
# Sergi Cucala
# Marc Luque
# Carlo Sala

# Financial Engineering UAB 2022
# HW0 practice

library(quantmod)
library(moments)
library(EnvStats)
library(stats)

### EXERCISE 1
getSymbols("GOOG", scr = "yahoo", from = "2011-01-03", to = "2014-12-31")
googleclose <- GOOG$GOOG.Close
getSymbols("SP", scr = "yahoo", from = "2011-01-03", to = "2014-12-31")
SPclose <- SP$SP.Close

sp <- as.numeric(SPclose)
goog <- as.numeric(googleclose)

get_net <- function(data) {
  nets <- c()
  for (i in 2:length(data)) {
    nets[i - 1] <- (data[i] / data[i - 1]) - 1
  }
  return(nets)
}

net_sp <- get_net(sp)
net_goog <- get_net(goog)

# a
get_statistics <- function(data) {
  VALUE <- c(skewness(data), mean(data), sd(data), kurtosis(data), min(data), max(data))
  STATISTIC <- c("SKEWNESS", "MEAN", "STD DEVIATION", "KURTOSIS", "MIN", "MAX")
  return(cbind(STATISTIC, VALUE))
}

View(get_statistics(net_sp))
View(get_statistics(net_goog))
plot(googleclose)
plot(SPclose)

# b
epdfPlot(net_sp)
epdfPlot(net_goog)

shapiro.test(net_sp) # no normally distributed
shapiro.test(net_goog) # no normally distributed

# c
log_sp <- log(1 + net_sp)
log_goog <- log(1 + net_goog)
get_statistics(log_sp)
get_statistics(log_goog)
# d
t.test(log_goog) # H0 accept, la mitjana si que es igual a 0

# e
epdfPlot(log_sp)
epdfPlot(log_goog)

shapiro.test(log_sp) # no normally distributed
shapiro.test(log_goog) # no normally distributed

# f
se_goog <- sd(log_goog) / sqrt(length(log_goog))
inf_goog <- mean(log_goog) - 1.96 * se_goog
sup_goog <- mean(log_goog) + 1.96 * se_goog
ic <- c(mean(log_goog), inf_goog, sup_goog)
ic

### EXERCISE 2
D <- 240000 # debt
Y <- 30 # years
R <- 1.99 / 100 # (annual) interest rate

# a.
# Monthly installment of the loan
M <- (D * R / 12 * (1 + R / 12)^(12 * Y)) / ((1 + R / 12)^(12 * Y) - 1)
M

# b.
caprep <- c()
caprep[1] <- (D * R / 12) / ((1 + R / 12)^(12 * Y) - 1)
int <- c()
int[1] <- D # we need to know the remaining debts first
for (i in 1:(12 * Y - 1)) {
  caprep[i + 1] <- caprep[i] * (1 + R / 12)
  int[i + 1] <- int[i] - caprep[i]
}
int <- int * R / 12
table1 <- cbind(int, caprep)
View(table1)

# Summary
int2 <- c()
caprep2 <- c()
for (i in 1:Y) {
  int2[i] <- sum(int[(12 * i - 11):(12 * i)]) # : operator has precedence !
  caprep2[i] <- sum(caprep[(12 * i - 11):(12 * i)])
}
table2 <- cbind(int2, caprep2)
View(table2)

# c.
# Total paid to the bank
bank <- D + sum(int2)
bank

# d.
newR <- seq(1.99 / 100, by = 0.10 / 100, length = Y)
newD <- c()
newM <- c()
newint <- c() 
newint[1] <- D
newcprp <- c()

for (i in 1:Y) {
  newD[i] <- newint[12 * (i - 1) + 1]
  newM[i] <- (newD[i] * newR[i] / 12 * (1 + newR[i] / 12)^(12 * (Y - i + 1))) / ((1 + newR[i] / 12)^(12 * (Y - i + 1)) - 1)
  newcprp[12 * (i - 1) + 1] <- (newD[i] * newR[i] / 12) / ((1 + newR[i] / 12)^(12 * (Y - i + 1)) - 1) # 1st of each year
  for (j in 1:11) {
    newcprp[12 * (i - 1) + j + 1] <- newcprp[12 * (i - 1) + j] * (1 + newR[i] / 12)
    newint[12 * (i - 1) + j + 1] <- newint[12 * (i - 1) + j] - newcprp[12 * (i - 1) + j]
    newint[12 * (i - 1) + j] <- newint[12 * (i - 1) + j] * newR[i] / 12
  }
  if (i < Y) {
    newint[12 * i + 1] <- newint[12 * i] - newcprp[12 * i]
  }
  newint[12 * i] <- newint[12 * i] * newR[i] / 12
}

# Total paid to the bank
newbank <- D + sum(newint)
newbank

