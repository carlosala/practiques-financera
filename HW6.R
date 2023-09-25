# HW6
# Julia Albero, Sergi Cucala, Marc Luque, Carlo Sala

### library imports
library(quantmod)

### 1
# a
getSymbols("GOOG", scr = "yahoo", from = "2015-01-01", to = "2015-12-31")
getSymbols("META", scr = "yahoo", from = "2015-01-01", to = "2015-12-31")
getSymbols("AAPL", scr = "yahoo", from = "2015-01-01", to = "2015-12-31")
getSymbols("MSFT", scr = "yahoo", from = "2015-01-01", to = "2015-12-31")

goog <- as.numeric(GOOG$GOOG.Adjusted)
meta <- as.numeric(META$META.Adjusted)
apl <- as.numeric(AAPL$AAPL.Adjusted)
msft <- as.numeric(MSFT$MSFT.Adjusted)

# b
get_net <- function(data) {
  nets <- c()
  for (i in 2:length(data)) {
    nets[i - 1] <- (data[i] / data[i - 1]) - 1
  }
  return(nets)
}
goog <- get_net(goog)
meta <- get_net(meta)
apl <- get_net(apl)
msft <- get_net(msft)

data <- cbind(goog, meta, apl, msft)

C <- cov(data)
mu <- c(mean(goog), mean(meta), mean(apl), mean(msft))

# c
set.seed(123)
num1 <- runif(1, 0, 1)
set.seed(124)
num2 <- runif(1, 0, 1)
set.seed(125)
num3 <- runif(1, 0, 1)
set.seed(126)
num4 <- runif(1, 0, 1)

num1_norm <- num1 / (num1 + num2 + num3 + num4)
num2_norm <- num2 / (num1 + num2 + num3 + num4)
num3_norm <- num3 / (num1 + num2 + num3 + num4)
num4_norm <- num4 / (num1 + num2 + num3 + num4)

vct_norm <- c(num1_norm, num2_norm, num3_norm, num4_norm)

mu_p <- t(vct_norm) %*% mu
std2 <- t(vct_norm) %*% C %*% vct_norm

# d
n <- 1000
mu_p <- c()
std2 <- c()
for (i in 1:n) {
  num1 <- runif(1, 0, 1)
  num2 <- runif(1, 0, 1)
  num3 <- runif(1, 0, 1)
  num4 <- runif(1, 0, 1)
  num1_norm <- num1 / (num1 + num2 + num3 + num4)
  num2_norm <- num2 / (num1 + num2 + num3 + num4)
  num3_norm <- num3 / (num1 + num2 + num3 + num4)
  num4_norm <- num4 / (num1 + num2 + num3 + num4)
  
  vct_norm <- c(num1_norm, num2_norm, num3_norm, num4_norm)
  
  mu_p[i] <- t(vct_norm) %*% mu
  std2[i] <- t(vct_norm) %*% C %*% vct_norm
}
plot(std2, mu_p)

### 2
# a
N <- 500
rbase <- seq(min(mu), max(mu), length = N)

# b
u <- c(1, 1, 1, 1)
matriuq <- cbind(2 * C, mu, u)
mut <- c(t(mu), 0, 0)
ut <- c(t(u), 0, 0)
matriuq <- rbind(matriuq, mut)
matriuq <- rbind(matriuq, ut)
matriuq <- round(matriuq, 6)

b <- matrix(c(rep(0, 4 * N), rbase, rep(1, N)), nrow = 6, byrow = TRUE)
b <- round(b, 6)

# c
y <- solve(matriuq, b[, 1])
y <- t(y)
ynova <- matrix(0, N, nrow = 6)
mu_p1 <- c()
std2_1 <- c()
for (i in 1:N) {
  ynova[, i] <- solve(matriuq, b[, i])
  mu_p1[i] <- t(ynova[1:4, i]) %*% mu
  std2_1[i] <- t(ynova[1:4, i]) %*% C %*% ynova[1:4, i]
}

# d
plot(std2_1, mu_p1)

### 3
which(std2_1%in%min(std2_1))
m=mu_p1[which(std2_1%in%min(std2_1))]
m
mu_p2=mu_p1[mu_p1>=m]
std2_2 = std2_1[which(mu_p1%in%mu_p2)]
par(mfrow = c(1, 2))
plot(std2, mu_p)
points(std2_2, mu_p2, col = 2)
points(min(std2_2), m,pch=16, col = 3)
plot(std2_1, mu_p1)
points(std2_2, mu_p2, col = 2)
points(min(std2_2), m,pch=16, col = 3)
