# HW5
# Julia Albero 1566550
# Sergi Cucala 1570044
# Marc Luque 1567604
# Carlo Sala 1570775

### library imports
library(extraDistr)
library(EnvStats)

# 1a
f <- function(N) {
  profit <- 1 / sqrt(N)
  l <- rbern(N, prob = 0.5)
  vec <- c()
  vec[1] <- 0
  for (i in 1:N) {
    if (l[i] == 0) {
      vec[i + 1] <- vec[i] + profit
    } else {
      vec[i + 1] <- vec[i] - profit
    }
  }
  return(vec)
}

par(mfrow = c(2, 3))
for (i in c(1, 5, 10, 50, 100, 10000)) {
  plot(seq(0, 1, 1 / i), f(i), type = "l", xlab = "time", ylab = "profit")
}

# 1b
f2 <- function(N, m) {
  tr <- c()
  for (i in 1:m) {
    tr[i] <- f(N)[N + 1]
  }
  return(tr)
}


par(mfrow = c(1, 3))
i <- 100
vec <- f2(i, i)
epdfPlot(vec, type = "l", xlab = "Profit", ylab = "Density")
shapiro.test(vec)

i <- 1000
vec <- f2(i, i)
epdfPlot(vec, type = "l", xlab = "Profit", ylab = "Density")
shapiro.test(vec)

i <- 10000
vec <- f2(i, i)
epdfPlot(vec, type = "l", xlab = "Profit", ylab = "Density")
# we cannot use shapiro because it's limited to 5000
ks.test(x = vec, y = "pnorm", alternative = "two.sided")

# 2
### a
path_sample <- function(N, t0, tn, S0, r, sigma) {
  et <- rnorm(N, 0, 1)
  x0 <- c()
  x0[1] <- S0
  for (i in 1:N) {
    x0[i + 1] <- x0[i] + r * S0 * ((tn - t0) / N) + sigma * S0 * sqrt((tn - t0) / N) * et[i]
  }
  return(x0)
}
N <- 1000
t0 <- 0
tn <- 1
S0 <- 100
r <- 0.01 # for values near 1 we get a model with less volatility but shows a increasing behaviour, while for lower values the time series shows a mean reversion level, even if there are huge levels of volatility
sigma <- 0.3 # for near 1 values of sigma we get huge volatility properties displayed on the time series, while for low values the time series turns lineal
plot(seq(t0, tn, 1 / N), path_sample(N, t0, tn, S0, r, sigma), type = "l", main = "Black-Scholes path realization")

### b (CALL)
payoff_function <- function(S, K) {
  return(max(S - K, 0))
}

### c
monte_carlo <- function(M, N, t0, tn, S0, r, sigma, payoff_function, K) {
  vec <- c()
  for (i in 1:M) {
    vec[i] <- payoff_function(path_sample(N, t0, tn, S0, r, sigma)[N], K)
  }
  price <- mean(vec) * exp(-r * (tn - t0))
  return(price)
}
N <- 1000
v <- c(path_sample(N, t0, tn, S0, 0.01, 0.3)[N + 1], path_sample(N, t0, tn, S0, 0.01, 0.999)[N + 1], path_sample(N, t0, tn, S0, 0.01, 0.001)[N + 1], path_sample(N, t0, tn, S0, 0.999, 0.3)[N + 1], path_sample(N, t0, tn, S0, 0.001, 0.3)[N + 1])
mean(v) # just to have an approximation of the strike k

monte_carlo(100, N, t0, tn, S0, 0.01, 0.3, payoff_function, 140)
monte_carlo(100, N, t0, tn, S0, 0.01, 0.999, payoff_function, 140)
monte_carlo(100, N, t0, tn, S0, 0.01, 0.001, payoff_function, 140)
monte_carlo(100, N, t0, tn, S0, 0.999, 0.3, payoff_function, 140)
monte_carlo(100, N, t0, tn, S0, 0.001, 0.3, payoff_function, 140)

### d (PUT)
payoff_function <- function(S, K) {
  return(max(K - S, 0))
}
N <- 365
M <- 100
t0 <- 0
tn <- 1 # TEMPS EN ANYS!!!!
S0 <- 90
r <- 0.05 # interès ANUAL
sigma <- 0.40
K <- 75
monte_carlo(M, N, t0, tn, S0, r, sigma, payoff_function, K)
