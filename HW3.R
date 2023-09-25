#Hw3

#Júlia Albero, Sergi Cucala, Marc Luque, Carlo Sala

# exercise 1

# strategies
# w/ same expiry & underlying
# spot price: 100

# call, put & stock
# k: strike; s: stock
call <- function(k, s, p = 0) {
  (s - k) * (s > k) - p
} # max(s-k,0) - p
put <- function(k, s, p = 0) {
  (k - s) * (k > s) - p
} # max(k-s,0)-p
stock <- function(spot, s) {
  s - spot
}

# a. Payoff functions (no premium)
bull <- function(s, p1, p2) {
  call(100, s, p1) - call(120, s, p2)
}
bear <- function(s, p1, p2) {
  -put(100, s, p1) + put(120, s, p2)
}
cvrcall <- function(s, p) {
  stock(100, s) - call(110, s, p)
}
cvrput <- function(s, p) {
  -stock(100, s) - put(90, s, p)
}
collar <- function(s, p1, p2) {
  stock(100, s) - call(110, s, p1) + put(90, s, p2)
}
bfly <- function(s, p1, p2, p3) {
  call(90, s, p1) - 2 * call(100, s, p2) + call(110, s, p3)
}
condor <- function(s, p1, p2, p3, p4) {
  call(90, s, p1) - call(100, s, p2) - call(110, s, p3) + call(120, s, p4)
}

# b. Plots (no premiums)
par(mfrow = c(1, 1))

curve(bull(x, p1 = 0, p2 = 0), from = 0, to = 200, col = 1, main = "Bull payoff diagram")
curve(call(100, x), add = TRUE, from = 0, to = 200, col = 3)
curve(-call(120, x), add = TRUE, from = 0, to = 200, col = 2)

curve(bear(x, p1 = 0, p2 = 0), from = 0, to = 200, col = 1, main = "Bear payoff diagram")
curve(-put(100, x), add = TRUE, from = 0, to = 200, col = 2)
curve(put(120, x), add = TRUE, from = 0, to = 200, col = 3)

curve(cvrcall(x, p = 0), from = 0, to = 200, col = 1, main = "Covercall payoff diagram")
curve(stock(100, x), add = TRUE, from = 0, to = 200, col = 3)
curve(-call(110, x), add = TRUE, from = 0, to = 200, col = 2)

curve(cvrput(x, p = 0), from = 0, to = 200, col = 1, main = "Coverput payoff diagram")
curve(-stock(100, x), add = TRUE, from = 0, to = 200, col = 3)
curve(-put(90, x), add = TRUE, from = 0, to = 200, col = 2)

curve(collar(x, p1 = 0, p2 = 0), from = 0, to = 200, col = 1, main = "Collar payoff diagram")
curve(stock(100, x), add = TRUE, from = 0, to = 200, col = 3)
curve(-call(110, x), add = TRUE, from = 0, to = 200, col = 2)
curve(put(90, x), add = TRUE, from = 0, to = 200, col = 3)

curve(bfly(x, p1 = 0, p2 = 0, p3 = 0), from = 0, to = 200, col = 1, main = "Butterflies payoff diagram")
curve(call(90, x), add = TRUE, from = 0, to = 200, col = 3)
curve(-call(100, x), add = TRUE, from = 0, to = 200, col = 2)
curve(call(110, x), add = TRUE, from = 0, to = 200, col = 3)

curve(condor(x, p1 = 0, p2 = 0, p3 = 0, p4 = 0), from = 0, to = 200, col = 1, main = "Condor payoff diagram")
abline(v = 100, col = "pink", lty = 2, lwd = 1)
abline(v = 110, col = "pink", lty = 2, lwd = 1)
curve(call(90, x), add = TRUE, from = 0, to = 200, col = 3)
curve(-call(100, x), add = TRUE, from = 0, to = 200, col = 2)
curve(-call(110, x), add = TRUE, from = 0, to = 200, col = 2)
curve(call(120, x), add = TRUE, from = 0, to = 200, col = 3)

# d. Plots (arbitrary premiums)
curve(bull(x, 10, 0), from = 0, to = 200, col = 1, main = "Bull payoff diagram with premium")
abline(h = 0, lty = 2)

curve(bear(x, 0, 10), from = 0, to = 200, col = 1, main = "Bear payoff diagram with premium")
abline(h = 0, lty = 2)

curve(cvrcall(x, 45), from = 0, to = 200, col = 1, main = "Covercall payoff diagram with premium")
abline(h = 0, lty = 2)
curve(cvrcall(x, 30), add = TRUE, col = "pink")
curve(cvrcall(x, 20), add = TRUE, col = 3)

curve(cvrput(x, 45), from = 0, to = 200, col = 1, main = "Coverput payoff diagram with premium")
abline(h = 0, lty = 2)
curve(cvrput(x, 30), add = TRUE, col = "pink")
curve(cvrput(x, 20), add = TRUE, col = 3)

curve(bfly(x, 0, 0, 5), from = 0, to = 200, col = 1, main = "Butterflies payoff diagram with premium")
abline(h = 0, lty = 2)

curve(condor(x, 5, 0, 0, 0), from = 0, to = 200, col = 1, main = "Condor payoff diagram with premium")
abline(h = 0, lty = 2)
abline(v = 100, col = "pink", lty = 2, lwd = 1)
abline(v = 110, col = "pink", lty = 2, lwd = 1)


# exercise 2

arbitrage <- function(s) {
  call(58, s, 3) - stock(60, s) - put(58, s, 2)
}

curve(arbitrage, from = 0, to = 100, col = 1, main = "arbitrage")
curve(call(58, x, 3), add = TRUE, from = 0, to = 100, col = 3)
curve(-stock(60, x), add = TRUE, from = 0, to = 100, col = 3)
curve(-put(58, x, 2), add = TRUE, from = 0, to = 100, col = 3)
