rm(list = ls())
library("quantmod")
options("getSymbols.warning4.0" = FALSE)
cat("\f")

### 1

getlog <- function(data) {
  return(log(as.numeric(data[-1])) - log(as.numeric(data[-length(data)])))
}

getSymbols("^IBEX", scr = "yahoo", from = "2016-01-01", to = "2016-12-16")
names(IBEX) 
IBEXlog <- getlog(IBEX[, 6]) # 6th col contains Adjusted prices
# https://es.finance.yahoo.com/quote/%5EIBEX/components?p=%5EIBEX
# ANE.MC no carrega
comp <- c(
  "BBVA.MC", "SGRE.MC", "AENA.MC", "MRL.MC", "COL.MC", "FER.MC", "IBE.MC", "SAB.MC", "SAN.MC", "MAP.MC",
  "ACS.MC", "AMS.MC", "CABK.MC", "ACX.MC", "ITX.MC", "FDR.MC", "ELE.MC", "NTGY.MC", "RED.MC", "MEL.MC",
  "PHM.MC", "CLNX.MC", "BKT.MC", "GRF.MC", "IAG.MC", "ENG.MC", "TEF.MC", "MTS.MC", "ANA.MC"
)
logs <- matrix(0, nrow = length(IBEXlog), ncol = length(comp))
colnames(logs) <- comp
getSymbols(comp, scr = "yahoo", from = "2016-01-01", to = "2016-12-16")

logs[, 1] <- getlog(BBVA.MC[, 6])
logs[, 2] <- getlog(SGRE.MC[, 6])
logs[, 3] <- getlog(AENA.MC[, 6])
logs[, 4] <- getlog(MRL.MC[, 6])
logs[, 5] <- getlog(COL.MC[, 6])
logs[, 6] <- getlog(FER.MC[, 6])
logs[, 7] <- getlog(IBE.MC[, 6])
logs[, 8] <- getlog(SAB.MC[, 6])
logs[, 9] <- getlog(SAN.MC[, 6])
logs[, 10] <- getlog(MAP.MC[, 6])
logs[, 11] <- getlog(ACS.MC[, 6])
logs[, 12] <- getlog(AMS.MC[, 6])
logs[, 13] <- getlog(CABK.MC[, 6])
logs[, 14] <- getlog(ACX.MC[, 6])
logs[, 15] <- getlog(ITX.MC[, 6])
logs[, 16] <- getlog(FDR.MC[, 6])
logs[, 17] <- getlog(ELE.MC[, 6])
logs[, 18] <- getlog(NTGY.MC[, 6])
logs[, 19] <- getlog(RED.MC[, 6])
logs[, 20] <- getlog(MEL.MC[, 6])
logs[, 21] <- getlog(PHM.MC[, 6])
logs[, 22] <- getlog(CLNX.MC[, 6])
logs[, 23] <- getlog(BKT.MC[, 6])
logs[, 24] <- getlog(GRF.MC[, 6])
logs[, 25] <- getlog(IAG.MC[, 6])
logs[, 26] <- getlog(ENG.MC[, 6])
logs[, 27] <- getlog(TEF.MC[, 6])
logs[, 28] <- getlog(MTS.MC[, 6])
logs[, 29] <- getlog(ANA.MC[, 6])

# 2. 

IBEXret <- exp(IBEXlog)-1
ret <- exp(logs) -1
betas <- matrix(0,ncol=1,nrow=length(comp))
rownames(betas)<-comp
coefs <- betas
for(i in 1:length(comp)){
  mod <- lm(ret[,i]~IBEXret - 1)
  coefs[i] <- mod$coefficients
  betas[i] <- cov(ret[,i],IBEXret)
}
betas <- betas/var(IBEXret)
all.equal(coefs,betas)

### 3
# let's choose BBVA (>1) and AENA (<1)
betas["BBVA.MC",] # BBVA
betas["AENA.MC",] # AENA

### 4
par(mfrow = c(1, 3))
plot(BBVA.MC[, 6]*100/as.numeric(BBVA.MC[1,6]), main = "BBVA", col = "red")
plot(AENA.MC[, 6]*100/as.numeric(AENA.MC[1,6]), main = "AENA", col = "red")
plot(IBEX[, 6]*100/as.numeric(IBEX[1,6]), main = "IBEX", col = "red")
