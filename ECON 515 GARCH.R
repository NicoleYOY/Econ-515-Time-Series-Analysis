rm(list=ls())
library(readxl)
Nvidia<- read_xlsx("ECON515_Dataset.xlsx")
plot(Nvidia$Date, Nvidia$Return, type="l", xlab="Year", ylab="Return (%)", 
     main="Nvidia Stock Return from 10/08/2018 to 10/05/2023")

mean(Nvidia$Return)
sd(Nvidia$Return)

par(mfrow=c(1,1))
acf(Nvidia$Return, main = "ACF of Nvidia stock return")
pacf(Nvidia$Return, main = "PACF of Nvidia stock return")

Box.test (Nvidia$Return , lag = 1, type="Ljung")
Box.test (Nvidia$Return , lag = 2, type="Ljung")
Box.test (Nvidia$Return , lag = 3, type="Ljung")

library(fGarch)
results11 <- garchFit(formula = ~ garch(1, 1), data = Nvidia$Return,trace=F)
summary(results11)
results10 <- garchFit(formula = ~ garch(1, 0), data = Nvidia$Return,trace=F)
summary(results10)
results12 <- garchFit(formula = ~ garch(1, 2), data = Nvidia$Return,trace=F)
summary(results12)
results21 <- garchFit(formula = ~ garch(2, 1), data = Nvidia$Return,trace=F)
summary(results21)
results22 <- garchFit(formula = ~ garch(2, 2), data = Nvidia$Return,trace=F)
summary(results22)
results23 <- garchFit(formula = ~ garch(2, 3), data = Nvidia$Return,trace=F)
summary(results23)
results32 <- garchFit(formula = ~ garch(3, 2), data = Nvidia$Return,trace=F)
summary(results32)
# GARCH(2,2) with lowest AIC

volatility_GARCH <- volatility(results22, type = "sigma")
par(mfrow=c(1,1))
plot(Nvidia$Date, Nvidia$Return, type = "l", xlab="Year", ylab="Return (%)", 
     main="Actual Nvidia Stock Return and Model Fitting")
lines(Nvidia$Date, mean(Nvidia$Return) + 1.96 * volatility_GARCH, col="red")
lines(Nvidia$Date, mean(Nvidia$Return) - 1.96 * volatility_GARCH, col="red")
lines(Nvidia$Date, mean(Nvidia$Return) + 1.28 * volatility_GARCH, col="blue")
lines(Nvidia$Date, mean(Nvidia$Return) - 1.28 * volatility_GARCH, col="blue")
legend("topleft",legend=c("Actual","95% CI","80% CI"),lty=c(1,1),col=c("black","red","blue"))

predict(results22, n.ahead=100, plot=TRUE, conf=.95, nx=length(Nvidia$Return))
predict(results22, n.ahead=100, plot=TRUE, conf=.80, nx=length(Nvidia$Return))
