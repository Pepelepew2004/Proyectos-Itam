

# Codigos
set.seed(20041997)

## Simular un MA

ma1 <- arima.sim(list(order = c(0,0,1), ma = 0.9), n = 300)
par(mfrow=c(2,1))
ts.plot(ma1)
acf(ma1)
pacf(ma1)

## SImular un AR

mod1 <- 5+arima.sim(list(order = c(1,0,0), ar = 0.2), n = 300)
mod2 <- arima.sim(list(order = c(1,0,0), ar = 0.5), n = 300)
mod3 <- (50/(1-0.9))+arima.sim(list(order = c(1,0,0), ar = 0.9), n = 300)


## comparacion AR(1) vs AR(2)

AR1 <- arima.sim(list(order = c(1,0,0), ar = 0.8), n = 300)
AR2 <- arima.sim(list(order = c(2,0,0), ar =  c(0.25, 0.5)), n = 300)


# max el numero de paramtros

# M?todo de momentos
yhat1<-ar(y1,order.max=1,AIC=F,method='yw')

# M?xima verosimilitud
yhat1ml<-ar(y1,order.max=1,AIC=F,method='mle')

# CSS
yhat1css<-ar(y1,order.max=1,AIC=F,method='ols')


# FIR
library(TSSS)

# AR model : y(n) = a(1)*y(n-1) + a(2)*y(n-2) + v(n)
a <- c(0.9 * sqrt(3), -0.81)
armaimp(arcoef = a, v = 1.0, n = 1000, lag = 20)


# Ejercicio Clase
library(dplyr)
datos <- read_excel("Ejercicio13.xlsx")

v1_1<-datos %>%  select(V1)
v2_2 <- datos %>% select(V2)


par(mfrow= c(2,1))
acf(v1_1)
pacf(v1_1)

v1 <- diff(ts(v1_1))
par(mfrow= c(2,1))
acf(v1)
pacf(v1)

est_v1 <-  arima(v1_1, c(0,1,2), method= 'ML')


par(mfrow= c(2,1))
acf(v2_2)
pacf(v2_2)

v2 <- diff(ts(v2_2))
acf(v2)
pacf(v2)

v2 <- diff(v2)
acf(v2)
pacf(v2)

est_v2_1 <-  arima(v2_2, c(1,2,0), method= 'ML')
est_v2 <- ar(v2[,1],order.max=1,AIC=F,method='mle')
est_v2
