library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)
library(ggfortify)

df <- iris[c(1, 2, 3, 4)]
autoplot(prcomp(df)）

autoplot(prcomp(df), data = iris, colour = 'Species')

autoplot(prcomp(df), data = iris, colour = 'Species', label = TRUE,         label.size = 3)

autoplot(prcomp(df), data = iris, colour = 'Species', shape = FALSE,         label.size = 3)

autoplot(prcomp(df), data = iris, colour = 'Species', loadings = TRUE)

autoplot(prcomp(df), data = iris, colour = 'Species',         loadings = TRUE, loadings.colour = 'blue',         loadings.label = TRUE, loadings.label.size = 3)


# K-均值聚类
autoplot(kmeans(USArrests, 3), data = USArrests)
autoplot(kmeans(USArrests, 3), data = USArrests, label = TRUE,         label.size = 3)

# 其他聚类
## ggfortify 也支持 cluster::clara, cluster::fanny, cluster::pam
library(cluster)
autoplot(clara(iris[-5], 3))
autoplot(fanny(iris[-5], 3), frame = TRUE)
autoplot(pam(iris[-5], 3), frame = TRUE, frame.type = 'norm')


# lfda（Fisher局部判别分析）
install.packages("lfda")
library(lfda) # Fisher局部判别分析 (LFDA)
model <- lfda(iris[-5], iris[, 5], 4, metric="plain")
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')


# 非线性核Fisher局部判别分析 (KLFDA)
model <- klfda(kmatrixGauss(iris[-5]), iris[, 5], 4, metric="plain")
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')



# 半监督Fisher局部判别分析 (SELF)
model <- self(iris[-5], iris[, 5], beta = 0.1, r = 3, metric="plain")
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')


# 时间序列的可视化

## ts对象
library(ggplot2)
library(ggfortify)
autoplot(AirPassengers)

autoplot(AirPassengers, ts.colour = 'red', ts.linetype = 'dashed')

# 多变量时间序列
install.packages("vars")
install.packages("MASS")
library(vars)
library(MASS)
library(strucchange)
library(zoo)

data(Canada)
autoplot(Canada)


# 使用 facets = FALSE 可以把所有变量画在一条轴上。
autoplot(Canada, facets = FALSE)


## autoplot 也可以理解其他的时间序列类别。可支持的R包有：

# zoo::zooreg

# xts::xts

# timeSeries::timSeries

# tseries::irts


# 一些例子：
install.packages("xts")
library(xts)
autoplot(as.xts(AirPassengers), ts.colour = 'green')


install.packages("timeSeries")
install.packages("timeDate")
library(timeDate)
library(timeSeries)

autoplot(as.timeSeries(AirPassengers), ts.colour = ('dodgerblue3'))

# 你也可以通过 ts.geom 来改变几何形状，目前支持的有 line， bar 和 point。
autoplot(AirPassengers, ts.geom = 'bar', fill = 'blue')

autoplot(AirPassengers, ts.geom = 'point', shape = 3)




# forecast包
install.packages("forecast")
library(forecast)

d.arima <- auto.arima(AirPassengers)
d.forecast <- forecast(d.arima, level = c(95), h = 50)
autoplot(d.forecast)

# 有很多设置可供调整：
autoplot(d.forecast, ts.colour = 'firebrick1', predict.colour = 'red',         predict.linetype = 'dashed', conf.int = FALSE)




# vars包
library(vars)
data(Canada)

d.vselect <- VARselect(Canada, lag.max = 5, type = 'const')$selection[1]

d.var <- VAR(Canada, p = d.vselect, type = 'const')

autoplot(predict(d.var, n.ahead = 50), ts.colour = 'dodgerblue4',
predict.colour = 'blue', predict.linetype = 'dashed')



# changepoint包
install.packages("changepoint")
library(changepoint)
autoplot(cpt.meanvar(AirPassengers))

autoplot(cpt.meanvar(AirPassengers), cpt.colour = 'blue', cpt.linetype = 'solid')



# strucchange包
library(strucchange)
autoplot(breakpoints(Nile ~ 1), ts.colour = 'blue', ts.linetype = 'dashed',         cpt.colour = 'dodgerblue3', cpt.linetype = 'solid')



# dlm包
install.packages("dlm")
library(dlm)
form <- function(theta){
    dlmModPoly(order = 1, dV = exp(theta[1]), dW = exp(theta[2])) }
model <- form(dlmMLE(Nile, parm = c(1, 1), form)$par)
filtered <- dlmFilter(Nile, model)

autoplot(filtered)

autoplot(filtered, ts.linetype = 'dashed', fitted.colour = 'blue')

smoothed <- dlmSmooth(filtered)
autoplot(smoothed)

p <- autoplot(filtered)
autoplot(smoothed, ts.colour = 'blue', p = p)

# KFAS包
install.packages("KFAS")
library(KFAS)

model <- SSModel(  Nile ~ SSMtrend(degree=1, Q=matrix(NA)), H=matrix(NA) )

fit <- fitSSM(model=model, inits=c(log(var(Nile)),log(var(Nile))),              method="BFGS")

smoothed <- KFS(fit$model)

autoplot(smoothed)



# 使用 smoothing='none' 可以画出过滤后的结果。
filtered <- KFS(fit$model, filtering="mean", smoothing='none')
autoplot(filtered)

trend <- signal(smoothed, states="trend")

p <- autoplot(filtered)

autoplot(trend, ts.colour = 'blue', p = p)




## stats包可支持的stats包里的对象有：

# stl, decomposed.ts

# acf, pacf, ccf

# spec.ar, spec.pgram

# cpgram


autoplot(stl(AirPassengers, s.window = 'periodic'), ts.colour = 'blue')

autoplot(acf(AirPassengers, plot = FALSE))

autoplot(acf(AirPassengers, plot = FALSE), conf.int.fill = '#0000FF',         conf.int.value = 0.8, conf.int.type = 'ma')

autoplot(spec.ar(AirPassengers, plot = FALSE))

ggcpgram(arima.sim(list(ar = c(0.7, -0.5)), n = 50))


library(forecast)
ggtsdiag(auto.arima(AirPassengers))

gglagplot(AirPassengers, lags = 4)
