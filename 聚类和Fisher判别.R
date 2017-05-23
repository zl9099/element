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


library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)
library(ggfortify)
df <- X2[c(2, 3, 4,5)]
autoplot(prcomp(df))
autoplot(prcomp(df))
autoplot(prcomp(df), data = X2, colour = 2)
autoplot(prcomp(df), data = X2, colour = 2, label = TRUE,         label.size = 3)
autoplot(prcomp(df), data = X2, colour = 2, shape = FALSE,         label.size = 3)
autoplot(prcomp(df), data = X2, colour = 2, loadings = TRUE)



## 双向聚类热图
install.packages("gplots")
library(gplots)
df <- X2[c(3, 4,5)]
df<-as.matrix(df)
heatmap.2(df)
heatmap.2(df,col=redgreen)


## 倾向性得分匹配
install.packages("nonrandom")
library(nonrandom)
library(lme4)
library(Matrix)
plot.pscore(ps,with.legend=T,par.1=list(lty=1,lwd=2),par.0=list(lty=3,lwd=2),xlab="",ylim=C(0,4.5))


# Roc
install.packages("pROC")
library(pROC)
attach(df)
roc1<-multiclass.roc(df$Disease,df$PCT)
roc2<-multiclass.roc(df$Disease,df$CRP)
roc.test(roc1,roc2)
roc1
