## 聚类
library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)
library(ggfortify)
a<-Rectal_cancer
b <- a[c(2, 3, 4,5)]
autoplot(prcomp(b))
autoplot(prcomp(b))
autoplot(prcomp(b), data = a, colour = 2)
autoplot(prcomp(b), data = a, colour = 2, label = TRUE,         label.size = 3)
autoplot(prcomp(b), data = a, colour = 2, shape = FALSE,         label.size = 3)
autoplot(prcomp(b), data = a, colour = 2, loadings = TRUE)

## 3D图
# 交互3D
attach(Rectal_cancer)
a<-Rectal_cancer

install.packages("rgl")
install.packages("httpuv")
library(rgl)
with(a,{
    plot3d(Age, PCT, CRP, col="red", size=3)
})

# 静态3D
library(scatterplot3d)
with(a,{
    scatterplot3d(Age,   # x-axis
    PCT, # y-axis
    CRP,  # z-axis
    main="3D Scatterplot",color =2)
})


## 聚类--K mean
install.packages("fpc")
library(fpc) # install.packages("fpc")
data(iris)
head(iris)
# 0-1 正规化数据
min.max.norm <- function(x){
    (x-min(x))/(max(x)-min(x))
}
raw.data <- a[,2:5]
norm.data <- data.frame(sl = min.max.norm(raw.data[,1]),
sw = min.max.norm(raw.data[,2]),
pl = min.max.norm(raw.data[,3]),
pw = min.max.norm(raw.data[,4]))

# k取2到8，评估K
K <- 2:8
round <- 30 # 每次迭代30次，避免局部最优
rst <- sapply(K, function(i){
    print(paste("K=",i))
    mean(sapply(1:round,function(r){
        print(paste("Round",r))
        result <- kmeans(norm.data, i)
        stats <- cluster.stats(dist(norm.data), result$cluster)
        stats$avg.silwidth
    }))
})
plot(K,rst,type='l',main='轮廓系数与K的关系', ylab='轮廓系数')

# 降纬度观察
old.par <- par(mfrow = c(1,2))
k = 2 # 根据上面的评估 k=2最优
clu <- kmeans(norm.data,k)
mds = cmdscale(dist(norm.data,method="euclidean"))
plot(mds, col=clu$cluster, main='kmeans聚类 k=2', pch = 19)
plot(mds, col=iris$Species, main='原始聚类', pch = 19)
par(old.par)


## 主成分与因子分析

#加载psych包
library(psych)
#选择主成分数目
a<-Rectal_cancer
b <- a[c(2, 3, 4,5)]
fa.parallel(x = b, fa="pc")
#提取主成分
pc <- principal(r = b, nfactors = 2, rotate = 'none')
pc

#旋转主成分，这里使用最常用的方差极大正交旋转法
rc <- principal(r = b, nfactors = 2, rotate = 'varimax')
rc

## 通过最大方差法的旋转，发现与之前的PC1，PC2值变化很大，而且更容易的找出主成分主要由哪些变量解释，
# 这里PC1可由x2、x4和x5变量来解释，而PC2可由x1和x3变量来解释。


#计算各主成分的得分(只需在函数中将参数scores设置为TRUE)
spc <- principal(r =b, nfactors = 2, rotate = 'varimax', scores = TRUE)
scores <- spc$scores
scores

## 这里需要指出的是：如果得到的数据不是原始数据，而是协方差矩阵，
# 可使用cov2cor()函数将协方差矩阵转换为相关系数矩阵。

#选择因子数目
library(psych)
fa.parallel(x = b, fa="fa")

# 提取因子(最大似然法(ml)更具良好的统计性质，但有时不会收敛，
# 此时可以使用主轴迭代法(pa))
f <- fa(r = b, nfactors = 2, n.obs = 112, fm = 'ml', rotate = 'none')
f

#因子旋转，有助于更好地解释因子
fr <- fa(r = b, nfactors = 2, n.obs = 112, fm = 'ml', rotate = 'varimax')
fr

计算因子得分：只需在函数中将参数scores设置为TRUE
#因子得分的计算
sf <- fr <- fa(r = my.data, nfactors = 2, n.obs = 112, fm = 'ml', rotate = 'varimax', scores = TRUE)
sf$scores
