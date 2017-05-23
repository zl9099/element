#创建数据集
x1 <- c(5700,1000,3400,3800,4000,8200,1200,9100,9900,9600,9600,9400)
x2 <- c(12.8,10.9,8.8,13.6,12.8,8.3,11.4,11.5,12.5,13.7,9.6,11.4)
x3 <- c(2500,600,1000,1700,1600,2600,400,3300,3400,3600,3300,4000)
x4 <- c(270,10,10,140,140,60,10,60,180,390,80,100)
x5 <- c(25000,10000,9000,25000,25000,12000,16000,14000,18000,25000,12000,13000)
my.data <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5)
#x1为总人口数；x2为雇员总数；x3为中等校平均校龄；x4为专业服务项目数；x5为中等房价

#加载psych包
library(psych)
#选择主成分数目
fa.parallel(x = my.data, fa="pc")
#提取主成分
pc <- principal(r = my.data, nfactors = 2, rotate = 'none')
pc

#旋转主成分，这里使用最常用的方差极大正交旋转法
rc <- principal(r = my.data, nfactors = 2, rotate = 'varimax')
rc

## 通过最大方差法的旋转，发现与之前的PC1，PC2值变化很大，而且更容易的找出主成分主要由哪些变量解释，
# 这里PC1可由x2、x4和x5变量来解释，而PC2可由x1和x3变量来解释。


#计算各主成分的得分(只需在函数中将参数scores设置为TRUE)
spc <- principal(r = my.data, nfactors = 2, rotate = 'varimax', scores = TRUE)
scores <- spc$scores
scores

## 这里需要指出的是：如果得到的数据不是原始数据，而是协方差矩阵，
# 可使用cov2cor()函数将协方差矩阵转换为相关系数矩阵。

#选择因子数目
library(psych)
fa.parallel(x = my.data, fa="fa")

# 提取因子(最大似然法(ml)更具良好的统计性质，但有时不会收敛，
# 此时可以使用主轴迭代法(pa))
f <- fa(r = my.data, nfactors = 2, n.obs = 112, fm = 'ml', rotate = 'none')
f

#因子旋转，有助于更好地解释因子
fr <- fa(r = my.data, nfactors = 2, n.obs = 112, fm = 'ml', rotate = 'varimax')
fr

计算因子得分：只需在函数中将参数scores设置为TRUE
#因子得分的计算
sf <- fr <- fa(r = my.data, nfactors = 2, n.obs = 112, fm = 'ml', rotate = 'varimax', scores = TRUE)
sf$scores
