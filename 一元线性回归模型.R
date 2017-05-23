# 数据集已存在df变量中
head(df)

# 分别给x,y赋值
x<-as.numeric(df[,1])
y<-as.numeric(df[,2])
# 画图
plot(y~x+1)

# 建立线性回归模型
lm.ab<-lm(y ~ 1+x)
# 打印参数估计的结果
lm.ab

#自己计算
# x均值
Xm<-mean(x);Xm
# y均值
Ym<-mean(y);Ym
# 计算回归系数
b <- sum((x-Xm)*(y-Ym)) / sum((x-Xm)^2) ;b
# 计算回归常数
a <- Ym - b * Xm;a

# 画出回归线
plot(y~x+1)
abline(lm.ab)

# 通常会采用三种显著性检验的方法。

# T检验法：T检验是检验模型某个自变量Xi对于Y的显著性，通常用P-value判断显著性，小于0.01更小时说明这个自变量Xi与Y相关关系显著。

# F检验法：F检验用于对所有的自变量X在整体上看对于Y的线性显著性，也是用P-value判断显著性，小于0.01更小时说明整体上自变量与Y相关关系显著。
#  R^2(R平方)相关系统检验法：用来判断回归方程的拟合程度，R^2的取值在0，1之间，越接近1说明拟合程度越好。


summary(lm.ab)      # 计算结果
# 模型解读：

# Call，列出了回归模型的公式。

# Residuals，列出了残差的最小值点，1/4分位点，中位数点，3/4分位点，最大值点。

# Coefficients，表示参数估计的计算结果。

# Estimate，为参数估计列。Intercept行表示常数参数a的估计值 ，x行表示自变量x的参数b的估计值。

# Std. Error，为参数的标准差，sd(a), sd(b)

# t value，为t值，为T检验的值

# Pr(>|t|) ，表示P-value值，用于T检验判定，匹配显著性标记

# 显著性标记，***为非常显著，**为高度显著, **为显著，·为不太显著，没有记号为不显著。

# Residual standard error，表示残差的标准差，自由度为n-2。

# Multiple R-squared，为相关系数R^2的检验，越接近1则越显著。

# Adjusted R-squared，为相关系数的修正系数，解决多元回归自变量越多，判定系数R^2越大的问题。

#F-statistic，表示F统计量，自由度为(1,n-2)，p-value:用于F检验判定，匹配显著性标记。


# 残差
> y.res<-residuals(lm.ab)

# 打印前6条数据
head(y.res)

# 正态分布检验
shapiro.test(y.res)

# 画出残差散点图
plot(y.res)
# 对残差进行Shapiro-Wilk正态分布检验，W接近1，p-value>0.05，证明数据集符合正态分布

#或者利用画图检验
# 画图，回车展示下一张
plot(lm.ab)
Hitto see nextplot:   # 残差拟合图
Hitto see nextplot:   # 残差QQ图
Hitto see nextplot:   # 标准化的残差对拟合值
Hitto see nextplot:   # 标准化残差对杠杆值

par(mfrow=c(2,2))
plot(lm.ab)


#去掉异常值
# 查看27和192
df[c(27,192),]

# 新建数据集，去掉27和192
> df2<-df[-c(27,192),]

回归建模和显著性检验。

x2<-as.numeric(df2[,1])
y2<-as.numeric(df2[,2])
lm.ab2<-lm(y2 ~ 1+x2)
summary(lm.ab2)


#我们可以用R语言的predict()函数来计算预测值y0，和相应的预测区间。程序算法如下。
new<-data.frame(x=14040)
lm.pred<-predict(lm.sol,new,interval="prediction",level=0.95)

# 预测结果
lm.pred
#当x0=14040时，在预测区间为0.95的概率时，y0的值为 14102，预测区间为[14093.73,14110.44]。

#我们通过图形来表示。
plot(y~x+1)
abline(lm.ab,col='red')
points(rep(newX$x,3),y=lm.pred,pch=19,col=c('red','blue','green'))
#其中，红色点为y0的值，蓝色点为预测区间最小值，绿色点为预测区间最大值。
