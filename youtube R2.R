#茎叶图
stem(LungCap)
#分为两个茎
stem(LungCap,scale = 2)

#柱状图
table(Smoke,Gender)
Table1<-table(Smoke,Gender)
Table1
barplot(Table1)
barplot(Table1,beside = T)
barplot(Table1,beside = T,legend.text = T)
barplot(Table1,beside = T,legend.text =c("Non-Smoke","Smoke"))

#马赛克暗箱
mosaicplot(Table1)

#皮尔森相关（两者线性关系强度的概念）

cor(Age,Height)
#具有较强的线性关系
abline(lm(Height~Age))
#添加线性回归

lines(smooth,spline(Height,Age))



#去掉前10%后10%数据后的平均值
mean(LungCap,trim = 0.10)
#median 中位数，var 方差，range 范围

#90%时对应的数据
quantile(LungCap,probs = 0.90)
#计算相关性
cor(LungCap,Age)


#编辑图改变默认值(spearman相关)
cor(LungCap,Age,method = "spearman")
plot(Age,Height,main="Scatterplot",cex=0.5,cex.main=2,cex.lab=1.5,cex.axis=0.7)
#cex ssandia散点大小，cex.main 标题大小,cex.lab x,y轴标签, cex.axis x,y轴数值

#改字体
attach(LungCapData_)
plot(Age,Height,main="Scatterplot",front.main=4,front.lab=2,front.axis=3)
#改图标
plot(Age,Height,main="Scatterplot",pch="w")

#分图(一排2个)
par(mfrow=c(1,2))
plot(Age[Gender="male"],Height[Gender="male"],xlab = "Age",ylab = "Height",main = "Height vs Age for Males",xlim = c(0,20),ylim = c(45,85))
#恢复默认
par(mfrow=c(1,1))

#去掉x轴与y轴
plot(Age,Height,main="TITLE" ,axes = F)
#设置轴
axis(side = 1,at=c(3,7,12,15),labels =c("3","7","12","15"))
axis(side = 2,at=c(55,65,75),labels =c("55","65","75"))
axis(side = 3,at=c(3,12,7,15),labels =c("3","7","12","15ge"))
axis(side = 4,at=c(55,65,75),labels =c("55","65","75"))
#使画面更简洁
box()

#添加文本
text(x=5,y=55,label="r=0.82")
#“0.82”开端在x=5，y=55坐标的开端
text(x=5,y=55,label="r=0.82",adj = 0)
#“0.82”开端在x=5，y=55坐标的末端（cex=1 为正常大小，font=4为字体斜体加粗）
text(x=5,y=65,label="r=0.82",adj = 0,cex = 1,col = 4,font = 4)
#添加在zzhuob坐标轴底侧
mtext(text="r=0.82",side = 1)
#添加在zzhuob坐标轴左侧
mtext(text="r=0.82",side = 2)
#添加在zzhuob坐标轴上右/上左
mtext(text="r=0.82",adj = 1)

mtext(text="r=0.82",adj = 0)

#改变显示图样
plot(Age[Smoke=="no"],LungCap[Smoke=="no"],main="LungCap VS Age for Smoke/Non-smoke",col=4,xlab = "Age",ylab = "LungCap",pch=16)
points(Age[Smoke=="Yes"],LungCap[Smoke=="Yes"],col=2,pch=17)
legend(x=3.5,y=14,legend = c("Non-smoke","smoke"),col=c(2,4),pch = c(16,17))

legend(x=3.5,y=14,legend = c("Non-smoke","smoke"),col=c(2,4),pch = c(16,17),bty ="n")

#添加趋势线
plot(Age[Smoke=="no"],LungCap[Smoke=="no"],main="LungCap VS Age for Smoke/Non-smoke",col=4,xlab = "Age",ylab = "LungCap",pch=16)
lines(smooth.spline(Age[Smoke=="no"],LungCap[Smoke=="no"]),col=4,lwd=3,lty=2)
lines(smooth.spline(Age[Smoke=="yes"],LungCap[Smoke=="yes"]),col=2,lwd=3,lty=3)

plot(Age[Smoke=="no"],LungCap[Smoke=="no"],main="LungCap VS Age for Smoke/Non-smoke",col=4,xlab = "Age",ylab = "LungCap",pch=16)
legend(x=3.5,y=14,legend = c("Non-smoke","smoke"),col=c(4,2),lty = c(2,3),bty = "n",lwd = 3)

#计算概率二项式（x=——x=0:3，size=30__样本大小，prob=1/6——成功的概率）
dbinom(x=3,size = 20,prob=1/6)
dbinom(x=0:3,size = 20,prob=1/6)
sum(dbinom(x=0:3,size = 20,prob=1/6))
#小于等于3的概率
pbinom(q=3,size=20,prob=1/6,lower.tail = T)
#dbinom：概率分布函数返回值  pbinom查找x的概率密度函数


#X~POISSON(λ=7)已知遵循泊松分布的随机变量
#dpois command可以用来找到值x的概率密度函数 ，rpois 可以采取随机样品从泊松分布和，qpois 用于查找位数
#P(x=4) 正好出现4次的概率
dpois(x=4,lambda = 7)

#P(x<=4)
sum(dpois(x=0:4,lambda = 7))
ppois(q=4,lambda = 7,lower.tail = T)

#P<=70 低尾概率（低于70的概率）
pnorm(q=70,mean = 75,sd=5,lower.tail = T)
#P>=85 低尾概率
pnorm(q=85,mean = 75,sd=5,lower.tail = F)

#Z>=1
pnorm(q=1,mean = 0,sd=1,lower.tail = F)


#四分位数
qnorm(p=0.25,mean = 75,sd=5,lower.tail = T)
#也可通过概率密度函数完成
x<-seq(from=55,to=95,by=0.25)
x
dens<-dnorm(x,mean=75,sd=5)
plot(x,dens)
abline(v=75)

rand<-rnorm(n=40,mean = 75,sd=5)
rand
hist(rand)
