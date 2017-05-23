#导入数据(但在菜单File   Import Dataset导入更方便)
data<-read.csv(file.choose(),header = T)

#导出储存在功能区的数据
ls()
#移出数据“Y"
rm(Y)

#字符变量与数字变量
xx<-"marin"
xx
yy<-"1"
yy
#作为字符而不是数字

#计算
11+14
7*5

x<-3
y<-4
z<-x+y
z
x^2+y^2

#平方根
sqrt(y)
y^(1/2)

log(y)
log2(y)
#ln
exp(y)
#绝对值
abs(-14)

#序列命令
#从1至7 增量为1
seq(from=1,to=7,by=1)
seq(from=1,to=7,by=1/3)
seq(from=1,to=7,by=0.25)

#创建重复的数字或字符
rep(1,time=10)
rep(1:3,time=5)
rep(seq(from=2,to=5,by=0.25),time=5)
x<-1:5
x+10

#if two vectors are of the same length ,we may add/subtract/mult/div

#x+y/x-y/x*y/ x/y
y<-seq(from=1,to=7,by=0.5)
y
y[3]
y[-3]
y[1:3]
y[c(1,5)]
y[-c(1,5)]
y[y<6]

#创建矩阵
#byrow=TRUE为按行排列
matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,byrow=TRUE)
mat<-matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,byrow=TRUE)
mat*10

#导入数据(但在右上角导入更方便)
data<-read.csv(file.choose(),header = T)

#查看数据由几行几列组成
dim(LungCapData_)
#查看前六行
head(LungCapData_)
#查看后六行
tail(LungCapData_)
#导出某行某列的方法与矩阵相同
LungCapData_[c(5,6,7,8,9),]
LungCapData_[5:9,]
LungCapData_[-(4:722),]

#检查变量名
names(LungCapData_)

#将变量链接到R
attach(LungCapData_)
mean(Age)
#卸载此数据从R中
detach(LungCapData_)

#询问变量类型
attach(LungCapData_)
class(Gender)
#询问层次
levels(Smoke)
levels(Age)

#对数值型变量进行简单的计算,对因子型使用概率概括
summary(LungCapData_)
#由数值型变为因子型
x<-c(0,1,1,1,0,0,0,0,0,0)
x<-as.factor(x)
class(x)
summary(x)

#长度命令
length(Age)

#平均
mean(Age[Gender=="female"])

#创建只含单一性别的数据子集
Femdata<-LungCapData_[Gender=="female",]
MaleOver15<-LungCapData_[Gender=="male"&Age>15,]
MaleOver15

#返回到上述值符合T否则为F
Age[1:5]
temp<-Age>15
temp[1:5]

#转变为逻辑矢量
FemSmoke<-Gender=="female"&Smoke=="Yes"
FemSmoke
FemSmoke[1:5]

#使用“cbind”命令做列式矩阵
MoreData<-cbind(LungCapData_,FemSmoke)
MoreData

#消除R储存的所有数据
rm(list = ls())

#保存工作空间图像（主菜单 session__ choose oirectory）

#频率表
table(Gender)
table(Gender)/725

#在饼图外加一个框
pie(Gender)
box()

#箱图(las=1 旋转y轴的值)
boxplot(LungCap)
quantile(LungCap,probs = c(0,0.25,0.5,0.75,1))
boxplot(LungCap,main="Boxplot",Ylab="LungCapData",Ylim=(10.16),las=1)
boxplot(LungCap~Gender)
boxplot(LungCap[Gender=="female"],LungCap[Gender=="male"])

AgeGrops<-cut(Age,breaks=c(0,13,15,17,25),labels=c("<13","14/15","16/77","18+"))
AgeGrops[1:5]
levels(AgeGrops)

boxplot(LungCap~Smoke*AgeGrops,Ylab="LungCap",main="LungCap VS Smoke by AgeGroup",las=2,col=(4.2))

hist(LungCap)
#概率密度
hist(LungCap,freq = FALSE)
hist(LungCap,freq = F)
#概率
hist(LungCap,prob = T)
hist(LungCap,prob = T,ylim = c(0,0.2),breaks=c(0,2,4,6,8,10,12,14,16))
hist(LungCap,prob =T,ylim = c(0,0.2),breaks=seq(from=0,to=16,by=2) )

lines(density(LungCap),col=2,lwd=3)
