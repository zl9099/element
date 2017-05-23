# 数据集已存在df变量中
head(df,20)


# 建立多元线性回归模型
> lm1<-lm(y~x1+x2+x3+x4,data=df)

# 打印参数估计的结果
> lm1
summary(lm1)

#T检验：所自变量都是非常显著***

#F检验：同样是非常显著，p-value < 2.2e-16

# 调整后的R^2：相关性非常强为0.972
par(mfrow=c(2,2))
plot(lm1)

#模型预测
par(mfrow=c(1,1))  #设置画面布局
# 预测计算
> dfp<-predict(lm1,interval="prediction")
# 打印预测时
> head(dfp,10)


# 合并数据
> mdf<-merge(df$y,dfp)
# 画图
> draw(mdf)

#模型优化
pairs(as.data.frame(df))

# 模型调整，从原模型中去掉x2变量。
> lm2<-update(lm1, .~. -x2)

> summary(lm2)

# 模型调整
> lm3<-update(lm1, .~. + x1*x2)
> summary(lm3)

#对lm1模型做逐步回归，ACI越小越好
> step(lm1)
#对lm3模型做逐步回归
> step(lm3)

lm9<-lm(y~x1+x2+x3+x4,data=df)  # 日K线数据
summary(lm9)

#数据集的基本统计信息。
summary(df)
