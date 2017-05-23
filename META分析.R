# 首选安装meta、metafor包，并加载
install.packages("meta")
install.packages("metafor")
library(meta)
library(metafor)




# 单组率meta分析的R编程

event <- c(81, 15, 0, 1)
n <- c(263, 148, 20, 29)
m1 <- metaprop(event, n, sm="PLOGIT")
forest(m1)
funnel(m1)


# 连续变量效应指标的Meta分析
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95, subset=c(41,47,51,59), sm="RR", method="I")
forest(meta1)
funnel(meta1)


# 分类变量效应指标的Meta分析
data(Fleiss93cont)
meta2 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont, sm="SMD")
forest(meta2)
funnel(meta2)



# Meta回归 1
data(dat.colditz1994, package="metafor")
data10 <- dat.colditz1994
mh2 <- metabin(Sex, Age, PCT,CRP,data=X2, studlab=paste(Number, Age))
mh2.mr <- metareg(mh2, ablat)
bubble(mh2.mr)

# Meta回归2
data(dat.colditz1994, package="metafor")
data10 <- dat.colditz1994
mh2 <- metabin(tpos, tpos+tneg, cpos, cpos+cneg,data=data10, studlab=paste(author, year))
mh2.mr <- metareg(mh2, ablat)
bubble(mh2.mr)
