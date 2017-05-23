---
title: "examples-of-textbook"
author: "kahn"
date: "2015年10月6日"
output: word_document
---

quartile

```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
x <- c(1, 2, 3, 5, 7, 8, 2, 5, 4, 6, 8, 7, 34)
quantile(x, probs = c(0.25, 0.5, 0.75))
IQR(x)
x <- c(101 , 107 , 48 , 74 , 57 , 93 , 51 , 10 , 23 , 38 , 67 , 60 , 107 , 67 , 90 , 89 , 99 , 78 , 100 , 88 , 87 , 65 , 89)
quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1))
fivenum(x)
attach(mtcars)
pdf("pic-boxplot-horiz-and-vertical.pdf")
opar<-par(no.readonly = T)
par(mfrow=c(1,2))
boxplot(mpg, horizontal = T, main="Horizontal", xlab="mpg")
boxplot(hp, main="Vertical", xlab="hp")
par(opar)
dev.off()
library(foreign)
library(readstata13)
cgss2013 <- read.dta13("cgss2013.dta", convert.factors = F)
system.time(cgss2013a <- read.dta13("cgss2013a.dta", convert.factors = F))
cgss2013a <- read.dta13("cgss2013a.dta", convert.factors = F)
cgss2013a[cgss2013a<0]<-NA
write.csv(cgss2013a, "cgss2013a-clean.csv")
cgss2013a.clean<-read.csv("cgss2013a-clean.csv")
head(cgss2013a.clean)
head(salary2012)
library(dplyr)
salary <- select(cgss2013a, salary2012=a8a)
length(salary$salary2012)
salary2 <- filter(salary, salary2012<9999996)
length(salary2$salary2012)
head(salary)
attach(salary2)
salary2012
quantile(salary2$salary2012, probs=c(seq(0,100, by=10)/100))
boxplot(salary2$salary2012, horizontal = T)
length(filter(cgss2013a, a8a==9999996))
million <- filter(cgss2013a, a8a==9999996)
head(million$a8a)
omitted <- filter(salary, salary2012>=9999996)
table(omitted$salary)
cgss2013short <- select(cgss2013a, id=id, gender=a2, religion=a501, salary=a8a, party=a10, height=a13, weight=a14, health=a15, trust=a33, fair=a35)
cgss2013short <- cgss2013short[, c(2:10)]
head(cgss2013short)
write.csv(cgss2013short, "cgss2013short.csv", row.names = F)
library(dplyr)
moral <- select(cgss2013a, identity = a10, moral = d1)
x <- select(cgss2013a, gender=a2, id=id)
head(x)
head(x)
plot(x$height, x$weight)
library(readstata13)
auto <- read.dta13("auto.dta")
auto
library(psych)
describeBy(cgss2013short, group = cgss2013short$gender)
hist(cgss2013short$trust)
x <- sample_n(cgss2013short, 100)
t.test(x$trust~x$gender)
describeBy(x, group = x$gender)
hist(x$trust)
```

songpoem
```{r}
txt=read.csv("SongPoem.csv",colClasses="character");
# 句子用标点符号分割。
sentences=strsplit(txt$Sentence,"，|。|！|？|、");
sentences=unlist(sentences);
sentences=sentences[sentences!=""];
s.len=nchar(sentences);

# 单句太长了说明有可能是错误的字符，去除掉。
sentences=sentences[s.len<=15];
s.len=nchar(sentences);

splitwords=function(x,x.len) substring(x,1:(x.len-1),2:x.len);

words=mapply(splitwords,sentences,s.len,SIMPLIFY=TRUE,USE.NAMES=FALSE);
words=unlist(words);
words.freq=table(words);
words.freq=sort(words.freq,decreasing=TRUE);
words.freq[2:101];
```


```{r}
rs2015 <- read.csv("rscore2015.csv")
library(dplyr)
attach(rs2015)
centering <- scale(final, center = T, scale = F)
z.score = scale(final, center = T, scale = T)
rs2015 <- mutate(rs2015, centering=scale(final, center = T, scale = F), z.score = scale(final, center = T, scale = T))
detach(rs2015)
rs2015
library(xtable)
xtable(head(rs2015[,2:8], 5))
```

```{r}
library(e1071)
set.seed(1234)
x=rnorm(100)
skewness(x)
kurtosis(x)
library(psych)
skew(x, type = 3)
kurtosi(x)
kurtosi(x, type = 1)
kurtosis(x)
```


```{r}
library(animation)
## it takes several seconds if 'redraw = TRUE'
oopt = ani.options(nmax = ifelse(interactive(), 500, 2), interval = 0.05)
par(mar = c(3, 2.5, 0.5, 0.2), pch = 20, mgp = c(1.5, 0.5, 0))
buffon.needle()

## this will be faster
buffon.needle(redraw = FALSE)

ani.options(oopt)

```


birthday problem

```{r}
x <- c(2: 100)
y <- 1-choose(365, x) * factorial(x) / 365^x
y <- round(y,2)
prob <- rbind(x, y)
library(xtable)
xtable(prob)
pdf("pic-birthdayprob.pdf")
plot(x, y, xlab = "size (n) ", ylab = "Prob", type = "l", lwd = 2)
dev.off()
```


```{r}
library(plotrix)
plot(1:5,seq(1,10,length=5),type="n",xlab="",ylab="",main="Test draw.circle")
draw.circle(2,4,c(1,0.66,0.33),border="purple",
col=c("#ff00ff","#ff77ff","#ffccff"),lty=1,lwd=1)
draw.circle(2.5,8,0.6,border="red",lty=3,lwd=3)
draw.circle(4,3,0.7,border="green",lty=1,lwd=1)
draw.circle(3.5,7,0.8,border="blue",lty=2,lwd=2)

plot(c(0,10), c(0,10), type="n", main="test draw.ellipse")
draw.ellipse(c(3,7), c(8,8), c(0.5,1), c(1,0.5), col=c(2,4),
angle=c(45,0), segment=rbind(c(0,45),c(45,360)))
draw.ellipse(c(3,7), c(6,6), c(0.5,1), c(1,0.5), col=c(2,4),
angle=c(45,0), segment=rbind(c(0,45),c(45,360)), arc.only=FALSE)
draw.ellipse(c(3,7), c(4,4), c(0.5,1), c(1,0.5), border=c(2,4),
angle=c(45,0), segment=rbind(c(0,45),c(45,360)), arc.only=FALSE)
draw.ellipse(c(3,7), c(2,2), c(0.5,1), c(1,0.5), border=1,
angle=c(45,0), lty=3)
draw.ellipse(c(3,7), c(2,2), c(0.5,1), c(1,0.5), border=c(5,3),
angle=c(45,0), nv=c(3,4), lty=2, lwd=2

library (rpart)
library(maptree)
data (oregon.env.vars)

draw.tree (clip.rpart (rpart (oregon.env.vars), best=7),
nodeinfo=TRUE, units="species", cases="cells", digits=0)
```


calcium intake, CI
```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
intake<-read.csv("C:/xkdog/courses/stats/textbooks/data/calcium.intake.csv", header=T)
mean(intake$calciumintake)
sd(intake$calciumintake)
t.test(intake$calciumintake, mu=1000,conf=0.95, alternative="less")
t.test(intake$calciumintake, mu=1000,conf=0.95, alternative="greater")
```
test scores
```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
scores<-read.csv("test.scores.csv")
scores<-within(scores, {d<-A-B})
mean(scores$d)
sd(scores$d)
opar<-par(no.readonly = T)
par(pin=c(6,1))
pdf("C:/xkdog/courses/stats/textbooks/mytext/pic-testscores.pdf")
boxplot(scores$d, horizontal =T, xlab="Paired Differences in Two Tests")
dev.off()
par(opar)
stripchart(scores$d, method = "stack", pch=19)
t.test(scores$A, scores$B,paired = T, conf.level = 0.90)$conf
t.test(scores$A, scores$B,paired = T, conf.level = 0.90)

```


```{r}
x <- rnorm(50, mean = 0, sd = 2)
y <- rnorm(30, mean = 1, sd = 1)
var.test(x, y, conf.level = 0.99)$conf.int
var.test(x, y, alternative="greater")
var.test(x)
```


```{r}
with(mtcars,
{
    summary(mpg)
    plot(mpg, wt)
    plot(mpg, disp)
}
)
```

```{r}
options(digits = 0)
a<-round(rnorm(100,85,5))
b<-round(rnorm(100, 75,10))
algebra<-data.frame(a,b)
algebra
write.csv(algebra, "algebra.csv",row.names = F)
```

```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
algebra<-read.csv("algebra.csv")
algebra
t.test(algebra$A,algebra$B,conf.level = 0.99)$conf.int
t.test(algebra$A,algebra$B,conf.level = 0.99)
t.test(algebra$A,algebra$B,conf.level = 0.99, alter="greater")
2*pt(2.3812, 14, lower.tail = F)
```
wechat.csv
```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
wechat<-read.csv("wechat.csv")
wechat
boxplot(wechat$a)
t.test(wechat$a, conf.level =0.90)$conf.int
t.test(wechat$a, conf.level =0.95)$conf.int
t.test(wechat$a, conf.level =0.99)$conf.int
n<-50
a<-(n-1)*var(wechat$a)/qchisq(0.975,n-1)
a
b<-(n-1)*var(wechat$a)/qchisq(0.025,n-1)
b
(CI<-c(sqrt(a), sqrt(b)))
```

chipms
```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
chimps<-read.csv("chimps.csv")
chimps
pdf("C:/xkdog/courses/stats/textbooks/mytext/chimps.pdf")
boxplot(chimps$times, horizontal = T, xlab="times of charitable response")
dev.off()
t.test(chimps$times, conf.level = 0.95)$conf.int
```


weight gain
```{r}
weight.gain<-read.csv("weight.gain.csv")
weight.gain
t.test(weight.gain$oncampus,weight.gain$offcampus,conf.level = 0.95)$conf.int
boxplot(c(weight.gain$oncampus, weight.gain$offcampus), horizontal=T)
t.test(weight.gain$oncampus,weight.gain$offcampus,conf.level = 0.95, alternative ="greater")$conf.int
```

fat
```{r}
fat<-read.csv("fat.csv")
fat<-within(fat, {d<-Xray-UT})
t.test(fat$d, conf.level = 0.95, alternative = "two.sided")$conf.int
library(resample)
fat$d
boot.d <- bootstrap(fat$d, mean, R = 10000)
pdf("pic-boot-fatdiff.pdf")
plot(boot.d, col = "lightblue")
dev.off()
library(car)
pdf("pic-qqplot-fatdiff.pdf")
qqPlot(boot.d$replicates, ylab = "paired differences")
dev.off()
CI.bca(boot.d)
```

water
```{r}
water<-read.csv("water.csv")
water
a<-var(water$A)
b<-var(water$B)
a/b
2*pf(a/b,lower.tail=F, df1 = 9, df2=9)
var.test(water$A, water$B, conf.level = 0.95)$conf.int
var.test(water$A, water$B, alternative = "two.sided",  conf.level = 0.95)
sqrt(9*sd(water$A))
sqrt(9*var(water$A)/qchisq(0.95,9))
sqrt(9*var(water$B)/qchisq(0.95,9))
```

官员财产公开
```{r}
((0.9323077-0.93)/sqrt(0.93*(1-0.93)/13000))
pnorm(1.031242, lower.tail = F)
pnorm((12100/13000-0.93)/sqrt(0.93*(1-0.93)/13000), lower.tail=F)
binom.test(12120, 13000, p=0.93, alternative="greater", conf.level = 0.95)
```

茶叶包装 tea

```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
tea<-read.csv("tea.csv")
tea
var(tea$weight)
13*22.41917/20
pchisq(13*22.41917/20, 13)
```

```{r}
set.seed(1234) # 设定随机数种子数以重现结果
x <- rnorm(50, 0, 2) # 生成服从N(0, 2)分布的50个随机数
set.seed(1234) # 设定随机数种子数以重现结果
y <- rnorm(30, 1, 1) # 生成服从N(1, 1)分布的30个随机数
var.test(x, y, conf.level=0.99)$conf.int # 计算方差比的99%置信区间
prop.test(c(512, 363), c(1200, 1023), conf.level=0.95)$conf.int
```
$\chi^2$ test
```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
x <- c(43, 49, 56, 45, 66, 41)
p <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
chisq.test(x, p=p)
x <- c(32, 104, 93, 30, 17)
p <- c(0.2, 0.35, 0.25, 0.12, 0.08)
chisq.test(x,p = p)
((53-65.65)^2)/65.65+((77-64.35)^2)/64.35+((29-22.725)^2)/22.725+((16-22.275)^2)/22.275+((19-12.625)^2)/12.625+((6-12.375)^2)/12.375
pchisq(14.92782, 2, lower.tail = F)
stock <- rbind(c(53, 77), c(29, 16), c(19, 6))
chisq.test(stock)
x <- rbind(c(15, 8, 21), c(1000, 850, 1800))
chisq.test(x)
# 主观幸福感数据
SHC <- read.csv("SHC.csv")
chisq.test(SHC)
library(vcd)
attach(Arthritis)
x <- table(Treatment, Improved)
assocstats(x)
detach(Arthritis)
```

Normality Test
```{r}
set.seed(1234)
x <- rnorm(10000, 10, 2)
shapiro.test(x)
ks.test(x, "pnorm", mean(x), sd(x))
library(fBasics)
shapiroTest(x)
sfTest(x)
ksnormTest(x)
jarqueberaTest(x)
dagoTest(x)
lillieTest(x)
pchiTest(x)
x <- c(1:40)
shapiro.test(x)
x <- rnorm(100, 10, 2)
y <- rchisq(100, 2)
pdf("pic-normalqq.pdf")
opar <- par(no.readonly = T)
par(mfrow=c(1, 2))
qqnorm(x, pch=20)
qqnorm(y, pch=20)
dev.off()
par(opar)
```

income: nonparametric quantile test
```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
x <- read.csv("income.csv")
wilcox.test(x$income, mu=50000, alternative = "greater")
shapiro.test(x$income)
pdf("pic-incomeboxplot.pdf")
boxplot(x$income, horizontal = T, xlab="income")
dev.off()
ps <- sum((x-50000)>0)
ns <- sum((x-50000)<0)
n <- ps+ns
binom.test(ps,n, 0.5, alternative = "greater")
mean(x)
sd(x)
shapiro.test(x)
stripchart(x, method = "stack", pch=20)
boxplot(x, horizontal=T)
quantile(x$income, probs=c(0.25, 0.50, 0.75))
x <- read.csv("income.csv")
ps <- sum((x-35000)>0)
ns <- sum((x-35000)<0)
n <- ps+ns
binom.test(ps,n, 0.75, alternative = "greater", conf.level = 0.95)
```


```{r}
x <- round(rchisq(10, 25))
y <- read.csv("rankdata.csv")
y <- sort(y)
y1 <- y-21
y2 <- abs(y1)
y2
total <- rbind(y, y1, y2)
total
y3 <- sort(y2)
y3
```
depression
```{r}
depression <- read.csv("depression.csv")
depression
depression <- within(depression, {diff <- before-after})
depression
wilcox.test(depression$before, depression$after, paired = T, alternative = "greater", conf.level = 0.95)
attach(depression)
pdf("pic-depression.pdf")
boxplot(before, after, diff, horizontal = T, xlab = "scores",    names = c("before", "after", "before-after"))
dev.off()
attach(depression)
depr <- rbind(before, after)
library(xtable)
print(xtable(depression))
```

```{r}
x <- c(32, 104, 93, 30, 17)
p <- c(0.2, 0.35, 0.25, 0.12, 0.08)
cor(x, p)
cor.test(x, p)
```

msce
```{r}
na <- c(8.5, 9.48, 8.65, 8.16, 8.83, 7.76,  8.63)
ca <- c(8.27, 8.20, 8.25, 8.14, 9.00, 8.10, 7.20, 8.32, 7.70)
boxplot(na, ca, horizontal = T, names = c("Native American", "Caucasian"))
stripchart(na, method = "stack",pch=19, at=0)
stripchart(ca, method = "stack", add = T, pch=19, at=1)
library(BHH2)
data(tab03B1)
attach(tab03B1)
stem(yield) #stem-leaf plot
plt <- dotPlot(yield) # equivalent dotPlot
plt
detach(tab03B1)
dotPlot(na)
control<- c(34, 31, 35, 29, 28, 12, 18, 30, 14, 22, 10, 29)
treated <- c(11, 15, 9, 4, 34, 17, 18, 14, 12, 13, 26, 31)
hay.fever <- data.frame(control, treated)
hay.fever
write.csv(hay.fever, "hay.fever.csv", row.names = F)
hay.fever <- read.csv("hay.fever")
hay.fever
wilcox.test(hay.fever$control, hay.fever$treated, alternative = "greater", conf.level = 0.95)
fever <- rbind(control, treated)
fever
library(xtable)
print(xtable(hay.fever))
boxplot(treated, control, horizontal = T, names = c("treated", "control"))
mixed <- c(34, 31, 35, 29, 28, 12, 18, 30, 14, 22, 10, 29, 11, 15, 9, 4, 34, 17, 18, 14, 12, 13, 26, 31)
mixed <- sort(mixed)
mixed
```

women
```{r}
attach(mtcars)
model <- lm(mpg~wt)
summary(model)
library(xtable)
print(xtable(summary(model)))
detach(mtcars)
```
chicken
```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
chicken <- read.csv("chicken.csv")
chicken
attach(chicken)
fit <- aov(weight~feeds)
summary(fit)
detach(chicken)
chicken2 <- read.csv("chicken2.csv")
chicken2
model2 <- aov(weight~feeds, data = chichen2)
summary(model2)
```

age rating and video games
```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
x <- read.csv("agerating.csv")
rating <- read.csv("agerating.csv")
library(tidyr)
rating2 <- gather(rating, key = age.label, value = rating, factor_key = TRUE)
rating2
kruskal.test(rating2$rating~rating2$age.label)
library(PMCMR)
posthoc.kruskal.dunn.test(rating ~ age.label, data = rating2, p.adjust="bonf")
lapply(x, shapiro.test)
shapiro.test(x$over7)
shapiro.test(x$over12)
shapiro.test(x$over16)
shapiro.test(x$over18)
library(reshape2)
y <- melt(x, measure.vars=c("over7", "over12", "over16", "over18"), variable.name = "age.label", value.name= "rating")
y
bartlett.test(y$rating~y$age.label)
model <- aov(y$rating~y$age.label)
summary(model)
library(gplots)
pdf("pic-agerating.pdf")
plotmeans(y$rating~y$age.label, xlab="Age Label", ylab="Rating",main = "Mean Plot  with 95% CI" , col="red", lwd = 2, barwidth = 2)
dev.off()
pairwise.t.test(y$rating, y$age.label, pool.sd = TRUE, p.adjust.method = "none")
TukeyHSD(model)
pdf("pic-tukey.pdf")
par(las=2)
par(mar=c(5, 7, 5, 2))
plot(TukeyHSD(model))
dev.off()
library(DTK)
DTK.test(y$rating, y$age.label)
pdf("pic-DTK.pdf")
par(mar=c(5, 10, 5, 1), las = 1)
DTK.plot(DTK.test(y$rating, y$age.label))
dev.off()
library(xtable)
xtable(TukeyHSD(model))
xtable(summary(model))
wide <- read.csv("widedata.csv")
wide
long <- melt(wide, id.vars = "person", measure.vars = c("grade", "score"))
long
memory <- read.csv("memory.csv")
memory
is.factor(memory$Process)
f <- list(memory$Process, memory$Age)
m <- split(memory, c(memory$Process, memory$Age))
m1 <- data.frame(m$Adjective$Words, m$Counting$Words)
m1
shapiro.test(m$Adjective$Words)
shapiro.test(m$Counting$Words)
shapiro.test(m$Imagery$Words)
shapiro.test(m$Intentional$Words)
shapiro.test(m$Rhyming$Words)
x <- rbind(memory[1:2,], memory[11:12, ], memory[21:22,], memory[31:32,], me)
x <- memory[c(1, 2, 11, 12, 21, 22, 31, 32, 41, 42, 51, 52, 61, 62, 71, 72, 81, 82, 91, 92), ]
x
library(xtable)
print(xtable(memory[1:3]))
y <- split(memory[,3], list(memory[,1],memory[,2]))
boxplot(y, las=2)
lapply(y, shapiro.test)
typeof(y)
library(dplyr)
memory <- mutate(memory, Group=rep(LETTERS[1:10], each=10))
memory
group <- c(rep(A, 10), rep("B", 10), rep("C", 10), rep("D", 10), rep("E", 10), rep("F", 10), rep("G", 10), rep("H", 10), rep("I", 10), rep("J", 10))
length(group)
new <- cbind(memory, group)
new2 <- new[c(1, 2, 11, 12, 21, 22, 31, 32, 41, 42, 51, 52, 61, 62, 71, 72, 81, 82, 91, 92), ]
xtable(new2)
a <- print(memory, row.names = F)
xtable(a, row.names=F)
print(xtable(a, row.names=F), include.rownames = F)
rep(LETTERS[1], 10)
boxplot(memory$Words~memory$Group)
pdf("pic-grouped-dotplot-of-memory.pdf")
stripchart(memory$Words~memory$Group, vertical = TRUE, method = "stack", offset = 0.8, pch = 16)
dev.off()
pdf("pic-grouped-dotplot-of-memory.pdf")
library(ggplot2)
ggplot(memory, aes(x=Group, y=Words))+geom_dotplot(binaxis = "y", binwidth = 0.5, stackdir = "center")
dev.off()
bartlett.test(memory$Words~memory$Group)
attach(memory)
fit <- aov(Words~Age+Process+Age: Process, data=memory)
TukeyHSD(fit)
summary(fit)
with(memory, {interaction.plot(Age, Process, Words, type = b)})
fit2 <- aov(Words~Process+Age+Age: Process)
summary(fit2)
xtable(summary(fit))
detach(memory)
head(memory)
library(gplots)
plotmeans(Words~interaction(Age, Process, sep=""))
library(HH)
pdf("pic-interaction-of-memory-byHH.pdf")
interaction2wt(Words~Age*Process)
dev.off()
opar<-par(no.readonly = T)
pdf("pic-interaction-of-memory.pdf")
par(mfrow=c(2,1))
with(memory, {interaction.plot(Process,Age, Words, type = "b", lwd=2, lty=c(1, 2), las=1)})
with(memory, {interaction.plot(Age, Process, Words, type = "b", lwd=2, lty=c(2,2,2,1, 2), las=1)})
dev.off()
par(opar)
```

2015 R test scores
```{r}
rs <- read.csv("rs2015.csv")
rsshort <- rs[c("id", "major", "mid", "end", "final")]
rsshort
library(reshape2)
rs2 <- melt(rsshort, id=c("major", "id"), variable.name = "type", value.name = "score")
rs2
rs3 <- rsshort[c("major", "final")]
rs3
library(ggplot2)
attach(rsshort)

ggplot(rs2, aes(major, score))+geom_boxplot(aes(colour = major), show.legend = F)+ coord_flip()

ggplot(rs2, aes(x = factor(type), fill= factor(type), y =score)) +
geom_dotplot(binaxis = "y", stackdir = "centerwhole")

ggplot(rsshort, aes(x = final, fill = factor(major))) +
geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all")

ggplot(rs2, aes(type, score))+geom_boxplot(aes(colour = type), fill=c("red", "green", "blue"))+ coord_flip()


ggplot(rs2, aes(x = factor(major), fill= factor(major), y =score)) +
geom_dotplot(binaxis = "y", stackdir = "centerwhole")

ggplot(rs3, aes(x = factor(major), fill= factor(major), y =final)) +
geom_dotplot(binaxis = "y", stackdir = "centerwhole")

library(psych)
describeBy(rs2$score, rs2$major)
setwd("C:/xkdog/courses/stats/textbooks")
stats <- read.csv("stats.score.csv")
stats
stats <- stats[c("id", "major", "mid", "end", "final", "gender")]
stats
stats2 <- subset(stats, final>=60)
stats2
library(ggplot2)
library(reshape2)
ggplot(stats, aes(major, final))+geom_boxplot(aes(colour = major), show.legend = F)+ coord_flip()
# 全部总评成绩的分专业点图
ggplot(stats, aes(x = final, fill = major)) +
geom_dotplot(stackgroups = TRUE, binwidth = 4, stackratio = 1.25, dotsize = 1, binpositions = "all")+geom_vline(xintercept = c(60, 90), lwd = 2, col = c("green", "pink"),  show.legend=T)+theme(legend.text=element_text(size = 26), legend.position=c(0.25,0.75))+guides(fill=guide_legend(title = NULL))+scale_fill_discrete(labels=c("Double Major", "PSY"))
# 及格同学的分专业点图
ggplot(stats2, aes(x = final, fill = factor(major))) +
geom_dotplot(stackgroups = TRUE, binwidth = 2, stackratio = 1.25, dotsize = 1, binpositions = "all")+geom_vline(xintercept = c(70, 80, 90), lwd = 2, col = c("green", "pink", "red"),  show.legend=T)
# 男女同学的点图
ggplot(stats, aes(x = final, fill = gender)) +
geom_dotplot(stackgroups = TRUE, binwidth = 3.5, stackratio = 1.25, dotsize = 1, binpositions = "all")+geom_vline(xintercept = c(60, 70, 80, 90), lwd = 2, col = c("grey", "green", "pink", "red"),  show.legend=T)+labs(fill="gender")+scale_fill_discrete(labels=c("Female", "Male"))+theme(legend.text=element_text(size = 26), legend.position=c(0.25,0.75))

library(reshape2)
stats3 <- melt(stats, id=c("major", "id"),  measure.vars = c("mid", "end", "final"), variable.name = "type", value.name = "score")

# 三个成绩的点图
ggplot(stats3, aes(x = type, fill = type, y=score)) +
geom_dotplot(binaxis = "y", stackdir = "centerwhole")+geom_hline(yintercept = 60, lwd = 2, col = "pink",  show.legend=T) + theme(legend.text=element_text(size = 26), legend.position=c(0.15,0.35))
hist(stats$final)
stripchart(stats$final, method = "stack", pch = 20, ylab="Final", at=1.5)
stripchart(stats$mid, method = "stack", pch = 20, at=1, add = T)
stripchart(stats$end, method = "stack", pch = 20, at=0.5, add = T)
abline(v=c(90, 85, 80, 60), col=c("red", "blue","green", "purple"), lwd=c(2,2,2,2))
stripchart(stats$final~stats$major, group.names=c("DO", "PSY"), pch=c(20, 18), at=c(1, 1.5), method="stack", col=c("blue", "pink"))
```



说明:

期中成绩占40%, 期末成绩60%. 期中, 期末及总评成绩的点图如下:

分专业的成绩统计如下:

DO="Double Major"(双辅修), PSY="Psychology"(心理学), SW="Social Work"(社工), SOC="Sociology"(社会学)

Mean
DO=84.28, PSY=84.15, SOC=80.73, SW=82.64

Median
DO=86.5, PSY=86, SOC=85, SW=85.75

图示如下:

```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
a <- read.csv("cgss2013a.csv")
b <- select(a, id=id, gender=a2, birth=a3a, religion=a501, salary=a8a, party=a10, height=a13, weight=a14, health=a15, trust=a33, fair=a35, rating.ph=c2g, trust2=b6, sexpre=a38, sexextra=a39, sexhomo=a40)
write.csv(b, "cgss2013short.csv", row.names = F)
d <- read.csv("cgss2013short.csv")
d0 <- filter(d, salary<9999996)
means <- aggregate(d0$salary, by=list(d0$gender, d0$party), FUN=mean, na.rm=T)
means
head(d)
library(dplyr)
d0 <- filter(d0, d0>=0)
nrow(d0)
means <- aggregate(d$sexhomo, by=list(d$party), FUN=mean, na.rm=T)
means
nr <- nrow(d)
res <- numeric()
for(i in 1:10000)
{
    d1 <- d[sample(nr,100,replace=F),]
    res[i] <-t.test(trust~gender,data = d1)$p.value
}

res
hist(res)
set.seed(1234)
d1 <- d[sample(nr,100,replace=F),]
d1
t.test(trust~gender, data=d1)$p.value
means <- aggregate(d$trust, by=list(d$party), FUN=mean)
means
barplot(means$x,d$gender)
means <- sapply()
head(d)
```

```{r}
x <- read.csv("cgss2013short.csv")
head(x$salary, 100)
x <- data.frame(a = c(1, 2, 3), b = c(-1, 3, -5))
x[x<0]<-NA
x[x<0 | x>9999996]<-NA
head(x$salary, 100)
```

```{r}
library(car)
model <- lm(prestige~education+income+women, data=Prestige)
summary(model)
```


data <- data.frame(a = c(1, 2, 3), b = c(-1, 3, -5))
data[data < 0] <- NA

```{r}
mtcars
library(dummies)
dummy(mtcars$cyl)
x <- dummy.data.frame(mtcars, mtcars$cyl, dummy.classes = "numeric")
x
x <- cbind(mtcars, dummy(mtcars$cyl))
fit <- lm(mpg~wt+am+as.factor(cyl), data=mtcars)
summary(fit)
library(xtable)
xtable(summary(fit))
dummy(mtcars$cyl)
dummy.data.frame(iris)
library(psych)
x<-dummy.code(mtcars$cyl)
x2<-data.frame(mtcars, x)
x2
```

```{r}
library(ISwR)
?cystfibr # 囊状纤维化
# 最大呼气压(pemax)

```

```{r}
rs <- read.csv("rscore2015.csv")
tl <- with(rs, cut(total,  c(60, 70, 80, 90, 100), labels = c("60~69", "70~79", "80~89", "90~99")), right = F)
table(tl)
```
paper judgement 论文价值评估，方差分析

```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
paper <- read.csv("paper.csv")
paper
x <- split(paper[,1], list(paper[,2], paper[,3]))
lapply(x, shapiro.test) # 正态性检验
z <- melt(x)
head(z)
bartlett.test(z$value~z$L1) # 方差齐性检验

rbind.fill(c(x[1], x[2], x[3]))

x1 <- as.data.frame(x[1])
x2 <- as.data.frame(x[2])
x3 <- as.data.frame(x[3])
x4 <- as.data.frame(x[4])
y <- rbind.fill(x1, x2, x3, x4)

qqnorm(y$X0.0)


library(reshape2)
paper
melt(paper, value.name="scores", id.vars="scores", variable.name = "group")
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
lapply(y, qqnorm) # 正态性检验
par(opar)
bartlett.test(scores ~ group, data=paper)
fit <- aov(scores ~ major, data=paper)
summary(fit)
one <- mtcars[1:10, ]
two <- mtcars[11:32, ]
one
two
rbind_list(one, two)
rbind_all(list(one, two))
```

word memory
```{r}
onehour <- c(14,12,18,7,11,9,16,15)
twohour <- c(10,6,14,6,9,6,12,12)
library(perm)
permTS(onehour, twohour, alternative = "greater", method = "exact.ce")
library(exactRankTests)
perm.test(onehour, twohour, alternative = "greater", paired = TRUE)
library(effsize)
cohen.d(onehour, twohour, paired = TRUE)
d <- onehour-twohour
setwd("C:/xkdog/courses/stats/textbooks/mytext")
pdf("pic-wordmemory.pdf")
boxplot(d, horizontal = T, xlab = "Difference between 1h and 2h")
dev.off
t.test(onehour, twohour, paired = TRUE, alternative = "greater", conf.level=0.98)
```

#####################################################
# Simulation of the Monty Hall Problem
# Demonstrates that switching is always better
# than staying with your initial guess
#
# Corey Chivers, 2012
#####################################################


```{r}
rm(list=ls())
monty<-function(strat='stay',N=1000,print_games=TRUE)
{
    doors<-1:3 #initialize the doors behind one of which is a good prize
    win<-0 #to keep track of number of wins
    
    for(i in 1:N)
    {
        prize<-floor(runif(1,1,4)) #randomize which door has the good prize
        guess<-floor(runif(1,1,4)) #guess a door at random
        
        ## Reveal one of the doors you didn't pick which has a bum prize
        if(prize!=guess)
        reveal<-doors[-c(prize,guess)]
        else
        reveal<-sample(doors[-c(prize,guess)],1)
        
        ## Stay with your initial guess or switch
        if(strat=='switch')
        select<-doors[-c(reveal,guess)]
        if(strat=='stay')
        select<-guess
        if(strat=='random')
        select<-sample(doors[-reveal],1)
        
        ## Count up your wins
        if(select==prize)
        {
            win<-win+1
            outcome<-'Winner!'
        }else
        outcome<-'Losser!'
        
        if(print_games)
        cat(paste('Guess: ',guess,
        '\nRevealed: ',reveal,
        '\nSelection: ',select,
        '\nPrize door: ',prize,
        '\n',outcome,'\n\n',sep=''))
    }
    cat(paste('Using the ',strat,' strategy, your win percentage was ',win/N*100,'%\n',sep='')) #Print the win percentage of your strategy
}
monty(strat="stay")
monty(strat="switch")
monty(strat="random")
```

TV time
```{r}
group1999 <- c(4, 5, 7, 7, 5, 7, 5, 6, 5, 6, 7, 8, 5, 6, 6)
group2009 <- c(5, 9, 5, 8, 7, 6, 7, 9, 7, 9, 6, 9, 10, 9, 8)
cohen.d(group1999, group2009, hedges.correction = T)
cohen.d(group1999, group2009)
TVtime <- data.frame(group1999, group2009)
write.csv(TVtime, "mediatime.csv", row.names = F)
mediatime <- read.csv("mediatime.csv")
mediatime
pdf("pic-mediatime.pdf")
boxplot(mediatime, names = c("1999", "2009"), ylab = "Watching Time per Week (hours) ")
dev.off()
t.test(group1999, group2009, alternative = "less")
library(perm)
permTS(group1999, group2009, alternative = "less")
```
why do we not check the nomality separatly in a paired t test?

```{r}
a <- c(14, 12, 18, 15, 11, 9, 16, 18)
b <- c(2, 6, 3, 2, 9, 9, 5, 6)
boxplot(a, b, a-b)
```

```{r}
x <- c(57,  11,  330,  6)
p <- c(0.177,  0.032,  0.734,  0.057)
p1 <- x/sum(x)
library(pwr)
ES.w1(p, p1)

movie <- rbind(c(255, 106, 130),  c( 85, 12, 48))
P <- movie/sum(movie)
ES.w2(P)
```


milk
```{r}
T0 <- c(11.4, 9.6, 10.1, 8.5, 10.3, 10.6, 11.8, 9.8, 10.9, 10.3, 10.2, 11.4, 9.2, 10.6, 10.8, 8.2)
C0 <- c(9.1, 8.7, 9.7, 10.8, 10.9, 10.6, 10.1, 12.3, 8.8, 10.4, 10.9, 10.4, 11.6, 10.9)
T9 <- c(15.3, 14.0, 11.4, 14.1, 15.0, 11.8, 12.3, 14.1, 17.6, 14.3, 14.0, 15.3, 12.0, 15.8, 13.5, 10.2)
C9 <- c(9.3, 8.8, 8.8, 10.1, 9.6, 8.6, 10.4, 12.4, 9.3, 9.5, 8.4, 8.7, 12.5, 9.1)
id.T <- c(1:length(T0))
id.C <- c(1:length(C0))
treated <- data.frame(id = id.T, T0, T9)
control <- data.frame(id = id.C, C0, C9)
library(dplyr)
left_join(treated, control, by="id")
full_join(treated, control, by="id")
milk <- merge(treated, control, by = "id", all.x = TRUE)
milk
library(xtable)
library(dplyr)
xtable(milk, digits = 1) %>%  print.xtable(include.rownames= F, booktabs = T, NA.string = "NA")
D1 <- T9-T0
D1
D2 <- C9-C0
D2
t.test(T9-T0, C9-C0, alternative = "greater", conf.level = 0.99)
# milk <- data.frame(T0, C0, T9, C9)
pdf("pic-milk.pdf")
boxplot(T9-T0, C9-C0, names = c("Treated", "Control"), ylab = "Paired Differences (after - before) (mg/L)")
dev.off()
T1 <- data.frame(id=c(1:16), T0, T9)
C1 <- data.frame(id=c(1:14), C0, C9)
library(dplyr)
left_join(T1, C1, by="id")
full_join(T1, C1, by="id")
library(reshape2)
```


```{r}
library(dplyr)
library(tidyr)
setwd("C:/xkdog/courses/stats/textbooks/data")
mtcars
names(mtcars)
cars <- mtcars%>%gather(variable, value, mpg:am)
head(cars)
cars
widedata <- read.csv("widedata.csv")
widedata
longdata1 <- gather(widedata, variable, value)
longdata2 <- gather(widedata, variable, value, -person)
library(xtable)
print(xtable(longdata2), row.names=F)
age <- c(20, 21, 22)
widedata1 <- data.frame(widedata, age)
widedata1
longdata1 <- widedata1%>%gather(variable, value, grade:age)
longdata1
long <- gather(widedata1, variable, value, grade:age)
long
xtable(long)
spread(long, variable, value)
```

```{r}
person <- c("Alex", "Bob", "Cathy")
grade <- c(2, 3, 4)
score <- c(78, 89, 90)
age <- c(20, 21, 22)
wide <- data.frame(person, grade, score, age)
wide # 宽数据格式, 每个变量占一列, 每行一个观测
library("tidyr")
library("dplyr")
long1 <- gather(wide, variable, value)
long1 # 长数据格式, 只有两列, 一列为变量名, 一列为值
# 通常并不需要把整个数据框转化为长格式, 而只对某些指定变量转为长格式
long2 <- gather(wide, variable, value, -person)
long2 # 比较与long1的区别
```
长宽数据转换用在哪些地方? 现在可能有些同学会感到迷惑. 实际上, 在很多后续统计方法的软件操作中, 就会时常要求长宽格式之间的转换.
```{r}
A <- rnorm(10, 2, 1)
B <- rnorm(10, 2.5, 1.2)
C <- rnorm(10, 2.8, 0.9)
D <- rnorm(10, 3, 1.1)
test <- round(data.frame(A, B, C, D), digits = 2)
library(xtable)
print.xtable(xtable(test, digits = 0), include.rownames = F, booktabs = T)
# 注意这里未设定随机数种子数, 故每个人结果可能会有不同.
test
summary(test) # 基本描述统计
library(psych)
describe(test) # 使用psych包来做描述统计
# 利用apply族函数来分组统计和检验
apply(test, 2, mean) # 求各列均值, 1表示行, 2表示列
apply(test, 2, sd) # 求各组标准差
lapply(test, shapiro.test) # 对test中每个向量做正态shapiro检验
# 问题: 如何对A, B, C, D四个变量是否同方差做bartlett显著性检验(命令: bartlett.test)?
# 此时就可能需要用到gather函数来做数据类型的转换.
library(dplyr)
library(tidyr)
test1 <- gather(test, group, value)
test1
bartlett.test(value~group, data = test1) # 请观察结果
# 完成同方差和正态性检验并未拒绝各自的原假设后, 方可进行方差分析的工作
aov(value ~ group, data = test1)
```


```{r}
one <- mtcars[1:10, ]
two <- mtcars[11:32, ]
one
two
onerbind_list(one, two)
rbind_all(list(one, two))


```
metal.csv
```{r}
metal <- c(9.25, 9.47, 9.63, 9.35, 9.44, 9.41, 9.35, 9.65,9.34, 9.23, 9.72, 9.24, 9.70, 9.29, 9.78, 9.34, 9.36, 9.71, 9.20, 9.25)
write.csv(metal, "metal.csv", row.names = F)
metal <- read.csv("metal.csv")
s2 <- var(metal)  # 计算样本方差
chi2 <- s2 * (20-1) / 0.0225
pchisq(chi2, 19, lower.tail = F)
```

gender and attitude towards sex behavior

```{r}
library(readstata13)
cgss2013a <- read.dta13("cgss2013a.dta", convert.factors = F)
cgss2013a[cgss2013a<0] <- NA # 将所有负值编码处理为缺失值
library(dplyr)
extra.marital <- select(cgss2013a, gender = a2, attitude = a39)
attitude <- table(extra.marital)
table(extra.marital) %>% xtable() %>% print.xtable( booktabs = T)
library(xtable)
addmargins(table(extra.marital)) %>% xtable(digits = 0) %>% print.xtable( booktabs = T)
t.test(extra.marital$attitude~extra.marital$gender)
t.test(extra.marital$attitude~extra.marital$gender, alternative = "greater")
```

merge function
```{r, merge function}
id <- c(1:4)
x <- letters[1:4]
A <- data.frame(id, x)
A
y <- LETTERS[1:6]
y
```

join in dplyr
```{r, join in dplyr}
library(dplyr)
T0 <- c(11.4, 9.6, 10.1, 8.5, 10.3, 10.6, 11.8, 9.8, 10.9, 10.3, 10.2, 11.4, 9.2, 10.6, 10.8, 8.2)
C0 <- c(9.1, 8.7, 9.7, 10.8, 10.9, 10.6, 10.1, 12.3, 8.8, 10.4, 10.9, 10.4, 11.6, 10.9)
T9 <- c(15.3, 14.0, 11.4, 14.1, 15.0, 11.8, 12.3, 14.1, 17.6, 14.3, 14.0, 15.3, 12.0, 15.8, 13.5, 10.2)
C9 <- c(9.3, 8.8, 8.8, 10.1, 9.6, 8.6, 10.4, 12.4, 9.3, 9.5, 8.4, 8.7, 12.5, 9.1)
T<-data.frame(id=c(1:16), T0, T9)
C<-data.frame(id=c(1:14), C0, C9)
left_join(T, C, by="id")
right_join(T, C, by= "id")
full_join(T, C, by="id")
A <- data.frame(id = c(1:4), A = letters[1:4])
A
B <- data.frame(id = c(1:6), B = LETTERS[1:6])
B
C <- merge(A, B, by = "id", all.y = TRUE)
C <- merge(B, A, by = "id", all.x = TRUE)
C
```

```{r}
png("roc.png")
opar<- par(no.readonly=TRUE)
par(pin=c(4,4), mai=c(1,1,1,1))
n = c(0.2, 0.4, 0.7)
sn = c(0.6, 0.7, 0.8)
plot(n, sn, xlab="P(y|N)",
ylab="P(y|SN)",
main="ROC",type = "b", xlim=c(0,1), ylim=c(0,1),
xaxs = "i", yaxs = "i",lwd=2)
lines(c(0:1),c(0:1),lwd=2)
par(opar)
dev.off()
```
rice
```{r}
A <- c(83, 98, 120, 84, 86, 82, 108, 85, 105, 93)
B <- c(90, 105, 101, 95, 102, 100, 101, 105, 93, 97)
rice <- data.frame(A, B)
write.csv(rice, "rice.csv", row.names = F)
```

alcohol
```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
alcohol <- c(4.12, 5.81, 7.63, 9.74, 10.39, 11.92, 12.32, 12.89, 13.54, 14.45)
median(alcohol)
write.csv(alcohol, "alcohol.csv", row.names = F)
wilcox.test(alcohol, mu = 10, alternative = "greater")
```
twins
```{r}
school.A <- c(82, 69, 73, 43, 58, 56, 76, 65)
school.B <- c(63, 42, 74, 37, 51, 43, 80, 62)
twins <- data.frame(school.A, school.B)
write.csv(twins, "twins.csv", row.names = F)
twins
wilcox.test(twins$school.A, twins$school.B, paired = TRUE, alternative = "greater")
```


traveling
```{r}
traveling <- read.csv("traveling.csv")
traveling
fit <- lm(expenditure~income+edu, data = traveling)
fit
```


data.table包
```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
library(xtable)
library(dplyr)
library(data.table)
flight <- fread("flights14.csv")
fshort <- flight[ , 1:8, with = F]
fshort
# xtable(fshort) %>% print.xtable(booktabs = T, include.rownames = F)
```

regression, cgss2013a
```{r}
library(dplyr)
cgss2013 <- read.csv("cgss2013a.csv")
cgss2013[cgss2013<0]<- NA
cgss2013n <- select(cgss2013, a2, edu =  a7a, salary = a8a)
cgss2013n <- mutate(cgss2013n, gender = a2-1)
cgss2013n <- filter(cgss2013n, salary < 9999996 & edu < 14)
cgss2013n <- within(cgss2013n,
{educ <- cut(edu, breaks = c(0, 4, 9, 12, 14), right = TRUE, labels = c("junior", "senior",  "undergraduate", "graduate"))
})
table(cgss2013n$educ)
library(xtable)
aggregate(salary~ educ, data = cgss2013n, mean)
aggregate(salary~gender, data = cgss2013n, mean)
model <- lm(salary ~ gender + as.factor(educ), data = cgss2013n)
summary(model)
xtable(summary(model))
model2 <- lm(salary~gender+as.factor(educ)+gender:educ, data = cgss2013n)
summary(model2)
```

CET6
```{r}
CET6 <- read.csv("CET6.csv")
library(car)
pdf("pic-scatterplot-CET6.pdf")
scatterplot(Score~Vocabulary | Gender, smoother = F, data = CET6, pch = c(16, 21), lwd = 2, grid = F, col = c("black", "grey"), legend.coords = "top")
dev.off()
CET.F <- subset(CET6, Gender=="F")
CET.M <- subset(CET6, Gender=="M")
model.F <- lm(Score~Vocabulary, data = CET.F)
model.M <- lm(Score~Vocabulary, data = CET.M)
summary(model.M)
summary(model.F)
pdf("pic-scatterplot-CET6.pdf")
plot(CETM$Vocabulary, CETM$Score, pch = 21, col = "black", xlab = "Vocabulary", ylab = "CET6 Score", xlim = c(2000, 14000), ylim = c(400, 700))
abline(lm(Score~Vocabulary, data = CETM), lty = 1, col = "black", lwd =2)
lines(CETF$Vocabulary, CETF$Score, pch = 16, col = "blue", type = "p")
abline(lm(Score~Vocabulary, data = CETF), lty = 2, col = "blue", lwd = 2)
legend(x = "topleft", legend = c("Male", "Female"), col = c("black", "blue"), lty = c(1, 2))
dev.off()
```

scatterplot using ggplot2
```{r}
library(ggplot2)
set.seed(955)
# Make some noisily increasing data
dat <- data.frame(cond = rep(c("A", "B"), each=10),
xvar = 1:20 + rnorm(20,sd=3),
yvar = 1:20 + rnorm(20,sd=3))
ggplot(CET6, aes(x=Vocabulary, y=Score, color=Gender))+                         geom_point(shape=0) +
scale_colour_hue(l=50) +                               geom_smooth(method=lm,se=FALSE, fullrange=TRUE)
```

```{r}
CET6 <- read.csv("CET6.csv")
CETM <- subset(CET6, Gender=="M")
CETF <- subset(CET6, Gender=="F")
CETM
attach(CETM)
plot(Vocabulary, Score, pch = 21, col = "blue", xlim = c(2000, 14000), ylim = c(400, 700))
abline(lm(Score~Vocabulary), lty = 2)
detach(CETM)
attach(CETF)
lines(Vocabulary, Score, pch = 16, col = "red", type = "p")
abline(lm(Score~Vocabulary), lty = 1)
detach(CETF)
```

CFPS
```{r}
library(readstata13)
CFPS2012 <- read.dta13("cfps2012adult.dta")
write.csv(CFPS2012, "CFPS2012.csv")
library(data.table)
```


sphericity assumption
```{r}
sphericity <- read.csv("sphericity.csv")
library(xtable)
library(dplyr)
sphericity.new <- mutate(sphericity, x2-x1, x3-x2, x3-x1)
sphericity.new
xtable(sphericity)%>%print.xtable(booktabs = T, include.rownames = F)
apply(sphericity.new, 2, sd)
boxplot(sphericity)
stripchart(sphericity.new, method = "stack", pch = 16, vertical = TRUE)

fit <- lm(as.matrix(sphericity) ~ 1)
fit
mauchly.test(fit, X = ~ 1)
mauchly.test(estVar(fit), X = ~ 1)
estVar(fit)

utils::example(SSD) # Brings in the mlmfit and reacttime objects
estVar(SSD)
### traditional test of intrasubj. contrasts
mauchly.test(mlmfit, X = ~1)
```


depression after earthquake
```{r}
quake <- read.csv("depression.aov.csv")
mlm <- lm(as.matrix(quake[, 2:6])~1) # quake数据中的第1列为ID, 不进入回归模型
mauchly.test(mlm, X = ~ 1)
library(xtable)
library(dplyr)
library(tidyr)
quake
quake2 <- quake[, 2:6]
quake2
means <- aggregate(depression~week, FUN = mean, data = quake.long)
x <- apply(quake2, 2, mean)
typeof(x)
week <- c(0, 3, 6, 9, 12)
depression <- c(8.68, 10.12, 11.36, 10.84, 11.24)
pdf("pic-depression-earthquake.pdf")
plot(week, depression, type = "b", pch = 20, lwd = 2, ylab = "depression score")
abline(v = 2, lty = 2, lwd = 2)
text(2.2, 9, "earthquake happened", pos = 4)
dev.off()
means
barplot(means$depression, names.arg = means$week)
plot(means$depression~means$week)
library(tidyr)
quake.long <- gather(quake, week, depression, -subject)
head(quake.long, 5) %>% xtable() %>% print.xtable(include.rownames = F)
aggregate()

fit <- aov(depression~week + Error(subject/week), data=quake.long)
summary(fit)
attach(quake.long)
pairwise.t.test(depression, week, p.adjust.method = "bonferroni", paired = TRUE)
lm(quake.long$depression~quake.long$week)

idata <- data.frame(deg = gl(3, 1, 6, labels = c(0,4,8)),noise = gl(2, 3, 6, labels = c("A","P")))
idata
mauchly.test(mlmfit, X = ~ deg + noise, idata = idata)
mauchly.test(mlmfit, M = ~ deg + noise, X = ~ noise, idata = idata)
utils::example(SSD)

reacttime <- matrix(c(
420, 420, 480, 480, 600, 780,
420, 480, 480, 360, 480, 600,
480, 480, 540, 660, 780, 780,
420, 540, 540, 480, 780, 900,
540, 660, 540, 480, 660, 720,
360, 420, 360, 360, 480, 540,
480, 480, 600, 540, 720, 840,
480, 600, 660, 540, 720, 900,
540, 600, 540, 480, 720, 780,
480, 420, 540, 540, 660, 780),
ncol = 6, byrow = TRUE,
dimnames = list(subj = 1:10,
cond = c("deg0NA", "deg4NA", "deg8NA",
"deg0NP", "deg4NP", "deg8NP")))
mlmfit <- lm(reacttime ~ 1)
mlmfit
quake2 <- as.matrix(quake[, 2:6])
quake2
mlmfit <- lm(quake2~1)
mauchly.test(mlmfit, X = ~1)
SSD(mlmfit)
estVar(mlmfit)
```


reading
```{r}
reading <- read.csv("reading.csv")
head(reading)
x <- split(reading[,2], list(reading[,3],reading[,4]))
x <- split(reading$score, list(reading$A, reading$B))
data.frame(x)
as.matrix(data.frame(x))
reading
```

anchoring

```{r}
setwd("C:/xkdog/courses/stats/textbooks/data")
library(Hmisc)
library(dplyr)
anchoring <- spss.get("anchoring.sav", use.value.labels =T)
anchoring <- na.omit(anchoring)
anchoring
study1 <- split(anchoring[,4], list(anchoring[,2], anchoring[,3]))
lapply(study1, shapiro.test)
pdf("pic-dotplot-of-anchoring.pdf")
# stripchart(study1, vertical = TRUE, method = "stack", pch = 20)
stripchart(study1, vertical = TRUE, method = "stack", pch = 20, cex = 0.8, offset = 0.66, group.names = c( "SN","ON", "SL", "OL", "SH", "OH"))
dev.off()
opar<-par(no.readonly = T)
pdf("pic-qqnorm-of-anchoring.pdf")
par(mfrow=c(2,3))
lapply(study1, qqnorm)
par(opar)
dev.off()
fit <- aov(MazeEstimate~MazeCondition*MazeAnchorCondition, data = anchoring)
fit2 <- aov(MazeEstimate~MazeCondition + MazeAnchorCondition + MazeCondition:MazeAnchorCondition, data = anchoring)
fit3 <- aov(MazeEstimate~ MazeAnchorCondition + MazeCondition+ MazeCondition:MazeAnchorCondition, data = anchoring)
summary(fit)
summary(fit2)
summary(fit3)
library(car)
pdf("pic-qqplot-anchoring.pdf")
par(mfrow=c(2,3))
lapply(study1, qqPlot)
dev.off()
par(opar)
friedman.test(MazeEstimate ~ MazeAnchorCondition | MazeCondition , data = anchoring)
pdf("pic-interactionplot-anchoring.pdf")
interaction2wt(MazeEstimate~MazeCondition*MazeAnchorCondition, data = anchoring)
dev.off()
library(dplyr)
fit2 <- aov(MazeEstimate~MazeAnchorCondition, data = anchoring)
summary(fit2)
library(lmPerm)
aov.perm <- aovp(MazeEstimate~ MazeCondition +  MazeAnchorCondition + MazeCondition:MazeAnchorCondition, seqs = TRUE, data = anchoring)
summary(aov.perm)
library(coin)
oneway_test(MazeEstimate~MazeCondition| MazeAnchorCondition, data = anchoring)
```

```{r}
library(data.table)
flight <- fread("flights14.csv")
fshort3 <- flight[, c("year", "month", "day" ,"origin", "dest"), with=FALSE]
ans <- fshort3[order(origin,-dest)]
ans
ans<-flight[, arr_delay]
head(ans)
ans <- flight[, year:day, with = FALSE]
ans <- flight[, day:year, with = FALSE]
ans <- flight[, -(year:day), with = FALSE]
ans
x = flight[carrier == "AA", lapply(.SD, mean), by = .(origin, dest, month), .SDcols = c("arr_delay", "dep_delay")]
```

pipe operator
```{r}
x1 <- rnorm(1000, 5, 1)
x2 <- sample(x1, 100, replace = FALSE)
x3 <- log(x2)
boxplot(x3)
library(dplyr)
rnorm(1000, 5, 1) %>% sample(100, replace = FALSE) %>% log %>% boxplot(col = "blue")
```


```{r}
library(coin)
oneway_test()
library(lmPerm)
rating <- read.csv("agerating.csv")
rating
library(tidyr)
rating <- gather(rating, age.label, rating)
rating
fit <- aovp(rating~age.label, data = rating, seqs = TRUE)
summary(fit)
xtable(fit)
memory <- read.csv("memory.csv")
memory
fit <- aovp(Words~Age+Process+Age:Process, data = memory, seqs = TRUE)
summary(fit)
```

multcomp
```{r}
library(multcomp)
attch(cholesterol)
cholesterol
bartlett.test(response~trt, data = cholesterol)
fit <- aov(response~trt, data = cholesterol)
summary(fit)
x <- TukeyHSD(fit)
xtable(x[[1]], digits = 3)
pdf("pic-HSD-Cholesterol.pdf")
par(mar=c(5, 7, 5, 2))
plot(TukeyHSD(fit), las = 1)
dev.off()
library(gplots)
plotmeans(response~trt,data = cholesterol, bars = TRUE)
```

randomization test
```{r}
set.seed(1)
A <- round(rnorm(7, 20, 5))
set.seed(2)
B <- round(rnorm(7, 17, 5))
set.seed(3)
C <- round(rnorm(7, 15, 5))
set.seed(4)
D <- round(rnorm(7, 13, 5))
library(coin)
oneway_test(A, B)
dataAB <- data.frame(A, B)
dataABCD <- data.frame(A, B, C, D)
block <- as.factor(c(1:7))
blockABCD <- data.frame(dataABCD, block)
blockABCD
ABCD
library(tidyr)
ABCD <- gather(dataABCD, group, value, factor_key = TRUE)
newABCD <- gather(blockABCD, group, value, -block, factor_key = TRUE)
newABCD
fit1 <- aov(value~block+group, data = newABCD)
summary(fit1)
oneway_test(value~group, data = ABCD, distribution = approximate(B=100000))
oneway_test(value~group | block, data = newABCD, distribution = approximate(B=100000))
library(lmPerm)
fit1 <- aovp(value~group, data = data1, perm = "Prob")
summary(fit1)
set.seed(1)
X <- round(rnorm(7, 20, 5))
set.seed(2)
Y <- round(rnorm(7, 15, 5))
permutationTest2(data = X, statistic = mean, data2 = Y, alternative = "greater")
permutationTest2(data = X, statistic = mean, data2 = Y, alternative = "greater", paired = TRUE)

t.test(X, Y, paired = TRUE, alternative = "greater")
dataXY <- data.frame(X, Y)
dataXY <- gather(dataXY, group, value, factor_key = TRUE)
oneway_test(value~group, data = dataXY, alternative = "greater", exact= TRUE)
library(exactRankTests)
perm.test(X, Y, alternative = "greater", paired = TRUE)
t.test(X, Y, paired  = TRUE, alternative = "greater")
library(BHH2)
permtest(X, Y)
t.test(X, Y, var.equal = F)
X <- c(17, 21, 16, 28, 22, 16, 22)
Y <- c(11, 16, 23, 9, 15, 16, 19)
t.test(X, Y, var.equal = TRUE, alternative = "greater")
mean(X)
var(X)
var(Y)
```

memory2
```{r}
memory2 <- read.csv("memory2.csv")
stripchart(memory2[, 2:5], method = "stack", pch = 20, vertical = T)
lapply(memory2[, 2:5], shapiro.test)
library(tidyr)
memory2 <- gather(memory2, grade, score, -id)
memory2
bartlett.test(score~grade, data = memory2)
library(car)
pdf("pic-qqPlot-memory2.pdf")
par(mfrow=c(2, 2))
lapply(memory2[, 2:5], qqPlot)
dev.off()
library(lmPerm)
library(coin)
oneway_test(score~as.factor(grade), data = memory2)
fit <- aovp(score~grade, seqs = TRUE, data = memory2)
summary(fit)
```

```{r}
library(multcomp)
data(package="multcomp")
library(exactRankTests)
perm.test(X, Y, alternative = "greater", paired = TRUE)
oneway_test(cooperation ~ condition, data = Guyer)
aggregate(Guyer$cooperation, by = list(Guyer$condition), mean)
```

hospital
```{r}
east <- c(862, 1991, 1057,1006)
middle <- c(506, 1971,1025,	864)
west <- c(474,	1939,	670,	949)
type <- c("Third", "Second", "First", "undefined")
hospital <- matrix(east, middle, west)
hospital
cells <- c(862,	1991,	1057,	1006, 506, 	1971, 	1025, 	864, 474, 	1939, 	670, 	949)
rn <- c("east","middle" ,"west")
cn <- c("Third", "Second", "First", "undefined")
hospital <- matrix(cells, byrow= TRUE, nrow = 3, ncol = 4, dimnames = list(rn, cn))
hospital

```
psyhis2016 心理学史2016成绩
```{r}
setwd("C:/xkdog/courses/psyhistory")
library(readxl)
library(dplyr)
psyhis2016 <- read_excel("psyhis2016.xlsx")
psyhis2016
means1 <- aggregate(psyhis2016$total, by = list(psyhis2016$major), mean)
means
barplot(means1$x, names.arg = means1$Group.1, ylim = c(80, 85), col = c("blue", "pink"), xpd = FALSE, main = "分专业总评成绩")
abline(h = 80)
text(0.75, 82, "81.3")
text(1.85, 83.5, "82.8")
stripchart(total~major, vertical = T, method = "stack", pch = 20, offset = 2, col = c("blue", "pink"), at = c(1, 1.5))
barplot(table(psyhis2016$Q1), names.arg = c("没有用", "有用", "部分有用"))
barplot(table(psyhis2016$Q2), names.arg = c("没有用", "有用", "部分有用"))
```


20160624 随机化检验例子
```{r}
set.seed(1) # 设定随机数种子数
X <- round(rnorm(7, 20, 5)) # 产生如例中所示的7个服从N(20, 52)的随机数
set.seed(2) # 设定随机数种子数
Y <- round(rnorm(7, 15, 5)) # 产生如例中所示的7个服从N(15, 52)的随机数
f <- function(x){
    Sx <- sum(x)
    Sy <- sum(c(X, Y)) - Sx
    return (mean(x) - Sy/length(Y))
} # 定义计算均值差的函数
d <- combn(c(X, Y), length(X), f)
p <- mean(d>=(mean(X) - mean(Y)))
p
hist(stat)
pool <- c(X, Y)
pool
x <- combn(pool, 7, mean)
t.test(X, Y, alternative = "greater", paired = T)
```


ToothGrowth
```{r}
attach(ToothGrowth)
ToothGrowth
x <- split(len, list(supp, dose))
x
stack(x)
library(reshape2)
melt(x)
lapply(x, shapiro.test)
library(dplyr)
y <- mutate(ToothGrowth, Group=rep(LETTERS[1:6], each = 10))
bartlett.test(len~Group, data = y)
fit <- aov(len~supp+dose+supp:dose)
summary(fit)
library(reshape2)
melt(ToothGrowth, id.vars = c(supp, dose), measure.vars = len)
head(ToothGrowth)
fit <- aov(len ~ supp + dose + supp : dose, data = ToothGrowth)
summary(fit)
xtable(summary(fit))
is.factor(ToothGrowth$dose)
typeof(ToothGrowth$dose)

fit <- aov(len ~ supp*factor(dose), data = ToothGrowth)
summary(fit)

```

stack data
```{r}
library(readxl)
x <- read_excel("stack.xlsx")
melt(x)
x <- read.csv("rscore2015.csv")
library(dplyr)
x <- select(x, major, score = total)
y <- split(x$score, x$major)
unsplit(y$, major)
x <- read.csv("memory.csv")
x <- split(x$Words, list(x$Age, x$Process))
stack(x)
library(reshape2)
y <- melt(x, value.name = "AaA")
bartlett.test(y$AaA~y$L1)
```

2016西方心理学史成绩
```{r}
library(readxl)
score <- read_excel("psyhis2016.xlsx")
names(score)
library(dplyr)
library(tidyr)
attach(score)
mean.major <- aggregate(total, list(major), mean)
mean.gender <- aggregate(total, list(gender), mean)
opar <- par(no.readonly = T)
par(mfrow=c(1, 2))
barplot(mean.major$x, names.arg = mean.major$Group.1, main = "原始分专业条形均值图")
barplot(mean.major$x, names.arg = mean.major$Group.1, ylim = c(80, 84), xpd = FALSE, col = c("blue", "pink"), main = "修改后分专业条形均值图")
abline(h=80)
text(0.75, 81.5, "81.3", col = "blue")
text(2, 83, "82.8", col = "pink")

barplot(mean.gender$x, names.arg = mean.gedner$Group.1, main = "原始分性别条形均值图")
barplot(mean.gender$x, names.arg = mean.gender$Group.1, ylim = c(80, 85), xpd = FALSE, col = c("blue", "pink"), main = "修改后分性别条形均值图")
abline(h=80)
text(0.75, 83.8, "83.6", col = "blue")
text(2, 82.2, "82.0", col = "pink")

scores <- score[, c("mid", "final", "total")]
scores
stripchart(scores, col = c("blue", "green", "red"), method = "stack", vertical = T, pch = 20)
boxplot(scores, col = c("blue", "green", "red"), method = "stack", horizontal = F)


setwd("C:/xkdog/courses/stats/textbooks/data")
library(readxl)
score <- read_excel("psyhis2016.xlsx")
names(score)
library(dplyr)
library(tidyr)
attach(score)
mean.major <- aggregate(total, list(major), mean)
mean.gender <- aggregate(total, list(gender), mean)
opar <- par(no.readonly = T)

# 分专业均值条形图
png("meanbymajor-original.png")
barplot(mean.major$x, names.arg = mean.major$Group.1, main = "分专业条形均值图-原始图")
dev.off()
png("meanbymajor.png")
barplot(mean.major$x, names.arg = mean.major$Group.1, ylim = c(80, 84), xpd = FALSE, col = c("blue", "pink"), main = "修改后分专业条形均值图")
abline(h=80)
text(0.75, 81.5, "81.3", col = "blue")
text(2, 83, "82.8", col = "pink")
dev.off()

# 分性别均值条形图

png("meanbygender.png")
par(mfrow=c(1,2))
barplot(mean.gender$x, names.arg = mean.gender$Group.1, main = "原始分性别条形均值图")
barplot(mean.gender$x, names.arg = mean.gender$Group.1, ylim = c(80, 85), xpd = FALSE, col = c("blue", "pink"), main = "修改后分性别条形均值图")
abline(h=80)
text(0.75, 83.8, "83.6", col = "blue")
text(2, 82.2, "82.0", col = "pink")
dev.off()

# 期中、期末与总评成绩的点图与箱线图
scores <- score[, c("mid", "final", "total")]
png("boxanddotplot.png")
par(mfrow=c(1, 2))
stripchart(scores, col = c("blue", "green", "red"), method = "stack", vertical = T, pch = 20)
boxplot(scores, col = c("blue", "green", "red"), method = "stack", horizontal = F)
dev.off()

# A卷三个大题的回答类型

png("answertypes.png")
par(mfrow=c(1, 3))
barplot(table(Q1), col = c("blue", "green", "pink"), sub = "Q1")
barplot(table(Q2), col = c("blue", "green", "pink"), sub = "Q2")
barplot(table(Q3), col = c("blue", "green", "pink"), sub = "Q3")
dev.off()
detach(score)
```

a用来说明type I error, b用来说明type II error
```{r}
opar <- par(no.readonly = T)
par(mfrow= c(1, 2), pin = c(2.5, 2.5))
X <- numeric(10000)
for (i in 1: 10000)
{
    samples<-rnorm(25, 11, 5)
    X[i]<-mean(samples)
}
hist(X, breaks = 100, main = "Simlution of Type II Error")

abline(v = 12.32635, col = "red", lwd = 2)
xfit <- seq(min(X), max(X), length = 1000)
yfit <- dnorm(xfit, 10, 5)
lines(xfit, yfit, col = "blue", lwd = 2)

h<-hist(g, breaks=10, density=10, col="lightgray", xlab="Accuracy", main="Overall")

xfit<-seq(min(g),max(g),length=40)
yfit<-dnorm(xfit,mean=mean(g),sd=sd(g))
yfit <- yfit*diff(h$mids[1:2])*length(g)
lines(xfit, yfit, col="black", lwd=2)


b <- numeric(1000)
for (i in 1: 1000)
{
    samples<-rnorm(25, 11, 5)
    b[i]<-mean(samples)
}
hist(a, breaks = 25, freq = F, main = "Simlution of Type II Error")
abline(v = 12.32635, col = "red")
par(opar)
```


```{r}
p.ttest <- function (x, y, H0, nx, ny, sx, sy)
{
    t = ((x-y) - H0) / sqrt((sx)^2/nx+(sy)^2/ny)
    df = ((sx)^2/nx+(sy)^2/ny)^2 / (((sx)^2/nx)^2/(nx-1) + ((sy)^2/ny)^2/(ny-1))
    result = list("t" = t, "p-value" = pt(t, df, lower.tail = F), "df" = df)
    return(result)
}
p.ttest(x = 15, y = 10, H0=0, nx = 10, ny = 10, sx = 8, sy = 6)
p.ttest(x = 15, y = 10, H0=0, nx = 30, ny = 30, sx = 8, sy = 6)
p.ttest(x = 15, y = 10, H0=0, nx = 100, ny = 100, sx = 8, sy = 6)
p.ttest(x = 15, y = 10, H0=0, nx = 1000, ny = 1000, sx = 8, sy = 6)
p.ttest(x = 10.1, y = 10, H0=0, nx = 100000, ny = 100000, sx = 8, sy = 6)


p <- numeric(99)

for (i in 2:100)
{
    reslut <- p.ttest(x = 15, y = 10, H0=0, nx = i, ny = i, sx = 8, sy = 6)
    p[i-1] <- reslut$`p-value`
}
n <- c(2:100)
pdf("pic-pvalue.pdf")
plot(n, p, type = "l", ylab = "p-value", lwd = 2)
dev.off()
```


```{r}
install.packages("Cairo")
library(Cairo)
CairoPDF("chinese.pdf",family="Times New Roman")
x <- pretty(c(-4.5,9.5), 200)
myfun <- function(x){dnorm(x,mean=0,sd=sqrt(2))}
curve(myfun,from=-5,to=5,xlim=c(-4.5,9.5),ylim=c(-0.1,0.3),xlab="",ylab="",lwd=1.5, axes = FALSE,col="white")
axis(1,at=c(-6,-4,-2,0,5,7,9,12),label=c("","","","","","","",""),tck=0.01,pos=0)
cord.x <- c(3, seq(3, 5, 0.001),5)
cord.y <- c( 0, myfun(seq(3, 5, 0.001)),0)
polygon(cord.x, cord.y,col="darkgrey")
myfu2<-function(x){dnorm(x,mean=5,sd=sqrt(2))}
curve(myfu2,from=0,to=10,lwd=1.5,add=TRUE)
cord.x <- c(0, seq(0,3,0.001),3)
cord.y <- c( 0, myfu2(seq(0,3,0.001)),0)
polygon(cord.x,cord.y,col="lightgrey")
arrows(0, 0, 0, 0.282, angle =0,lty=2,length=0.05)
arrows(5, 0, 5, 0.282, angle =0,lty=2,length=0.05)
text(0.05,-0.01,expression(italic(μ[1])))
text(5.05,-0.01,expression(italic(μ[2])))
text(0,0.295,expression(italic(H[0])))
text(5,0.295,expression(italic(H[a])))
text(7.4,0.19,expression(italic(1)- italic(β)))
arrows(7.1,0.175,5.8,0.11,angle=30,length=0.1)
text(4.2,0.048,expression(italic(α)))
arrows(4,0.038,3.35,0.006,angle=30,length=0.07)
text(1.15,0.068,expression(italic(β)))
arrows(1.32,0.055,2.5,0.02,angle=40,length=0.08)
text(3.05,-0.01,expression(italic(X[1])))
curve(myfun,from=-5,to=5,xlim=c(-4.5,9.5),ylim=c(-0.1,0.3),xlab="",ylab="",lwd=1.5, axes=FALSE,add=TRUE)
dev.off()
```

effect size of chi squared test
```{r}
library(pwr)
p0 <- rep(1/6, 6)
p1 <- c(43, 49, 56, 45, 66, 41)/300
ES.w1(p0, p1)
stock <- rbind(c(53, 77), c(29, 16), c(19, 6))
p <- stock/200
ES.w2(p)
library(vcd)
library(dplyr)
x <- table(Arthritis$Treatment, Arthritis$Improved)
P <- prop.table(x)
ES.w2(P)
pwr.chisq.test(w = 0.3942295, df = 2, sig.level = 0.05, N = 84)
length(Arthritis$ID)
```

```{r}
n <- seq(4, 1000, 2)
power <- sapply(seq_along(n), function(i) power.t.test(n = n[i], delta = .15, sd = 1, type = 'two.sample')$power)
power
library(effsize)
set.seed(1)
x <- rnorm(10, 2, 1)
y <- rnorm(10, 2.5, 1)
cohen.d(x, y, paired = T)
cohen.d(x, y, paired = F)
(mean(x)-mean(y))/sqrt(9*(sd(x))^2/18+9*(sd(y))^2/18)
(mean(x)-mean(y))/sd(x-y)
(mean(x)-mean(y))/sqrt(var(x)+var(y)-2*cor(x, y)*sd(x)*sd(y))
algebra <- read.csv("algebra.csv")
cohen.d(algebra$A, algebra$B, hedges.correction = T)
cohen.d(algebra$A, algebra$B)
score <- read.csv("test.scores.csv")
cohen.d(score$A, score$B, paired = T)
cohen.d(score$A, score$B)
length(score$A)
```
randomization test
```{r}
A <- c(4.7, 6.6, 7.3)
B <- c(4.6, 5.2, 6.1)
All <- c(A, B)
setdiff(All, A)
choose(6, 3)
TreatA <- combn(c(A, B), 3)
is.matrix(r)
r
mean(A)-mean(B)
x <- as.data.frame(TreatA)

n = c(1:20)

power <- sapply(seq_along(n), function(i) power.t.test(n = n[i], delta = .15, sd = 1, type = 'two.sample')$power)
```

vecsets
```{r}
set.seed(1)
X <- round(rnorm(7, 20, 5))
set.seed(2)
Y <- round(rnorm(7, 15, 5))
pool <- c(X, Y)
pool
x <- combn(pool, 7)
y <- matrix(, nrow(x), ncol(x))
z <- numeric(ncol(x))
library(vecsets)
for(i in 1:ncol(x)){
    y[ , i] <- vsetdiff(pool, x[ , i])
    z[i] <- mean(x[ , i]) - mean(y[ , i])
}
p <- mean(z >= mean(X)-mean(Y))
p

A <- c(4, 5, 6)
B <- c(3, 5, 4)
pool <- c(A, B)
a <- combn(pool, 3)
b <- matrix(, nrow(a), ncol(a))
d <- numeric(ncol(a))
library(vecsets)
for(i in 1:ncol(a)){
    b[ , i] <- vsetdiff(pool, a[ , i])
    d[i] <- mean(a[ , i]) - mean(b[ , i])
}
p1 <- 2*mean(d >= mean(A)-mean(B))
p1
par(pin = c(4, 1.5))
stripchart(d, method = "stack", pch = 21, at = 0.2, offset = 1)
mean(A)-mean(B)
p2 <- 2*length(d[d>=mean(A)-mean(B)])/length(d)
p2


pdf("pic-exactdistr-meandiff.pdf")

par(pin = c(4, 1.5))
stripchart(d, method = "stack", pch = 21, at = 0.1, offset = 1, main = "Exact Distribution of Mean Differences", xlab = expression(italic(bar(X)[A]-bar(X)[B])))

par(new = T)

stripchart(d[abs(d)>=1], method = "stack", pch = 16, at = 0.1, offset = 1, xlim = c(min(d), max(d)))

dev.off()

library(tikzDevice)

```

```{r}
library(Cairo)
CairoPDF("pic-power-alpha1.pdf",family="Times New Roman")
x<-pretty(c(5,16),500)
myfun<-function(x){dnorm(x,mean=10,sd=1)}
curve(myfun,from=6,to=14,xlim=c(5,16),ylim=c(0,0.5),xlab=expression (italic(bar(X))),ylab="",lwd=1.5,xaxt="n",yaxt="n",col="white",  main = "(a)")
axis(1,at=c(10,11,12.33),label=c("10","11","12.33"),tck=0.01)
cord.x <- c(12.33, seq(12.33, 15, 0.001),15)
cord.y <- c( 0, myfun(seq(12.33, 15, 0.001)),0)
polygon(cord.x, cord.y,col="gray35")
myfu2<-function(x){dnorm(x,mean=11,sd=1)}
curve(myfu2,from=7,to=15,lwd=1.5,add=TRUE,lty=2,col="white")
cord.x <- c(6, seq(6,12.33,0.001),12.33)
cord.y <- c( 0, myfu2(seq(6,12.33,0.001)),0)
polygon(cord.x,cord.y, col="gray90",border="white")
text(13.3,-0.01,expression(italic(α) ==0.01))
curve(myfun,from=6,to=14,xlim=c(5,16),add=TRUE,ylim=c(0,0.5),xlab="",ylab="",lwd=1.5,xaxt="n",yaxt="n", lty = 2)
arrows(12.33, 0, 12.33, 0.16, angle =0,lty=1,length=0.05)
arrows(6, 0, 12.33, 0, angle =0,lty=1,length=0.05)
curve(myfu2,from=7,to=15,lwd=1.5,add=TRUE,lty=1)
text(10.5,0.04,expression(italic(β)==0.91))
text(15,0.04,expression(power == 0.09))
arrows(13.5, 0.04, 12.8, 0.04, angle =40,lty=1,length=0.05)
# arrows(7.6, 0.04, 9.4, 0.04, angle =40,lty=1,length=0.08)
legend("topleft", inset=0,c("N(10, 1)","N(11, 1)"), lty=c(2,1))
dev.off()

CairoPDF("pic-power-alpha2.pdf",family="Times New Roman")
x2<-pretty(c(5,16),500)
myfun<-function(x2){dnorm(x2,mean=10,sd=1)}
curve(myfun,from=6,to=14,xlim=c(5,16),ylim=c(0,0.5),xlab=expression (italic(bar(X))),ylab="",lwd=1.5,xaxt="n",yaxt="n",col="white", main = "(b)")
axis(1,at=c(10,11,12.57),label=c("10","11","13.09"),tck=0.01)
cord.x2 <- c(12.57, seq(12.57, 15, 0.001),15)
cord.y2 <- c( 0, myfun(seq(12.57, 15, 0.001)),0)
polygon(cord.x2, cord.y2,col="gray35")
myfu2<-function(x2){dnorm(x2,mean=11,sd=1)}
curve(myfu2,from=7,to=15,lwd=1.5,add=TRUE,lty=2,col="white")
cord.x3 <- c(6, seq(6,12.57,0.001),12.57)
cord.y3 <- c( 0, myfu2(seq(6,12.57,0.001)),0)
polygon(cord.x3,cord.y3,col="gray90",border="white")
text(13.8,-0.01,expression(italic(α) ==0.001))
curve(myfun,from=6,to=14,xlim=c(5,16),add=TRUE,ylim=c(0,0.5),xlab="",ylab="",lwd=1.5,xaxt="n",yaxt="n", lty = 2)
arrows(12.57, 0, 12.57, 0.116, angle =0,lty=1,length=0.05)
arrows(6, 0, 12.57, 0, angle =0,lty=1,length=0.05)
curve(myfu2,from=7,to=15,lwd=1.5,add=TRUE,lty=1)
text(10.5,0.04,expression(italic(β)==0.98))
text(15,0.04,expression(power == 0.02))
arrows(13.5, 0.04, 12.8, 0.04, angle =40,lty=1,length=0.05)
# arrows(7.6, 0.04, 9.4, 0.04, angle =40,lty=1,length=0.08)
legend("topleft", inset=0,c("N(10, 1)","N(11, 1)"), lty=c(2,1))
dev.off()
par(opar)
```

```{r}

c = 10000
F.stat <- numeric(c)
F.stat[1] <- summary(aov(value ~ group, data = ABCD))[[1]][4][[1]][1]
set.seed(1234)
for(i in 2:c) {
    F.stat[i] <- summary(aov(sample(value)~group, data =  ABCD)) [[1]] [4][[1]][1]
}
p = mean(F.stat >= F.stat[1])


```

```{r}
set.seed(1)
a <- round(rnorm(7, 20, 5))
set.seed(2)
b <- round(rnorm(7, 17, 5))
set.seed(3)
c <- round(rnorm(7, 15, 5))
set.seed(4)
d <- round(rnorm(7, 13, 5))
x <- c(a, b, c, d)
group <- factor(c(rep("a", 7), rep("b", 7), rep("c", 7), rep("d", 7)))
c = 10000
F.stat <- numeric(c)
F.stat[1] <- anova(lm(x ~ group))$F[1]
F.df <- anova(lm(x ~ group))$Df
set.seed(1234)
for(i in 2:c){
    F.stat[i] <- anova(lm(sample(x) ~ group))$F[1]
}
p = mean(F.stat >= F.stat[1])
randomdistri <- hist(F.stat, probability = T, breaks = 50,xlab = bquote(paste(italic(F))), main = "Silmulated Randomization Distribution")
abline(v = F.stat[1], lty = 2, col = 2)
mtext(bquote(paste(italic(F), " = ", .(round(F.stat[1], 2)))),3, -2, at = F.stat[1], adj = -.2, cex = 1, col = 2)
no.shade <- sum(randomdistri$mids <= F.stat[1])
shade <- sum(randomdistri$mids > F.stat[1])
par(new = T)
hist(F.stat, probability = T, breaks = 50, col = c(rep(0, no.shade), rep(2, shade)), axes = F, ann = F, add = T, density = 30, border = 1)
mtext(bquote(paste(italic(p), " = ", .(round(p, 5)))), 1, -1.5, at = 5.5, col = "red")
rg.x <- range(randomdistri$breaks)
xx <- seq(rg.x[1], rg.x[2], length = 150)
lines(xx, df(xx, F.df[1], F.df[2]), col = 4, lwd = 2)
lxx <- xx[xx > F.stat[1]]
lyy <- df(xx[xx > F.stat[1]], F.df[1], F.df[2])
lines(lxx, lyy, type = "h", col = 4)
mtext(bquote(paste(italic(p), " = ", .(round(anova(lm(x ~ group))$Pr[1], 4)))), 1, -4, at = 5.5, col = "blue")
```

```{r}
x <- pretty(c(5, 16), 500)
myfun <- function(x) {
    dnorm(x, mean = 10, sd = 1)
}
curve(myfun, from = 6, to = 14, xlim = c(5, 16), ylim = c(0, 0.5), xlab = expression(italic(bar(X))),
ylab = "", lwd = 1.5, xaxt = "n", yaxt = "n", col = "white")
axis(1, at = c(10, 11, 12.33), label = c("10", "11", "12.33"), tck = 0.01)
cord.x <- c(12.33, seq(12.33, 15, 0.001), 15)
cord.y <- c(0, myfun(seq(12.33, 15, 0.001)), 0)
polygon(cord.x, cord.y, col = "darkgrey")
myfu2 <- function(x) {
    dnorm(x, mean = 11, sd = 1)
}
curve(myfu2, from = 7, to = 15, lwd = 1.5, add = TRUE, lty = 2, col = "white")
cord.x <- c(6, seq(6, 12.33, 0.001), 12.33)
cord.y <- c(0, myfu2(seq(6, 12.33, 0.001)), 0)
polygon(cord.x, cord.y, col = "lightgrey", border = "white")
text(13.3, -0.01, expression(italic(α) == 0.01))
curve(myfun, from = 6, to = 14, xlim = c(5, 16), add = TRUE, ylim = c(0,
0.5), xlab = "", ylab = "", lwd = 1.5, xaxt = "n", yaxt = "n")
arrows(12.33, 0, 12.33, 0.16, angle = 0, lty = 1, length = 0.05)
arrows(6, 0, 12.33, 0, angle = 0, lty = 1, length = 0.05)
curve(myfu2, from = 7, to = 15, lwd = 1.5, add = TRUE, lty = 2)
text(6.5, 0.04, expression(italic(β) == 0.91))
text(15, 0.04, expression(power == 0.09))
arrows(13.5, 0.04, 12.8, 0.04, angle = 40, lty = 1, length = 0.05)
arrows(7.6, 0.04, 9.4, 0.04, angle = 40, lty = 1, length = 0.08)
legend("topleft", inset = 0, c("Null distribution", "Real distribution"),
lty = c(1, 2))
```

```{r}
x<-c(21,17,16,28,22,16,22)
y<-c(11,16,23,9,15,16,19)
obs <- matrix(c(x,y), nrow = 2, ncol = 7, byrow = T)
c = 10000
stat <- numeric(c)
stat[1] <- mean(x)-mean(y)
per <- obs
for(i in 2:c){
    for(j in 1:7){
        per[ ,j] <- sample(obs[ ,j])
    }
    stat[i] <- mean(per[1, ]) - mean(per[2, ])
}
p = mean(stat >= stat[1])
p

library(exactRankTests)
perm.test(X, Y, alternative = "greater", paired = TRUE)
perm.test(X, Y, alternative = "greater")
```

bootstrap with resample  package

```{r}
library(readstata13)
library(dplyr)
salary <- read.dta13("cgss2013a.dta") %>%
select(salary2012 = a8a, gender = a2 ) %>%
filter(salary2012 < 9999996 & salary2012 > 0)
salary <- filter(salary, salary2012 >= 1000 & salary2012 <=
100000)
head(salary)
set.seed(1234)
A <- sample_n(salary, 10)
A
original <- A$salary2012
length(salary$salary2012)
pdf("pic-salary2012-population.pdf")
hist(salary$salary2012, breaks = 50, main = "Distribution of Population", xlab = "Salary")
dev.off()
options(scipen = 999)
S1 <- numeric(10000)
for (i in 1: 10000)
{
    set.seed(i)
    x <- sample(original, 10, replace = TRUE)
    S1[i]<- mean(x)
}

S2 <- numeric(10000)
for (i in 1: 10000)
{
    set.seed(i)
    x <- sample(salary$salary2012, 10, replace = FALSE)
    S2[i]<- mean(x)
}

S3 <- numeric(10000)
for (i in 1: 10000)
{
    set.seed(i)
    x <- sample(original, 10, replace = TRUE)
    S3[i]<- median(x)
}

S4 <- numeric(10000)
for (i in 1: 10000)
{
    set.seed(i)
    x <- sample(salary$salary2012, 10, replace = FALSE)
    S4[i]<- median(x)
}

mean(original)
median(original)
pdf("pic-sampling-and-bootstrap-distri.pdf")
par(mfrow=c(2,2))

hist(S1, breaks = 50, main = "Bootstrap Distribution of Mean", xlab = "Salary", xlim = c(0, 60000))
# abline(v=mean(S1), lwd=3,  lty=2, col="red")
abline(v=mean(original), lwd=3, lty = 2, col="red")
text(25000, 400, "sample mean \n  23100", pos = 4)
# text(30150, 500, "bootstrap mean = 30139.8", pos = 4)
mean(original)
mean(S1)


hist(S2, breaks = 50, main = "Sampling Distribution of Mean", xlab = "Salary", xlim = c(0, 60000))
abline(v=mean(salary$salary2012), lwd=3,  lty=2, col="red")
text(25500, 600, "population mean \n 23873.07", pos = 4)
mean(salary$salary2012)


hist(S3, breaks = 50, main = "Bootstrap Distribution of Median", xlab = "Salary", xlim = c(0, 60000))
abline(v=median(original), lwd=3,  lty=2, col="red")
text(20050, 2600, "sample median \n 20000", pos = 4)
median(original)

mean(S3)


hist(S4, breaks = 50, main = "Sampling Distribution of Median", xlab = "Salary", xlim = c(0, 60000))
abline(v=median(salary$salary2012), lwd=3,  lty=2, col="red")
text(20050, 920, "population median \n 20000", pos = 4)
dev.off()
median(salary$salary2012)

data <- data.frame(S1, S2, S3, S4)
apply(data, 2, sd)
apply(data, 2, mean)

library(boot)
library(resample)
bootmean <- bootstrap(original, mean, 10000)
bootmean
plot(bootmean, col = "lightblue")

bootmedian <- bootstrap(original, median, 10000)
bootmedian

par(mfrow = c(1, 2))
pdf("pic-bootmean-by-resample.pdf")
plot(bootmean, col = "lightblue")
dev.off()
pdf("pic-qqplot-bootmean.pdf")
qqPlot(bootmean$replicates)
dev.off()
pdf("pic-bootmedian-by-resample.pdf")
plot(bootmedian, col = "lightgreen")
dev.off()
pdf("pic-qqplot-bootmedian.pdf")
qqPlot(bootmedian$replicates)
dev.off()
par(opar)
data(Verizon)
CLEC <- with(Verizon, Time[Group == "CLEC"])
bootC <- bootstrap(CLEC, mean)
hist(bootC)
bootstrap(original, mean)

CLEC <- with(Verizon, Time[Group == "CLEC"])
bootC <- bootstrap(CLEC, mean, seed = 0)

bootC2 <- bootstrap(CLEC, c(mean = mean(CLEC), sd = sd(CLEC)), seed = 0)

CI.bootstrapT(bootC2)

bootC2
bootmean <- bootstrap(original, mean, 10000, seed = 2016)

bootmean2 <- bootstrap(original, statistic = c(mean = mean(original), sd = sd(original)), R = 10000, seed = 0)
CI.bootstrapT(bootmean2)

bootmean2

sd(original)

bootmean

original



x = original
x1 = bootstrap(x, statistic = c(mean = mean(x), sd = sd(x)), 100, seed = 0)

CI.bootstrapT(x1)

bootsd <- bootstrap(original, sd, 10000, seed = 2016)

CI.percentile(bootsd)

CI.bca(bootsd)

CI.bootstrapT(bootmean2)
t.test(original)$conf.int

35748.51-13500
34050.57-12149.43
40099.71-14500
41239.06-14724.84


CI.percentile(bootsd)

CI.t(bootsd)

CI.bca(bootsd)

CI.bootstrapT(bootsd)

bootmedian <- bootstrap(original, median, 10000, seed = 2016)

bootmedian

length(salary$salary2012[salary$salary2012>500000])
length(salary$salary2012[salary$salary2012<1000])

hist(salary$salary2012, breaks = 30)

S1 <- numeric(10000)

for (i in 1: 10000)
{
    set.seed(i)
    x <- sample(salary$salary2012, 30)
    S1[i]<- mean(x)
}
hist(S1, breaks = 30)

hist(salary$salary2012)
x <- salary$salary2012
is.vector(x)
length(x[x==0])
head(salary)
x <- aggregate(salary2012~gender, mean, data = salary)
barplot(x$salary2012)
female <- filter(salary, gender=="女")
male <- filter(salary, gender=="男")
head(male)
head(female)
length(male$gender)
length(female$gender)
count(salary, gender)
set.seed(1234)
malesample <- sample_n(male, 30)
femalesample <- sample_n(female, 30)
hist(malesample$salary2012, breaks = 15)
hist(femalesample$salary2012, breaks = 15)

M1 <- numeric(10000)

for (i in 1: 10000)
{
    set.seed(i)
    x <- sample(male$salary2012, 30)
    M1[i]<- mean(x)
}
hist(M1, xlab = "salary of 2012", main = "Sampling Distribution", breaks = 30, ylim = c(0, 1200))
abline(v=mean(male$salary2012), lwd=3,  lty=2, col="red")
text(26320, 1150, expression(paste(mu, " = 25318")))

mean(male$salary2012)


M2 <- numeric(10000)
set.seed(1234)
original <- sample(male$salary2012, 30)
for (i in 1: 10000)
{
    set.seed(i)
    x <- sample(original, 30, replace =                 TRUE)
    M2[i]<- mean(x)
}
hist(M2, xlab = "salary of 2012", main = "Boostrap Distribution", breaks = 30, ylim = c(0, 1200))
abline(v=mean(original), lwd=3,  lty=2, col="red")
text(26900, 1180, expression(paste(bar(x), " = 26753")), pos = 4)

M3 <- numeric(10000)
for (i in 1: 10000)
{
    x <- sample(male$salary2012, 30)
    M3[i]<- median(x)
}
hist(M3, breaks = 30)
median(original)
mean(M3)

M4 <- numeric(10000)
for (i in 1: 10000)
{
    x <- sample(original, 30, replace =                 TRUE)
    M4[i]<- median(x)
}
hist(M4, breaks = 30)

par(mfrow = c(1, 2))

hist(M2, xlab = "salary of 2012", main = "Boostrap Distribution", breaks = 30, ylim = c(0, 1200))
abline(v=mean(original), lwd=3,  lty=2, col="red")
text(26900, 1180, expression(paste(bar(x), " = 26753")), pos = 4)

hist(M1, xlab = "salary of 2012", main = "Sampling Distribution", breaks = 30, ylim = c(0, 1200))
abline(v=mean(male$salary2012), lwd=3,  lty=2, col="red")
text(26320, 1150, expression(paste(mu, " = 25318")))

mean(original)

library(boot)
boot(male$salary2012, mean, 100)

x = c(1, 2, 3, 4)
y <- boot(x, statistic = sd, R = 100)

library(resample)
phone <- read.csv("phone.csv")

bootstrap(phone$a, statistic = c(mean = mean(phone$a), sd = sd(phone$a)), R = 10000)

times <- phone$a
x = bootstrap(times, statistic = c(mean = mean(times), sd = sd(times)), R = 10000)
CI.bca(x, probs = c(0.005, 0.995))

set.seed(1)

x <- round(rnorm(30, 20, 5))

set.seed(2)

y <- round(rnorm(30, 15, 5))

meandiff <- numeric(10000)

for (i in 1:10000)
{
    x1 <- sample(x, 30, replace = TRUE)
    y1 <- sample(y, 30, replace = TRUE)
    meandiff[i] <- mean(x1) - mean(y1)
}
pdf("pic-inde-meandiff-boot.pdf")
hist(meandiff, breaks = 100, main = "Histogram of Mean Differences", xlab = "Mean Differences")
abline(v = quantile(meandiff, probs = c(.025,0.975)), lty = 2, lwd = 2, col = "red")
dev.off()
quantile(meandiff, probs = c(.025,0.975))
meandiff = function (x, y) {mean(x)-mean(y)}
meandiff(x, y)
boot.diff =  bootstrap2(x, mean, data2 = y)
CI.percentile(boot.diff)
hist(boot.diff$replicates, breaks = 100)
```


```{r}
library(tidyr)
memeda<-array(dim=c(10000,2))
colnames(memeda)<-c("F","e-a")
for (i in 1:10000){
    a=rnorm(75,1.1,1)
    b=rnorm(75,1.2,1)
    c=rnorm(75,1.3,1)
    d=rnorm(75,1.4,1)
    e=rnorm(75,1.5,1)
    x<-data.frame(a,b,c,d,e)
    xnew<-gather(x,var,value)
    memeda[i,1]<-anova(lm(value~var,data=xnew))$"Pr(>F)"[1]
    with<-with(xnew,{
        fit<-aov(value~var)
        summary(fit)
        TukeyHSD(fit)})
    memeda[i,2]<-with$var[4,4]
}
plot(memeda)
abline(h=0.05)
abline(v=0.05)
```
