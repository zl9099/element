## 生存分析（与我之前整理的有重合）


library(readxl)
unemployment <- read_excel("E:/unemployment.xlsx")
View(unemployment)
library(survival)

time <- spell
attach(unemployment)
time <- spell
event <- event

X <- cbind(logwage, ui, age)
group <- ui

summary(time)
summary(event)
summary(X)
summary(group)

kmsurvival <- survfit(Surv(time,event) ~ 1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")

kmsurvival1 <- survfit(Surv(time, event) ~ group)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")

nasurvival <- survfit(coxph(Surv(time,event)~1), type="aalen")
summary(nasurvival)

plot(nasurvival, xlab="Time", ylab="Survival Probability")
coxph <- coxph(Surv(time,event) ~ X, method="breslow")
summary(coxph)

exponential <- survreg(Surv(time,event) ~ X, dist="exponential")
nasurvival <- survfit(coxph(Surv(time,event)~1), type="aalen")
summary(nasurvival)
plot(nasurvival, xlab="Time", ylab="Survival Probability")
coxph <- coxph(Surv(time,event) ~ X, method="breslow")
summary(coxph)

exponential <- survreg(Surv(time,event) ~ X, dist="exponential")
summary(exponential)
weibull <- survreg(Surv(time,event) ~ X, dist="weibull")
summary(weibull)
loglogistic <- survreg(Surv(time,event) ~ X, dist="loglogistic")
summary(loglogistic)


## 支持向量机（与我之前整理的有重合）

install.packages("MASS")
library(e1071)
data(cats,package="MASS")
inputData<-data.frame(cats[,c(2,3)],response=as.factor(cats$Sex))# response as factor
svmfit <- svm(response ~ ., data = inputData, kernel = "linear", cost = 10, scale = FALSE)
svmfit <- svm(response ~ ., data = inputData, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
plot(svmfit, inputData)
compareTable <- table (inputData$response, predict(svmfit))
mean(inputData$response != predict(svmfit))
svmfit<-svm(response~.,data=inputData,kernel="radial",cost=10,scale=FALSE)# radial svm, scaling turned OFF

print(svmfit)
plot(svmfit,inputData)
compareTable<-table(inputData$response,predict(svmfit))  # tabulate
mean(inputData$response!=predict(svmfit))# 18.75% misclassification error
set.seed(100)# for reproducing results
rowIndices<-1:nrow(inputData)# prepare row indices
sampleSize<-0.8*length(rowIndices)# training sample size
trainingRows<-sample(rowIndices,sampleSize)# random sampling
trainingData<-inputData[trainingRows,]# training data
testData<-inputData[-trainingRows,]# test data
tuned<-tune.svm(response~.,data=trainingData,gamma=10^(-6:-1),cost=10^(1:2))# tune
summary(tuned)# to select best gamma and cost
svmfit<-svm(response~.,data=trainingData,kernel="radial",cost=100,gamma=0.001,scale=FALSE)# radial svm, scaling turned OFF

print(svmfit)
plot(svmfit,trainingData)
compareTable<-table(testData$response,predict(svmfit,testData))  # comparison table
mean(testData$response!=predict(svmfit,testData))# 13.79% misclassification error
# F M F 6 3 M 1 19
n_points_in_grid=60# num grid points in a line
x_axis_range&lt;-range(inputData[,2])# range of X axis
y_axis_range&lt;-range(inputData[,1])# range of Y axis
X_grid_points&lt;-seq(from=x_axis_range[1],to=x_axis_range[2],length=n_points_in_grid)# grid points along x-axis
Y_grid_points&lt;-seq(from=y_axis_range[1],to=y_axis_range[2],length=n_points_in_grid)# grid points along y-axis
all_grid_points&lt;-expand.grid(X_grid_points,Y_grid_points)# generate all grid points
names(all_grid_points)&lt;-c(&quot;Hwt&quot;,&quot;Bwt&quot;)# rename
all_points_predited&lt;-predict(svmfit,all_grid_points)# predict for all points in grid
color_array&lt;-c(&quot;red&quot;,&quot;blue&quot;)[as.numeric(all_points_predited)]# colors for all points based on predictions
plot(all_grid_points,col=color_array,pch=20,cex=0.25)# plot all grid points
points(x=trainingData$Hwt,y=trainingData$Bwt,col=c(&quot;red&quot;,&quot;blue&quot;)[as.numeric(trainingData$response)],pch=19)# plot data points
points(trainingData[svmfit$index,c(2,1)],pch=5,cex=2)# plot support vectors


# Setup
library(e1071)
data(cats,package="MASS")
inputData<-data.frame(cats[,c(2,3)],response=as.factor(cats$Sex))
# response as factor
# linear SVM
svmfit <- svm(response ~ ., data = inputData, kernel = "linear", cost = 10, scale = FALSE)
# linear svm, scaling turned OFF
print(svmfit)
plot(svmfit, inputData)
compareTable <- table (inputData$response, predict(svmfit))
# tabulate
mean(inputData$response != predict(svmfit))
# 19.44% misclassification error
# radial SVM
svmfit<-svm(response~.,data=inputData,kernel="radial",cost=10,scale=FALSE)# radial svm, scaling turned OFF
print(svmfit)
plot(svmfit,inputData)
compareTable<-table(inputData$response,predict(svmfit))  # tabulate
mean(inputData$response!=predict(svmfit))# 18.75% misclassification error
### Tuning
# Prepare training and test data
set.seed(100)# for reproducing results
rowIndices<-1:nrow(inputData)# prepare row indices
sampleSize<-0.8*length(rowIndices)# training sample size
trainingRows<-sample(rowIndices,sampleSize)# random sampling
trainingData<-inputData[trainingRows,]# training data
testData<-inputData[-trainingRows,]# test data
tuned<-tune.svm(response~.,data=trainingData,gamma=10^(-6:-1),cost=10^(1:2))# tune
summary(tuned)# to select best gamma and cost
# Parameter tuning of 'svm': # - sampling method: 10-fold cross validation # # - best parameters: # gamma cost # 0.001 100 # # - best performance: 0.26 # # - Detailed performance results: # gamma cost error dispersion # 1 1e-06 10 0.36 0.09660918 # 2 1e-05 10 0.36 0.09660918 # 3 1e-04 10 0.36 0.09660918 # 4 1e-03 10 0.36 0.09660918 # 5 1e-02 10 0.27 0.20027759 # 6 1e-01 10 0.27 0.14944341 # 7 1e-06 100 0.36 0.09660918 # 8 1e-05 100 0.36 0.09660918 # 9 1e-04 100 0.36 0.09660918 # 10 1e-03 100 0.26 0.18378732 # 11 1e-02 100 0.26 0.17763883 # 12 1e-01 100 0.26 0.15055453
# Parameter tuning of 'svm':
#  - sampling method: 10-fold cross validation
#
# - best parameters:
#  gamma cost
# 0.001  100
#
# - best performance: 0.26
#
# - Detailed performance results:
#  gamma cost error dispersion
# 1  1e-06  10  0.36 0.09660918
# 2  1e-05  10  0.36 0.09660918
# 3  1e-04  10  0.36 0.09660918
# 4  1e-03  10  0.36 0.09660918
# 5  1e-02  10  0.27 0.20027759
# 6  1e-01  10  0.27 0.14944341
# 7  1e-06  100  0.36 0.09660918
# 8  1e-05  100  0.36 0.09660918
# 9  1e-04  100  0.36 0.09660918
# 10 1e-03  100  0.26 0.18378732
# 11 1e-02  100  0.26 0.17763883
# 12 1e-01  100  0.26 0.15055453
svmfit<-svm(response~.,data=trainingData,kernel="radial",cost=100,gamma=0.001,scale=FALSE)# radial svm, scaling turned OFF
print(svmfit)
plot(svmfit,trainingData)
compareTable<-table(testData$response,predict(svmfit,testData))  # comparison table
mean(testData$response!=predict(svmfit,testData))# 13.79
n_points_in_grid=60# num grid points in a line
x_axis_range&lt;-range(inputData[,2])# range of X axis
y_axis_range&lt;-range(inputData[,1])# range of Y axis
X_grid_points&lt;-seq(from=x_axis_range[1],to=x_axis_range[2],length=n_points_in_grid)# grid points along x-axis
Y_grid_points&lt;-seq(from=y_axis_range[1],to=y_axis_range[2],length=n_points_in_grid)# grid points along y-axis
all_grid_points&lt;-expand.grid(X_grid_points,Y_grid_points)# generate all grid points
names(all_grid_points)&lt;-c(&quot;Hwt&quot;,&quot;Bwt&quot;)# rename
all_points_predited&lt;-predict(svmfit,all_grid_points)# predict for all points in grid
color_array&lt;-c(&quot;red&quot;,&quot;blue&quot;)[as.numeric(all_points_predited)]# colors for all points based on predictions
plot(all_grid_points,col=color_array,pch=20,cex=0.25)# plot all grid points
points(x=trainingData$Hwt,y=trainingData$Bwt,col=c(&quot;red&quot;,&quot;blue&quot;)[as.numeric(trainingData$response)],pch=19)# plot data points
points(trainingData[svmfit$index,c(2,1)],pch=5,cex=2)# plot support vectors
library(devtools)
install.packages(devtools)
library(devtools) install_github("Gibbsdavidl/CatterPlots")
install_github("Gibbsdavidl/CatterPlots")
install.packages(CatterPlots)
library(devtools) install_github("Gibbsdavidl/CatterPlots")
install.packages("CatterPlots")
library(devtools) install_github("Gibbsdavidl/CatterPlots")
install.packages("devtools")
install.packages("CatterPlots")
library(devtools)
install_github("Gibbsdavidl/CatterPlots")
radius <- 1 theta <- seq(0, 2 * pi, length = 30) multicat(xs=radius*cos(theta),ys=radius* in(theta),
cat=c(1,3,6,9), catcolor= list(c(1,0.4,0,1),
c(0,0.4,0,1), c(0.7,0,0,1), c(0,0,0.6,1)))
radius <- 1 theta <- seq(0, 2 * pi, length = 30) multicat(xs=radius*cos(theta),ys=radius* in(theta),
cat=c(1,3,6,9), catcolor= list(c(1,0.4,0,1),
c(0,0.4,0,1), c(0.7,0,0,1), c(0,0,0.6,1)))
radius <- 1 theta <- seq(0, 2 * pi, length = 30)
x <- -10:10 y <- x^2 purr <- catplot(xs=x, ys=y, cat=3, catcolor=c(0,1,1,1))
radius <- 1
theta <- seq(0, 2 * pi, length = 30)
multicat(xs=radius*cos(theta),ys=radius* in(theta)
multicat(xs=radius*cos(theta
multicat(xs=radius*cos(theta)
multicat(xs=radius*cos(theta)
ys=radius* in(theta
multicat(xs=radius*cos(theta),ys=radius* in(theta)
multicat(xs=radius*cos(theta)
ys=radius* in(theta)
x <- -10:10
y <- -x^2 + 10
multicat(xs=x,ys=y,cat=c(1,3,6,9)
x <- -10:10 y <- -x^2 + 10 multicat(xs=x,ys=y,cat=c(1,3,6,9),catcolor=list(c(1,0.4,0,1),
c(0.7,0,0,1),c(0,0,0.6,1)))
multicat(xs=x,ys=y,cat=c(1,3,6,9),catcolor=list(c(1,0.4,0,1),
c(0.7,0,0,1),c(0,0,0.6,1)))
multicat(xs=x,ys=y,cat=c(1,3,6,9)
cat=c(1,3,6,9)


?CatterPlots
??CatterPlots
install.packages("CatterPlots")
install.packages("cat")
## Not run:
library(plotly)
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.8))
rules
# interactive scatter plot visualization
plotly_arules(rules)
plotly_arules(rules, measure = c("support", "lift"), shading = "confidence")
plotly_arules(rules, method = "two-key plot")
# add jitter, change color and markers and add a title
plotly_arules(rules, jitter = 10, opacity = .7, size = 10, symbol = 1,
colors = c("blue", "green"))
# save a plot as a html page
p <- plotly_arules(rules)
htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
browseURL("arules.html")
# Note: selfcontained seems to make the browser slow.
# interactive matrix visualization
plotly_arules(rules, method = "matrix")
library(plotly)
library(ggplot2)
library(plotly)
data(Groceries)
install.packages("Groceries")
library(Groceries)
install.packages(Gro)
library(gcookbook)
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.8))
rules
library(plotly)
data(Groceries)
install.packages("plotly")
install.packages("plotly")
library(plotly)
library(ggplot2)
lidata(Groceries)
install.packages("lidata")
lidata(Groceries)
lidata(lidata)
install.packages("Groceries")
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.8))
rules

#模型解读：
+ Call，列出了回归模型的公式。
+Residuals，列出了残差的最小值点，1/4分位点，中位数点，3/4分位点，最大值点。
?CatterPlots
??CatterPlots
install.packages("CatterPlots")
load("E:/4年数据/.RData")
