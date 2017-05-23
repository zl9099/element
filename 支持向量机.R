#使用的数据是来自MASS包的cats数据集。在本例中你将尝试使用体重和心脏重量来预测一只猫的性别。
#我们拿数据集中20%的数据点，用于测试模型的准确性（在其余的80%的数据上建立模型）。
# Setup
library(e1071)
data(cats,package="MASS")
inputData<-data.frame(cats[,c(2,3)],response=as.factor(cats$Sex))# response as factor
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
mean(testData$response!=predict(svmfit,testData))# 13.79% misclassification error

F M F 6 3 M 1 19
# Grid Plot


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
