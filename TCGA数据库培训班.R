# -*- coding: utf-8 -*-
"""
Created on Mon May 11 2017
@author: wxhu
"""


####安装R包##########

source("http://bioconductor.org/biocLite.R")
biocLite("WGCNA")

####R赋值语句##########
a<-5
a
b=7
b

#####创建向量的方法(函数)#####
1:10
h<-c(21, 12, 2, 5)
h
seq(1,10,by=0.5)
rep(1:5,2)
rep(1:5,rep(2,5))

m <- rep("mmm",3)
m

#####因子(factor)#####
result<-c("a","a","b","b","c")
result
result.factor<-factor(result)
result.factor

#####创建矩阵的函数#####
matrix(1:6, 2, 3)
matrix(1:6, 2, 3, byrow=TRUE)
m <- matrix(data=1:12,nrow=4,ncol=3,dimnames=list(c("r1","r2","r3","r4"),c("c1","c2","c3")))
m
x <- 1:6
x
dim(x)
dim(x) <- c(2, 3)
x

#####生成数据框#####
d <- data.frame(name=c("张三", "李四", "王二"), age=c(35, 32, 48), height=c(170, 172, 175))
d

#####生成列表(list)######
foo = list(x = 1:5, y = matrix(1:6, nrow = 2))
foo
foo$y
foo[2]
foo[[2]]
foo[[1]][2]
foo$y[2]

#####对象类型判断#####
x <- c(1,2,3)
is.vector(x)
is.data.frame(x)
is.character(x)
is.numeric(x)

#####类的转换#####
x <- c(1,2,3)
y <- as.matrix(x)
y
z <- as.character(x)
z

#####R语言数据子集选取#####
d <- data.frame(name=c("张三", "李四", "王二"), age=c(35, 32, 48), height=c(170, 172, 175))
d[1,]
d[,1]
d[2,2]
d[,-2]

y <- list("a", "b", "c", 1:3)
y
class(y)
y[3:4]
class(y[3:4])

x <- matrix(1:6, 2, 3)
x[1, 2]
x[1, 2, drop=FALSE]
x[1,]
x[, 1, drop=FALSE]

s <- list(foo = 1:4, bar = 0.05,kuz="amd")
s
s[1]
s[[1]]
s$bar
s[["bar"]]
s["bar"]
s$kuz
s[["kuz"]]
s["kuz"]
s[c(1, 3)]
s[c(2, 3)]
s[[c(1, 3)]]
s[[1]][[3]]

#####R的循环for和while#####
for(i in 1:10)
print(i)

i <- 1
while(i<11){print(i);  i <- i + 1}


#####R编程#####
sum<-function(x,y){x+y}
sum(4,9)


#######CRC mRNA差异表达分析:limma########
library(limma)# Load the limma package
setwd("F:/prj_training/")#设置工作目录，可自定义修改
exp<-read.table("CRC_mRNA_case_control.txt",sep="\t",header=T)#读取表达文件，分隔符为"\t"，第一行为列名；以下雷同
#文件读取另一种形式 exp<-read.table(file.choose(),sep="\t",header=T)
rownames(exp)<-exp[,1]#第一列为行名
exp<-exp[2:65]#表达值为第2列到第65列
exp<-as.matrix(exp)#把表达的表格转化为矩阵形式
samps<-factor(c(rep("case",32),rep("control",32)))#前32个为case，后32个为control
design<-model.matrix(~0+samps)#设置差异表达的对比模式
colnames(design)<-c("case","control")#设置列名
fit<-lmFit(exp,design)#Estimate the fold changes and standard errors by fitting a linear model for each gene.
cont.matrix<-makeContrasts(case-control,levels=design)#设置对比模式，这里即癌和癌旁
fit2<-contrasts.fit(fit, cont.matrix)#The cont.matrix can be used to expand the linear model fit
fit2 <- eBayes(fit2) #Apply empirical Bayes smoothing to the standard errors
final<-topTable(fit2, coef=1,number=dim(exp)[1],adjust.method="BH",sort.by="B")#对统计结果按照B值进行排序
deg<-subset(final, adj.P.Val<0.001&abs(logFC)>0.7&AveExpr>5&B>5) #自定义标准筛选差异表达基因
deg.names <- rownames(deg)#提取差异表达基因的名字
deg.data<-exp[deg.names,]#提取差异表达基因对应的表达数据
write.table(deg.data,paste("case_control","_limama.txt"),quote=F,sep="\t")#把差异表达基因的数据输出到本地文件

#差异表达基因的热图制作
library(pheatmap)# Load the pheatmap package
tiff(filename="F:/prj_training/CRC_heatmap_0.05_mRNA.tif",res=300, units = 'in',width=6,height=7)##打开一个tiff类型的绘图设备，输出文件名为"CRC_heatmap_0.05_mRNA.tif"，res设置图片分辨率为300，units指定长宽的单位，px(默认)，in(inches),cm或者mm；宽度为6英寸，高度为7英寸
pheatmap(deg.data,color = colorRampPalette(c("green", "black", "red"))(100),  fontsize_row = 10, scale = "row", border_color = NA)#绘制差异表达基因的热图，用colorRampPalette函数自定义调色板（即绿/黑/红三种颜色间的100种颜色顺序），行名（基因名）的字体大小为10，按行（即基因）进行标准化，边界不着色
dev.off ()#关闭当前设备

#差异表达基因的火山图制作
require(ggplot2)
##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off
a <- read.table(file.choose(), header=TRUE,sep="\t",stringsAsFactors = F)
id<-c(a$id)
id
P.Value <- c(a$padj)
FC <- c(a$log2FoldChange)
df <- data.frame(id,P.Value, FC)
df$threshold = as.factor(abs(df$FC) > 0.7 & df$P.Value < 0.01)

logFC_cutoff <- with(df,mean(abs(FC)) + 2*sd(abs(FC)) )
df$change = as.factor(ifelse(df$P.Value < 0.05 & abs(df$FC) >0.58,ifelse(df$FC > 0.58 ,'UP','DOWN'),'NOT'))
this_tile <- paste0('Cutoff for log2FC is ',round(0.58,3),'\nThe number of up gene is ',nrow(df[df$change =='UP',]),'\nThe number of down gene is ',nrow(df[df$change =='DOWN',]))
g = ggplot(data=df, aes(x=FC, y=-log10(P.Value), color=change)) +
geom_point(alpha=0.4, size=1.75) +
theme_set(theme_set(theme_bw(base_size=20)))+
xlab("log2 fold change") + ylab("-log10 p-value") +
ggtitle( this_tile  ) + theme(plot.title = element_text(size=15,hjust = 0.5))+
scale_colour_manual(values = c('blue','black','red'))  ## corresponding to the levels(res$change)print(g)
g

##Construct the plot object
g = ggplot(data=df, aes(x=FC, y=-log10(P.Value), colour=threshold)) +
geom_point(alpha=0.4, size=1.75) +
theme(legend.position = "none") +
xlim(c(-5, 5)) + ylim(c(0, 5)) +
xlab("log2 fold change") + ylab("-log10 p-value")
g
#add text for all significant genes
#g + geom_text(aes(x=FC, y=-log10(P.Value)-0.1,label=id, size=0.1), colour="grey")

#add text for selected significant genes
dd_text = df[(abs(FC) > 0.58) & (P.Value < 0.05),]
dd_text
g + geom_text(data=dd_text, aes(x=FC, y=-log10(P.Value)-0.1, label=id, size=0.03), colour="black")

##add text for specified a few of significant genes
selectedRows <- df[grep("CDC45", df$id), ]
selectedRows # or
new<-df[which(df[,"id"]=="CDC45"),,drop=FALSE]
new
g + geom_text(data=selectedRows, aes(x=FC, y=-log10(P.Value)-0.1, label=id, size=0.05), colour="black")+scale_y_continuous(limits=c(0,10))#调整y轴范围以利于显示text


P.Value <- c(final$adj.P.Val)
FC <- c(final$logFC)
df <- data.frame(P.Value, FC)
df$change = as.factor(ifelse(final$P.Value < 0.01 & abs(final$FC) > 0.7,ifelse(final$FC > 0.7 ,'UP','DOWN'),'NOT'))
this_tile <- paste0('Cutoff for logFC is ',round(0.010,3),'\nThe number of up gene is ',nrow(df[df$change =='UP',]),'\nThe number of down gene is ',nrow(df[df$change =='DOWN',]))
g = ggplot(data=df, aes(x=-log10(FC), y=-log10(P.Value), color=change)) +
geom_point(alpha=0.4, size=1.75) +
theme_set(theme_set(theme_bw(base_size=20)))+
xlab("log2 fold change") + ylab("-log10 p-value") +
ggtitle( this_tile  ) + theme(plot.title = element_text(size=15,hjust = 0.5))+
scale_colour_manual(values = c('blue','black','red'))  ## corresponding to the levels(res$change)print(g)

##Construct the plot object
g = ggplot(data=df, aes(x=FC, y=-log10(P.Value), colour=threshold)) +
geom_point(alpha=0.4, size=1.75) +
theme(legend.position = "none") +
xlim(c(-5, 5)) + ylim(c(0, 5)) +
xlab("log2 fold change") + ylab("-log10 p-value")
g


########CRC 共表达分析:WGCNA########
library(WGCNA) # Load the WGCNA package
getwd() # Display the current working directory
#set the working directory
workingDir = "F:\\prj_training"
setwd(workingDir)
a<-read.table("case_control_limama.txt",header=T,sep='\t') #Read in the CRC DEGs data set，header参数设定是否带有表头。sep参数设定了列之间的间隔方式。
b<-a[,-c(34:65)] #Only keep the tumor samples
options(stringsAsFactors = FALSE)#read.table函数读取数据后将存为data.frame格式，而且所有的字符将被转为因子格式，如果不想这么做需要将参数stringsAsFactors设为FALSE
# Take a quick look at what is in the data set
dim(b)
names(b)
datExpr0 = as.data.frame(t(b[,-1]))
datExpr0<-log2(datExpr0+1)
names(datExpr0) = b$gene.names;
rownames(datExpr0) = names(b)[-1]
#We first check for genes and samples with too many missing values;
#If the last statement returns TRUE, all genes have passed the cuts.
#If not, we remove the offending genes and samples from the data
gsg = goodSamplesGenes(datExpr0, verbose = 3);
gsg$allOK
#Next we cluster the samples (in contrast to clustering genes that will come later) to see if there are any obvious outliers.
sampleTree = hclust(dist(datExpr0), method = "average")
# 聚类中集合之间的距离：
# 1，类平均法：average 将聚类中的所有向量之和的平局向量作为聚类中心点，离中心点最近的聚合成一类;average聚类的效果介于single和complte之间
# 2，重心法：centroid
# 3，中间距离法:median
# 4，最长距离法：complete 默认, 两个聚类之间最远的点作为聚类的距离
# 5，最短距离法：single 两个聚类之间最近的点作为聚类的距离
# 6，离差平方和法：ward
# 7，密度估计法：density
# Plot the sample tree: Open a graphic output window of size 12 by 9 inches
# You should change the dimensions if the window is too large or too small.
sizeGrWindow(12,9)
#pdf(file = "Plots/sampleClustering.pdf", width = 12, height = 9);
par(cex = 0.6);#cex用于表示对默认的绘图文本和符号放大多少倍
par(mar = c(0,4,2,0))#mar控制绘图区的大小，4个数字代表绘图区域距离下，左，上，右边界的行数
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5,
cex.axis = 1.5, cex.main = 2)
#sub即Sub-title (at bottom); xlab即 X axis label;
#cex.lab 坐标轴标签（名称）的缩放倍数。类似于cex;
#cex.axis 坐标轴刻度文字的缩放倍数。类似于cex
#cex.main 标题的缩放倍数。类似于cex

# Look if there is outlier or not. One can remove the outlier by hand, or use an automatic approach.
# Choose a height cut that will remove the offending sample, and use a branch cut at that height.
# Plot a line to show the cut
abline(h = 205, col = "red");
# Determine cluster under the line
clust = cutreeStatic(sampleTree, cutHeight = 205, minSize = 10)
table(clust)
# clust 1 contains the samples we want to keep.
keepSamples = (clust==1)
datExpr = datExpr0[keepSamples, ]
nGenes = ncol(datExpr)
nSamples = nrow(datExpr)
#The variable datExpr now contains the expression data ready for network analysis
# Choose a set of soft-thresholding powers

# Constructing a weighted gene network entails the choice of the soft thresholding power β to which co-expression
# similarity is raised to calculate adjacency. The authors of WGCNA have proposed to choose the soft thresholding power
# based on the criterion of approximate scale-free topology. We refer the reader to that work for more details; here
# we illustrate the use of the function pickSoftThreshold that performs the analysis of network topology and aids the
# user in choosing a proper soft-thresholding power. The user chooses a set of candidate powers (the function provides
# suitable default values), and the function returns a set of network indices that should be inspected, for example as
# follows:
powers = c(c(1:10), seq(from = 12, to=20, by=2)) # Choose a set of soft-thresholding powers
sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5) # Call the network topology analysis function
# Plot the results:
sizeGrWindow(9, 5)
par(mfrow = c(1,2));#指将绘图区域分成1行2列，并按行的顺序依次绘图填充
cex1 = 0.9;
# Scale-free topology fit index as a function of the soft-thresholding power
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
main = paste("Scale independence"));  #xlab表示x轴标题，ylab表示y轴标题，main表示图片主标题
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
labels=powers,cex=cex1,col="red");
# this line corresponds to using an R^2 cut-off of h
abline(h=0.90,col="red") #向图中添加直线，颜色是红色
# Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], sft$fitIndices[,5],
xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
main = paste("Mean connectivity"))
# type参数可以设置所绘图形中数据的显示类型，如将绘图函数plot中的参数type取值设置为p，则可以将数据以点的形式显示
# type= "p " 在图形中数据显示为点
# type= "l "  在图形中数据显示为线
# type= "b "  在图形中数据显示为点和连接线
# type= "o "  在图形中数据点覆盖在线上
# type= "h "  在图形中数据显示为从点到x轴的垂直线
# type= "s "   在图形中数据显示为阶梯图
# type= "n "  在图形中数据不显示
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")

# We choose the power 5, which is the lowest power for which the scale-free topology fit
# index curve flattens out upon reaching a high value (in this case, roughly 0.90)
softPower = 5;
adjacency = adjacency(datExpr, power = softPower);
# To minimize effects of noise and spurious associations, we transform the adjacency into Topological Overlap Matrix,
# and calculate the corresponding dissimilarity
# Turn adjacency into topological overlap
TOM = TOMsimilarity(adjacency);
dissTOM = 1-TOM
#We now use hierarchical clustering to produce a hierarchical clustering tree (dendrogram) of genes.
geneTree = hclust(as.dist(dissTOM), method = "average") # Call the hierarchical clustering function
# Plot the resulting clustering tree (dendrogram)
sizeGrWindow(12,9) # open a graphics window
plot(geneTree, xlab="", sub="", main = "Gene clustering on TOM-based dissimilarity",
labels = FALSE, hang = 0.04);
#hang 等于数值，表示标签与末端树杈之间的距离,负数会在纵坐标0以下,正值则在0以上,默认为0.1

# In the clustering tree (dendrogram),
# each leaf, that is a short vertical line, corresponds to a gene. Branches of the dendrogram group together densely
# interconnected, highly co-expressed genes. Module identification amounts to the identification of individual branches
# ("cutting the branches off the dendrogram"). There are several methods for branch cutting; our standard method is
# the Dynamic Tree Cut from the package dynamicTreeCut.
minModuleSize = 30; # We like large modules, so we set the minimum module size relatively high:
# Module identification using dynamic tree cut:
dynamicMods = cutreeDynamic(dendro = geneTree, distM = dissTOM,
deepSplit = 2, pamRespectsDendro = FALSE,
minClusterSize = minModuleSize);
table(dynamicMods)#计数每个模块的基因数目
#Label 0 is reserved for unassigned genes.
#We now plot the module assignment under the gene dendrogram:
dynamicColors = labels2colors(dynamicMods) # Convert numeric lables into colors
table(dynamicColors)
# Plot the dendrogram and colors underneath
sizeGrWindow(8,6) # open a graphics window
plotDendroAndColors(geneTree, dynamicColors, "Dynamic Tree Cut",
dendroLabels = FALSE, hang = 0.03,
addGuide = TRUE, guideHang = 0.05,
main = "Gene dendrogram and module colors")

# Merging of modules whose expression profiles are very similar
# The Dynamic Tree Cut may identify modules whose expression profiles are very similar. It may be prudent to merge
# such modules since their genes are highly co-expressed. To quantify co-expression similarity of entire modules, we
# calculate their eigengenes and cluster them on their correlation:
# Calculate eigengenes
MEList = moduleEigengenes(datExpr, colors = dynamicColors)
MEs = MEList$eigengenes
# Calculate dissimilarity of module eigengenes
MEDiss = 1-cor(MEs);
# Cluster module eigengenes
METree = hclust(as.dist(MEDiss), method = "average");
# Plot the result
sizeGrWindow(7, 6)
plot(METree, main = "Clustering of module eigengenes",
xlab = "", sub = "")
MEDissThres = 0.25 #We choose a height cut of 0.25, corresponding to correlation of 0.75, to merge
abline(h=MEDissThres, col = "red") # Plot the cut line into the dendrogram
# Call an automatic merging function
merge = mergeCloseModules(datExpr, dynamicColors, cutHeight = MEDissThres, verbose = 3)
mergedColors = merge$colors; # The merged module colors
mergedMEs = merge$newMEs;# Eigengenes of the new merged modules:
# To see what the merging did to our module colors, we plot the gene dendrogram again, with the original and merged
# module colors underneath
sizeGrWindow(12, 9)
#pdf(file = "Plots/geneDendro-3.pdf", wi = 9, he = 6)
plotDendroAndColors(geneTree, cbind(dynamicColors, mergedColors),
c("Dynamic Tree Cut", "Merged dynamic"),
dendroLabels = FALSE, hang = 0.03,
addGuide = TRUE, guideHang = 0.05)
#dev.off()
#In the subsequent analysis, we will use the merged module colors in mergedColors
moduleColors = mergedColors # Rename to moduleColors
# Construct numerical labels corresponding to the colors
colorOrder = c("grey", standardColors(50));
moduleLabels = match(moduleColors, colorOrder)-1;#match returns a vector of the positions of (first) matches of its first argument in its second.
MEs = mergedMEs;
# Save module colors and labels for use in subsequent parts
save(MEs, moduleLabels, moduleColors, geneTree, file = "CRC-02-networkConstruction-stepByStep.RData")

#Visualizing the gene network
# One way to visualize a weighted network is to plot its heatmap. Each row and column of the heatmap
# correspond to a single gene. The heatmap can depict adjacencies or topological overlaps, with light colors denoting
# low adjacency (overlap) and darker colors higher adjacency (overlap). In addition, the gene dendrograms and module
# colors are plotted along the top and left side of the heatmap.
# Calculate topological overlap anew: this could be done more efficiently by saving the TOM
# calculated during module detection, but let us do it again here.
dissTOM = 1-TOMsimilarityFromExpr(datExpr, power = 6);
# Note that the generating the heatmap plot may take a substantial amount of time. It is possible to restrict the
# number of genes to speed up the plotting; however, the gene dendrogram of a subset of genes will often look different
# from the gene dendrogram of all genes. In the following example we restrict the number of plotted genes to 1000:
nSelect = 1000
set.seed(10);# For reproducibility, we set the random seed
select = sample(nGenes, size = nSelect);
#sample是随机取样函数，可以从多个数据集中取样，可设置重复或者非重复抽样，sample（x, size, replace = FALSE）
#即replace 如果为F（默认），则是不重复抽样，此时size不能大于x的长度；如果为T，则是重复抽样，此时size允许大于x的长度
selectTOM = dissTOM[select, select];
# There’s no simple way of restricting a clustering tree to a subset of genes, so we must re-cluster.
selectTree = hclust(as.dist(selectTOM), method = "average")
selectColors = moduleColors[select];
sizeGrWindow(9,9)# Open a graphical window
# Taking the dissimilarity to a power, say 10, makes the plot more informative by effectively changing
# the color palette; setting the diagonal to NA also improves the clarity of the plot
plotDiss = selectTOM^7;
diag(plotDiss) = NA; #diag()用于提取或替换一个矩阵的对角线,或构造一个对角矩阵
TOMplot(plotDiss, selectTree, selectColors, main = "CRC TOMplot")#利用TOMplot函数绘制TOM图

# allLLIDs = a$gene_id;
# # $ Choose interesting modules
# intModules = c("royalblue","darkorange","magenta","paleturquoise","saddlebrown","darkgreen","lightgreen","salmon","green","grey60","tan","greenyellow","white","black","orange","pink","darkgrey","midnightblue","brown","violet","blue","turquoise","darkturquoise","lightyellow","darkred","purple","yellow","darkmagenta","lightcyan","red","yellowgreen","steelblue","darkolivegreen","skyblue","paleturquoise","plum2","thistle1","pink","yellow", "violet","blue","white","darkorange","orange","brown","darkgreen","magenta","purple","red","tan","green","white","lightgreen")
# intModules = c("blue","darkorange","magenta","paleturquoise","saddlebrown","darkgreen","lightgreen","salmon","green","grey60","tan","greenyellow","white","black","orange","pink","darkgrey","midnightblue","brown","violet","blue","turquoise","darkturquoise","lightyellow","darkred","purple","yellow","darkmagenta","lightcyan","red","yellowgreen","steelblue","darkolivegreen","skyblue")
# intModules = c("midnightblue","yellowgreen","darkorange","paleturquoise","turquoise","saddlebrown","darkgreen","skyblue3","lightgreen","salmon","white","green","grey60","tan","greenyellow","plum1","black","orange","pink","darkgrey","royalblue","brown","violet","blue","mediumpurple3","cyan","lightyellow","darkred","purple","yellow","darkmagenta","lightcyan","red","yellowgreen","steelblue","darkolivegreen","skyblue","orangered4","magenta","lightcyan1","darkturquoise","lightsteelblue1","darkmagenta","sienna3","darkorange2","grey","ivory")
# for (module in intModules)
# {
# # Select module probes
# modGenes = (moduleColors==module)
# # Get their entrez ID codes
# modLLIDs = allLLIDs[modGenes];
# # Write them into a file
# fileName = paste("LocusLinkIDs-", module, ".txt", sep="");
# write.table(as.data.frame(modLLIDs), file = fileName,
# row.names = FALSE, col.names = FALSE)
# } #
# fileName = paste("LocusLinkIDs-all.txt", sep="");
# write.table(as.data.frame(allLLIDs), file = fileName,
# row.names = FALSE, col.names = FALSE)
# GOenr = GOenrichmentAnalysis(moduleColors, allLLIDs, organism = "human", nBestP = 10)

# 提取hub genes; 首先Recalculate topological overlap
TOM = TOMsimilarityFromExpr(datExpr, power = 5);
# Select module
# module = "brown";
module = "blue";
# module = "green";
# module = "yellow";
# module = "pink";
# module = "red";
# module = "magenta";
# module = "black";
# module = "turquoise";
# module = "darkturquoise";
# Select module probes
probes = names(datExpr)
inModule = (moduleColors==module);
modProbes = probes[inModule];
# Select the corresponding Topological Overlap
modTOM = TOM[inModule, inModule];
dimnames(modTOM) = list(modProbes, modProbes)
# Export the network into an edge list file VisANT can read
vis = exportNetworkToVisANT(modTOM,
file = paste("VisANTInput-", module, ".txt", sep=""),
weighted = TRUE,
threshold = 0,
)
#Because the module we interested may be rather large, we can restrict the genes in the output to say the 30 top hub genes in the module:
nTop = 30;
IMConn = softConnectivity(datExpr[, modProbes]);
top = (rank(-IMConn) <= nTop)
vis = exportNetworkToVisANT(modTOM[top, top],
file = paste("VisANTInput-", module, "-top30.txt", sep=""),
weighted = TRUE,
threshold = 0,
)


########差异甲基化区域鉴定:bumphunter########
a<-read.table(file.choose(),sep="\t",header=T,row.names = 1)#读取含染色体号、基因组位置和甲基化程度的文件
D<-as.matrix(a)#把读取进来的文件转换成矩阵形式
b<-read.table(file.choose(),sep="\t",header=T)#读取分组信息，即那些样本是癌和癌旁
#c<-read.table(file.choose(),sep="\t",header=T)
X = model.matrix(~b$status)#依据文件b进行分组
library(bumphunter)#加载"bumphunter"程序包
chr = as.factor(a$Chromosome)#把含染色体信息的这一列转换成因子
pos = a$Genomic_Coordinate#把甲基化位点的基因组位置信息赋值到变量pos
cl = clusterMaker(chr,pos,maxGap=500)#依据染色体和基因组的位置信息构建cluster，maxGap可自定义
res = bumphunter(D,X,chr=chr,pos=pos,cluster=cl,cutoff=0.1,B=0)#利用bumphunter函数寻找差异性甲基化位点
tab<-res$table
Index=(tab[1,7]-3):(tab[1,8]+3)
cols=ifelse(b$status=="normal",1,2)#利用ifelse函数判断哪些是癌和癌旁，相当于分2组
matplot(pos[Index],D[Index,,drop=TRUE],col=cols,pch=1,xlab="genomic location",ylab="Methylation",ylim=c(0,1))
#drop：是否丢弃没有数据的分组，如果为TRUE（默认），则空数据组不绘图
#ylim：表示y轴上下限
plot(pos[Index],res$fitted[Index,1],xlab="genomic location",ylab="Methylation difference",ylim=c(-1,1))
abline(h=c(-0.1,0,.1),lty=2)


########生存分析:survival########
library(survival)#加载"survival"程序包
#read file
a<-read.table(file.choose(),header=T,sep="\t")
crc_surv<-Surv(a$day,a$status)#Surv用于创建生存数据对象
#exclue the first three colums
b<-a[,4:ncol(a)]
#univariable cox regression
unicox.result <- lapply(b, function(j){coxph(crc_surv ~ j)}) #coxph 构建COX回归模型
#keep output to file
sink(file="F:/prj_training/cg.txt",options(max.print = 1000000000))
unicox.result
sink()

#lasso feature selection
library("glmnet")#加载"glmnet"程序包
library("survival")#加载"survival"程序包
a<-read.table(file.choose(),header=T,sep="\t")#读取含预后信息（生存时间和删失与否）的表达文件
x<-a[,4:ncol(a)]#exclue the first three colums
x1<-as.matrix(x)#把表达数据表转换成矩阵
cvfit = cv.glmnet(x1, Surv(a$days,a$status), family = "cox")#cv做交叉验证来确定模型，可以加上nfold=10
plot(cvfit) #绘制cv变化图
cvfit$lambda.min #最佳lambda值
cvfit$lambda.1se #一倍SE内的更简洁的模型
coef.min = coef(cvfit, s = "lambda.min") #系数
active.min = which(coef.min != 0) #选择的变量, The left vertical line in our plot shows us where the CV-error curve hits its minimum.
#The right vertical line shows us the most regularized model with CV-error within 1 standard deviation of the minimum
index.min = coef.min[active.min]
active.min #return the variable left in model
index.min  #return corresponding coefficient of variable
geneids <- colnames(x1)[active.min]#找到匹配的基因名
combine<-cbind(geneids, index.min)#把基因名和其相应系数合并起来
#rs=risk score
rs <- as.matrix(a[, geneids]) %*% as.matrix(index.min)#%*%矩阵相乘
good.prog <- (rs < median(rs))#rs中位数以下的代表预后好的组
crc_surv<-Surv(a$day,a$status)
fit <- survfit(crc_surv ~ good.prog) #survfit：创建KM生存曲线或是Cox调整生存曲线
plot(fit, lwd = 2, lty = c(1,1), col = c("red","blue"), xlab = 'Time (days)', ylab = 'Survival Probability') #lty：线条类型
legend("topright", legend=c('PI > median', 'PI < median'), lty = c(1,1), col = c("red", "blue"), lwd = 2)
title("CRC")
x.test <- as.data.frame(a[-1, ])
logrank <- survdiff(crc_surv ~ good.prog, data = x.test, rho = 0) #survdiff：用于不同组的统计检验
legend("bottom", legend='P= 1.18e-09', lwd = 2)#lwd: 线条宽度
p.median <- 1-pchisq(logrank$chisq, 1) # calculate the p-value
pi <- coxph(crc_surv ~ rs)
p.pi <- 1-pchisq(pi$score, 1)





########合并同一文件夹下各个样本的表达值########
###如果合并诸如甲基化芯片的文件，可以利用linux下的paste命令先合并所有文件，再利用python等语言提取相应的列后输出到文件
setwd("E:/TCGA/miRNA/normal/") #设定工作目录
file<-dir()#将normal文件夹下所有文件名输入file
file
N=length(file)#计算文件个数
datalist<-list()
fd=read.table(file[1],sep="\t",header=T) #读入第一个文件内容
head(fd)
gene_name=fd[,1]#提取基因名字
genematrix=c()
#循环读入基因名字并存储到genematrix
for(i in 1:length(gene_name)){
    gene=as.vector(gene_name[i])
    genematrix=rbind(genematrix,unlist(gene))
}
samplenames=c()
datamatrix=c()
#循环读入所有文件，提取每个文件的第三列进行合并，并组合到datamatrix变量中
for(i in 1:N){
    fd=read.table(file[i],sep="\t",header=T)
    datamatrix=cbind(datamatrix,fd[,3])
    filenameString=file[i]
    filenameStringlist=strsplit(filenameString, "[.]")
    samplename=filenameStringlist[[1]][2]
    samplenames=cbind(samplenames,samplename)
}
rownames(datamatrix)=genematrix
colnames(datamatrix)= samplenames
datamatrix=cbind(rownames(datamatrix),datamatrix)
colnames(datamatrix)[1]="geneSymbol"
write.table(datamatrix,file="E:/TCGA/miRNA/normal/merge_miRNA_normal_sample_read_count.txt",quote=F,sep="\t",row.names=F)
