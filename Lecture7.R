library(ggplot2)
library(gcookbook)

#Making a Basic Histogram
?faithful
head(faithful)
ggplot(faithful, aes(x=waiting)) + geom_histogram()
w = faithful$waiting
ggplot(NULL, aes(x=w)) + geom_histogram()

ggplot(faithful, aes(x=waiting)) +
  geom_histogram(binwidth=5, fill="white", colour="black")

ggplot(faithful, aes(x=waiting)) +
  geom_histogram(fill="white", colour="black",origin=30)


#Making Multiple Histograms from Grouped Data
library(MASS)
?birthwt
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +
  facet_grid(smoke ~ .)

birthwt1 = birthwt # Make a copy of the data
# Convert smoke to a factor
birthwt1$smoke = factor(birthwt1$smoke)
levels(birthwt1$smoke)

library(plyr)
birthwt1$smoke <- revalue(birthwt1$smoke, c("0"="No Smoke", "1"="Smoke"))
ggplot(birthwt1, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +
  facet_grid(smoke ~ .)

ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +
  facet_grid(race ~ .)
#resize y
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +
  facet_grid(race ~ ., scales="free")

ggplot(birthwt1, aes(x=bwt, fill=smoke)) +
  geom_histogram(position="identity", alpha=0.4)


#Making a Density Curve
ggplot(faithful, aes(x=waiting)) + geom_density()
ggplot(faithful, aes(x=waiting)) + geom_line(stat="density") +
  expand_limits(x=0)
ggplot(faithful, aes(x=waiting)) + geom_line(stat="density")

ggplot(faithful, aes(x=waiting)) +
  geom_line(stat="density", adjust=0.25, colour="red") +
  geom_line(stat="density") +
  geom_line(stat="density", adjust=2, colour="blue")

ggplot(faithful, aes(x=waiting)) +
  geom_density(fill="blue", colour=NA, alpha=.2) +
  geom_line(stat="density") +
  xlim(35, 105)

ggplot(faithful, aes(x=waiting, y=..density..)) +
  geom_histogram(fill="blue", colour="red", size=.2) +
  geom_density(color="green") +
  xlim(35, 105)

#Making Multiple Density Curves from Grouped Data
ggplot(birthwt1, aes(x=bwt, fill=smoke)) + geom_density(alpha=.3)


#Making a Frequency Polygon
ggplot(faithful, aes(x=waiting)) + geom_freqpoly()
ggplot(faithful, aes(x=waiting)) + geom_freqpoly(binwidth=4)

#Making a Basic Box Plot
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot()
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot(width=0.5)
ggplot(birthwt, aes(x=factor(race), y=bwt)) +
  geom_boxplot(outlier.size=15, outlier.shape=24)

ggplot(birthwt, aes(x=1, y=bwt)) + geom_boxplot()
#Adding Notches to a Box Plot
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot(notch=TRUE)
#Adding Means to a Box Plot
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot() +
  stat_summary(fun.y="mean", geom="point", shape=23, size=10, fill="BLUE")

#Making a Violin Plot
p = ggplot(heightweight, aes(x=sex, y=heightIn))
p + geom_violin()
p + geom_violin(trim=FALSE)
p + geom_violin(adjust=2)

#Making a Density Plot of Two-Dimensional Data
p = ggplot(faithful, aes(x=eruptions, y=waiting))
p + geom_point() + stat_density2d()
p + stat_density2d(aes(colour=..level..))
p + stat_density2d(aes(fill=..density..), geom="raster", contour=FALSE)
p + geom_point() +
  stat_density2d(aes(alpha=..density..), geom="tile", contour=FALSE)
p + stat_density2d(aes(fill=..density..), geom="raster",
                   contour=FALSE, h=c(0.5,5))
