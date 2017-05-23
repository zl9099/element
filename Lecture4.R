#Chapter 5 Toolbox
#This chapter lists some of the many geoms and 
#stats included in ggplot2, broken down by their purpose.
#but it does not describe each geom and stat in detail.

library(ggplot2)
#Basic plot types
df = data.frame(
  x = c(3, 1, 5),
  y = c(2, 4, 6),
  label = c("a","b","c")
)
p = ggplot(df, aes(x, y, label = label)) + xlab(NULL) + ylab(NULL)
p + geom_point() + opts(title = "geom_point")
p + geom_bar(stat="identity") #counting #The identity stat leaves the data unchanged
p + geom_line()
p + geom_area()
p + geom_path()
p + geom_text()
p + geom_tile()
p + geom_polygon()

#Displaying distributions
depth_dist = ggplot(diamonds, aes(depth)) + xlim(58, 68)
depth_dist + geom_histogram(aes(y = ..density..), binwidth = 0.1) + facet_grid(cut ~ .)
depth_dist + geom_histogram(aes(fill = cut), binwidth = 0.1, position = "fill")
depth_dist + geom_freqpoly(aes(y = ..density.., colour = cut), binwidth = 0.1)
#Both the histogram and frequency polygon geom use stat_bin.
#This statistic produces two output variables count and density.

qplot(cut, depth, data=diamonds, geom="boxplot")
qplot(carat, depth, data=diamonds, geom="boxplot",
      group = round(carat, 1), xlim = c(0, 3)) #typo

#Dealing with overplotting
df = data.frame(x = rnorm(2000), y = rnorm(2000))
#small dataset
norm = ggplot(df, aes(x, y))
norm + geom_point()
norm + geom_point(shape = 1) #from 0 to 112
norm + geom_point(shape = ".") #pixel sized

#large dataset
#typo: norm + geom_point(colour = alpha("black", 1/3))
norm + geom_point(alpha = I(1/3))
norm + geom_point(alpha = I(1/5))
norm + geom_point(alpha = I(1/10))

#two more approaches
library(hexbin)
d = ggplot(diamonds, aes(carat, price)) + xlim(1,3)
d + stat_bin2d()
d + stat_bin2d(bins = 10)
d + stat_bin2d(binwidth=c(0.02,200)) #binwidth =c(width, height)
d + stat_binhex()
d + stat_binhex(bins = 10)
d + stat_binhex(binwidth=c(0.02, 200))

#Surface plots
library(plot3D)
example(persp3D)
example(surf3D)
example(colkey)
example(plot.plist)
example(ImageOcean)

#Drawing maps
library(maps)
data(us.cities)
big_cities = subset(us.cities, pop > 500000)
qplot(long, lat, data = big_cities) + borders("state", size = 0.5)
tx_cities = subset(us.cities, country.etc == "TX")
ggplot(tx_cities, aes(long, lat)) + 
  borders("county", "texas", colour = "grey70") + 
  geom_point(alpha=I(0.5))



