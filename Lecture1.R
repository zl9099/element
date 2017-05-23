#Referrence Book

#ggplot2: Elegant Graphics for Data Analysis (Use R!): 
#http://www.amazon.com/ggplot2-Elegant-Graphics-Data-Analysis/dp/0387981403

#R Graphics Cookbook: 
#http://www.cookbook-r.com/Graphs/
#http://www.amazon.com/R-Graphics-Cookbook-Winston-Chang/dp/1449316956




#install.packages("ggplot2")
library(ggplot2)


#Scatter Plot
plot(mtcars$wt, mtcars$mpg)

#Line Graph
plot(pressure$temperature, pressure$pressure, type="l")
points(pressure$temperature, pressure$pressure) #add points

lines(pressure$temperature, pressure$pressure/2, type="l",col="red")
points(pressure$temperature, pressure$pressure/2,col="red")

#Bar Graph
barplot(table(mtcars$cyl))

#Histogram
hist(mtcars$mpg, breaks=10)

#Box Plot
boxplot(mtcars$mpg)

#Plotting a Function Curve
curve(x^3 - 5*x, from=-4, to=4)
myfun <- function(xvar) {
  1/(1 + exp(-xvar + 10))
}
curve(myfun(x), from=0, to=20)


#Basic use
dat=diamonds
qplot(carat, price, data = diamonds)
qplot(log(carat), log(price), data = diamonds)
qplot(carat, x * y * z, data = diamonds)

#Colour, size, shape and other aesthetic attributes
dsmall = diamonds[sample(nrow(diamonds), 100), ]
qplot(carat, price, data = dsmall, colour = color)
qplot(carat, price, data = dsmall, shape = cut)
#make a semi-transparent colour, from 0 to 1
qplot(carat, price, data = diamonds, alpha= I(1/10))
qplot(carat, price, data = diamonds, alpha = I(1/100))
qplot(carat, price, data = diamonds, alpha = I(1/200))


#Plot geoms

#geom = "point" draws points to produce a scatterplot. This is the default
#when you supply both x and y arguments to qplot().

#geom = "smooth" fits a smoother to the data and displays the smooth and
#its standard error.

#geom = "boxplot" produces a box-and-whisker plot to summarise the
#distribution of a set of points.

#geom = "path" and geom = "line" draw lines between the data points.
#Traditionally these are used to explore relationships between time and
#another variable, but lines may be used to join observations connected in
#some other way.

#geom = "histogram"
#geom = "freqpoly"
#geom = "density"
#geom = "bar"

qplot(carat, price, data = dsmall, geom = c("point", "smooth"))

library(splines)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "lm")
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "lm", formula = y ~ ns(x,5))

#color, fill
qplot(color, price, data = dsmall, geom = "boxplot")
qplot(color, price, data = diamonds, geom = "boxplot",fill=I("blue"))
qplot(color, price, data = dsmall, geom = "boxplot",size=2)
#geom_boxplot()
qplot(color, price, 
      data = dsmall, 
      geom = "boxplot") + geom_boxplot(outlier.colour = "green", 
                                       outlier.size = 10,
                                       fill = "red", 
                                       colour = "blue",
                                       size=2)


qplot(carat, data = diamonds, geom = "histogram",colour=color,fill=color)
qplot(carat, data = diamonds, geom = "density")
qplot(carat, data = diamonds, geom = "density", color = color)
qplot(carat, data = diamonds, geom = "density", fill = color)

qplot(color, data = diamonds, geom = "bar",fill=color)

qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

qplot(unemploy / pop, uempmed, data = economics, geom = c("point", "path"))
year = function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics,
      geom = "path", colour = year(date))



