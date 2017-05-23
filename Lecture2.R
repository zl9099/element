#Lecture 2
#minghaowu_2015@163.com
setwd("~/documents/R programming/ggplot2")
library(ggplot2)
attach(mpg)
head(mpg)

#Review
qplot(displ, hwy, data = mpg, colour = factor(cyl))
qplot(displ, hwy, data = mpg, 
      colour = factor(cyl),
      geom=c("smooth","point"),
      method="lm")
qplot(displ, hwy, data=mpg, facets = . ~ year) + geom_smooth()


#Save
p = qplot(displ, hwy, data = mpg, colour = factor(cyl))
summary(p)
save(p, file = "plot.rdata")
load("plot.rdata")
ggsave("plot.png", width = 5, height = 5)


#Build a plot layer by layer
p = ggplot(diamonds, aes(carat, price, colour = cut))
p
p = p + layer(geom = "point")
#ggplot(diamonds, aes(carat, price, colour = cut)) + layer(geom = "point")
p
#layer(geom, geom_params, stat, stat_params, data, mapping, position)
p <- ggplot(diamonds, aes(x = carat))
p <- p + layer(
  geom = "bar",
  geom_params = list(fill = "steelblue"),
  stat = "bin",
  stat_params = list(binwidth = 0.5)
)
p
#basic form
#geom_XXX(mapping, data, ..., geom, position)
#stat_XXX(mapping, data, ..., stat, position)
#Example 1
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point()
#is equivalent to
qplot(sleep_rem / sleep_total, awake, data = msleep)
#Example 2
qplot(sleep_rem / sleep_total, 
      awake, data = msleep,
      geom = c("point", "smooth"))
#is equivalent to
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point() + geom_smooth()
#is equivalent to
plot = ggplot(msleep, aes(sleep_rem / sleep_total, awake))
plot = plot + geom_point() + geom_smooth()
plot
#Example 3 (cont.)
bestfit = geom_smooth(method = "lm", 
                      se = T,
                      colour = "steelblue",
                      alpha=0.5,
                      size = 2)
qplot(sleep_rem, sleep_total, data = msleep) + bestfit
qplot(displ, hwy, data=mpg, facets = . ~ year) + bestfit


#Transform the data in the plot using %+%
p = ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point(size=5)
p
mtcars = transform(mtcars, mpg = mpg ^ 2)
p %+% mtcars


#Aesthetic mappings
qplot(color, price, data = diamonds, geom = "boxplot",fill=I("blue"))
