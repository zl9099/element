#Aesthetic mappings
library(ggplot2)

#Plots and Layers
p = ggplot(mtcars, aes(x = mpg, y = wt))
p + geom_point()
p + geom_point(aes(colour = factor(cyl)))
p + geom_point(aes(y = disp))
#Add aes(colour = cyl) ======>  aes(mpg, wt, colour = cyl)
#Override aes(y = disp) ======> aes(mpg, disp)
#Remove aes(y = NULL) ======> aes(mpg)

#Setting vs. mapping
# Mapping: aes(colour = cut))
# Setting: color = "red"
p + geom_point(colour = "green")
# is different from
p + geom_point(aes(colour = "blue"))
#When "green" is mapped to colour, it is treated 
#as a regular value and scaled with the default colour scale.

#Grouping
library(nlme)
data(package="nlme")
?Oxboys
head(Oxboys)
str(Oxboys)
p = ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()
ggplot(Oxboys, aes(age, height, group = 1)) + geom_line()

#Different groups on different layers
p + geom_smooth(aes(group = Subject), method="lm", se = F)
p + geom_smooth(aes(group = 1), method="lm", se = F, size=2)

#Overriding the default grouping.
#no need to specify the group aesthetic here; the default grouping
#works because occasion is a discrete variable
boysbox = ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
boysbox
boysbox + geom_line(aes(group = Subject), colour = "blue")

#Geom
#Stat

#Position adjustments
#dodge: Adjust position by dodging overlaps to the side
#fill: Stack overlapping objects and standardise have equal height
#identity: Do not adjust position
#jitter: Jitter points to avoid overplotting
#stack: Stack overlapping objects on top of one another

ggplot(diamonds,
       aes(clarity, fill = cut)) + geom_bar(position = "stack")
ggplot(diamonds,
       aes(clarity, fill = cut)) + geom_bar(position = "fill")
ggplot(diamonds,
       aes(clarity, fill = cut)) + geom_bar(position = "dodge")



#You are limited only by your imagination!



