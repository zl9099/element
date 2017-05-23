#announcement:
#source code uploaded

library(ggplot2)
library(gcookbook)

#annotate(geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL,
#           ymin = NULL, ymax = NULL, ...)

#geom: text, segment, rect, pointrange

p = ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point()
p
p + annotate("text", x=3, y=48, label="Group 1") +
  annotate("text", x=4.5, y=66, label="Group 2")

# 1=plain, 2=bold, 3=italic, 4=bold-italic
p + annotate("text", x=3, y=48, label="Group 1", family="serif",
             fontface="italic", colour="darkred", size=7) +
  annotate("text", x=4.5, y=66, label="Group 2", family="serif",
           fontface="bold", colour="darkred", size=20, alpha=0.2) + 
  annotate("text", x=2.5, y=26, label="Group 3", family="serif",
           fontface=4, colour="blue", size=7)

#install.packages("extrafont")
#library(extrafont)
#font_import()
library(extrafont)
fonts()

#If the axes are continuous
p + annotate("text", x=-Inf, y=Inf, label="Upper left", 
             hjust=-.2, vjust=2) +
  annotate("text", x=mean(range(faithful$eruptions)), y=-Inf, 
           vjust=-0.4,
           label="Bottom middle")


#Using Mathematical Expressions in Annotations
#plotmath
p = ggplot(data.frame(x=c(-3,3)), aes(x=x)) + 
  stat_function(fun = dnorm)
p
p + annotate("text", x=2, y=0.3, parse=TRUE,
             label="frac(1, sqrt(2 * pi)) * e ^ {-x^2 / 2}")
p + annotate("text", x=0, y=0.05, parse=TRUE, size=4,
             label="'Function: ' * y==frac(1, sqrt(2*pi)) * e^{-x^2/2}")


#Adding Lines
p = ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + 
  geom_point()
p
# Add horizontal and vertical lines
p + geom_hline(yintercept=60) + geom_vline(xintercept=14)
## Add angled line
p + geom_abline(intercept=37.4, slope=1.75)

library(plyr)
hw_means = ddply(heightweight, "sex",
                 summarise, heightIn=mean(heightIn))
hw_means
p + geom_hline(aes(yintercept=heightIn, colour=sex), data=hw_means,
               linetype="dashed", size=1)

summary(PlantGrowth)
pg = ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_point()
pg
pg + geom_vline(xintercept = which(levels(PlantGrowth$group)=="ctrl"))

#Adding Line Segments and Arrows
p = ggplot(subset(climate, Source=="Berkeley"), 
           aes(x=Year, y=Anomaly10y)) +
  geom_line()
p
p + annotate("segment", x=1950, xend=1980, y=-0.25, yend=-0.25)
library(grid)
p + annotate("segment", x=1850, xend=1820, y=-0.8, yend=-0.95, 
             colour="blue",
             size=2, arrow=arrow()) +
  annotate("segment", x=1950, xend=1980, y=-0.25, yend=-0.25,
           arrow=arrow(ends="both", angle=60, length=unit(0.5,"cm")))


#Adding a Shaded Rectangle
p = ggplot(subset(climate, 
                  Source=="Berkeley"), aes(x=Year,y=Anomaly10y)) +
  geom_line()
p
p + annotate("rect", xmin=1950, xmax=1980, 
             ymin=-Inf, ymax=Inf, alpha=.1,
             fill="blue")

#Highlighting an Item
pg = PlantGrowth # Make a copy of the PlantGrowth data
pg$hl = "no" # Set all to "no"
pg$hl[pg$group=="trt2"] = "yes" ## If group is "trt2", set to "yes"
ggplot(pg, aes(x=group, y=weight, fill=hl)) + geom_boxplot() +
  scale_fill_manual(values=c("grey85", "#FFDDCC"))

#Adding Error Bars
ce = subset(cabbage_exp, Cultivar == "c39")
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_bar(fill="white", colour="black",stat="identity") +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2)

ggplot(ce, aes(x=Date, y=Weight)) +
  geom_line(aes(group=1)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2)

#Adding Annotations to Individual Facets
#base plot
p = ggplot(mpg, aes(x=displ, y=hwy)) + 
  geom_point() + facet_grid(. ~ drv)
p
f_labels = data.frame(drv = c("4", "f", "r"),
                      label = c("4wd", "Front", "Rear"))
p + geom_text(x=6, y=40, aes(label=label), data=f_labels)
p + annotate("text", x=6, y=42, label="label text")

#for the sake of completeness
#something not covered in textbook: Pointrange
#http://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
p = ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + annotate("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28,
             colour = "red", size = 1.5)












