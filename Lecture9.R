#Controlling the Overall Appearance of Graphs

library(ggplot2)
library(gcookbook)

#Setting the Title of a Graph
p = ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
p + ggtitle("Age and Height of Schoolchildren")
p + ggtitle("Age and Height\nof Schoolchildren")# Use \n for a newline
p + annotate("text", 
             x=mean(range(heightweight$ageYear)), 
             y=Inf,
             label="Age and Height of Schoolchildren", 
             vjust=1.5, size=6)# Use a text annotation instead

#Changing the Appearance of Text
p + theme(axis.title.x=element_text(size=16, lineheight=.9, family="Times",
                                    face="bold.italic", colour="red"))
p + ggtitle("Age and Height\nof Schoolchildren") +
  theme(plot.title=element_text(size=rel(1.5), lineheight=.9, family="Times",
                                face="bold.italic", colour="red"))

p + annotate("text", x=15, y=53, label="Some text", size = 7, family="Times",
             fontface="bold.italic", colour="red")
p + geom_text(aes(label=weightLb), size=4, family="Times", colour="red",fontface="bold")
# For text geoms, font size is in mm

#check the textbook

#Using Themes
p + theme_grey()
p + theme_bw()
p
theme_set(theme_bw())
p
theme_set(theme_grey())
p

#Changing the Appearance of Theme Elements
p = ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + 
  geom_point()
p + theme(
  panel.grid.major = element_line(colour="red"),
  panel.grid.minor = element_line(colour="red", linetype="dashed", size=0.2),
  panel.background = element_rect(fill="lightblue"),
  panel.border = element_rect(colour="blue", fill=NA, size=2))
# Options for text items
p + ggtitle("Plot title here") +
  theme(
    axis.title.x = element_text(colour="red", size=14,family="Times",face="bold"),
    axis.text.x = element_text(colour="blue"),
    axis.title.y = element_text(colour="red", size=14, angle = 90),
    axis.text.y = element_text(colour="blue"),
    plot.title = element_text(colour="red", size=20, face="bold"))
# Options for the legend
p + theme(
  legend.background = element_rect(fill="grey85", colour="red", size=1),
  legend.title = element_text(colour="blue", face="bold", size=14),
  legend.text = element_text(colour="red"),
  legend.key = element_rect(colour="green",fill="black", size=0.25))
# Options for facets
p + facet_grid(sex ~ .) + theme(
  strip.background = element_rect(fill="pink"),
  strip.text.y = element_text(size=14, angle=-90, face="bold"))
# strip.text.x is the same, but for horizontal facets


#Creating Your Own Themes
mytheme = 
  theme_bw() +
  theme(text = element_text(colour="red"),
        axis.title = element_text(size = rel(1.25)),
        plot.title = element_text(size = rel(3)))
p = ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point() + ggtitle("some title")
q = ggplot(mtcars,aes(x=wt,y=mpg)) + geom_point() + ggtitle("some title")
p + mytheme
q + mytheme


#Hiding Grid Lines
p + theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
# Hide the vertical grid lines (which intersect with the x-axis)
p + theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())






