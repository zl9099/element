#Annoucement: 
#image quality
#new textbook: R Graphic Cookbook

library(gcookbook) #For the data set

#Bar Graph

#Making a Basic Bar Graph
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
?BOD
str(BOD)
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat="identity")
ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", fill="lightblue", colour="black")

#Grouping Bars Together
?cabbage_exp
cabbage_exp
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(position="dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(position="stack",stat="identity")
#dodge: Adjust position by dodging overlaps to the side
#fill: Stack overlapping objects and standardise have equal height
#identity: Do not adjust position
#jitter: Jitter points to avoid overplotting
#stack: Stack overlapping objects on top of one another

#Making a Bar Graph of Counts
ggplot(diamonds, aes(x=cut)) + geom_bar()

#Using Colors in a Bar Graph
str(uspopchange)
upc = subset(uspopchange, rank(Change)>40)
ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat="identity")
ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
  geom_bar(stat="identity", colour="black")

#Coloring Negative and Positive Bars Differently
?climate
str(climate)
csub = subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos = csub$Anomaly10y >= 0
str(csub)
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity") +
  scale_fill_manual(values=c("#669933", "#FFCC66"), guide=FALSE)

#Adjusting Bar Width and Spacing
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
ggplot(pg_mean, aes(x=group, y=weight)) + 
  geom_bar(stat="identity", width=0.5)
ggplot(pg_mean, aes(x=group, y=weight)) + 
  geom_bar(stat="identity", width=1)
#Grouped data
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position="dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))

#Adding Labels to a Bar Graph
# Below the top
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white")
# Above the top
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=0)

# Adjust y limits to be a little higher
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)

# Map y positions slightly above bar top 
#- y range of plot will auto-adjust
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=Weight+0.1, label=Weight))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Weight), vjust=1.5, colour="black",
            position=position_dodge(0.9), size=10)

