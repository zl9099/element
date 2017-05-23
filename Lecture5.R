library(ggplot2)
library(gcookbook)

#Making a Basic Line Graph
BOD
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
BOD1 = BOD # Make a copy of the data
BOD1$Time = factor(BOD1$Time)
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()

ggplot(BOD, aes(x=Time, y=demand)) + 
  geom_line() + 
  ylim(0, max(BOD$demand))

#Adding Points to a Line Graph
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()

#Making a Line Graph with Multiple Lines
library(plyr)
tg = ddply(ToothGrowth, c("supp", "dose"), 
           summarise, length=mean(len))
tg
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()
ggplot(tg, aes(x=dose, y=length, linetype=supp)) + geom_line()
ggplot(tg, aes(x=dose, y=length)) + geom_line()
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line() +
  geom_point(size=4, shape=21)


#Changing the Appearance of Lines
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="dashed", size=1, colour="blue")

#Changing the Appearance of Points
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line() +
  geom_point(size=4, shape=22, colour="red", fill="blue")

#Making a Graph with a Shaded Area
sunspotyear <- data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area()
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) +
  geom_area(colour="black", fill="blue", alpha=0.2)
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_line()

#Making a Stacked Area Graph
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(colour="black", size=0.2, alpha=0.4)
ggplot(uspopage, aes(x=Year, y=Thousands, 
                     fill=AgeGroup, order=desc(AgeGroup))) +
  geom_area(colour=NA, alpha=0.4) +
  geom_line(position="stack", size=0.2)

#Making a Proportional Stacked Area Graph
uspopage_prop = ddply(uspopage, "Year", transform,
                       Percent = Thousands / sum(Thousands) * 100)
ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4)
ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup)) +
  geom_area(colour=NA, size=.2, alpha=.4) + 
  geom_line(position="stack", size=0.2)

#Adding a Confidence Region
clim = subset(climate, Source == "Berkeley",
               select=c("Year", "Anomaly10y", "Unc10y"))
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y),
              alpha=0.2) + 
  geom_line()

ggplot(clim, aes(x=Year, y=Anomaly10y)) + 
  geom_line() + #main
  geom_line(aes(y=Anomaly10y-Unc10y), 
            colour="grey50", linetype="dashed") +#lower
  geom_line(aes(y=Anomaly10y+Unc10y), 
            colour="grey50", linetype="dashed") #upper 
