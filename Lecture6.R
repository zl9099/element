library(gcookbook)
library(ggplot2)

#Making a Basic Scatter Plot
head(heightweight[, c("ageYear", "heightIn")])
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(shape=11,size=20)


#Grouping Data Points by a Variable Using Shape or Color
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + 
  geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex)) + 
  geom_point()

#Grouping Data Points with certain standard
hw = heightweight
hw$weightGroup = cut(hw$weightLb, breaks=c(-Inf, 100, Inf),
                      labels=c("< 100", ">= 100"))
head(hw)
ggplot(hw, aes(x=ageYear, y=heightIn, shape=sex, fill=weightGroup)) +
  geom_point(size=2.5) +
  scale_shape_manual(values=c(21, 24)) +
  scale_fill_manual(values=c(NA, "black"),
                    guide=guide_legend(override.aes=list(shape=21)))


#Mapping a Continuous Variable to Color or Size
ggplot(heightweight, 
       aes(x=ageYear, y=heightIn, colour=weightLb)) + 
  geom_point()
ggplot(heightweight, 
       aes(x=ageYear, y=heightIn, size=weightLb, colour=sex)) + 
  geom_point()

#Dealing with Overplotting
sp = ggplot(diamonds, aes(x=carat, y=price))
sp + geom_point()
sp + geom_point(alpha=.1) #semitransparent
sp + stat_bin2d()
sp + stat_bin2d(bins=50) +
  scale_fill_gradient(low="lightblue", 
                      high="red", limits=c(0, 6000))
library(hexbin)
sp + stat_binhex() +
  scale_fill_gradient(low="lightblue", high="red",
                      limits=c(0, 8000))
sp + stat_binhex() +
  scale_fill_gradient(low="lightblue", high="red",
                      breaks=c(0, 250, 500, 1000, 2000, 4000, 6000),
                      limits=c(0, 6000))

sp1 = ggplot(ChickWeight, aes(x=Time, y=weight))
sp1 + geom_point()
sp1 + geom_point(position="jitter")
# Could also use geom_jitter(), which is equivalent
sp1 + geom_point(position=position_jitter(width=.5, height=0))


#Adding Fitted Regression Model Lines
sp = ggplot(heightweight, aes(x=ageYear, y=heightIn))
?stat_smooth
sp + geom_point() + stat_smooth(method=lm)
#By default, stat_smooth() also adds 
#a 95% confidence region for the regression fit
sp + geom_point() + stat_smooth(method=lm, level=0.99)
sp + geom_point() + stat_smooth(method=lm, se=FALSE)
sp + geom_point() + stat_smooth()
#loess (locally weighted polynomial) curve

sps = ggplot(heightweight, 
              aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
sps + geom_smooth()

#Adding Fitted Lines from an Existing Model
#You have already created a fitted regression model object

#model
model = lm(heightIn ~ ageYear + I(ageYear^2), heightweight)
#range
xmin = min(heightweight$ageYear)
xmax = max(heightweight$ageYear)
predicted = data.frame(ageYear=seq(xmin, xmax, length.out=100))
predicted$heightIn = predict(model, predicted)

sp = ggplot(heightweight, aes(x=ageYear, y=heightIn)) +
  geom_point()
sp + geom_line(data=predicted, size=1)



#Adding Fitted Lines from Multiple Existing Models

predictvals = function(model, xvar, yvar, 
                       xrange=NULL, samples=100, ...) {
  if (is.null(xrange)) {
    if (any(class(model) %in% c("lm", "glm")))
      xrange = range(model$model[[xvar]])
    else if (any(class(model) %in% "loess"))
      xrange = range(model$x)
  }
  newdata = data.frame(x = seq(xrange[1], 
                               xrange[2], length.out = samples))
  names(newdata) = xvar
  newdata[[yvar]] = predict(model, newdata = newdata, ...)
  newdata
}

library(plyr)
make_model <- function(data) {
  lm(heightIn ~ ageYear, data)
}
models = dlply(heightweight, "sex", .fun = make_model)
predvals = ldply(models, .fun=predictvals, 
                 xvar="ageYear", yvar="heightIn")
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
  geom_point() + geom_line(data=predvals)


#Adding Annotations with Model Coefficients
model <- lm(heightIn ~ ageYear, heightweight)
pred = predictvals(model, "ageYear", "heightIn")
sp = ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point() +
  geom_line(data=pred)
sp + annotate("text", label="r^2=0.42", x=16.5, y=52)
sp + annotate("text", label="r^2 == 0.42", 
              parse = TRUE, x=16.5, y=52)
#plotmath

#Adding Marginal Rugs to a Scatter Plot
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_point() + geom_rug()

#Labeling Points in a Scatter Plot
?countries
sp = ggplot(subset(countries, Year==2009 & healthexp>2000),
             aes(x=healthexp, y=infmortality)) + 
  geom_point()
sp + annotate("text", x=4350, y=5.4, label="Canada") +
  annotate("text", x=7400, y=6.8, label="USA")
sp + geom_text(aes(label=Name), size=4)
sp + geom_text(aes(y=infmortality+0.1, label=Name), 
               size=4, vjust=0)
sp + geom_text(aes(x=healthexp+100, label=Name), 
               size=4, hjust=0)


#Creating a Balloon Plot
cdat = subset(countries, Year==2009 &
                 Name %in% c("Canada", "Ireland", 
                             "United Kingdom", "United States",
                             "New Zealand", "Iceland", 
                             "Japan", "Luxembourg",
                             "Netherlands", "Switzerland"))
p = ggplot(cdat, aes(x=healthexp, y=infmortality, size=GDP)) +
  geom_point(shape=21, colour="black", fill="cornsilk")
p
p + scale_size_area(max_size=15)

#Making a Scatter Plot Matrix
c2009 = subset(countries, Year==2009,
                select=c(Name, GDP, laborrate, 
                         healthexp, infmortality))
plot(c2009[,2:5])
pairs(c2009[,2:5])
