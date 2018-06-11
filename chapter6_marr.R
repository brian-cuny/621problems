library(tidyverse)
library(faraway)
library(simex)
library(caret)
library(car)

data(caution, package='alr3')
View(caution)

lmod <- lm(y ~ x1 + x2, caution)
plot(lmod)

non <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\nonlinearx.txt', delim='\t')

l.mod.2 <- lm(y ~ x1 + x2, non)
plot(l.mod.2)

food.data <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\nyc.csv') %>%
  select(-Case) %>%
  mutate(East = East) %>%
  column_to_rownames('Restaurant')

l.test.1 <- lm(Price ~ Food + Decor + Service + East, food.data)
summary(l.test.1)


termplot(l.test.1, partial.resid=TRUE, smooth=panel.smooth, term=1:2) #plot based on the given terms 1:4 regression on all terms, 3 alone - just service term

avPlots(l.test.1) #each plot minus the effect of the other terms


l.test.2 <- lm(Price ~ Service, food.data)
termplot(l.test.2, partial.resid=TRUE, smooth=panel.smooth, term=1)


defect <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\defects.txt', delim='\t')

l.defect.1 <- lm(Defective ~ Temperature + Density + Rate, defect)

termplot(l.defect.1, partial.resid=TRUE, smooth=panel.smooth, term=1:3)

pairs(Defective ~ Temperature + Density + Rate, defect)

car::avPlots(l.defect.1)
plot(l.defect.1)

inverseResponsePlot(l.defect.1)

l.defect.2 <- lm(sqrt(Defective) ~ Temperature + Density + Rate, defect)
pairs(sqrt(Defective) ~ Temperature + Density + Rate, defect)
plot(l.defect.2)

summary(l.defect.2)

cor(defect[, c(-1, -5)])

l.defect.3 <- lm(sqrt(Defective) ~ Temperature + Density, defect)
termplot(l.defect.3, partial.resid=TRUE, smooth=panel.smooth, term=1:2)

inverseResponsePlot(l.defect.3)

summary(l.defect.3)


MASS::boxcox(l.defect.1, plotit=T, lambda=seq(0, 1, by=0.1))


# magazines ---------------------------------------------------------------

magazines <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\magazines.csv')

pairs(AdRevenue ~ AdPages + SubRevenue + NewsRevenue, magazines)

l.mag.1 <- lm(AdRevenue ~ AdPages + SubRevenue + NewsRevenue, magazines)
plot(l.mag.1)

termplot(l.mag.1, partial.resid=TRUE, smooth=panel.smooth, term=1:3)

l.mag.2 <- lm(AdRevenue ~ log(AdPages) + log(SubRevenue) + log(NewsRevenue), magazines)
plot(l.mag.2)

pairs(log(AdRevenue) ~ log(AdPages) + log(SubRevenue) + log(NewsRevenue), magazines)

inverseResponsePlot(l.mag.2)

l.mag.3 <- lm(log(AdRevenue) ~ log(AdPages) + log(SubRevenue) + log(NewsRevenue), magazines)
plot(l.mag.3)

inverseResponsePlot(l.mag.3)

summary(l.mag.3)

termplot(l.mag.3, partial.resid=TRUE, smooth=panel.smooth, term=1:3)

l.mag.4 <- lm(log(AdRevenue) ~ log(AdPages) + log(SubRevenue), magazines)
plot(l.mag.4)

summary(l.mag.4)

termplot(l.mag.4, partial.resid=TRUE, smooth=panel.smooth, term=1:2)

avPlots(l.mag.4) #each plot minus the effect of the other terms

summary(car::powerTransform(cbind(AdPages, SubRevenue, NewsRevenue) ~ 1, magazines)) #transform all predictors then -> inverseREsponsePlot

#Type 2 powerTransform all
summary(car::powerTransform(cbind(AdRevenue, AdPages, SubRevenue, NewsRevenue) ~ 1, magazines))



# multicollineartiy -------------------------------------------------------

bridge <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\bridge.txt', delim='\t')

pairs(bridge[, -1])

summary(car::powerTransform(cbind(Time, DArea, CCost, Dwgs, Length, Spans) ~ 1, bridge))

pairs(log(bridge[, -1]))

l.bridge <- lm(log(Time) ~ log(DArea) + log(CCost) + log(Dwgs) + log(Length) + log(Spans), bridge)
summary(l.bridge)
plot(l.bridge)

termplot(l.bridge, partial.resid=TRUE, smooth=panel.smooth, term=5)

mmps(l.bridge)

avPlots(l.bridge) 

cor(log(bridge[, c(-1, -2)]))

x <- model.matrix(l.bridge)[, -1]
faraway::vif(x) #variance increases as colinearity increases. the cutoff of 5 is used to determine whether this is problematic.



