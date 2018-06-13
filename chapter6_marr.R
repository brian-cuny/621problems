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


# case study --------------------------------------------------------------

wine <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\Bordeaux.csv')

l.wine.1 <- lm(log(Price) ~ log(ParkerPoints) + log(CoatesPoints) + P95andAbove + FirstGrowth + CultWine + Pomerol + VintageSuperstar, wine)
l.wine.2 <- lm(Price ~ ParkerPoints + CoatesPoints + P95andAbove + FirstGrowth + CultWine + Pomerol + VintageSuperstar, wine)
summary(l.wine.1)

pairs(log(wine[, c(2, 3, 4)])) #Linear Relationship -- There must be a linear relationship between independent variables and predictors, use this to verify.

plot(l.wine.1) #Multivariate Normality -- residuals are normally distributed. Normal QQ plot shows this. Pattern indicates a transformation needs to occur
#Short tails are generally OK, long tails are a problem.

car::residualPlots(l.wine.2) #Homoscedasticity -- constant variance across each predictor and all of them together

faraway::vif(model.matrix(l.wine.1)[, -1]) #No Multicollinearity -- cannot have variables that are too highly correlated. Above 5 is bad, if colinearity is too large,
# then SE is increased. The value here indicates how much larger the range is than it would be with no correlation.
cor(wine[, c(-1, -2)]) #Check original correlation

car::avPlots(l.wine.1) #Graph of each predictor against output, holding the other variables constant (allows for viewing like a simple linear regression)
#can be used to find outliers, points to inspect, and heteroscedacity

termplot(l.wine.1, partial.resid=TRUE, smooth=panel.smooth, term=1:7) #Plots each predictor against partial of response variable. Useful for finding transformations
#that better fit data. If smooth line is greatly different from lm line, it may be need to be transformed. Term determines which variables to include. If terms are done
#one at a time, then you end up with avPlots.

car::mmps(l.wine.1) #Helps determine whether model fit is good. If it follows the line in all cases, then the model is good. If not, there may be outliers or need
#for transformation. What is the difference between this and termplot?


summary(car::powerTransform(cbind(Price, ParkerPoints, CoatesPoints) ~ 1, wine)) #Used to determine transformations on all variables at once. 
summary(car::powerTransform(cbind(ParkerPoints, CoatesPoints) ~ 1, wine)) #Can also be done without response variable, followed by inverseResponsePlot
car::inverseResponsePlot(l.wine.1) #After transforming predictors, use to determine transformation (if any) on response variable
MASS::boxcox(l.wine.1, plotit=T, lambda=seq(-1, 2, by=0.1)) #Used to determine if response variable needs to be transformed. Want 1.0 to be above 95%

anova(l.wine.1) #Useful for examining models where a x*y model has been fit. Can determine whether there is relationship between slopes of two predictors

anova(l.wine.1, l.wine.2) #Useful for determining whether there is a statistically significant difference between models.


#6.3

#a. This model is not valid. The normal plot shows long tails on the upper end, thus the residuals are not nearly normal. The residual plot does not show data random
#placed either. 

#b. This indicates that there is not homoscedasticty. This is a violation of assumptions and means the model is not valid. It could indicate that a transfomration
#is needed.

#c. Based on the leverage graph, point 223 is a bad leverage point. 

#d. This is a valid model. The scatterplot demonstrates a linear relationship between the response and the predictors. The Normal QQ plot is mostly normal indicating
#multivariate normality. Multi-collinearity may be a concern as several predictors are highly correlated. The marginal model plots demonstrate homoscadecity.

#e. An anova test on the two models would indicate if there is a significant difference between the two plots. Considering their similarity and the removeal of
#2 statisticall insignificant predictors, it is likely that the models will not be statisticatlly different.

#f. A dummy variable that represents whether or not a vehicle is a BMW could be added to the model and then refit. 

#6.5

golf <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\pgatour2006.csv')

l.golf.1 <- lm(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + BounceBack + PuttsPerRound, golf)

pairs(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + BounceBack + PuttsPerRound, golf)

plot(l.golf.1)

car::residualPlots(l.golf.1)

termplot(l.golf.1, partial.resid=TRUE, smooth=panel.smooth, term=1:7) 

summary(l.golf.1)

#a
#I do. The model simply using all the predictors is not valid while transforming the response variable with log is. There appears to be some room for improvement
#that may allow better fitting of the individual predictors. Whether we want to introduce that level of complexity is worth more exploration but the simple
#log transformation is great to begin with.


#b
#I will use backwards selection to eliminate the unneeded predictors

l.golf.2 <- lm(log(PrizeMoney) ~ GIR + BirdieConversion + Scrambling, golf)
summary(l.golf.2)

pairs(log(PrizeMoney) ~ GIR + BirdieConversion + Scrambling, golf)
#Linear Relationship

car::residualPlots(l.golf.2)
#There appears to be some minor heteroscedasticity, further investigation may be needed

plot(l.golf.2) #Multivariate Normality

faraway::vif(model.matrix(l.golf.2)[, -1])
#No multi-collinearity problems

termplot(l.golf.2, partial.resid = TRUE, smooth=panel.smooth, term=1:3)
#Predictors fairly matched

#c
#There is one odd point (185) that should be examined but otherwise this is a strong model. This point appeared in all 3 added-variable plots as an influential point
#It also appeared as an outlier in the residual plot and as the higest quantile point in the normal qq plot
#However, it is not past cook's distance, so it may just be an outlier


#d
#It appears as though there may be a need for transformations of some sort on the predictors as their marginal plots don't line up exactly. 
#There could also be confounding variables, linking prize money to performance seems odd to me. Isn't it likely that better golfers perform better and they 
#earn more money from their tournaments? It seems like looking at worldwide rankings may be a smarter idea.


#e
#Backwards steps should be done one at a time. The regression slope will change based on how highly correlated the removed term is with another term. This may cause
# a left over term to "pick up the slack" left by the removed term, which leads it to become statistically significant.




