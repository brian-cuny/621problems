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
