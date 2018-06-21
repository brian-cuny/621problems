library(tidyverse)


data(bridge, package='faraway')

bridge <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\bridge.txt', delim = '\t')

step(lm(log(Time) ~ log(DArea) + log(CCost) + log(Dwgs) + log(Length) + log(Spans), bridge))

a <- lm(log(Time) ~ log(Dwgs) + log(Spans), bridge)
summary(a)

drop1(lm(log(Time) ~ log(DArea) + log(CCost) + log(Dwgs) + log(Length) + log(Spans), bridge))
drop1(lm(log(Time) ~ log(DArea) + log(CCost) + log(Dwgs) + log(Spans), bridge))
drop1(lm(log(Time) ~ log(CCost) + log(Dwgs) + log(Spans), bridge))



full <- lm(log(Time) ~ 1, bridge)
add1(full, ~ log(DArea) + log(CCost) + log(Dwgs) + log(Length) + log(Spans))


# prostate example --------------------------------------------------------

prostate.training <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\prostateTraining.txt', delim = '\t')
prostate.test <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\prostateTest.txt', delim = '\t')

pairs(prostate.training)

lm.all <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, prostate.training)

car::residualPlots(lm.all)
plot(lm.all)
car::mmps(lm.all) 
summary(lm.all)

car::avPlots(lm.all)

faraway::vif(model.matrix(lm.all)[, -1])

step(lm.all)




edf <- length(lm.all$coefficients)
n <- nobs(lm.all, use.fallback = TRUE) #number of observations
k <- 2 #standard for AIC
n/k #if > 40 use AIC else use AICc

MASS::stepAIC(lm.all) #AIC
MASS::stepAIC(lm.all, k=log(nrow(prostate.training))) #BIC
MASS::stepAIC(lm.all, k=(2*(edf+1)*(edf+2))/(n - edf -2)) #AICc

lm.aic <- lm(lpsa ~ lcavol + lweight, prostate.test)
lm.aicc <- lm(lpsa ~ lcavol + lweight + svi + lbph, prostate.test)
lm.bic <- lm(lpsa ~ lcavol + lweight + svi + lbph + pgg45 + lcp + age, prostate.test)
summary(lm.aic)


# lasson method -----------------------------------------------------------

library(lars)
l.lasso <- lars::lars(as.matrix(prostate.training[, c(-1, -10)]), prostate.training$lpsa)
plot(l.lasso)

set.seed(11)
cv.lasso <- lars::cv.lars(as.matrix(prostate.training[, c(-1, -10)]), prostate.training$lpsa)


cv.lasso$index[which.min(cv.lasso$cv)]
predict(l.lasso, s=0.5454545, type='coef', mode='fraction')$coef



# questions ---------------------------------------------------------------

#7.1

mantel <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\mantel.txt', delim = '\t')

lm.mantel <- lm(Y ~ X1 + X2 + X3, mantel)
summary(lm.mantel)
plot(lm.mantel)

#a
edf <- length(lm.mantel$coefficients)
n <- nobs(lm.mantel, use.fallback = TRUE) #number of observations
k <- 2 #standard for AIC
n/k #if > 40 use AIC else use AICc

MASS::stepAIC(lm.mantel) #AIC
MASS::stepAIC(lm.mantel, k=log(nrow(prostate.training))) #BIC

#Use x1 and x2 but not x3 in all cases

#b
MASS::stepAIC(lm.mantel, direction='forward')
MASS::stepAIC(lm.mantel, k=log(nrow(prostate.training)), direction='forward') #BIC

#c
#Model selections that pick all predictors at once can and sometimes will have different predictors than incremental processes. This is due to the stopping/continuing
#condition on the incremental processes. They cannot "see" the relationship between the variables and how their selection will affect the overall regression.

#d
#The models from part a are better. There is no need for x3 to be present in the regression.

#2

hald <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\hALDCEMENT.txt', delim = '\t')

#a
#R2 - 3 predictors, AIC = 3 Predictors, AICC = 2 predictors, BIC = 2predictors

#b
#AIC - 3 predictors, same as a, BIC - 3 predictors, difference from a

#c
#AIC - 3 predictors, same as a, BIC - 2 predictors, same as a

#d
#I would go with 2 predictors. The data set is small so preference should be given to AICc. Then the 3 predictor regression has a non significant predictor.
#Combine that with the fact that these p-values are all smaller than they really are and that leads us towards 2 predictors as the best model. Also the correlation
#on the later models is terrible.














