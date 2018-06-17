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
(2*edf*(edf+1))/(n - edf -1) #AICc
-2*edf + k*edf  #AIC
-2*edf + log(n)*edf #BIC

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







