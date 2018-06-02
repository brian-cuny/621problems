library(tidyverse)
library(faraway)
library(simex)

data(cars)
plot(dist ~ speed, cars)
lmod <- lm(dist ~ speed, cars)
sumary(lmod)
abline(lmod)

set.seed(123)
lmod <- lm(dist ~ speed, cars, x=TRUE)
simout <- simex(lmod, 'speed', 0.5, B=1000)
simout

#1
data(faithful)

lfaith <- lm(eruptions ~ waiting, faithful, x=TRUE)
sumary(lfaith)

plot(eruptions ~ waiting, faithful)

set.seed(123)
simout <- simex(lfaith, 'waiting', 30, B=1000)
simout

#2

set.seed(123)
simout2 <- simex(lfaith, 'eruptions', 30, B=1000)
simout2

#A different, 3rd set of coefficients are produced. However, the values are fairly close to their originals. 

#3

data("divusa")

ldiv <- lm(divorce ~ unemployed + femlab + marriage + birth + military, divusa)
sumary(ldiv)

#Each predictor has a different scale. Unemployed says that for every 1% increase in unemployment there is a 0.01% decrease in divorce
#For every 1% increase in female labor participation ther eis a 3.8 increase in divorce

#b
x <- model.matrix(ldiv)[, -1]
vif(x)

round(cor(divusa[,c(-1,-2)]),2)

#There appears to be strong colinearity in a few of the predictors. For example, femalelabor is negatively correlated with births and marriage
#marriage is positively correlated with births

#c
sumary(ldiv)

ldiv2 <- lm(divorce ~ unemployed + femlab + marriage + birth, divusa)
sumary(ldiv2)

ldiv3 <- lm(divorce ~ femlab + marriage + birth, divusa)
sumary(ldiv3)

x3 <- model.matrix(ldiv3)[, -1]
vif(x3)

#There is still colinearity but it has been reduced. However, by removing the insignificant predictors, we have left the most highly correlated predictors
#because since one of them is significant, then all of them are significant.

#4

data("longley")

llong <- lm(Employed ~ GNP.deflator + GNP + Unemployed + Armed.Forces + Population + Year, longley)
sumary(llong)

#a
x <- model.matrix(llong)[, -1]
e <- eigen(t(x) %*% x)
e$val
sqrt(e$val[1]/e$val)
sqrt(e$val[2]/e$val)
sqrt(e$val[3]/e$val)
sqrt(e$val[4]/e$val)

#The condition numbers are large and there is a wide range of eigenvalues. This indicates that there is colinearity from more than just one linear combination

#b
round(cor(longley[,c(-1,-2)]),2)

#The correlations between the predictors is large. Year is highly correlated with Population and Employment. Employment is highly correlated with population 
#amongst others

#c
vif(x)

#The VIF are incredibly large indicating large variance inflation

#5

data(prostate)

lpros <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, prostate)
sumary(lpros)

#a
x <- model.matrix(lpros)[, -1]
e <- eigen(t(x) %*% x)
e$val
sqrt(e$val[1]/e$val)
sqrt(e$val[2]/e$val)
sqrt(e$val[3]/e$val)
sqrt(e$val[4]/e$val)
sqrt(e$val[5]/e$val)
sqrt(e$val[6]/e$val)
sqrt(e$val[7]/e$val)
sqrt(e$val[8]/e$val)

#The eigenvalue for the first predictor, lcavol, and the second, lweight, are large and diverse. The remaining values are all small.

#b

round(cor(prostate[,-9]),2)

#There is strong correlation between many of the predictors. 

#c
vif(x)

#None of the vifs seem outragiously large

#6

data(cheddar)

lched <- lm(taste ~ Acetic + H2S + Lactic, cheddar)
summary(lched)

#a
#Yes. It is significant with a p-value of 0.03

#b
summary(lched)$coef[4, 4]

#c

lched2 <- lm(taste ~ Acetic + H2S + I(Lactic + rnorm(30, 0, .01)), cheddar)
summary(lched2)

summary(lched2)$coef[4, 4]

#d

set.seed(123)
p <- numeric(1000)
for(i in 1:1000){
  p[i] <- summary(lm(taste ~ Acetic + H2S + I(Lactic + rnorm(30, 0, .01)), cheddar))$coef[4, 4]
}
mean(p)

#No. This difference is inconsequential

#e
set.seed(123)
p2 <- numeric(1000)
for(i in 1:1000){
  p2[i] <- summary(lm(taste ~ Acetic + H2S + I(Lactic + rnorm(30, 0, .1)), cheddar))$coef[4, 4]
}
mean(p2)

#Yes. This is consequential. The predictor is now non-significant.

#7

data(happy)

lhap <- lm(happy ~ money + sex + love + work, happy)
summary(lhap)

#Money is significantly larger than the other data points, which all lie below 5. It could be scaled down by dividing by 10 to measure money in 10s of thousands.
#Sex, love and work are all categorical and should also be treated as such and not numerically. Maybe love and work could be converted to a 10 point scale?

#8

data(fat)

#a

lmod <- lm(brozek ~ age + weight + height + neck + chest + abdom + hip + thigh + knee + ankle + biceps + forearm + wrist, fat)

x <- model.matrix(lmod)[, -1]
e <- eigen(t(x) %*% x)
e$val
sqrt(e$val[1]/e$val)
sqrt(e$val[2]/e$val)
sqrt(e$val[3]/e$val)
sqrt(e$val[4]/e$val)
sqrt(e$val[5]/e$val)
sqrt(e$val[6]/e$val)
sqrt(e$val[7]/e$val)
sqrt(e$val[8]/e$val)
sqrt(e$val[9]/e$val)
sqrt(e$val[10]/e$val)
sqrt(e$val[11]/e$val)
sqrt(e$val[12]/e$val)
sqrt(e$val[13]/e$val)

vif(x)

#There is a large amount of colinearity between the predictors used. Weight is the largest with appoximately 5 times as much standard error due to colinearity.
sqrt(33)

#b

fat2 <- fat %>%
  filter(row_number() != c(39, 42))

lmod2 <- lm(brozek ~ age + weight + height + neck + chest + abdom + hip + thigh + knee + ankle + biceps + forearm + wrist, fat2)

x2 <- model.matrix(lmod2)[, -1]
vif(x2)

#Some predictors are nominally larger but there has been a great reduction in the largest predictor (weight) 

#c

lmod3 <- lm(brozek ~ age + weight + height, fat2)

vif(model.matrix(lmod3)[, -1])

#The colinearity is very small in this case. 

#d

new.data <- data.frame(age=median(fat2$age), weight = median(fat2$weight), height = median(fat2$height))
predict(lmod3, new.data, interval='prediction')

#e

new.data.2 <- data.frame(age=40, weight = 200, height = 73)
predict(lmod3, new.data.2, interval='prediction')


#The size of the range is about equal but the fitted value is slightly larger.

#f

new.data.3 <- data.frame(age=40, weight=130, height=73)
predict(lmod3, new.data.3, interval='prediction')

#The interval is much smaller but also contains what would appear to be impossible or nonsense answers. It is unlikely that anyone's bodyfat is 3% and of course
#bodyfat cannot be negative. It appears this sample point is quite unrepresentative of the larger dataset.







