library(faraway)
library(tidyverse)
library(ellipse)

data(gala, package='faraway')

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)

summary(lmod)           

nullmod <- lm(Species ~ 1, gala)

summary(nullmod)

anova(nullmod, lmod)

lmods <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, gala)
summary(lmods)

anova(lmods, lmod)

lmod <- lm(Species ~ Nearest + Scruz, gala)
lms <- summary(lmod)
lms$fstatistic

#1. 
data(prostate, package='faraway')

lpro <- lm(lpsa ~ lcavol + lweight + age +lbph + svi + lcp + gleason + pgg45, prostate)
sumary(lpro)

qt(0.975, 97-9)

-0.0196372 + c(-1, 1)*qt(0.975, 97-9)*0.0111727

#We could deduce that the p-value is greater than 0.05. That is, it is not significant at the 95% level

#b
#age, lbph

plot(ellipse(lpro, c(4, 5)), type='l')
points(coef(lpro)[4], coef(lpro)[5], pch=19)
abline(v=confint(lpro)[4, ], lty=2)
abline(h=confint(lpro)[5, ], lty=2)

#H0: BAge = Blbph = 0 We fail to reject the null hypothesis as the origin is contained in the ellipse

#c
nreps <- 4000
tstats <- numeric(nreps)
set.seed(123)
for(i in 1:nreps){
  lpro <- lm(lpsa ~ lcavol + lweight + sample(age) +lbph + svi + lcp + gleason + pgg45, prostate)
  tstats[i] <- summary(lpro)$coef[4,3]
}
mean(abs(tstats) > abs(summary(lm(lpsa ~ age, prostate))$coef[2,3]))

#d
lpro <- lm(lpsa ~ lcavol + lweight + svi, prostate)
sumary(lpro)

#In general, the goal is to have as simple a model as possible that still is able to adequitely identify the model. The loss of these indciators
#does not significantly inhibit the models predictive ability. Therefore, the new model is preferred as it is more simple.

#2.


data(cheddar, package='faraway')

#a

lche <- lm(taste ~ Acetic + H2S + Lactic, cheddar)
sumary(lche)

#H2S and Lactic are significant at the 5% level. Acetic is not

#b
lche <- lm(taste ~ I(exp(Acetic)) + I(exp(H2S)) + Lactic, cheddar)
sumary(lche)

#Lactic is still significant at the 5% level while Acetic and H2S are not

#c
#No we cannot. F-tests can only be used to fit linear models and the second model is nonlinear
#The first model is a better fit. First it is a valid linear model and second is accounts for more of the variation seen in the taste

#d.
#With a regression slope of 3.91184, an increase of 0.01 in H2S would result in an increase in taste of 0.0391184

#e.
#This indicates a 1% increase in H2S

#3.
data(teengamb, package='faraway')

lbam <- lm(gamble ~ sex + status + income + verbal, teengamb)
summary(lbam)

#a
#sex and income are statistically significant at the 5% level

#b
#Sex is a dummy variable representing the categorical data of sex. A 1 represents female and a 0 represents male.

#c
lf <- lm(gamble ~ income, teengamb)
summary(lf)

anova(lf, lbam)

#Since the p-value is so small the null hypothesis, B1 = B2 = B4 = 0, is rejected. There is evidence to suggest inferential ability of the other categories

#4.

data(sat, package='faraway')

#a

lsat <- lm(total ~ expend + ratio + salary, sat)
summary(lsat)

lsal <- lm(total ~ salary, sat)
anova(lsal, lsat)

#With a p-value so large we fail to reject the null hypothesis. There is no evidence to suggest that salary is statistically significant

lnull <- lm(total ~ 1, sat)
anova(lnull, lsat)

#With a p-value so small we reject the null hypothesis. There is evidence to suggest that not all of the predictors are statistically insignificant

#b

lsat2 <- lm(total ~ expend + ratio + salary + takers, sat)
summary(lsat)

ltak <- lm(total ~ takers, sat)
anova(ltak, lsat)

#With a p-value so small we reject the null hypothesis. There is evidence to suggest that takers is statistically significant

anova(lsat, lsat2)

sumary(lsat2)
(-12.5594)^2

#5

#proof


#6

data(happy, package='faraway')

lhap <- lm(happy ~ money + sex + love + work, happy)
summary(lhap)

#a

#The only predictor significant at the 1% level is love

#b
table(happy[, 1])
table(happy[, 2])
table(happy[, 3])
table(happy[, 4])
table(happy[, 5])

#The data uses different scales. Specifically, love appears to be a 1, 2, 3 categorical ranking which cannot be used as is in a linear model

#c

lmon <- summary(lm(happy ~ money, happy))

nreps <- 4000
set.seed(123)
tstats <- numeric(nreps)
for(i in 1:nreps){
  lmods <- lm(happy ~ sample(money), happy)
  tstats[i] <- summary(lmods$coef[2])
}
mean(abs(tstats) > abs(lmon$coef[2]))

#With a p-value of 0.09675 we fail to reject the null hypothesis. There is no evidence to suggest that money is statistically significant

#d
ggplot(data=data.frame(x=seq(-3, 3, length=300)), aes(x)) +
  #geom_histogram(aes(tstats, ..count../sum(..count..))) +
  stat_function(fun=dt, args=list(x= seq(-3, 3, length=300), df=37), n=300) +
  scale_x_continuous(limits=c(-3, 3), breaks=seq(-3, 3, .5))



dt(grid, df=1)

#7

data(punting, package='faraway')
#a

lpunt <- lm(Distance ~ RStr + LStr + RFlex + LFlex, punting)
sumary(lpunt)

#None of the predictors are significant at the 5% level

#b

lnull <- lm(Distance ~ 1, punting)

anova(lnull, lpunt)

#Yes, together these predictors are significant

#c

lleft <- lm(Distance ~ LStr, punting)
lright <- lm(Distance ~ RStr, punting)

anova(lleft, lpunt)
anova(lright, lpunt)

#They do not have the same effect. LStr has a higher F-value

#d

lboth <- lm(Distance ~ I(LStr + RStr), punting)
sumary(lboth)

confint(lboth)

# 0.2219484, 0.6982697

#The sum of the two predictors is more significant than each one individually

#e

#Comparison of the summary from (a) and (d) show staistical significance for LSTr + RStr as opposed to the model where they are seperate

#f

lflex <- lm(Distance ~ I(LFlex + RFlex), punting)
summary(lflex)

#Yes, combining lflex and rflex creates a similar (although not as significant) improvement in the model

#g

lcomb <- lm(Distance ~ I(LFlex + RFlex) + I(LStr + RStr), punting)
summary(lcomb)

#h
lhang <- lm(Hang ~ RStr + LStr + RFlex + LFlex, punting)
sumary(lhang)

anova(lhang, lpunt) #error
#No we cannot because the response variable is different





