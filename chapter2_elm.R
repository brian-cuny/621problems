library(tidyverse)

data(wcgs, package = 'faraway')

plot(height ~ chd, wcgs)
wcgs$y <- ifelse(wcgs$chd == 'no', 0, 1)
plot(jitter(y, 0.1) ~ jitter(height), wcgs)

lmod <- glm(chd ~ height + cigs, family=binomial, wcgs)
summary(lmod)

1 - pchisq(32.2, 2) #test if significantly different than no predictors model

lmodc <- glm(chd ~ cigs, family=binomial, wcgs)
anova(lmod, lmodc) # height isn't significant


drop1(lmod, test='Chi')
confint(lmod)
plot(lmod)

#diagnostics
linpred <- predict(lmod)
predprob <- predict(lmod, type='response')
rawres <- wcgs$y - predprob
plot(rawres ~ linpred)

arm::binnedplot(predict(lmod), residuals(lmod))

faraway::halfnorm(hatvalues(lmod))

predict(lmod, newdata=data_frame(height=60, cigs=20), type='response')


# video exmaple -----------------------------------------------------------

require(ISLR)
names(Smarket)
summary(Smarket)
pairs(Smarket, col=Smarket$Direction)

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, type='response')
glm.probs[1:5]



# video #1 ----------------------------------------------------------------

data(mtcars)

model <- glm(vs ~ wt + disp, data=mtcars, family=binomial)
summary(model)

predict(model, newdata=data.frame(wt=2.1, disp=180), type='response')



# problems ----------------------------------------------------------------

#2.2

data(pima, package='faraway')

#a
ggplot(pima, aes(insulin, group=factor(test), fill=factor(test))) +
  geom_histogram(position='dodge')

#Yes, there are tons of people who apparently have diabetes that do not take insulin. This seems unlikely.

#b
pima %>%
  mutate(insulin = ifelse(insulin == 0, NA, insulin)) %>%
  ggplot(aes(insulin, fill=factor(test))) +
  geom_histogram(position='dodge')

#Both are roughly normal, ahtough at difference numbers

#c
pima.2 <- pima %>%
  mutate(glucose = ifelse(glucose == 0, NA, glucose),
         diastolic = ifelse(diastolic == 0, NA, diastolic),
         triceps = ifelse(triceps == 0, NA, triceps),
         insulin = ifelse(insulin == 0, NA, insulin),
         bmi = ifelse(bmi == 0, NA, bmi))

pima.3 <- pima.2[complete.cases(pima.2), ]

l.p <- glm(test ~ pregnant + glucose + diastolic + triceps + insulin + bmi + diabetes, pima.3, family=binomial)
summary(l.p)
#768-376 observations were used. The others were removed due to NA.

#d


l.p2 <- glm(test ~ pregnant + glucose + diastolic + bmi + diabetes, pima.3, family=binomial)
summary(l.p2)

anova(l.p, l.p2)

#There is no significant difference between the two models. THerefore insulin and tricpes were not meaningful predictors

#e
edf <- length(model.3$coefficients)
n <- nobs(model.3, use.fallback = TRUE) #number of observations
k <- 2 #standard for AIC
n/k #if > 40 use AIC else use AICc
MASS::stepAIC(l.p) #AIC
MASS::stepAIC(l.p, k=log(nrow(pima.3))) #BIC
MASS::stepAIC(model.3, k=(2*(edf+1)*(edf+2))/(n - edf -2)) #AICc

#diabetes + pregnant + bmi +glucose

lm.aic <- glm(test ~ diabetes + pregnant + bmi + glucose, data=pima, family=binomial)
summary(lm.aic)
#all of them

#f

pima.4 <- pima.2 %>%
  mutate(complete = complete.cases(pima.2))

lm.missing <- glm(test ~ complete, pima.4, family=binomial)
summary(lm.missing)

#No missingness is not associated with test results
#I could impute the missing data for each column

summary(pima.3$bmi)
predict(lm.aic, newdata=data_frame(weight=28.40, 37.10), type='response')

#g

#There is no difference because weight is not part of the model

#h
summary(lm.aic)

#Diastolic was not included in the aic model. THere is a difference between a higher average a statistically significant difference. That is, one is answering a set
#question, is this average higher than that average, while the other is asking, 'can we make a future prediction about diabetes by examining diastolic.'

#2.3

data(kyphosis, package='rpart')

#a
ggplot(kyphosis, aes(Kyphosis, Age, color=Kyphosis)) +
  geom_jitter()

ggplot(kyphosis, aes(Kyphosis, Number, color=Kyphosis)) +
  geom_jitter()

ggplot(kyphosis, aes(Kyphosis, Start, color=Kyphosis)) +
  geom_jitter()

#b
l.k <- glm(Kyphosis ~ Age + Number + Start, kyphosis, family='binomial')
summary(l.k)

kyphosis %>%
  mutate(residuals = residuals(l.k), linpred=predict(l.k)) %>%
  group_by(Age) %>%
  summarize(residuals = mean(residuals), count=n()) %>%
  ggplot(aes(x=Age, y=residuals, size=sqrt(count))) +
  geom_point()

kyphosis %>%
  mutate(residuals = residuals(l.k), linpred=predict(l.k)) %>%
  group_by(Number) %>%
  summarize(residuals = mean(residuals), count=n()) %>%
  ggplot(aes(x=Number, y=residuals, size=sqrt(count))) +
  geom_point()

kyphosis %>%
  mutate(residuals = residuals(l.k), linpred=predict(l.k)) %>%
  group_by(Start) %>%
  summarize(residuals = mean(residuals), count=n()) %>%
  ggplot(aes(x=Start, y=residuals, size=sqrt(count))) +
  geom_point()

#e
faraway::halfnorm(hatvalues(l.k))

#roughly normal, good to go

#f
plot(l.k)

#one anomylous point #43 that should be inspected further but technicaly not a bad leverage point

#g

kyphosis$pred <- predict(l.k, type='response')
kyphosis$pred2 <- ifelse(kyphosis$pred <= 0.5, 'pabsent', 'ppresent')

table(kyphosis$Kyphosis, kyphosis$pred2)
7/17
