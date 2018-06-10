library(tidyverse)
library(faraway)
library(simex)
library(caret)

data(chmiss, package="faraway")

#13.1
data("kanga")
kanga.2 <- kanga %>%
  select(-sex, -species)

#a
rowSums(is.na(kanga.2))

summary(kanga.2)

#b

kanga.2 %>%
  map_dbl(~sum(is.na(.))/nrow(kanga.2))

#I will remove the 2 cases that have 3 missing values and the 1 variable that is missing more than 10% of it's values

#c
#PCA is principal component analysis and is featured in chapter 11, which we have not done yet. We do this chapter in week 3

#d
#PCA is principal component analysis and is featured in chapter 11, which we have not done yet. We do this chapter in week 3

#13.2

l.gala <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)

gala.miss <- alr3::galapagos

l.gala.miss <- lm(NS ~ Area + Elevation + Dist + DistSC + Anear, gala.miss)
sumary(l.gala)
sumary(l.gala.miss)

#While the R^2 are roughly the same, the predictive value of the data is smaller.
#The p-values are higher in the second regression compared to the first

#c
gala.miss.mean <- gala.miss %>%
  map_df(~ifelse(is.na(.), mean(., na.rm = TRUE), .))

l.gala.mean <- lm(NS ~ Area + Elevation + Dist + DistSC + Anear, gala.miss.mean)
sumary(l.gala.mean)

#The predictive value is much lower than previously with worse p-values. Taking the mean does not appear to be a good idea.

#d
dummy.vars <- dummyVars(~ ., data = gala.miss[, -1])
train.dummy <- predict(dummy.vars, gala.miss[, -1])

pre.process <- preProcess(train.dummy, method='bagImpute')
imputed.data <- predict(pre.process, train.dummy)

gala.impute <- gala.miss %>%
  mutate(Elevation = imputed.data[,6])

l.gala.impute <- lm(NS ~ Area + Elevation + Dist + DistSC + Anear, gala.impute)
sumary(l.gala.impute)

#The sumary shows a much better R^2 and strong p-values
