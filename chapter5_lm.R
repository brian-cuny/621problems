library(faraway)
library(tidyverse)
library(ellipse)
library(Matching)

data(gala, package='faraway')

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
sumary(lmod)

#1

data(teengamb, package='faraway')

l1 <- lm(gamble ~ sex, teengamb)
l2 <- lm(gamble ~ sex + status, teengamb)
l3 <- lm(gamble ~ sex + income, teengamb)
l4 <- lm(gamble ~ sex + verbal, teengamb)
l5 <- lm(gamble ~ sex + status + income, teengamb)
l6 <- lm(gamble ~ sex + income + verbal, teengamb)
l7 <- lm(gamble ~ sex + status + verbal, teengamb)
l8 <- lm(gamble ~ sex + status + income + verbal, teengamb)

sumary(l1) #-25.9092  0.004437
sumary(l2) #-35.70937 0.0004934
sumary(l3) #-21.63439 0.002717
sumary(l4) #-27.7221  0.001957
sumary(l5) #-24.33934 0.004543
sumary(l6) #-22.96022 0.001502
sumary(l7) #-33.75202 0.0011444
sumary(l8) #-22.11833 0.01011

#Sex is statisticaly significant in all 8 cases. The weakest is in the model that uses all the predictors and even in this case it is just barely not significant
# at the 1% level


#2
data(odor, package='faraway')

lodor1 <- lm(odor ~ temp, odor)
lodor2 <- lm(odor ~ temp + gas, odor) 
lodor3 <- lm(odor ~ temp + pack, odor)
lodor4 <- lm(odor ~ temp + gas + pack, odor)

sumary(lodor1) 
sumary(lodor2) 
sumary(lodor3) 
sumary(lodor4)

#The estimate stays the same in all 4 modoels while the error decreases. The p-value fluctuates around .4 in all the cases but drops with the addition of each
#new predictor

#The best model, if forced to use temp (which appears to not be significant) is the last model featuring all the predictors. It does the best job of explaining
#the data even though none of the predictors are statistically significant.

#3

ggplot(teengamb) +
  geom_point(aes(income, gamble, shape=factor(sex))) +
  geom_smooth(aes(income, gamble), method='lm', se=FALSE)

set.seed(123)
mm <- GenMatch(teengamb$sex, teengamb$income, ties=FALSE, caliper=0.05, pop.size=1000)

mm$matches[, 1:2]

#14 matched pairs were found
#47 - 28 = 19 cases were not matched

Pair.Match <- function(data){
  x <- data.frame()
  for(i in 1:nrow(mm$matches)){
    x <- rbind(x, data_frame(x = data[mm$matches[i, 1], ]$income, 
               y = data[mm$matches[i, 1], ]$gamble,
               xend = data[mm$matches[i, 2], ]$income,
               yend = data[mm$matches[i, 2], ]$gamble))
  }
  return(x)
}
lines <- Pair.Match(teengamb)

ggplot(teengamb) +
  geom_point(aes(income, gamble, shape=factor(sex))) +
  geom_smooth(aes(income, gamble), method='lm', se=FALSE) +
  geom_segment(aes(x = x, 
                   y = y,
                   xend = xend,
                   yend = yend),
               color='red',
               data=lines)  


#e

pdiff <- teengamb$gamble[mm$matches[, 1]] - teengamb$gamble[mm$matches[, 2]]
t.test(pdiff)

#No. There is not a significant difference.

#f

sum(pdiff > 0) / length(pdiff)

#g

#No. The regressions indicate that sex is statistically significant while the pairs match indicate that it is not. 


#4

data(happy, package='faraway')

#a
lhap <- lm(happy ~ money + sex + love + work, happy)
sumary(lhap)

#Love is a factor, that may contain a value of 1, 2 or 3 ranking a person's fullfillment in love. Perhaps single, dating or happily in a relationship?

#b
happy$clove <- ifelse(happy$love <= 2, 0, 1)

lhap2 <- lm(happy ~ money + sex + clove + work, happy)
sumary(lhap2)

#The results differ slightly. The p-value is slightly larger, but not in any meaningful way. The estimate on clove does appear to be significantly larger though

#c
lclove <- lm(happy ~ clove, happy)
sumary(lclove)

#Love in this context is the more significant than in any other regression

#d

ggplot(happy, aes(work, happy, shape=factor(clove))) +
  geom_jitter()

#e
xtabs(~ clove + work, happy)

1 + 1 + 5 + 7 
#14 matches max

#f

x <- happy %>%
  dplyr::select(happy, work, clove) %>%
  group_by(work, clove) %>%
  summarize(mean = mean(happy)) %>%
  spread(clove, mean) %>%
  mutate(diff = `1` - `0`)

mean(x$diff, na.rm=TRUE)
 
#Clove. This makes sense as we are comparing the difference in the response variable when considering the predictor value clove



 






