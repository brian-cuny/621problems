library(tidyverse)
library(faraway)
library(simex)
library(caret)

data(sexab, package='faraway')
sexab$csa <- relevel(sexab$csa, ref='NotAbused')
lmod <- lm(ptsd ~ csa, sexab)
summary(lmod)


lmod4 <- lm(ptsd ~ cpa*csa, sexab)
sumary(lmod4)

data("fruitfly")

ggplot(fruitfly, aes(x=thorax, y=longevity, color=activity, group=activity)) +
  geom_point() +
  geom_smooth(method='lm')

lmod <- lm(longevity ~ thorax*activity, fruitfly)
plot(lmod)  

anova(lmod)

lmodp <- lm(log(longevity) ~ thorax + activity, fruitfly)
sumary(lmodp)

drop1(lmodp, test='F')

#14.2

data("infmort")

infmort$oil <- relevel(infmort$oil, ref='no oil exports')

l.inf <- lm(mortality ~ income + region + oil, infmort)
summary(l.inf)


l.test.1 <- lm(mortality ~ income*region, infmort)
anova(l.test.1)

l.test.2 <- lm(mortality ~ income*oil, infmort)
anova(l.test.2)

l.test.3 <- lm(mortality ~ region*oil, infmort)
anova(l.test.3)

l.test.4 <- lm(mortality ~ region + income*oil, infmort)
anova(l.test.4)

l.test.5 <- lm(mortality ~ region + income*oil, infmort)
anova(l.test.5)


drop1(l.test.5, test='F')

#Income is not significant. Drop it. 

l.info <- lm(mortality ~ region + oil, infmort)
summary(l.info)

l.test.5 <- lm(log(mortality) ~ region + income*oil, infmort)
summary(l.test.5)
plot(l.test.5)

infmort.2 <- infmort %>%
  rownames_to_column('temp') %>%
  filter(!row_number() %in% c(26, 28, 29)) %>%
  column_to_rownames('temp')

l.test.6 <- lm(log(mortality) ~ region + log(income), infmort.2)
summary(l.test.6)

anova(l.test.6)

drop1(l.test.6, test='F')

plot(l.test.6)

l.test.6$coefficients

exp(predict(l.test.6, newdata=data.frame(region = 'Africa', income = 122), interval='prediction'))


#By default we have started measuring based on Africa. If the country is in Asia the mortality rate drops, then Americas it drops more then Europe. Finally
#as income increases, child mortality decreases. 3 outlier and high leverage points, Libya, Venezuela and Saudi Arabia were removed.

termplot(l.test.6, partial.resid=TRUE, smooth=panel.smooth)




