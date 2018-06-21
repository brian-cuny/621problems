library(tidyverse)

food <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\MichelinFood.txt', delim = '\t')

lm.food <- glm(cbind(InMichelin, NotInMichelin) ~ Food, family=binomial, data=food)
summary(lm.food)

ggplot(food, aes(Food, proportion)) +
  geom_point() +
  geom_smooth(method='glm', method.args=list(family='binomial'), se=FALSE)

plot(lm.food)

predict(lm.food, newdata=data_frame(Food = 20), type='response')

m.food <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\MichelinNY.csv')

l.1 <- glm(InMichelin ~ Food, family=binomial, data=m.food)
summary(l.1)

ggplot(m.food, aes(Food, InMichelin)) +
  geom_point(alpha=0.2) +
  geom_smooth(method='glm', method.args=list(family='binomial'), se=FALSE)

l.2 <- glm(InMichelin ~ Food + Decor*Service + Price + log(Price), data=m.food, family=binomial)
summary(l.2)


library(car)
car::mmps(l.2)
plot(l.2)

l.3 <- glm(InMichelin ~ Food + Decor*Service + log(Price), data=m.food, family=binomial)
summary(l.3)
car::mmps(l.3)


# questions ---------------------------------------------------------------

#8.2

america <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\MissAmericato2008.txt', delim = '\t')

l.a <- glm(cbind(Top10, 81-Top10) ~ LogPopulation + LogContestants + LogTotalArea + Latitude + Longitude, family=binomial, data=america)
summary(l.a)

car::mmps(l.a)

l.a2 <- glm(cbind(Top10, 81-Top10) ~ LogPopulation + LogContestants + LogTotalArea + Latitude, family=binomial, data=america)
summary(l.a2)
anova(l.a, l.a2)

car::mmps(l.a2)
plot(l.a2)

#8.6

bank <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\banknote.txt', delim = '\t')

lm.b <- glm(Y ~ Bottom + Diagonal, family='binomial', bank)
summary(lm.b)
















