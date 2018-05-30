library(faraway)
library(tidyverse)
library(ellipse)

data(fat, package='faraway')

lmod <- lm(brozek ~ age + weight + height + neck + chest + abdom + hip + thigh +
             knee + ankle + biceps + forearm + wrist, data=fat)

x <- model.matrix(lmod)
(x0 <- apply(x, 2, median))
(y0 <- sum(x0*coef(lmod)))

predict(lmod, new=data.frame(t(x0)), interval='prediction')

data(airpass, package='faraway')
plot(pass ~ year, airpass, type='l', ylab='Passengers')
lmod <- lm(log(pass) ~ year, airpass)
lines(exp(predict(lmod)) ~ year, airpass)

lagdf <- embed(log(airpass$pass), 14)
colnames(lagdf) <- c('y', paste0('lag', 1:13))

lagdf <- data.frame(lagdf)

armod <- lm(y ~ lag1 + lag12 + lag13, data.frame(lagdf))
sumary(armod)

#1

data(prostate, package='faraway')

lpro <- lm(lpsa ~ lcavol + lweight + age +lbph + svi + lcp + gleason + pgg45, prostate)
sumary(lpro)
#a
dat = data.frame(lcavol = 1.44692, lweight=3.62301, age=65, lbph=0.3001, svi=0, lcp=-0.79851, gleason=7, pgg45=15)
predict(lpro, new=dat, interval='prediction')

#b
dat20 = data.frame(lcavol = 1.44692, lweight=3.62301, age=20, lbph=0.3001, svi=0, lcp=-0.79851, gleason=7, pgg45=15)
predict(lpro, new=dat20, interval='prediction')

#The age value is severly outside the quantitative range of the other ages. The further away from the mean, the larger the range of values and the less
#predictive the regression line is

#c

lsimple <- lm(lpsa ~ lcavol + lweight + svi, prostate)
sumary(lsimple)

predict(lsimple, new=dat20, interval='prediction')

#The ci is more narrow in the simple model. This model is preferable. It is more simple without losing much predictive value

#2

data(teengamb, package='faraway')

lbam <- lm(gamble ~ sex + status + income + verbal, teengamb)
summary(lbam)

#a

male.dat <- data.frame(sex = 0, status = mean(teengamb$status), income = mean(teengamb$income), verbal = mean(teengamb$verbal))
predict(lbam, new=male.dat, interval='prediction')

#b

male.max <- data.frame(sex = 0, status = max(teengamb$status), income = max(teengamb$income), verbal = max(teengamb$verbal))
predict(lbam, new=male.max, interval='prediction')

#The ci on the max data is wider as it is further from the quantitative values used in the data. This is to be expected as the further the boundaries of the
#regression are pushed, the more error must be accounted for

#c

sqrt.gam <- lm(sqrt(gamble) ~ sex + status + income + verbal, teengamb)
summary(sqrt.gam)

to.display <- predict(sqrt.gam, new=male.dat, interval='prediction')

#The predicted values should be quared to obtain the values in their original units

to.display^2

#d

female.dat <- data.frame(sex = 1, status=20, income=1, verbal = 10)
predict(sqrt.gam, new=female.dat, interval='prediction')

#The predictive capability of seems poor. There is really no other person in the data set remotely comparable to this person and will lead to extreme values on the
#low and high side of each of the predictors

#3. 

data(snail, package='faraway')

#a
xtabs(water ~ temp + humid, snail)/4

#If asked to make an estimate I would use the 4 data points that are equally spread around the point of interest and mind the mean or median of those points
#With that being said, examining the 100% humidity column would seem to indicate that this may not be an appropriate idea. 

#b
lsnail <- lm(water ~ temp + humid, snail)
sumary(lsnail)

new.snail <- data.frame(temp=25, humid=60)
predict(lsnail, new=new.snail, interval='prediction')

#c
second.snail <- data.frame(temp=30, humid=75)
predict(lsnail, new=second.snail, interval='prediction')

#The model in part c is surprisingly accurate. This could indicate that part b's model is more accurate than I had previously given credit.

#d
int.snail <- data.frame(temp=0, humid=0)
predict(lsnail, new=int.snail, interval='prediction')

#a = 2.582672b

int2.snail <- data.frame(temp=25.82672, humid=10)

predict(lsnail, new=int2.snail, interval='prediction')

#No my answers are not unique. There are an infinite number of combinations that give this answer. No. The values needed all required predictors
#outside the regression domain and require a prediction far outisde the range

#e

#80 = -0.183333*25 + 0.473489a + 52.610806

(80-52.610806 + 0.183333*25)/0.473489

#4

data(UKLungDeaths, package='datasets')

#a
library(zoo)



m.data <- data.frame(year = as.yearmon(time(mdeaths)),
                       count = mdeaths)

ggplot(m.data, aes(year, count)) +
  geom_point() +
  geom_line()

#The winter time has a higher rate of deaths from lung disease

#b

lagdf <- embed(m.data$count, 14)
colnames(lagdf) <- c('y', paste0('lag', 1:13))
lagdf <- data.frame(lagdf)
armod <- lm(y ~ lag1 + lag12 + lag13, data.frame(lagdf))
sumary(armod)

#No lag 13 is no statistically significant

#c
predict(armod, new=data.frame(lag1=1341, lag12 = 2263, lag13 = 1812), interval='prediction')

predict(armod, new=data.frame(lag1=1879.599, lag12 = 1820, lag13 = 2263), interval='prediction')

#d

#No. There are sizable deviations within the data set that will likely lead to missized errors.

#5

data(fat, package='faraway')

#a
lfat <- lm(brozek ~ age + weight + height + abdom, fat)
sumary(lfat)

lmod <- lm(brozek ~ age + weight + height + neck + chest + abdom + hip + thigh +
             knee + ankle + biceps + forearm + wrist, data=fat)
sumary(lmod)

#Justifiable depends on the situation. The model is indeed significantly simplier but it loses accuracy and that includes the dropping of 
#statistically significant predictors. However, if the goal is to provide the general population with a very general guideline and to promote people
#to take responsibilty for their health, then yes, I believe it is justified.

#b
med.person <- data.frame(age=median(fat$age), weight = median(fat$weight), height = median(fat$height), abdom=median(fat$abdom))
predict(lfat, new=med.person, interval='prediction')

#No, the values do not differ in an important amount

#c
for(i in 25:50){
  print(c(fat[i, 1], predict(lfat, new=fat[i, ], interval='prediction')))
}

#case 39 is outside the prediction range and case 26 is very far from it's predicted value (but still inside the prediction range)

fat <- fat[c(1:25, 27:38, 40:252), ]
  
med.person <- data.frame(age=median(fat$age), weight = median(fat$weight), height = median(fat$height), abdom=median(fat$abdom))
predict(lfat, new=med.person, interval='prediction')

#No. This made no decernable difference to the outcome. They are, after all, only 2 cases amongst 250












