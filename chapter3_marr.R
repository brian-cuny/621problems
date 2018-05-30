library(faraway)
library(tidyverse)
library(ellipse)
library(Matching)
library(alr3)

data(teengamb, package='faraway')

lgamb <- lm(gamble ~ sex + status + income + verbal, teengamb)
plot(lgamb)

rt <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\responsetransformation.txt', delim='\t')

rtlinear <- lm(y ~ x, rt)
plot(rtlinear)

inverseResponsePlot(rtlinear)

data("salarygov")

govlm <- lm(log(MaxSalary) ~ sqrt(Score), salarygov)
plot(govlm)

ggplot(salarygov, aes(sqrt(Score), log(MaxSalary))) +
  geom_point() +
  geom_smooth(method='lm')

summary(powerTransform(cbind(MaxSalary,Score)~1, salarygov))


#1

air <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\airfares.txt', delim='\t')

#a
#This conclusion is incorrect. Prior to drawing a conclusion from the model, it must be first acertained whether the regression is appropriate. In this case
#the graph of the standardized residuals shows a non random distribution and non constant variance. THis would indicate that the regression as fit is 
#in appropriate and thus the conclusion are invalid.

#b
ggplot(air, aes(Distance, Fare)) +
  geom_point() +
  geom_smooth(method='lm')


airlm <- lm(Fare ~ Distance, air)
plot(airlm)

summary(powerTransform(cbind(Fare,Distance)~1, air))

#The straight line regression visually appears to fit the data well but the residuals graphs tell a different story. We would either need to make a transformation
#or add another predictor to the data set.


#2
#False. While the x term appears acceptable, it may be beneficial to consider a transformation of the y variable. A transformation may succeed in meeting the 
#prerequisities for a regression.


#3

mag <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\AdRevenue.csv')

#part a

#a
maglm <- lm(log(AdRevenue) ~ log(Circulation), mag)
plot(maglm)

ggplot(mag, aes(log(Circulation), log(AdRevenue))) +
  geom_point() +
  geom_smooth(method='lm')

#A simple exploration indicates that a linear regression is not appropriate

ggplot() +
  geom_histogram(aes(log(mag$AdRevenue)), bins=25)

ggplot() +
  geom_histogram(aes(log(mag$Circulation)))

inverseResponsePlot(maglm)

#log(AdRevenue) = log(Circulation)

summary(maglm)

newdata <- data.frame(Circulation=c(0.5, 20))

#b

predict(maglm, newdata, interval='predict')
#4.308227 
#6.258752

#c

#The raw data contains a small number of upper outliers that may be skewing the data. That is, there are an extremely small number of 
#of incredibly popular magazines.

#part b

polylm <- lm(AdRevenue ~ Circulation + I(Circulation^2) + I(Circulation^3), mag)
plot(polylm)
summary(polylm)

mag %>%
  mutate(my_model = predict(polylm)) %>%
  ggplot() +
  geom_point(aes(Circulation, AdRevenue)) +
  geom_line(aes(Circulation, my_model))

predict(polylm, newdata, interval='predict')
#84.16846
#499.53342

#c
#This model may fit the data nicely but does not meet the requirements for a regression. Namely, there is not constant variance in the standardized residuals and a 
#descernable pattern is found in the residual plot


#part c

#a

#Part a is the better model. Not only does it meet all the requirements for regression but it works with the data in log form. This allows for us to 
#make determinations about the information in % terms. For example, the slop ei s0.52 which means that for every 1% increase in circulation, there is a 0.5% increase
#in ad revenue

#I would use the interval in part a, but I would first transform it back into non-log form for easier interpretation.

#4

#a
#There are numerous weakness with the straight line regression model. Both time and tonnage are heavily skewed. There also appers to be points of high leverage
#One of which may be an outlier.

#b
#It appears that it would be too low. The outlier that is also a leverage point appears to be pulling the regression line down, lowering all the predictions.

#a2

#Yes it is. First, it meets the prerequisities for perofrming a regression meaning the model is valid and can be used for prediction. 

#b2

#The model still has upper outliers that may be affecting the regression line. It may be pulling it downwards. It is possible that this leverage point is
#an invalid point that needs to be discarded. (Further investigation is needed)


#5

car <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\cars04.csv')

#a
#While the regression appears linear, there are numerous areas of concern. The graph of the standardized residuals seems to show an increase in variance as
#DealerCost increases. The Normal QQ plot shows non-normal distribution in the DealerCost. Specifically, there appears to be a few extreme upper outliers in price.
#These points have high leverage although visual inspection indicates they may not be outliers.

#b
#The skewed distribution can be transformed to make the residuals normal and to help create constant variance in the residuals. This may also address
#the few extreme upper outliers. 

#An examination would need to determine whether the high leverage points are negatively impacting the regression and if so, which course of action may be appropriate

#c

#The variance of the residuals is now constant and the distirbution is less skewed. In addition by working with the log of each x and y, we present the data
#in a more easy to digest fashion. That is, for each 1% increase in dealercost, there is rougly a 1% increase in suggested retail price. This would helpfully indicate
#that dealers appears to push all additional costs onto the purchaser.

#d
#See above

#e
#The qq plot still shows upper and (more extreme) lower outliers. These few points fall so far out of the norm that they should be manually reviewed
#to ensure they do not have any mistakes in them or whether some other interpretation is more appropriate.

#6 (3)

#In order to successfully use the inverse response plot the distribution of x must be an elliptically symmetric distribution. This would indicate that first
#the x distribution would need to be transformed and then the y via the IRP. This would likely create a transformation close to the answer provided.

#7

#proof

#8

dia <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\diamonds.txt', delim='\t')

#part 1

#a

ldia <- lm(Price ~ Size, dia)
summary(ldia)

ggplot(ldia, aes(Size, Price)) +
  geom_point() +
  geom_smooth(method='lm')

plot(ldia)

#b

#The original plot appears linear. The residual plots show a non-constant variance and a small number of high leverage points.
#Both price and size are heavily skewed. Both appear to need transforming

ggplot() +
  geom_density(aes(dia$Size))

ggplot() +
  geom_density(aes(dia$Price))

#part 2

#a

tdia <- lm(sqrt(sqrt(Price)) ~ log(Size), dia)
plot(tdia)

ggplot(ldia, aes(log(Size), Price^0.25)) +
  geom_point() +
  geom_smooth(method='lm')


boxplot(log(dia$Size))
boxplot(sqrt(sqrt(dia$Price)))

inverseResponsePlot(tdia)

sumary(tdia)

#I first transformed Size examining the boxplot with various transofmrations. Taking the log of price produced a roughly normal distribution
#I then used an inverse response plot that indicated the 4th root of price would be a strong indicator

#There are still outliers despite my transformations. The distirbutions are only very roughly normal.



#part 3

#The the second model has roughly normal distributions and roughly speaking a more constant variance, it is the superior model.


