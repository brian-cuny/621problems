library(caret)
library(tidyverse)
data(GermanCredit)
set.seed(1)
Train <- createDataPartition(GermanCredit$Class, p=0.6, list=FALSE)
training <- GermanCredit[ Train, ]
testing <- GermanCredit[ -Train, ]

mod_fit_one <- glm(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own + CreditHistory.Critical, data=training, family='binomial')
mod_fit_two <- glm(Class ~ Age + ForeignWorker, data=training, family='binomial')
summary(mod_fit_one)

# Goodness of fit ---------------------------------------------------------
anova(mod_fit_one, mod_fit_two, test='Chisq')

library(lmtest)
lmtest::lrtest(mod_fit_one, mod_fit_two)


# Pseudo R2 ---------------------------------------------------------------
pscl::pR2(mod_fit_one)


# hosmer-lemeshow test ----------------------------------------------------

MKmisc::HLgof.test(fit = fitted(mod_fit_one), obs=training$Class)

ResourceSelection::hoslem.test(training$Class, fitted(mod_fit_one), g=10)


# individual predictors ---------------------------------------------------


# wald test ---------------------------------------------------------------

survey::regTermTest(mod_fit_one, 'ForeignWorker')

survey::regTermTest(mod_fit_one, 'CreditHistory.Critical')


# variable importance -----------------------------------------------------

caret::varImp(mod_fit_one)

car::residualPlots(mod_fit_one)
car::mmps(mod_fit_one)



# food --------------------------------------------------------------------


#When the predictor variable X is normally distributed with the same variance for the two values of Y, the log odds are a linear function of x, with the slope equal
#to the difference in the mean of X across the two groups divided by the common variance of X in each group.

#If the variance of the predictors differs across the two groups then the log odds are a function of x, x^2 and xi, xj

food <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\MichelinNY.csv')

ggplot(food, aes(Food, InMichelin, alpha=0.2)) +
  geom_jitter(height=0.05, width=2) +
  geom_smooth()

ggplot(food, aes(x=InMichelin, y=Food, group=InMichelin)) +
  geom_boxplot()

food.glm <- glm(InMichelin ~ Food, family='binomial', data=food)
summary(food.glm)

food %>%
  gather('predictor', 'value', 3:6) %>%
  ggplot(aes(x=factor(InMichelin), y=value)) +
  geom_boxplot() +
  facet_wrap(~predictor, scales='free_y')


require(gridExtra)
plot.1 <- ggplot(food, aes(Price)) +
  geom_density()

plot.2 <- ggplot(food, aes(factor(InMichelin), Price)) +
  geom_boxplot()

grid.arrange(plot.1, plot.2, ncol=2)


all.glm <- glm(InMichelin ~ Food + Decor + Service + Price + log(Price), family='binomial', data=food)
summary(all.glm)

car::marginalModelPlots(all.glm, terms = ~ . -Service, layout=c(2, 3))

ggplot(food, aes(Service, InMichelin)) +
  geom_point() + 
  geom_smooth(method='loess', se=FALSE) +
  geom_smooth(aes(Service, predict(all.glm, type='response')), method='loess', se = FALSE, color='red')
  
  geom_smooth(aes(log(Price), predict(all.glm, type='response')), method='loess', se=FALSE, color='red', linetype='dotted', span=1/4)

ggplot(food, aes(Decor, InMichelin)) +
  geom_point() +
  geom_smooth(method='loess', se=FALSE) +
  geom_smooth(aes(Decor, predict(all.glm, type='response')), method='loess', se=FALSE, span=3/4, color='red', linetype='dotted')



m1 <- glm(lfp ~ ., family="binomial", data=carData::Mroz)
car::mmps(m1, smooth=list(smoother=loessLine, span=2/3))


carData::Prestige
library(car)
mod.working <- glm(partic != "not.work" ~ hincome + children, family=binomial, data=Womenlf)
summary(mod.working)

residualPlots(mod.working, layout=c(1,3))

avPlots(mod.working)









