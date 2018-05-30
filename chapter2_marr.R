library(tidyverse)

production <- read_delim(file='C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\production.txt', delim = '\t') %>%
  select(-`Case `) %>%
  rename(RunSize = `RunSize `)

plm <- lm(RunTime ~ RunSize, data=production)
summary(plm)

ssx <- sum((production$RunSize - mean(production$RunSize))^2)
d <- sqrt(1+1/20+((160.7097-mean(production$RunSize))^2)/ssx)
162.7099-2.445*6.98*d


change <- read_delim(file='C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\changeover_times.txt', delim = '\t')
clm <- lm(Changeover ~ New, data=change)
summary(clm)



#1
broadway <- read_csv(file='C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\playbill.csv')

blm <- lm(CurrentWeek ~ LastWeek, broadway)
summary(blm)

#a
#estimate = 0.9821 se = 0.001443 ci = 95% t value = 2.473
0.9821 + 2.473*0.001443
0.9821 - 2.473*0.001443
#0.9785315, 0.9856685

#No 1 is no a reasonable value as it falls outside the 95% confidence interval for the slope as calculated above

#b
#estimate = 6805 se = 9929 ci = 95% t value = 2.473
6805 + 9929*2.473
6805 - 9929*2.473

#We fail to reject the null hypothesis. The 95% confidence interval contains a range from -17749.42 to 31359.42 This range includes 10000 and thus makes it a 
#possible value for B0

#c
coef(blm)

#y = 0.9820815x + 6804.8860355, x = 400000
0.9820815*400000 + 6804.8860355
#399637.5

newdata <- data.frame(LastWeek = 400000)
predict(blm, newdata, interval='predict')

#(359832.8, 439442.2)

#No. The upper bound on the 95% confidence interval is well below 450000. An input of that size would not be expected

#d
#This prediction is not a terrible general guideline for quick, simple calculations. Our regression model determined that a week could expected
#about 98% of the business of the previous week. For a quick approximation, 100% is close enough to 98%

#2

indicators <- read_delim(file='C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\indicators.txt', delim = '\t')

ilm <- lm(PriceChange ~ LoanPaymentsOverdue, indicators)

summary(ilm)

ggplot(ilm) +
  geom_point(aes(LoanPaymentsOverdue, PriceChange))

#y = -2.2485x+4.5145

#a
-2.2485 + 0.9033*2.472878
-2.2485 - 0.9033*2.472878

#The confidence interval is from -4.482251 to -0.0147493 There is evidence to suggest the slope is statistically significant. This is because the slope does
#not include 0

#b
-2.2485*4+4.5145
dat <- data_frame(LoanPaymentsOverdue = 4)

predict(ilm, dat, interval='confidence')

#No 95% is not reasonable. It can be stated with 95% confidence that the regression slope in contained in -6.648849 and -2.310322

#3

#a

# b0 = 0.6417099 se = 0.1222707 t-value 2.368452

0.6417099 + 0.1222707*2.368452
0.6417099 - 0.1222707*2.368452

#0.3521176 - 0.9313022


#b
# b1 = 0.0112916 se = 0.0008184 t-value 2.368452

0.0112916 + 0.0008184*2.368452
0.0112916 - 0.0008184*2.368452

# 0.009353259 - 0.01322994

# We fail to reject the null hypothesis. There is evidence to suggest that the slope is equal to 0.01

#c

0.0112916*130+0.6417099 #2.109618

2.1092618 + 2.368452*0.0008184*sqrt(1 + 1/30 + 0)
2.1092618 - 2.368452*0.0008184*sqrt(1 + 1/30 + 0)

# 2.107291 - 2.111232

#5

#D. RSS measures the size of the error in the regression line. That is, the amount of variability not explained by the model. The second model has
# significantly more variability in this regard. Conversly, SSreg indicates the variability from the model. The variability in the model is much smaller than
#that of model 2.

#7

#The true slope of the regression line (that is, of the population) is unknown. The slope of the regression line as plotted is our best guess as to that slope
#The confidence interval indicates the range of values that the true slope could potentially be. However, this does not mean that all points should fall inside
#this range. Any given single point could potentially be significantly higher or lower due to error and thus fall out of the range of the confidence interval
#for the regression line. We are in essence making a prediction based on a prediction. That is, the regression slope could be any of a range of value and thus
#any given point could be in an even larger range of values as it must encompass all potential range of values for the regression line.






















