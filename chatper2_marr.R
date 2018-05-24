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














