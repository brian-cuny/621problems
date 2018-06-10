library(tidyverse)
library(faraway)
library(simex)
library(caret)


salary.data <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\profsalary.txt', delim='\t') %>%
  select(-Case)

ggplot(salary.data, aes(Experience, Salary)) +
  geom_point()

food.data <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\nyc.csv') %>%
  select(-Case) %>%
  mutate(East = factor(East)) %>%
  column_to_rownames('Restaurant')

l.test.1 <- lm(Price ~ Food + Decor + Service + East, food.data)
summary(l.test.1)

travel <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\travel.txt', delim='\t')

ggplot(travel, aes(Age, Amount, group=C)) +
  geom_point(aes(color=factor(C)))

l.travel <- lm(Amount ~ Age*C, travel)
summary(l.travel)

anova(l.travel)

l.travel.less <- lm(Amount ~ Age, travel)
anova(l.travel.less) #bad p-value

l.test.2 <- lm(Price ~ Food*East + Decor*East + Service*East, food.data)
summary(l.test.2)


l.test.3 <- lm(Price ~ Food + Decor + East, food.data)
summary(l.test.3)

anova(l.test.2, l.test.3)
#3 is fine, 2 is unnecessarily complicated



#5.1

overdue <- read_delim('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\overdue.txt', delim='\t')
overdue <- overdue %>%
  mutate(TYPE = ifelse(row_number() <= 48, 'R', 'C'))

l.test.1 <- lm(LATE ~ BILL + TYPE, overdue)
summary(l.test.1)

l.test.2 <- lm(LATE ~ BILL*TYPE, overdue)
summary(l.test.2)

ggplot(overdue, aes(BILL, LATE, group=TYPE)) +
  geom_point(aes(color=TYPE))

anova(l.test.1, l.test.2)

#l.test.2, with the relationship between bill and type is the best model


#5.2
houston <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\marr\\HoustonChronicle.csv') 

#a

ggplot(houston, aes(`%Low income students`, `%Repeating 1st Grade`, group=Year, color=factor(Year))) +
  geom_point() +
  geom_smooth(method='lm')

l.test.1 <- lm(`%Repeating 1st Grade` ~ `%Low income students`, houston)
summary(l.test.1)

#Yes, there is strong evidence to suggest that an increase in low income students correlates with an increase in repeating 1st grade

#b

houston <- houston %>%
  mutate(Year = factor(Year))

l.test.2 <- lm(`%Repeating 1st Grade` ~ Year, houston)
summary(l.test.2)

#There is no evidence to suggest that there has been a change in students repeating first grade between 1994 and 2004

#c

l.test.3 <- lm(`%Repeating 1st Grade` ~ `%Low income students`*Year, houston)
summary(l.test.3)

anova(l.test.1, l.test.3)

#No, there is no evidence of a relationship between %Low income students and year. That is, low income students are failing at the same rate in 1994 and 2004




