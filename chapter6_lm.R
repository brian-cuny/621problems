library(tidyverse)
library(faraway)

data(savings, package='faraway')
lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)

ggplot() +
  geom_point(aes(fitted(lmod), residuals(lmod))) +
  geom_hline(yintercept=0, color='red')

ggplot() +
  geom_density(aes(residuals(lmod)))

qqnorm(residuals(lmod))
qqline(residuals(lmod))

shapiro.test(residuals(lmod))

countries <- row.names(savings)
halfnorm(hatvalues(lmod), labs=countries)

qqnorm(rstandard(lmod))
qqline(rstandard(lmod))

stud <- rstudent(lmod)
stud[which.max(abs(stud))]

qt(.05/(50*2), 44)

termplot(lmod, partial.resid=TRUE, terms=1)


# - -----------------------------------------------------------------------


data(gala, package='faraway')
lmod2 <- lm(sqrt(Species) ~ Area + Elevation + Scruz + Nearest + Adjacent, gala)

ggplot() +
  geom_density(aes(residuals(lmod)))

qqnorm(residuals(lmod2))
qqline(residuals(lmod2))

shapiro.test(residuals(lmod2))


# 3 -----------------------------------------------------------------------

data(star, package='faraway')
plot(star$temp, star$light)


#1

data(sat, package='faraway')

lmod <- lm(total ~ expend + salary + ratio + takers, sat)

#a

ggplot() +
  geom_point(aes(fitted(lmod), residuals(lmod))) +
  geom_hline(yintercept=0, color='red')

#Constant variation appears reasonable

#b

ggplot() +
  geom_density(aes(residuals(lmod)))

qqnorm(residuals(lmod))
qqline(residuals(lmod))

shapiro.test(residuals(lmod))

#The QQ plot shows no major deviations from normality. The shaprio.test p-value indicates that the null hypothesis cannot be rejected


#c

states <- row.names(sat)
halfnorm(hatvalues(lmod), labs=states)

#Utah and California are high leverage states


#d

stud <- rstudent(lmod)
stud[which.max(abs(stud))]

#West Virginia is close to, but not technically an outlier

#e

plot(lmod)

#The final plot shows that Utar is incredibly close to being an influential point due to the fact that it has high leverage. However, it is just barely show of the
#threshold. It is probably still a good idea to examine this point closer.

#f
par(mfrow=c(2,2))
termplot(lmod, partial.resid=TRUE, terms=1:4)


sat$status <- ifelse(sat$takers < 40, 'few', 'many')
ggplot(sat, aes(takers, total)) +
  geom_point() +
  stat_smooth(aes(group=status), method='lm') +
  stat_smooth(color='red')

#There appears to be 2 distinct groups in takers that form a non-linear relationship. Specifically takers below 40 and above 40. Above 40 the trend
#appears to level out while below that there is a steep decline. This indicates a exponential decay fit or possible quadratic





