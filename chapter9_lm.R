library(tidyverse)
library(faraway)
library(simex)

data(savings, package='faraway')
lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
MASS::boxcox(lmod, plotit=T)
MASS::boxcox(lmod, plotit=T, lambda=seq(0.5, 1.5, by=0.1))

data(gala, package='faraway')
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
MASS::boxcox(lmod, lambda=seq(-.25, 0.75, by=0.05), plotit=T)

MASS::boxcox(l.combine, plotit=T, lambda=seq(1, 2, by=0.1))

gamod <- mgcv::gam(TARGET_WINS ~ s(TEAM_BATTING_SO) + s(TEAM_PITCHING_BB) + s(TEAM_PITCHING_SO) + s(TEAM_FIELDING_E) +
             s(TEAM_BATTING_POS), data=money.ball.2)
plot(gamod)

#9.1

data(aatemp, package='faraway')


#a
l.temp <- lm(temp ~ year, aatemp)
plot(l.temp)

ggplot(aatemp, aes(year, temp)) +
  geom_point() +
  geom_smooth(method='lm')

#No. There is not a linear trend. The residual plot and the ggplot demonstrate a significant curve in the data.

#b
ggplot() +
  geom_point(aes(aatemp$temp, lag(aatemp$temp))) +
  geom_smooth(aes(aatemp$temp, lag(aatemp$temp)), method='lm')

#A plot comparing values to previous ones shows a positive correlation. This indicates the errors are not independent.

#c
l.temp.5 <- lm(temp ~ poly(year, 5), aatemp)
summary(l.temp.5)

predicted <- data.frame(temp = predict(l.temp.5, aatemp), year=aatemp$year)

ggplot(aatemp, aes(year, temp)) +
  geom_point() +
  geom_line(color='red', data=predicted, aes(year, temp))

predict(l.temp.5, newdata = data.frame(year=2020), interval='prediction')

#d
lhs <- function(x) ifelse(x <= 1930, 1930-x, 0)
rhs <- function(x) ifelse(x <= 1930, 0, x-1930)
l.temp.2 <- lm(temp ~ lhs(year) + rhs(year), aatemp)

predicted.2 <- data.frame(temp = predict(l.temp.2, aatemp), year=aatemp$year)

ggplot(aatemp, aes(year, temp)) +
  geom_point() +
  geom_line(color='red', data=predicted.2, aes(year, temp))

#The data does not support this conclusion. The slope for dates pre 1930 is actually steeper than those afterwards

#e

ssf <- smooth.spline(aatemp$year, aatemp$temp)
matplot(aatemp$year, cbind(aatemp$temp, ssf$y), type='pl', ylab='y', lty=1, pch=20, col=1)

#Not really. I feel like this is an overfit. 


#3

data(ozone, package='faraway')

l.ozone <- lm(I(O3^(1/3)) ~ temp + humidity + ibh, ozone)
MASS::boxcox(l.ozone, plotit=T, lambda=seq(0, .6, by=0.1))
summary(l.ozone)
#I would suggest a cube root transformation
plot(l.ozone)

termplot(l.ozone, partial.resid=TRUE, smooth=panel.smooth, terms=1:3)




