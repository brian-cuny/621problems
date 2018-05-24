library(faraway)
library(tidyverse)

data(gala, package='faraway')

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data=gala)

cor(model.matrix(~Area + Elevation + Nearest + Scruz + Adjacent, data=gala))

summary(lmod)

residuals(lmod)
fitted(lmod)
df.residual(lmod)
deviance(lmod)

coef(lmod)

sigma(lmod)


data(odor, package='faraway')

View(odor)

cov(gala[, -1])


#Linear Models w R
#Chapter 2

#1 
data(teengamb, package='faraway')
teenlm <- lm(gamble ~ sex + status + income + verbal, teengamb)
summary(teenlm)

#a 
#R^2 is 0.5267 and adjusted is 0.4816. So about 52.67% of the variation in the response variable can be explained by the predictors

#b
res <- data_frame(y = residuals(teenlm)) %>%
        mutate(x=row_number(),
               z = abs(y))

res %>%
  filter(y == max(y))

residuals(teenlm) %>%
  max()
#Case number 24 has a residual of 94.25222. This is the largest

# c
mean(residuals(teenlm))
# The mean is essentially 0

median(residuals(teenlm))
# The median is -1.451392

#d
cor(residuals(teenlm), fitted(teenlm))
# The correllation is essentially 0

#e
cor(residuals(teenlm), teengamb$income)
#The correlation is essentially 0

#f
#There would be a difference of 22.11833 dollars (depending on whether 1 or 0 represents male/female)

#2
data(uswages, package='faraway')

uswageslm1 <- lm(wage ~ educ + exper, uswages)
summary(uswageslm1)

#Each year of education is associated with approximately $51.18 increase in weekly wages

uswageslm2 <- lm(log(wage) ~ educ + exper, uswages)
summary(uswageslm2)

#Each year of education is associated with approximately 10 percent increase in logged weekly wages.
#The second interpretation makes more sense. An examination of the graph shows extreme upper outliers on the original data set

ggplot() +
  geom_histogram(aes(uswages$wage))

ggplot() +
  geom_histogram(aes(log(uswages$wage)))

#3
set.seed(1)

x <- 1:20
y <- x + rnorm(20)

dat <- data_frame(x = x, 
                  y = y)

polylm <- lm(y ~ x + I(x^2))
summary(polylm)
#y = 1.0065237x + 0.0007171x^2

ggplot() +
  geom_point(aes(x, y))

#Direct Calculation

directx <- model.matrix(~ poly(x, degree=2, raw=TRUE), dat)
directxtxi <- solve(t(directx) %*% directx)
directxtxi %*% t(directx) %*% dat$y

solve(crossprod(directx, directx), crossprod(directx, dat$y))

#Second power still works

polylm3 <- lm(y ~ x + I(x^2) + I(x^3))
summary(polylm3)

directx <- model.matrix(~ poly(x, degree=3, raw=TRUE), dat)
directxtxi <- solve(t(directx) %*% directx)
directxtxi %*% t(directx) %*% dat$y

#Third power works

polylm4 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4))
summary(polylm4)

directx <- model.matrix(~ poly(x, degree=4, raw=TRUE), dat)
directxtxi <- solve(t(directx) %*% directx)
directxtxi %*% t(directx) %*% dat$y

#Forth power works

polylm5 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
summary(polylm5)

directx <- model.matrix(~ poly(x, degree=5, raw=TRUE), dat)
directxtxi <- solve(t(directx) %*% directx)
directxtxi %*% t(directx) %*% dat$y

#Fifth power works

polylm6 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6))
summary(polylm6)

directx <- model.matrix(~ poly(x, degree=6, raw=TRUE), dat)
directxtxi <- solve(t(directx) %*% directx)
directxtxi %*% t(directx) %*% dat$y

#Stops works at 6th power

#4
data(prostate, package = 'faraway')

plm <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp + pgg45 + gleason, data=prostate)
summary(plm)
rse <- c(0.78750, 0.7506, 0.7168, 0.7108, 0.7073, 0.7102, 0.7048, 0.7084)
r2 <- c(0.5394, 0.5859, 0.6264, 0.6366, 0.6441, 0.6451, 0.6544, 0.6548)

ggplot() +
  geom_point(aes(rse, r2))

#5

psa <- lm(lpsa ~ lcavol, data=prostate)
cavol <- lm(lcavol ~ lpsa, data=prostate)

coefficients(psa)
coefficients(cavol)

ggplot() +
  geom_abline(aes(slope=0.7193201, intercept=1.5072979), color='red') +
  geom_abline(aes(slope=0.7499191, intercept=-0.5085802), color='blue') +
  scale_x_continuous(limits=c(50, 100)) + 
  scale_y_continuous(limits=c(40, 80))

#Intersection is at 65.88052, 48.89648

#6

data(cheddar, package='faraway')
#a
clm <- lm(taste ~ Acetic + H2S + Lactic, data=cheddar)
sumary(clm)

#Acetic = 0.32774, H2S = 3.91184 and Lactic = 19.67054

#b
cor(cheddar$taste, fitted.values(clm))^2
#The correlation is 0.8073256. The square is 0.6517747 This is the r-squared value

#c
clm0 <- lm(taste ~ 0 + Acetic + H2S + Lactic, data=cheddar)
sumary(clm0)

cor(cheddar$taste, fitted.values(clm0))^2
#The correlation is 0.7901946. The square is 0.6244075. R^2 reported is 0.89. My square is a more reasonable term.

#d
mm <- model.matrix(~ Acetic + H2S + Lactic, cheddar)
qrx <- qr(mm)
f <- t(qr.Q(qrx)) %*% cheddar$taste
backsolve(qr.R(qrx), f)

#7

data(wafer, package='faraway')

#a

wlm <- lm(resist ~ x1 + x2 + x3 +x4, wafer)
sumary(wlm)
wmm <- model.matrix(~ x1 + x2 + x3 + x4, wafer)

#+ has become 1 while - has become 0

#b
cor(wmm)
#There are intercept correlations missing because there is no reasonable meaning or interpretation or relationship with the intercept

#c
#The estimate for x1 is 25.762 so an increase of that much resistance is expected in the transition form low to high

#d
wlm4 <- lm(resist ~ x1 + x2 + x3, wafer)
sumary(wlm4)
#The numer of predictors has changed (obviously) and the r^2 has lowered. The standard error has increased (as expectd based on the earlier problem). The
#intercept has also changed. The interpretational value of x1, x2 and x3 have stayed the same

#e
#The is no correlation between x1, x2, x3 and x4. That is, they are independant. As a result the dropping of x4 has had no effect on the estimate of the other
#3 predictors.


#8

data(truck, package='faraway')

truck <- truck %>%
  mutate(B = ifelse(B == '-', -1, 1),
         C = ifelse(C == '-', -1, 1),
         D = ifelse(D == '-', -1, 1),
         E = ifelse(E == '-', -1, 1),
         O = ifelse(O == '-', -1, 1))

#a
tlm <- lm(height ~ B + C + D + E + O, truck)
sumary(tlm)

#b

tlmO <- lm(height ~ B + C + D + E, truck)
sumary(tlmO)

tmm <- model.matrix(~ B + C + D + E + O, truck)
cor(tmm)

#There is no change in the estimates. This could have been perdicted because there is no correlation between the B, C, D, E and O elements.

#c
truck <- truck %>%
  mutate(A = B + C + D + E + O)

tlmA <- lm(height ~ A + B + C + D + E + O, truck)
sumary(tlmA)

#Coefficient O is not present because all of its predictive value is present in A. That is, the matrix is not linearly independent so the O column was left out.

#d
mmA <- model.matrix(~ A + B + C + D + E + O, truck)
solve(t(mmA) %*% mmA)

#The inverse of the matrix X^T*X cannot be found because the matrix's row vectors is not linearly independent

#e

mmAqrx <- qr(mmA)
f <- t(qr.Q(mmAqrx)) %*% truck$height
backsolve(qr.R(mmAqrx), f)

#No, these results are not sasisfactory. The vector (other than the intercept) is essentially 0

#f
qr.coef(mmAqrx, truck$height)



















