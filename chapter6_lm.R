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


#2

data(teengamb, package='faraway')

lteen <- lm(gamble ~ sex + status + income + verbal, teengamb)

#a

ggplot() +
  geom_point(aes(fitted(lteen), residuals(lteen))) +
  geom_hline(yintercept=0, color='red')

ggplot() +
  geom_point(aes(fitted(lteen), sqrt(abs(residuals(lteen))))) +
  geom_hline(yintercept=0, color='red')

#This model does not have constant variance. Specifically, it appears to follow a strong pattern


#b

ggplot() +
  geom_density(aes(residuals(lteen)))

qqnorm(residuals(lteen))
qqline(residuals(lteen))

shapiro.test(residuals(lteen))

#The residuals appears to show a long-tail error. There appears to be an extreme upper oulier and possible a lower outlier.

#c

people <- row.names(teengamb)
halfnorm(hatvalues(lteen), nlab=3, labs=people)

#2 users, 42 and 35 appear to be high leverage points

#d

stud <- rstudent(lteen)
stud[which.max(abs(stud))]

#Teen number 24 is almost garunteed to be an outlier.


#e

plot(lteen)

#Teen number 24 is an influential point. It appears to be influential as it is an incredibly far outlier

#f

par(mfrow=c(2,2))
termplot(lteen, partial.resid=TRUE, terms=1:4)


ggplot(teengamb, aes(income, gamble)) +
  geom_point() +
  stat_smooth(aes(group=factor(sex), color=factor(sex)), method='lm')

#Gender apparently plays a gigantic roll in gambling amount

#3

data(prostate, package='faraway')

lpros <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, prostate)

#a

ggplot() +
  geom_point(aes(fitted(lpros), residuals(lpros))) +
  geom_hline(yintercept=0, color='red')

ggplot() +
  geom_point(aes(fitted(lpros), sqrt(abs(residuals(lpros))))) +
  geom_hline(yintercept=0, color='red')

#There appears to be constant variance across the errors

#b

ggplot() +
  geom_density(aes(residuals(lpros)))

qqnorm(residuals(lpros))
qqline(residuals(lpros))

shapiro.test(residuals(lpros))

#The errors are roughly normally distributed

#c

halfnorm(hatvalues(lpros), nlab=3, labs=row.names(prostate))

#There appears to be one high leverage point, case #32

#d

stud <- rstudent(lpros)
stud[which.max(abs(stud))]

#There appear to be no outliers

#e
par(mfrow=c(1,1))
plot(lpros)

#There are no influential points

#f

par(mfrow=c(2,4))
termplot(lpros, partial.resid=TRUE, terms=1:8)

#There are a few areas where further exploration may be beneficial, but nothing that immediately stands out as distinct demarcation lines. There area
# a number of situations where there are groupings, but this appears to not create outliers or distinctly different information.


#4

data(swiss)

#a

lswiss <- lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, swiss)
sumary(lswiss)

#a

ggplot() +
  geom_point(aes(fitted(lswiss), residuals(lswiss))) +
  geom_hline(yintercept=0, color='red')

ggplot() +
  geom_point(aes(fitted(lswiss), sqrt(abs(residuals(lswiss))))) +
  geom_hline(yintercept=0, color='red')

#There is pretty clear non-constant variance due almost entirely to a single point. It is likely that this point will end up being an outlier or of high leverage or 
#both

#b
par(mfrow=c(1,1))
ggplot() +
  geom_density(aes(residuals(lswiss)))

qqnorm(residuals(lswiss))
qqline(residuals(lswiss))

shapiro.test(residuals(lswiss))

#All the available evidence supports normality of the errors

#c

halfnorm(hatvalues(lswiss), nlab=3, labs=row.names(swiss))

#2 locations are very high leverage. La Vallee and V. De Geneve

#d

stud <- rstudent(lswiss)
stud[which.max(abs(stud))]

#There are no outliers.

#e
par(mfrow=c(2,2))
plot(lswiss)


#The two high leverage points do not appear to be influential. The apparent outlier was neither an outlier nor influential

#f

par(mfrow=c(2,3))
termplot(lswiss, partial.resid=TRUE, terms=1:5)


swiss$relig <- ifelse(swiss$Catholic > 60, '3', ifelse(swiss$Catholic > 40, '2', '1'))

ggplot(swiss, aes(Catholic, Fertility)) +
  geom_point() +
  stat_smooth(aes(group=relig, color=relig), method='lm') +
  stat_smooth(method='lm')

#The catholic proportion appears to be significantly seperated. Specifically there are two distinct groups or low and high populations. Their rates may
#be statistically different. There is a middle group that is of inconsequential size and doesn't really fit into either group.

swiss$smart <- ifelse(swiss$Education > 15, '1', '2')

ggplot(swiss, aes(Education, Fertility)) +
  geom_point() +
  stat_smooth(aes(group=smart, color=smart), method='lm') +
  stat_smooth(method='lm')

#I originally thought that there would be a significance between education levels but seperating the date does not appear to produce such a result. There
#are few upper education points, but they seem generally consistent with the overall data.

#5

data(cheddar)

lched <- lm(taste ~ Acetic + H2S + Lactic, cheddar)
sumary(lched)

#a

ggplot() +
  geom_point(aes(fitted(lched), residuals(lched))) +
  geom_hline(yintercept=0, color='red')

ggplot() +
  geom_point(aes(fitted(lched), sqrt(abs(residuals(lched))))) +
  geom_hline(yintercept=0, color='red')

#The variance appears constant

#b

par(mfrow=c(1,1))
ggplot() +
  geom_density(aes(residuals(lched)))

qqnorm(residuals(lched))
qqline(residuals(lched))

shapiro.test(residuals(lched))

#The normality assumption is reasonable.

#c

halfnorm(hatvalues(lched), nlab=3, labs=row.names(cheddar))

#I see no points with extremely high leverage

#d

stud <- rstudent(lched)
stud[which.max(abs(stud))]

#There are no apparent outliers

#e
par(mfrow=c(2,2))
plot(lched)

#There are no influential points

#f

par(mfrow=c(2,2))
termplot(lched, partial.resid=TRUE, terms=1:3)

#There is no apparent pattern or grouping in any of the term plots. 

#6

data(happy)

lhap <- lm(happy ~ money + sex + love + work, happy)
sumary(lhap)

#a

ggplot() +
  geom_point(aes(fitted(lhap), residuals(lhap))) +
  geom_hline(yintercept=0, color='red')

ggplot() +
  geom_point(aes(fitted(lhap), sqrt(abs(residuals(lhap))))) +
  geom_hline(yintercept=0, color='red')


#There appears to be non constant variance and perhaps a quadradic pattern in the errors.

#b

par(mfrow=c(1,1))
ggplot() +
  geom_density(aes(residuals(lhap)))

qqnorm(residuals(lhap))
qqline(residuals(lhap))

shapiro.test(residuals(lhap))

#The errors are approximately normal

#c

halfnorm(hatvalues(lhap), nlab=5, labs=row.names(happy))

#There are numerous upper points that appear to be high leverage

#d

stud <- rstudent(lhap)
stud[which.max(abs(stud))]

qt(0.05/(39*2), df=39-4)

#A point number 36 is very close to being an outlier. It should probably receive manual inspection. From manual inspection it appears that it is 
#missing a value for money while none of the other cases are.

#e
par(mfrow=c(2,2))
plot(lched)


#There are no influential points

#f
par(mfrow=c(2,2))
termplot(lhap, partial.resid=TRUE, terms=1:4)

#Sex love and work all appear to be categorial and should be dealt with as such.

#7

data(tvdoctor)

ldoc <- lm(life ~ tv + doctor, tvdoctor)
sumary(ldoc)

#a

ggplot() +
  geom_point(aes(fitted(ldoc), residuals(ldoc))) +
  geom_hline(yintercept=0, color='red')

#There is clear non constant variance in the data. The model is completely invalid as created.

#b

par(mfrow=c(1,1))
ggplot() +
  geom_density(aes(residuals(ldoc)))

qqnorm(residuals(ldoc))
qqline(residuals(ldoc))

shapiro.test(residuals(ldoc))

#The data appears short tailed. This may be an issue. Under normal circumstances short-tailed distributions are not a problem, however, there is a 
#small numbe rof cases (38). 

#c

halfnorm(hatvalues(ldoc), nlab=5, labs=row.names(tvdoctor))

#Ethiopia and Myanmar are clear high leverage points. Both of there TV values are an order of magnitude larger than the rest of the data.

#d

stud <- rstudent(ldoc)
stud[which.max(abs(stud))]

#Ethiopia is a clear outlier.

#e
par(mfrow=c(2,2))
plot(ldoc)

#Once again Ethiopia is a clear influential point. This point may be heavily skewing the data.

#f

par(mfrow=c(1,2))
termplot(ldoc, partial.resid=TRUE, terms=1:2)

tvdoctor$isE <- ifelse(tvdoctor$doctor == 36660, '1', '0')

ggplot(tvdoctor, aes(tv, life)) +
  geom_point() +
  stat_smooth(aes(group=isE), method='lm')

ggplot(tvdoctor, aes(doctor, life)) +
  geom_point() +
  stat_smooth(aes(group=isE), method='lm')

#The graphs continue to show ethiopia as a clear outlier. The lack of normality and constant variation show a number of potential issues with the regression. 



