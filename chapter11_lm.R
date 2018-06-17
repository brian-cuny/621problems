data(fat, package='faraway')

plot(neck ~ knee, fat)
plot(chest ~ thigh, fat)
plot(hip ~ wrist, fat)

cfat <- fat[, 9:18]
prfat <- prcomp(cfat)
summary(prfat)
prfat$rot[, 1] #not scaled

prfatc <- prcomp(cfat, scale=TRUE)
summary(prfatc)
prfatc$rot[, 1] #scaled



#Outliers in higher dimensions
robfat <- MASS::cov.rob(cfat)
md <- mahalanobis(cfat, center=robfat$center, cov=robfat$cov)
n <- nrow(cfat)
p <- ncol(cfat)
plot(qchisq(1:n/(n+1), p), sort(md), xlab="chi2", ylab='Sorted Mahalanobis dist')
abline(0, 1)

l.all <- lm(fat$brozek ~ ., data=cfat)
summary(l.all)

l.modpcr <- lm(fat$brozek ~ prfatc$x[, 1:2])
summary(l.modpcr)
faraway::vif(model.matrix(l.modpcr)[, -1])


l.modr <- lm(fat$brozek ~ scale(abdom) + I(scale(ankle)-scale(abdom)), data=cfat)
summary(l.modr)
faraway::vif(model.matrix(l.modr)[, -1])


# meat example ------------------------------------------------------------

data(meatspec, package='faraway')

trainmeat <- meatspec[1:172, ]
testmeat <- meatspec[173:215, ]

modlm <- lm(fat ~ ., trainmeat)
summary(modlm)
step(modlm)

meatpca <- prcomp(trainmeat[, -101])
round(meatpca$sdev, 3)

pcrmod <- pls::pcr(fat ~ ., data=trainmeat, ncomp=50)

rmse <- function(x, y){
  sqrt(mean((x-y)^2))
}

rmse(predict(pcrmod, ncomp=4), trainmeat$fat)


set.seed(123)
pcrmod <- pls::pcr(fat ~ ., data=trainmeat, validation='CV', ncomp=50)
pcrCV <- pls::RMSEP(pcrmod, estimate='CV')
plot(pcrCV)
which.min(pcrCV$val)
ypred <- predict(pcrmod, testmeat, ncomp=18)
rmse(ypred, testmeat$fat)


# partial least squares ---------------------------------------------------

set.seed(123)
plsmod <- pls::plsr(fat ~ ., data=meatspec[1:172, ], ncomp=50, validation='CV')
coefplot(plsmod, ncomp=4, xlab='Frequency')
plsCV <- pls::RMSEP(plsmod, estimate='CV')
plot(plsCV)

#what is the model though??????



# ridge regression --------------------------------------------------------

rgmod <- MASS::lm.ridge(fat ~ ., trainmeat, lambda = seq(0, 5e-8, len=21))
matplot(rgmod$lambda, coef(rgmod), type='l', xlab=expression(lambda), ylab=expression(hat(beta)), col=1)
summary(rgmod)

which.min(rgmod$GCV)

ypred <- cbind(1, as.matrix(trainmeat[, -101])) %*% coef(rgmod)[8,]
rmse(ypred, trainmeat$fat)

library(lars)
lmod <- lars::lars(as.matrix(statedata[, -4]), statedata$Life.Exp)
summary(lmod)
plot(lmod)

set.seed(123)
cvlmod <- lars::cv.lars(as.matrix(statedata[, -4]), statedata$Life.Exp)
cvlmod$index[which.min(cvlmod$cv)]
predict(lmod, s=0.65657, type='coef', mode='fraction')$coef

l.scale <- lm(Life.Exp ~ scale(Population) + scale(Murder) + scale(HS.Grad) + scale(Frost), statedata) #not same scaling as lasso method??
summary(l.scale)


# questions ---------------------------------------------------------------

#11.1

cor(seatpos) #high correlation

lm.reg <- lm(hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg, seatpos)
summary(lm.reg)

seat.part <- seatpos %>%
  select(hipcenter, HtShoes, Ht, Seated, Arm, Thigh, Leg)

pr.seat <- prcomp(seat.part[, -1], scale=TRUE)
summary(pr.seat)
round(pr.seat$rot[, 1], 2)

#pr.seat contains model, looking for outliers in high dimensions, which can mess up pcr

lm.2 <- lm(seat.part$hipcenter ~ pr.seat$x[, 1])
summary(lm.2)

round(pr.seat$rot[, 1], 2) #select height, it is the most extreme
round(pr.seat$rot[, 2], 2) # thigh - seated, the most extreme difference

lm.3 <- lm(seat.part$hipcenter ~ scale(Ht), data=seat.part)
summary(lm.3)

pairs(seat.part)

new.data <- data.frame(Age = 64.8, Weight = 263.7, HtShoes = 181.08, Ht = 178.56, Seated = 91.44, Arm = 35.64, Thigh = 40.95, Leg = 38.79)
predict(lm.reg, newdata = new.data, interval='prediction')
predict(lm.2, newdata = new.data, interval='prediction') #doesn't work?
predict(lm.3, newdata = new.data, interval='prediction')

#11.2

seat.plsr <- pls::plsr(hipcenter ~ ., data=seat.part, ncomp=6, validation='CV')
summary(seat.plsr)

scale.seat.part <- cbind(seat.part[, 1], as_data_frame(scale(seat.part[, -1]))) %>%
  rename('hipcenter' = 'seat.part[, 1]')

scale.seat.plsr <- pls::plsr(hipcenter ~ ., data=scale.seat.part, ncomp=6, validation='CV')
summary(scale.seat.plsr)

plot(pls::RMSEP(seat.plsr))

scale.seat.plsr$coefficients

answer <- pls::plsr(hipcenter ~ ., data=seat.part, ncomp=1, validation='CV')
summary(answer)

predict(answer, newdata = new.data, interval='prediction')

answer$coefficients


summary(lm(hipcenter ~ HtShoes + Ht + Seated + Arm + Thigh + Leg, seatpos))



