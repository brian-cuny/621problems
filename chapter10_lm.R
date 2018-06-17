
statedata <- data.frame(state.x77, row.names = state.abb)

b <- leaps::regsubsets(Life.Exp ~ . , data=statedata)
rs <- summary(b)
rs
rs$which
which.max(rs$adjr2)

AIC <- 50*log(rs$rss/50) + (2:8)*2
plot(AIC ~ I(1:7), ylab='AIC', xlab='#')

plot(2:8, rs$adjr2, xlab='Num', ylab='R2')
which.max(rs$adjr2)


plot(2:8, rs$cp, xlab='Num', ylab='CP')
abline(0, 1)


# for large models --------------------------------------------------------

lmod <- lm(Life.Exp ~ ., data=statedata)
step(lmod)


#10.1

data(prostate, package='faraway')

pairs(prostate)

l.pros <- lm(lpsa ~ ., prostate)
summary(l.pros)

#a
l.back <- lm(lpsa ~ lcavol + lweight + svi, prostate)
summary(l.back)
plot(l.back)

#b
b <- leaps::regsubsets(lpsa ~ ., prostate)
rs <- summary(b)
summary(b)
rs$which

AIC <- nrow(prostate)*log(rs$rss/nrow(prostate)) + (2:9)*2
plot(AIC ~ I(1:8), ylab='AIC', xlab='#')

l.aic <- lm(lpsa ~ lcavol + lweight + svi, prostate)

#c

plot(2:9, rs$adjr2, xlab='Num', ylab='R2')
which.max(rs$adjr2)

l.r2 <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + pgg45, prostate)
summary(l.r2)

#d

plot(2:9, rs$cp, xlab='Num', ylab='CP')
abline(0, 1)


#6 predictors

#10.6

data(seatpos, package='faraway')

#a
l.all <- lm(hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg, seatpos)
summary(l.all)

#Leg length is negatively associated with hipcenter. However, the relationship is not statistically significant

#b

predict(l.all, newdata = data.frame(Age=mean(seatpos$Age), Weight = mean(seatpos$Weight), HtShoes = mean(seatpos$HtShoes), Ht = mean(seatpos$Ht),
                                    Seated = mean(seatpos$Seated), Arm = mean(seatpos$Arm), Thigh = mean(seatpos$Thigh),  Leg = mean(seatpos$Leg)),
                                    interval='prediction')



#c
b <- leaps::regsubsets(hipcenter ~ ., seatpos)
rs <- summary(b)
summary(b)
rs$which

AIC <- nrow(seatpos)*log(rs$rss/nrow(seatpos)) + (2:9)*2
plot(AIC ~ I(1:8), ylab='AIC', xlab='# of PREDICTORS')

l.aic <- lm(hipcenter ~ Age + Ht + Leg, seatpos)
summary(l.aic)

#Leg length is still negatively associated with hipcenter. The realtionship is still not statistically significant

predict(l.aic, newdata = data.frame(Age=mean(seatpos$Age), Weight = mean(seatpos$Weight), HtShoes = mean(seatpos$HtShoes), Ht = mean(seatpos$Ht),
                                    Seated = mean(seatpos$Seated), Arm = mean(seatpos$Arm), Thigh = mean(seatpos$Thigh),  Leg = mean(seatpos$Leg)),
        interval='prediction')

#They are identical. We were able to find the same level of confidence and prediction with 5 fewer predictors. This is clearly a better model.

stats::AIC(l.all)

plot(b, scale='Cp')
plot(b)






