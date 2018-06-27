library(tidyverse)


pima <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621problems\\Pima.te.csv') %>%
  select(-X1) %>%
  mutate(type = ifelse(type == 'Yes', 1, 0))

library(caTools)

split <- sample.split(pima, SplitRatio = 0.8)
training <- subset(pima, split == 'TRUE')
test <- subset(pima, split == 'FALSE')

model <- glm(type ~ .-skin, training, family=binomial)
summary(model)

res <- predict(model, test, type='response')
table(ActualValue=test$type, PredictedValue=res > 0.3)

llibrary(ROCR)

ROCRPred <- prediction(res, training$type)
ROCRPref <- performance(ROCRPred, 'tpr', 'fpr')
plot(ROCRPref, colorize=TRUE, print.cutoffs.at = seq(0.1, by=0.1))
