#1.1 - reading
letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)
table(test$isB)
1175/(1175+383)

#1.2
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
summary(CARTb)
prp(CARTb)
pred01 = predict(CARTb, newdata=test, type="class")
table(test$isB,pred01)
(1118 + 340)/(1118 + 340+ 43+ 57)

#1.3 Random Forest
library(randomForest)
set.seed(1000)
bForest = randomForest(isB ~ . - letter, data = train )
PredictForest = predict(bForest, newdata = test)
table(test$isB, PredictForest)
(1165+374)/(1165+374+9+10)


#2.1
letters$letter = as.factor( letters$letter )

set.seed(2000)
lsplit = sample.split(letters$letter, SplitRatio = 0.5)
ltrain = subset(letters, lsplit==TRUE)
ltest = subset(letters, lsplit==FALSE)

table(ltest$letter)
401/(395 +383 +401 +379)

#2.2 
CART = rpart(letter ~ . -isB, data=ltrain, method="class")
predCART = predict(CART, newdata=ltest, type="class")
table(ltest$letter, predCART)
(348 + 318 + 363 + 340)/nrow(ltest)

#2.3
library(randomForest)
set.seed(1000)
forest = randomForest(letter ~ . - isB, data = ltrain )
randomPred = predict(forest, newdata = ltest)
table(ltest$letter, randomPred)
(390+380+393+364)/nrow(ltest)