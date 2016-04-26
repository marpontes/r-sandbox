# 1.1 
# Reading the data
census = read.csv("census.csv")
str(census)

# Split the data
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)

# Build the logistic regression model
logmodel = glm(over50k ~ . , data = train, family=binomial)
summary(logmodel)

# 1.2 Accuracy of prediction
logpredict = predict(logmodel, newdata=test, type="response")
table(test$over50k, logpredict>=0.5)
(9051+ 1888)/(9051+ 1888+1190+662)

# 1.3 baseline accuracy
table(test$over50k)
9713/(9713+3078)

# 1.4 AUC
library(ROCR)
ROCRpred = prediction(logpredict, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 2.1 - Working with trees
CART = rpart(over50k ~ . , data= train,method="class")
prp(CART)

# 2.4

pred = predict(CART, newdata=test,type = "class")
pred
table(test$over50k, pred)
(9243+ 1596)/(9243+ 1596+ 1482+ 470)

# 2.5 
library(ROCR)
PredictROC = predict(CART, newdata = test)
PredictROC

pred = prediction(PredictROC[,2], test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# 2.6 AUC
as.numeric(performance(pred, "auc")@y.values)

# 3.1 Random Forest
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
library(randomForest)
randomForest(over50k ~ . , data = trainSmall, ntree=200, nodesize=25 )
str(trainSmall)

# 3.2 
set.seed(1)
forest = randomForest(over50k ~ . -nativecountry, data = trainSmall, ntree=200, nodesize=25 )
(1404+ 299)/(1404+ 299+110+187)

# 3.3

vu = varUsed(forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forest$forest$xlevels[vusorted$ix]))

# 3.4
varImpPlot(forest)

# 4.1
set.seed(2)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

# 4.2 Fit the training set using cp=0.002
tree2 = rpart(over50k ~ . , data = train, control=rpart.control(cp = 0.002))
pred2 = predict(tree2,newdata=test,type = "class")
table(test$over50k,pred2)

(9178+ 1838)/(9178+ 1838+ 535+ 1240)
prp(tree2)