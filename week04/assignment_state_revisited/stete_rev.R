# 1.1
# Loading the data
data(state)
statedata = data.frame(state.x77)
str(statedata)

# Building the linear regression model
linearmodel = lm(Life.Exp ~ . ,data= statedata)

# Watching the output to see the Adjusted R-Squared
summary(linearmodel)

# 1.2 SSE
SSE = sum(linearmodel$residuals^2)
SSE

# 1.3

linearmodel02 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad,data=statedata)
summary(linearmodel02)

# 1.4 SSE of adjusted model
SSE2 = sum(linearmodel02 $residuals^2)
SSE2

# 2.1 CART Tree
CART = rpart(Life.Exp ~ . , data= statedata)
prp(CART)

# 2.2

pred = predict(CART)
summary(pred)
SSE3 = sum((pred - statedata$Life.Exp)^2)
SSE3

# 2.3 Recreating with minbucket=5
CART = rpart(Life.Exp ~ . , data= statedata,minbucket=5)
prp(CART)


# 2.4 SSE of this tree
pred = predict(CART)
SSE3 = sum((pred - statedata$Life.Exp)^2)
SSE3

# 2.6 Can we do better?
CART2 = rpart(Life.Exp ~ Area , data= statedata,minbucket=1)
prp(CART2)
pred2 = predict(CART2)
SSE4 = sum((pred2 - statedata$Life.Exp)^2)
SSE4

# 3.1
library(caret)
set.seed(111)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.01) 
train(Life.Exp ~ . , data = statedata, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )


# 3.2 Creating a tree with cp=0.12
CVTree = rpart(Life.Exp ~ . , data = statedata, control=rpart.control(cp = 0.12))
prp(CVTree)


# 3.3 SSE

predCV = predict(CVTree)
SSECV = sum((predCV - statedata$Life.Exp)^2)
SSECV

#3.5 Now just with area 

set.seed(111)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.01) 
train(Life.Exp ~ Area , data = statedata, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

CVTreeArea = rpart(Life.Exp ~ Area , data = statedata, control=rpart.control(cp = 0.02))
prp(CVTreeArea)

# 3.6 SSE
predCVArea = predict(CVTreeArea)
SSECVArea = sum((predCVArea - statedata$Life.Exp)^2)
SSECVArea
