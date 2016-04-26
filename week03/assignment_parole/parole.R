# 1.1 - Loading the data
  parole = read.csv("parole.csv")
  str(parole)
  summary(parole)
  
# 1.2  - violators
  table(parole$violator)
  
# 2.1 
  table(parole$state)
  parole$state = as.factor(parole$state)
  parole$state = relevel(parole$state,4)
  table(parole$crime)
  parole$crime = as.factor(parole$crime)
  parole$crime = relevel(parole$crime,1)
  str(parole)
  

  
# 3.1 
  set.seed(144)
  library(caTools)
  split = sample.split(parole$violator, SplitRatio = 0.7)
  train = subset(parole, split == TRUE)
  test = subset(parole, split == FALSE)
  str(train)
  str(test)
  
# 4.1
  mod01 = glm(violator ~ . , data= train, family=binomial)
  summary(mod01)  
  
# 4.3 - applying the formula 
  odds = exp(-4.2411574 + (0.3869904*1) + (0.8867192*1) + (-0.0001756*50) + (-0.1238867*3) + (0.0802954*12) + (0.6837143*1))
  odds
  logit = -4.2411574 + (0.3869904*1) + (0.8867192*1) + (-0.0001756*50) + (-0.1238867*3) + (0.0802954*12) + (0.6837143*1)
  logit
  prob = 1/(1+exp(-logit))
  prob
  
# 5.1 - 
  TestPrediction = predict(mod01, newdata= test, type="response")
  sort(TestPrediction)
  
# 5.2 - 
  table(test$violator, TestPrediction >= 0.5)
  sens = 12/(12+11)
  sens
  spec = 167/(167+12)
  spec
  accu = (167+12)/(167+12+12+11)
  accu
  
# 5.3 - simple model
  table(parole$violator==0)
  597/(597+78)
  
# 5.4 being accurate
  table(test$violator, TestPrediction >= 0.01)
  
# 5.6 - AUC
  library(ROCR)
   ROCRpred = prediction(TestPrediction, test$violator)
  as.numeric(performance(ROCRpred, "auc")@y.values)