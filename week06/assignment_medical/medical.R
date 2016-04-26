# 1.1 Loading the data
  claims = read.csv("reimbursement.csv")
  str(claims)

# 1.2 Who's got at least one chronic cond
  table(claims$alzheimers+ claims$arthritis+
          claims$cancer+ claims$copd+
          claims$depression+ claims$diabetes+
          claims$heart.failure+ claims$ihd+
          claims$kidney+
          claims$osteoporosis+ claims$stroke >0)
  280427/(177578+280427 )
  
#1.3 Finding correlations
sort(cor(claims[1:12]))
image(cor(claims[1:12]), axes = FALSE, col=rainbow(k))

# 1.3 Histogram
  hist(claims$reimbursement2009)

# 1.5
  claims$reimbursement2008 = log(claims$reimbursement2008+1)
  claims$reimbursement2009 = log(claims$reimbursement2009+1)

# 1.6
  hist(claims$reimbursement2009)
  table(claims$reimbursement2009==0)
  90498/(367507 + 90498)

# 2.1
  set.seed(144)
  spl = sample(1:nrow(claims), size=0.7*nrow(claims))
  train = claims[spl,]
  test = claims[-spl,]
  

  lmclaim = lm(reimbursement2009 ~ .,data=train)
  summary(lmclaim)
  
# 2.2
  lmpred = predict(lmclaim)
# SSE
  SSE = sum(  ( train$reimbursement2009 - lmpred )^2  )
  SSE
  RMSE = sqrt( SSE/nrow(train) )
  RMSE

# 2.4
# Baseline
  baseline = mean(train$reimbursement2009)
  baseline
# SSE baseline testing set
  SSE2 = sum(  ( test$reimbursement2009 - baseline )^2  )
  SSE2
  RMSE2 = sqrt( SSE2/nrow(test) )
  RMSE2
  
# 2.5 Smart baseline model  
  baseline2 = test$reimbursement2008
# SSE baseline testing set
  SSE3 = sum(  ( test$reimbursement2009 - baseline2 )^2  )
  SSE3
  RMSE3 = sqrt( SSE3/nrow(test) )
  RMSE3

# 3.1 Clustering
  train.limited = train
  train.limited$reimbursement2009 = NULL
  test.limited = test
  test.limited$reimbursement2009 = NULL

  
# 3.2
  library(caret)
  preproc = preProcess(train.limited)
  train.norm = predict(preproc, train.limited)
  test.norm = predict(preproc, test.limited)
  
  mean(train.norm$arthritis)
  mean(test.norm$arthritis)

# 3.4 K-means
  k=3
  
  # Run k-means
  set.seed(144)
  km = kmeans(train.norm, centers = k,iter.max = 1000)
  str(km)
  table(km$cluster)

# Check for the question options
  sort(tapply(train$age, km$cluster, mean))
  sort(tapply(train$stroke, km$cluster, mean))
  sort(tapply(train$reimbursement2008, km$cluster, mean))
  mean(train$age)
  mean(train$stroke)
  mean(train$reimbursement2008)
  
# 3.5 
  library(flexclust)
  km.kcca = as.kcca(km, train.norm)
  cluster.train = predict(km.kcca)
  cluster.test = predict(km.kcca, newdata=test.norm)
  table(cluster.test)

# 4.1
  train1 = subset(train,cluster.train==1)
  train2 = subset(train,cluster.train==2)
  train3 = subset(train,cluster.train==3)
  
  test1 = subset(test,cluster.test==1)
  test2 = subset(test,cluster.test==2)
  test3 = subset(test,cluster.test==3)
  
  mean(train1$reimbursement2009)
  mean(train2$reimbursement2009)
  mean(train3$reimbursement2009)
  
  
# 4.2 L models
  lm1 = lm(reimbursement2009 ~ . , data=train1)
  lm2 = lm(reimbursement2009 ~ . , data=train2)
  lm3 = lm(reimbursement2009 ~ . , data=train3)
  
  summary(lm1)
  summary(lm2)
  summary(lm3)

# 4.3 Test-set predictions
  pred.test1 = predict(lm1, newdata=test1)
  pred.test2 = predict(lm2, newdata=test2)
  pred.test3 = predict(lm3, newdata=test3)
  
  mean(pred.test1)
  mean(pred.test2)
  mean(pred.test3)
  
# 4.4 Test-set RMSE
  RMSE01 = sqrt( (sum(  ( test1$reimbursement2009 - pred.test1 )^2  ))/nrow(test1) )
  RMSE02 = sqrt( (sum(  ( test2$reimbursement2009 - pred.test2 )^2  ))/nrow(test2) )
  RMSE03 = sqrt( (sum(  ( test3$reimbursement2009 - pred.test3 )^2  ))/nrow(test3) )
  
  RMSE01
  RMSE02
  RMSE03
  
# 4.5 All predictions performance
  all.predictions = c(pred.test1, pred.test2, pred.test3)
  all.outcomes = c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)

  SSE.all = sum(  ( all.outcomes - all.predictions )^2 ) 
  SSE.all
  str(all.outcomes)
  450807.1/
  sqrt( SSE.all/137402 )
  RMSE.all
