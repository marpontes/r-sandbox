# 1.1 Reading the emails
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
nrow(emails)


# 1.2 How many of the emails are spam
table(emails$spam)


# 1.3 Which word appears at the beginning of every email in the dataset?
emails$text[2]


# 1.5 What's the length of the biggest email body?
max(nchar(emails$text))


# 1.6 row that contains the shortest email body
which.min(nchar(emails$text))

# 2.1 Preparing the corpus
corpus = Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)

dtm

# 2.2
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

# 2.3
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
sort(colSums(emailsSparse))


# 2.4 How many word stems appear at least 5000 times in the ham emails in the dataset?
emailsSparse$spam = emails$spam

sort(colSums(subset(emailsSparse, spam==0)))


# 2.5
sort(colSums(subset(emailsSparse, spam==1))) 


# 3.1 Building machine learning models THREE MODELS
emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)


# Building a logistic regression model
spamLog = glm(spam ~ . , data= train, family=binomial)

# Building a CART model
spamCART = rpart(spam~., data=train, method="class")

#Building a random forest
library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)

# Predicting each of the models on the training set
predLog = predict(spamLog)
predCART = predict(spamCART)
predRF = predict(spamRF,type="prob")

# how many predLOG are too small
table(predLog<0.00001)

# how many of predlog are very big
table(predLog>0.99999)

table(predLog >=0.00001 ,predLog<=0.99999)


# 3.2
summary(spamLog)

# 3.3
prp(spamCART)


# 3.4 training set accuracy
table(train$spam,predLog>=0.5)
(3052+954)/(3052+954+4)

library(ROCR)
predROCR = prediction(predLog, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values


# 3.6 Now the CART
table(train$spam,predCART[,2]>=0.5)
(2885+894)/(2885+894+167+64)

# 3.7
predROCR = prediction(predCART[,2], train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values


# 3.8
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)
predRF = predict(spamRF,type="prob")
table(train$spam,predRF[,2]>=0.5)
(3013+914)/(3013+914+44+39)

# 3.9 
predROCR = prediction(predRF[,2], train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values


# 4.1 Doing it on the testing set
predLog = predict(spamLog,newdata=test)
predCART = predict(spamCART,newdata=test)
predRF = predict(spamRF,newdata=test,type="prob")

# Testing set acc for spamLog
table(test$spam,predLog>=0.5)
(1258+376)/(1258+376+50+34)

# 4.2
predROCR = prediction(predLog, test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
performance(predROCR, "auc")@y.values

# 4.3
table(test$spam,predCART[,2]>=0.5)
(1228+386)/(1228+386+80+24)

# 4.4
predROCR = prediction(predCART[,2], test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
performance(predROCR, "auc")@y.values

# 4.5
table(test$spam,predRF[,2]>=0.5)
(1290+386)/(1290+386+18+24)

# 4.6
predROCR = prediction(predRF[,2], test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
performance(predROCR, "auc")@y.values

# 6.1
wordCount = rowSums(as.matrix(dtm))

# 6.2
hist(wordCount)

# 6.3
hist(log(wordCount))

# 6.4
emailsSparse$logWordCount = log(wordCount)
boxplot(logWordCount~spam, data=emailsSparse)

# 6.5

set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train2 = subset(emailsSparse, split==TRUE)
test2 = subset(emailsSparse, split==FALSE)

spam2CART = rpart(spam~., data=train2)
set.seed(123)
spam2RF = randomForest(spam ~ ., data=train2)

prp(spam2CART)

# 6.6
pred2CART = predict(spam2CART,newdata=test2)
pred2RF = predict(spam2RF,newdata=test2,type="prob")
table(test2$spam,pred2CART[,2]>=0.5)
(1214+384)/(1214+384+26+94)

# 6.7
predROCR = prediction(pred2CART[,2], test2$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
performance(predROCR, "auc")@y.values

# 6.8
table(test2$spam,pred2RF[,2]>=0.5)
(1296+383)/(1296+383+27+12)

# 6.9
predROCR = prediction(pred2RF[,2], test2$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
performance(predROCR, "auc")@y.values


# 7.1
library(RTextTools)
dtm2gram = create_matrix(as.character(corpus), ngramLength=2)
dtm2gram
spdtm2gram = removeSparseTerms(dtm2gram, 0.95)
spdtm2gram


# 7.4

emailsSparse2gram = as.data.frame(as.matrix(spdtm2gram))
colnames(emailsSparse2gram) = make.names(colnames(emailsSparse2gram))
emailsCombined = cbind(emailsSparse, emailsSparse2gram)

set.seed(123)
split = sample.split(emailsCombined$spam, SplitRatio = 0.7)
trainCombined = subset(emailsCombined, split==TRUE)
testCombined = subset(emailsCombined, split==FALSE)

# Training a CART
set.seed(123)
spamCARTcombined = rpart(spam~., data=trainCombined)
spamRFcombined = randomForest(spam ~ ., data=trainCombined)
prp(spamCARTcombined,varlen=0)

colnames(trainCombined)






# 7.5
pred3CART = predict(spamCARTcombined,newdata=testCombined)
table(testCombined$spam,pred3CART[,2]>=0.5)
(1248+367)/(1248+367+60+43)

predROCR = prediction(pred3CART[,2], testCombined$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
performance(predROCR, "auc")@y.values

# 7.7
pred3RF = predict(spamRFcombined,newdata=testCombined,type="prob")
table(testCombined$spam,pred3RF[,2]>=0.5)
(1297+389)/(1297+389+11+21)

# 7.8
predROCR = prediction(pred3RF[,2], testCombined$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
performance(predROCR, "auc")@y.values
