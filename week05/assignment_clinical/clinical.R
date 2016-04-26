# 1.1 Checking the longest abstract
trials = read.csv("clinical_trial.csv",stringsAsFactors = FALSE)
summary(trials)
str(trials)
sort(nchar(trials$abstract))

# 1.2 Checking how many submissions without an abstract
table(nchar(trials$abstract)==0)

# 1.3 Checking the smallest title
trials$title[which.min(nchar(trials$title))]

# 2.1 Creating separate corpuses
###### corpusTitle
corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle <- tm_map(corpusTitle, tolower)
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle <- tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))

###### corpusAbstract
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

# Checking for the answer
dtmTitle

dtmAbstract


# 2.3 finding the most frequent word stem across abstracts
sort(colSums(dtmAbstract))


# 3.1 preparing to combine
?paste0
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtmTitle


# 3.2 combining
dtm = cbind(dtmTitle,dtmAbstract)
dtm$trial = trials$trial
str(dtm)


# 3.3 Splitting the data
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
table(train$trial)
730/(730+572)

# 3.4 the CART model
library(rpart)
library(rpart.plot)
CART = rpart(trial~., data=train, method="class")
prp(CART)

# 3.5 Predicting on the training
pred = predict(CART)

# The maximum predicted probability for any result
table(pred[,2])

# 3.7 Printing table to get
#     accuracy, specificity and sensitivity
table(train$trial,pred[,2]>=0.5)
(631+441)/(631+441+99+131)


# 4.1 predicting over the testing set
predTest = predict(CART,newdata=test,type="class")
predTest
table(test$trial,predTest)

(261+162)/(261+162+52+83)

# 4.2 Getting AUC
library(ROCR)
predTest = predict(CART,newdata=test)
predROCR = prediction(predTest[,2], test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values



# 5.3 Confirming the result
table(test$trial,predTest[,2]>=0.27158)
