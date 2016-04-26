# 1.1 
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
str(wiki)
table(wiki$Vandal)


# 1.2 
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded[[1]]
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded[[1]]
corpusAdded <- tm_map(corpusAdded, stemDocument)
corpusAdded[[1]]
dtmAdded = DocumentTermMatrix(corpusAdded)
length(stopwords("english"))
dtmAdded

# 1.3
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

#1.4
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)

# 1.5

wikiWords = cbind(wordsAdded,wordsRemoved)
wikiWords$Vandal = wiki$Vandal
table(wikiWords$Vandal)
2061 / (2061+1815)

library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)

train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

# 1.6
library(rpart)
library(rpart.plot)
CART = rpart(Vandal~., data=train, method="class")
pred = predict(CART, newdata=test)
table(test$Vandal, pred[,2] >= 0.5)
(618+8)/(618+8+537)


# 1.7
prp(CART)

# 2.1
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)


# 2.2 
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
CART2 = rpart(Vandal~., data=wikiTrain2, method="class")
pred2 = predict(CART2, newdata=wikiTest2)
table(wikiTest2$Vandal, pred2[,2] >= 0.5)
(607+60)/(607+60+11+485)

# 2.3
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

summary(wikiWords2$NumWordsAdded)

# 2.4
set.seed(123) 
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)
CART3 = rpart(Vandal~., data=wikiTrain3, method="class")
pred3 = predict(CART3, newdata=wikiTest3)
table(wikiTest3$Vandal, pred3[,2] >= 0.5)
(514+248)/(514+248+104+297)


# 3.1
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

set.seed(123) 
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain4 = subset(wikiWords3, split==TRUE)
wikiTest4 = subset(wikiWords3, split==FALSE)
CART4 = rpart(Vandal~., data=wikiTrain4, method="class")
pred4 = predict(CART4, newdata=wikiTest4)
table(wikiTest4$Vandal, pred4[,2] >= 0.5)
(595+241)/(595+241+23+304)
