articles = read.csv("nytimes.csv",stringsAsFactors=FALSE)
str(articles)

table(articles$type)
967/(967+6)
cor(nchar(articles$headline),articles$popular)


articles$popular =  as.factor(articles$popular) 
articles$type =  as.factor(articles$type) 


library(caTools)
set.seed(144)
split = sample.split(articles$popular, SplitRatio = 0.7)
train = subset(articles, split==TRUE)
test = subset(articles, split==FALSE)

logmodel = glm(popular ~ print+type+word.count,  data=train, family=binomial)

summary(logmodel)



1/(1+(-2.5075573 + (1*-0.8468333) + (682*0.0002600) ) )
e <- exp(1)
xxx = -2.5075573 + (1*-0.8468333) + (682*0.0002600)

1/ ( 1+(e^(-xxx)  ) )

table(train$popular)
baseline = 608/(608 + 74)
baseline

pred = predict(logmodel,newData=test, type="response")
table(train$popular,pred>=0.5)
table(pred>=0.5)

