library(SnowballC)
library(tm)
tweets = read.csv("tweets.csv")
corpus = Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus[[1]]
#corpus <- tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
str(allTweets)


# 2.1 wordcloud
install.packages("wordcloud")
library("wordcloud")
?wordcloud
wordcloud(colnames(allTweets),colSums(allTweets))
allTweets[[1]]


# 2.4 removing apple
corpus = Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"apple"))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))

wordcloud(colnames(allTweets),colSums(allTweets), random.order=FALSE)


# 3
?wordcloud


# 4.1
display.brewer.all()


# 4.2
wordcloud(colnames(allTweets),colSums(allTweets),colors=brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])
