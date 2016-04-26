# 1.1 - reading the songs data
  songs = read.csv("songs.csv")
  
# Getting the number of songs per year
  str(songs)
  summary(songs)
  
# 1.2 - Getting the number of occurrencies for Michael Jackson
  table(songs$artistname=="Michael Jackson")
  
# 1.3 - Which of these songs by Michael Jackson made it to the Top 10?
  michael = subset(songs, artistname=="Michael Jackson")
  str(michael)
  michael[c("songtitle","Top10")]

# 1.4 - What are the values of timesignature that occur in our dataset? 
  table(songs$timesignature)
  
# 1.5 - Finding which song has the highest tempo
  songs$songtitle[which.max(songs$tempo)]
  
# 2.1 - Spliting the data
  SongsTrain = subset(songs,year<=2009)
  SongsTest =  subset(songs,year==2010)
  str(SongsTrain)


# 2.2 - Building the model
# Building it declaring each var, because the other way has frozen pc.
  nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
  SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
  SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
  SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
  summary(SongsLog1)

# 3.1 - BEWARE OF MULTICOLLINEARITY ISSUES!
  cor(SongsTrain$loudness,SongsTrain$energy)
  
# 3.2 - Creating models
  SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
  summary(SongsLog2)
  
#3.3 Same like last, but extracting energy instead
  SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
  summary(SongsLog3)
  
# 4.1 - Predicting
  TestPrediction = predict(SongsLog3, newdata= SongsTest, type="response")
  table(SongsTest$Top10, TestPrediction >= 0.45)
  acc = (19+309)/(19+309+40+5)
  acc
  
# 4.2 - What about the baseline?
  table(SongsTest$Top10 == 0)
  314/(314+59)
  
# 4.4 - sensitivity and specificity
  19/(19+40)
  309/(309+5)