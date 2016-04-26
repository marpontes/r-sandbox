# 1.1 Reading the data
  ptrain = read.csv("pisa2009train.csv")
  ptest = read.csv("pisa2009test.csv")
  
# showing its structure
  str(ptrain)
  
# 1.2 Gathering the average reading test score for males
tapply(ptrain$readingScore , ptrain$male , mean)

# 1.3 Which has NA?
  summary(ptrain)
  
# 1.4 Omiting NAs
  ptrain = na.omit(ptrain)
  ptest = na.omit(ptest)
  str(ptrain)
  str(ptest)

# 2.1 Discrete values - Grade is integer
# 2.2 Just checking with the table
  sort(table(ptest$raceeth))
  
# 2.3
# 3.1 Releveling
  ptrain$raceeth = relevel(ptrain$raceeth, "White")
  ptest$raceeth = relevel(ptest$raceeth, "White")
  
  lmScore = lm( readingScore ~ . , data=ptrain )
  summary(lmScore)
  
# 3.2 Calculating RMSE
  SSE = sum( lmScore$residuals^2 )
  SSE
  RMSE = sqrt( SSE/nrow(ptrain) )
  RMSE
  
# 3.3 Think - equations help - 11*29 - 9*29
  summary(lmScore)
  
# 3.4 !@#$!@%$
# 3.5 See summary above

# 4.1 Creating the prediction
  predTest = predict(lmScore,newdata=ptest)
  max(predTest) - min(predTest)

# 4.2 RMSE  
  SSE = sum(  ( ptest$readingScore - predTest )^2  )
  SSE
  RMSE = sqrt( SSE/nrow(ptest) )
  RMSE
  
# 4.3 baseline
  baseline = sum(ptrain$readingScore)/nrow(ptrain)
  baseline
  
  SST = sum( ( ptest$readingScore - baseline)^2 )
  SST
# 4.4 R-squared
  rsquared = 1 - SSE/SST
  rsquared
 