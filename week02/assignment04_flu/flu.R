# 1.1 Loading the data
  FluTrain=read.csv("FluTrain.csv")
  summary(FluTrain)
# Highest weeks
  FluTrain$Week[which.max(FluTrain$ILI)]
  FluTrain$Week[which.max(FluTrain$Queries)]

  
# 1.2 Histogram plot
  hist(FluTrain$ILI)
  
# 1.3 Logarithm
  plot(log(FluTrain$ILI),FluTrain$Queries)
  plot(FluTrain$Queries,log(FluTrain$ILI))

# 2.1 We want to predict the ILI

# 2.2 Making the model
  FluTrend1 = lm( log(ILI) ~ Queries , data=FluTrain )
  summary(FluTrend1)

# 2.3 Getting correlations
  cor(FluTrain$Queries,log(FluTrain$ILI)) #0.8142115
# check01 
  0.8420333^2			# 0.7090201
# check02 
  log(1/0.8420333)		# 0.1719357
# check03 
  exp(-0.5*0.8420333)	# 0.6563792

# 3.1
  FluTest = read.csv("FluTest.csv")
  PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
  summary(PredTest1)
  PredTest1[match("2012-03-11 - 2012-03-17",FluTest$Week)]

# 3.2 Relataive Error
  RERR = (FluTest$ILI[match("2012-03-11 - 2012-03-17",FluTest$Week)] - PredTest1[match("2012-03-11 - 2012-03-17",FluTest$Week)])/FluTest$ILI[match("2012-03-11 - 2012-03-17",FluTest$Week)] 
  RERR
  
# 3.3 RMSE

  SSE = sum(  ( FluTest$ILI - PredTest1 )^2  )
  SSE
  RMSE = sqrt( SSE/nrow(FluTest) )
  RMSE
  
# 4.1 Working with time-series
  install.packages("zoo")
  library(zoo)
  
  ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
  FluTrain$ILILag2 = coredata(ILILag2)
  
  summary(FluTrain$ILILag2)
  
# 4.2 plotting 
  plot(log(FluTrain$ILILag2),log(FluTrain$ILI))
  
# 4.3
  FluTrend2 = lm( log(ILI) ~ Queries + log(ILILag2) , data=FluTrain )
  summary(FluTrend2)  
  
# 5.1
  ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
  FluTest$ILILag2 = coredata(ILILag2)
  summary(FluTest$ILILag2)
  
# 5.3 
  FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]
  FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]

# 5.4
  PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
  summary(PredTest2)
  
  SSE = sum(  ( FluTest$ILI - PredTest2 )^2  )
  SSE
  RMSE = sqrt( SSE/nrow(FluTest) )
  RMSE
