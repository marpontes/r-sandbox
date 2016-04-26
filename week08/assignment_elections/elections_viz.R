# 1.1 - Preparing

  library(ggplot2)
  library(ggmap)

  statesMap = map_data("state")
  
  str(statesMap)
  nrow(table( statesMap$group))

# 1.2 - Drawing US
  
  ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")

  
# 2.1 - Coloring the map
  polling = read.csv("PollingImputed.csv")
  str(polling)
  table(polling$Year)
  Train = subset(polling,Year<2012)
  str(Train)
  Test = subset(polling,Year==2012)
  str(Test)
  
# The Logistic Regression Model
  mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
  TestPrediction = predict(mod2, newdata=Test, type="response")
  TestPredictionBinary = as.numeric(TestPrediction > 0.5)
# Joining predictions and its labels
  predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
# How many states as 1 (Republican)  
  table(predictionDataFrame$TestPredictionBinary)
# Average predicted probability
  mean(TestPrediction)

# 2.2 - Joining to the map
  predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
  predictionMap = merge(statesMap, predictionDataFrame, by = "region")
  predictionMap = predictionMap[order(predictionMap$order),]
  str(predictionMap)
  str(statesMap)

# 2.4 Coloring the map
  ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

# 2.5 Changing the color schema and printing discrete values
  ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
  ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
  
  
# 2.6 Checking out the purple
  str(predictionDataFrame)
  predictionDataFrame[]
  predictionDataFrame

  
# 4
  ?geom_polygon
  ?linetype
  ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary,linetype=3))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
