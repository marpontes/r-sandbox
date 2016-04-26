# 1.1 Reading the data
  data = read.csv("climate_change.csv")
  str(data)
  
# Splitting the data
  training = subset(data,Year <=2006)
  summary(training)

  test = subset(data,Year >2006)
  summary(test)

# Building up the model with all vars but time
  model = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,data=training)
  summary(model)

# 1.2 - See summary above
# 2.1 - Guessing correlations to investigate candidate answers
  cor(training$CFC.11,training)

# 2.2 Computing correlations between all vars in the training set
  cor(training,training$N2O)
  cor(training,training$CFC.11)
  
# 3 Simplifying the model
  modelred = lm(Temp ~ MEI + TSI + Aerosols + N2O,data=training)
  summary(modelred)
  
# 4 Using the step function to get the fine tune
  modelstep = step(model)
  summary(modelstep)
  
# 5 Predicting the future
  predictTemp = predict(modelstep,newdata=test)
  predictTemp
  
# Calculating r-squared
  SSE = sum(  ( test$Temp - predictTemp )^2  )
  SST = sum(  ( test$Temp - mean(training$Temp))^2 )
  RS = 1-SSE/SST
  RS