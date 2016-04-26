# Problem 1

elantra = read.csv("elantra.csv")
str(elantra)

train = subset(elantra,Year<=2012)
test =  subset(elantra,Year>2012)
str(train)


# Problem 2

model = lm(ElantraSales ~ Unemployment+CPI_all+CPI_energy +Queries,data=train )
summary(model)


# Problem 6 
model2 = lm(ElantraSales ~ Month+Unemployment+CPI_all+CPI_energy +Queries,data=train )
summary(model2)


# Problem 10

elantra$Month = as.factor(elantra$Month)

train = subset(elantra,Year<=2012)
test =  subset(elantra,Year>2012)
str(train)

model2 = lm(ElantraSales ~ Month+Unemployment+CPI_all+CPI_energy +Queries,data=train )
summary(model2)
str(elantra)

# Problem 13
elantra2 = read.csv("elantra.csv")
str(elantra2)
cor(elantra2)


# Problem 15
summary(model2)
model2 = lm(ElantraSales ~ Month+Unemployment+CPI_all+CPI_energy ,data=train )
summary(model2)
model2 = lm(ElantraSales ~ Month+Unemployment+CPI_all+CPI_energy ,data=train )

# Problem 16
pred = predict(model2, newdata= test)
summary(pred)


SSE = sum(  ( test$ElantraSales - pred )^2  )
SSE
RMSE = sqrt( SSE/nrow(ptest) )
RMSE

mean(train$ElantraSales)


# R-squared
SST = sum( ( test$ElantraSales - mean(train$ElantraSales))^2 )
SST
rsquared = 1 - SSE/SST
rsquared

test$maxdif = pred - test$ElantraSales
test[14,]
test[which.max(test$maxdif),]
