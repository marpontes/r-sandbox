#Load the dataset
data(state)

statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)

#1.1 - plotting on the "map"
plot(statedata$x, statedata$y)

#1.2 finding the region 
sort(tapply( statedata$HS.Grad , statedata$state.region,mean ))

#1.3 box plot
#boxplot(value~variable, data = xx1)
boxplot(statedata$Murder~statedata$state.region)

#1.4 finding the outlier on northeast
ne = subset(statedata,state.region=="Northeast")
str(ne)
sort(tapply(ne$Murder,ne$state.name,mean))


#2.1 Life expectancy
model = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area,data=statedata)

#2.2 - checking some coeficients
summary(model)

#2.3 - the plot
plot(statedata$Income,statedata$Life.Exp)
?jitter

#2.4 - digging into income to investigate 
modeli = lm(Life.Exp ~ Income ,data=statedata)
summary(modeli)

#3.1 - Refining the model by removing vars
summary(model)#0.7362
model = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost,data=statedata)
summary(model)#0.7361
model = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost,data=statedata)
summary(model) #0.7361
model = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost,data=statedata)
summary(model) #0.736

#3.2 r-squared always gets worse by removing irrelevant variables

#3.3 - predicting without testdata
pred = predict(model)
#shows the results sorted
sort(pred)

#shows who actually has lower life exp.
statedata$state.name[which.min(statedata$Life.Exp)]

#3.4 highest life expectancy
sort(pred) #again
statedata$state.name[which.max(statedata$Life.Exp)] # now with max

#3.5 residuals
sort(model$residuals)