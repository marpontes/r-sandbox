# 1.1 - loading the data
gerber = read.csv("gerber.csv")
str(gerber)
nrow( subset(gerber,voting==1) )/nrow( gerber )

#1.2 - fraction of voters
table(gerber[c("civicduty","hawthorne","self","neighbors")])
nrow( subset(gerber, civicduty==1 & voting==1)  )/nrow( subset(gerber, civicduty==1) )
nrow( subset(gerber, hawthorne ==1 & voting==1)  )/nrow( subset(gerber, hawthorne ==1) )
nrow( subset(gerber, self ==1 & voting==1)  )/nrow( subset(gerber, self ==1) )
nrow( subset(gerber, neighbors ==1 & voting==1)  )/nrow( subset(gerber, neighbors ==1) )

#1.3 Logistic regression
modl = glm(voting ~ civicduty+ hawthorne+ self+ neighbors , data= gerber, family=binomial)
summary(modl)
predl = predict(modl, type="response")

#1.4 - theshold 0.3
table(gerber$voting,predl >= 0.3)
(134513+ 51966)/(134513+ 51966+ 56730+ 100875)

#1.5 - theshold 0.5
table(gerber$voting,predl >= 0.5)
235388/(235388+ 108696)

#2.1 building a cart
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

#2.2 Now, another tree
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

#2.4 including sex
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

#3.1 just control and control+sex
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel5 = rpart(voting ~ control+sex, data=gerber, cp=0.0)
prp(CARTmodel4,digits=6)
abs(0.296638-0.34)

#3.2 control and sex analysis
prp(CARTmodel5,digits=6)
abs(0.290456-0.334176)
0.290456/0.334176
abs(0.302795-0.345818)
0.302795/0.345818
abs(0.290456-0.334176)-abs(0.302795-0.345818)

#3.3 going back to LM with sex and control
modSexControl = glm(voting ~ control+sex , data= gerber, family=binomial)
summary(modSexControl)

#3.4 
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(modSexControl, newdata=Possibilities, type="response")
abs(0.290456-0.2908065)

#3.5
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

#3.6
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.2904558-0.290456)