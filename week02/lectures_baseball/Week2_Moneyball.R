# VIDEO 2

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

## 713 runs and allows 614 
# w= 80.8814 + 0.1058(RD)
80.8814 + 0.1058*(713-614)


# VIDEO 3

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
#scores = -804.63 + 2737.77*(OBP) + 1584.9*1(SLG)

#Quick question 3
-804.63 + 2737.77*(0.311) + 1584.91*(0.405)


#Which player to hire
#-804.63 + 2737.77*(0.338) + 1584.91*(0.540) = 976.5877
#-804.63 + 2737.77*(0.391) + 1584.91*(0.450) = 979.0476
#-804.63 + 2737.77*(0.369) + 1584.91*(0.374) = 798.3635
#-804.63 + 2737.77*(0.313) + 1584.91*(0.447) = 760.7468
#-804.63 + 2737.77*(0.361) + 1584.91*(0.500) = 976.16


#Quick question 5
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
mbframe = data.frame(teamRank,wins2012,wins2013)

cor(mbframe)