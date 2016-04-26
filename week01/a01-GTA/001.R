# Reads the file into the variable mvt
mvt = read.csv("mvtWeek1.csv")

#shows the dataset structure with #variables and #observations
str(mvt)

#shows the maximum value for ID
max(mvt$ID)

#shows the minimum value for Beat
min(mvt$Beat)

#Shows a summary about the Arrest measure
summary(mvt$Arrest)

#Shows the LocationDescription summary
summary(mvt$LocationDescription)

#Looking up for the first date row
mvt$Date[1]

#Converting to date and checking it out
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

#Applying it into mvt
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

#Checking by month
table(mvt$Month)

#Checking by Weekday
table(mvt$Weekday)

#Arrests by month
tapply(mvt$Arrest,mvt$Month,sum)

#histogram with 100 bars
hist(mvt$Date, breaks=100)

#boxplot of variable Date by arrest
boxplot(mvt$Date ~ mvt$Arrest)

#Analyzing Years vs Arrest
table(mvt$Arrest,mvt$Year)

#sorting the data to retrieve the top 5
sort(table(mvt$LocationDescription))

#subset of top5
Top5 = subset(mvt, LocationDescription == "STREET" | LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription == "ALLEY" | LocationDescription == "GAS STATION" | LocationDescription == "DRIVEWAY - RESIDENTIAL"  )

#Reading how many observations we have
str(Top5)

#Refreshing the memory of categs
Top5$LocationDescription = factor(Top5$LocationDescription)

#Gathering which location has high arrest rate
table(Top5$Arrest,Top5$LocationDescription)