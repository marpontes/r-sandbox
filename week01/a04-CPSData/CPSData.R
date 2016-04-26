#Loading the CSV
CPS = read.csv("CPSData.csv")
summary(CPS)
str(CPS)

#Summary for Industry
sort(table(CPS$Industry))

#Gathering data of interviewees 
sort(table(CPS$State))

#Getting citizenship info
table(CPS$Citizenship)

#Crossing Race and Hispanic
table(CPS$Race,CPS$Hispanic)

#Checking is married against region
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

#Analyzing people that live in non-metropolitan area by state
table(CPS$State,is.na(CPS$MetroAreaCode))

#Analyzing people that live in non-metropolitan area by region
table(CPS$Region,is.na(CPS$MetroAreaCode))

#Analyzing proportions using mean
tapply(is.na(CPS$MetroAreaCode),CPS$State,mean)

#Reading MetroAreaMap
MetroAreaMap = read.csv("MetroAreaCodes.csv")
str(MetroAreaMap)

#Reading CountryMap
CountryMap = read.csv("CountryCodes.csv")
str(CountryMap)

#Merging MetroAreaMap to CPS
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)
str(CPS)

#Understanding the integrated data
sort(table(CPS$MetroArea))

#Proportion of interviewees by hispanic and metroarea
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean)

#Determining metroareas with more than 20% asians
sort(tapply(CPS$Race == "Asian",CPS$MetroArea,mean))

#Determining which metroarea has the smallest proportion of diplomas
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))

#Integrating Country data
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
summary(CPS)
str(CPS)

#Most common outside of north america
table( CPS$Country,CPS$Country!="Canada" & CPS$Country!="United States"  )

#Some specific check
tapply( CPS$Country != "United States",CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA",mean,na.rm=TRUE)

#Quantity of indian people by MetroArea
tapply(CPS$MetroArea,CPS$Country=="India",summary,rm.na=TRUE)

#Quantity of brazilian people by MetroArea
tapply(CPS$MetroArea,CPS$Country=="Brazil",summary,rm.na=TRUE)

#Quantity of somalian people by MetroArea
tapply(CPS$MetroArea,CPS$Country=="Somalia",summary,rm.na=TRUE)

