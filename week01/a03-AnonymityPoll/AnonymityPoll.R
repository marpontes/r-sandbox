#Reading the CSV
poll = read.csv("AnonymityPoll.csv")

#Summarizing it with str and summary
str(poll)
summary(poll)

#Smartphone breakdown
table(poll$Smartphone)
summary(poll$Smartphone)

#crossing out region vs state
table(poll$State, poll$Region)

#Crossing data about Internet.Use and Smartphone
table(poll$Internet.Use, poll$Smartphone)

#finding out more info about internet use
summary(poll$Internet.Use)

#finding out more info about smartphone use
summary(poll$Smartphone)

#making a subset for Internet.use=1 or Smartphone=1
limited = subset(poll,Internet.Use==1 | Smartphone==1)

#summarizing limited
summary(limited)
str(limited)

#Table number of pieces of personal information on the internet
sort(table(limited $Info.On.Internet))

#Catching data about Worry.About.Info
table(limited$Worry.About.Info)

#Catching info about anonymity possible
table(limited$Anonymity.Possible)

#Catching info about tried masking
table(limited$Tried.Masking.Identity)

#Catching info about Privacy.Laws.Effective 
table(limited$Privacy.Laws.Effective)

#The histogram by $Age
hist(limited$Age)

#Plotting with the help of table()
plot(limited$Age, limited$Info.On.Internet)
table(limited$Age, limited$Info.On.Internet)


#Using jitter
jitter(c(2, 12, 3))
?jitter

#Ploting with jitter
plot(jitter(limited$Age,0.4), jitter(limited$Info.On.Internet,0.4),type="p")

#Crossing information about smartphone users and info on internet
tapply(limited$Info.On.Internet,limited$Smartphone,summary)

#Crossing information about smartphone users and Tried.Masking.Identity
table(limited$Tried.Masking.Identity,limited$Smartphone)