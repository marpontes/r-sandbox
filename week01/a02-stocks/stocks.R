#importing the data frames
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
Boeing = read.csv("BoeingStock.csv")

# Converting the data format
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")



#Checking their structure to validate the number of obs
str(IBM)
str(GE)
str(CocaCola)
str(ProcterGamble)
str(Boeing)

#Checking their dates summary to retrieve the earliest year
summary(IBM$Date)
summary(GE$Date)
summary(CocaCola$Date)
summary(ProcterGamble$Date)
summary(Boeing$Date)

#Way to check the mean stock price for IBM
summary(IBM)

#Way to check the minimum stock price for GE
summary(GE)

#Way to check the maximum stock price for CocaCola
summary(CocaCola)

#Way to check the median stock price for Boeing
summary(Boeing)

#Way to check the standard deviation stock price for ProcterGamble
sd(ProcterGamble$StockPrice)

#Ploting Date on x-axis and StockPrice on Y-axis for Coca cola
plot(CocaCola$Date,CocaCola$StockPrice,type="l")

#Adding the lines for ProcterGamble
plot(CocaCola$Date,CocaCola$StockPrice,type="l",col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=1) # helper line

# Helper lines in 1983

abline(v=as.Date(c("1983-01-01")), lwd=1) # helper line
abline(v=as.Date(c("1983-12-01")), lwd=1) # helper line

# regenerating it to clear ablines and get new ones from 95-2005 
plot(CocaCola$Date,CocaCola$StockPrice,type="l",col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
abline(v=as.Date(c("1995-01-01")), lwd=1) # helper line
abline(v=as.Date(c("2005-12-01")), lwd=1) # helper line

# Analyzing only 95-2005
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432],col="green")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],col="orange")
lines(IBM$Date[301:432], IBM$StockPrice[301:432],col="black")

#helper lines
abline(v=as.Date(c("2000-03-01")), lwd=1) # helper line
abline(v=as.Date(c("1997-11-01")), lwd=1) # helper line
abline(v=as.Date(c("1997-09-01")), lwd=1) # helper line

abline(v=as.Date(c("2005-12-31")), lwd=1) # helper line
abline(v=as.Date(c("2004-01-01")), lwd=1) # helper line


#monthly average for IBM
tapply(IBM$StockPrice,months(IBM$Date),mean,na.rm=TRUE)

#Overall average stock price for IBM
summary(IBM$StockPrice)

#monthly average for All stocks
tapply(GE$StockPrice,months(GE$Date),mean,na.rm=TRUE)
tapply(IBM$StockPrice,months(IBM$Date),mean,na.rm=TRUE)
tapply(CocaCola$StockPrice,months(CocaCola$Date),mean,na.rm=TRUE)
tapply(ProcterGamble$StockPrice,months(ProcterGamble$Date),mean,na.rm=TRUE)
tapply(Boeing$StockPrice,months(Boeing$Date),mean,na.rm=TRUE)