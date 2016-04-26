# 1.1 Reading the data
  parole = read.csv("parole.csv")
  parole$male = as.factor(parole$male)
  parole$state = as.factor(parole$state)
  parole$crime = as.factor(parole$crime)
  str(parole)
  nrow(subset(parole,male==0 & violator==1))/nrow(subset(parole, violator==1))
  table(parole$male, parole$violator)
  14/(14+64)

  
# 1.2 Crime in Kentucky
  table( parole$state , parole$crime) 

# 2.1 Ploting
  ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5,color="blue")

  
# 3.1 Adding another dimension
  ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(.~male)

# 3.3 Coloring in the same chart
  ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)
  
  
# 3.4 Un stacking
  ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5,position="identity",alpha=0.5)
  
  
# 4.1 - Time served plot
  ggplot(data = parole, aes(x = time.served, fill = male)) + geom_histogram(binwidth = 1,position="identity",alpha=0.5)
  
# 4.2 changing the time served length on the chart
  ggplot(data = parole, aes(x = time.served, fill = male)) + geom_histogram(binwidth = 0.1,position="identity",alpha=0.5)
  
  
# 4.3 - Grid
  ggplot(data = parole, aes(x = time.served, fill = male)) + geom_histogram(binwidth = 1,position="identity",alpha=0.5) + facet_grid(crime~.)
  

  
# 4.4 
  ggplot(data = parole, aes(x = time.served, fill = crime)) + geom_histogram(binwidth = 1,position="identity",alpha=0.5)
