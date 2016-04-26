# 1.1 Loading the data
  airlines = read.csv("AirlinesCluster.csv")
  summary(airlines)
  
# 1.3 Normalizing the data
  library(caret)
  preproc = preProcess(airlines)
  airlinesNorm = predict(preproc, airlines)
  summary(airlinesNorm)

# 2.1
  distances = dist(airlinesNorm, method = "euclidean")
  cluster = hclust(distances, method = "ward") 
  plot(cluster)

# 2.2
  clusterGroups = cutree(cluster, k = 5)
  table(clusterGroups)
  
#2.3 Calculating the Means
  sort(tapply(airlines$Balance, clusterGroups, mean))
  sort(tapply(airlines$QualMiles, clusterGroups, mean))
  sort(tapply(airlines$BonusMiles, clusterGroups, mean))
  sort(tapply(airlines$BonusTrans, clusterGroups, mean))
  sort(tapply(airlines$FlightMiles, clusterGroups, mean))
  sort(tapply(airlines$FlightTrans, clusterGroups, mean))
  sort(tapply(airlines$DaysSinceEnroll, clusterGroups, mean))
  
# 3.1
  k=5
  
  # Run k-means
  set.seed(88)
  KMC = kmeans(airlinesNorm, centers = k,iter.max = 1000)
  str(KMC)
  sort(table(KMC$cluster))

# 3.2
  KMC$centers
  