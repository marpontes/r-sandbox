# 1.1 
dailykos = read.csv("dailykos.csv")
str(dailykos)
distances = dist(dailykos[2:1546], method = "euclidean")

# 1.2
clusterDK = hclust(distances, method = "ward") 
plot(clusterDK)

# 1.4

clusterGroups = cutree(clusterDK, k = 7)
dailykos$cg = clusterGroups
ds1 = subset(dailykos,cg==1)
ds2 = subset(dailykos,cg==2)
ds3 = subset(dailykos,cg==3)
ds4 = subset(dailykos,cg==4)
ds5 = subset(dailykos,cg==5)
ds6 = subset(dailykos,cg==6)
ds7 = subset(dailykos,cg==7)

ds1$cg=NULL
ds2$cg=NULL
ds3$cg=NULL
ds4$cg=NULL
ds5$cg=NULL
ds6$cg=NULL
ds7$cg=NULL

table(clusterGroups)

# 1.5 Largest column means for cluster
tail(sort(colMeans(ds1[-1])))

# 1.6
sort(tail(sort(colMeans(ds2[-1]))))

sort(tail(sort(colMeans(ds1[-1]))))
sort(tail(sort(colMeans(ds2[-1]))))
sort(tail(sort(colMeans(ds3[-1]))))
sort(tail(sort(colMeans(ds4[-1]))))
sort(tail(sort(colMeans(ds5[-1]))))
sort(tail(sort(colMeans(ds6[-1]))))
sort(tail(sort(colMeans(ds7[-1]))))



# 2.1 K-MEANS

k=7

# Run k-means
set.seed(1000)
KMC = kmeans(dailykos[2:1546], centers = k)
str(KMC)
sort(table(KMC$cluster))

kmcClusters = KMC$cluster
dailykos$kmcCluster = kmcClusters
ds1 = subset(dailykos,kmcCluster==1)
ds2 = subset(dailykos,kmcCluster==2)
ds3 = subset(dailykos,kmcCluster==3)
ds4 = subset(dailykos,kmcCluster==4)
ds5 = subset(dailykos,kmcCluster==5)
ds6 = subset(dailykos,kmcCluster==6)
ds7 = subset(dailykos,kmcCluster==7)

ds1$kmcCluster = NULL
ds2$kmcCluster = NULL
ds3$kmcCluster = NULL
ds4$kmcCluster = NULL
ds5$kmcCluster = NULL
ds6$kmcCluster = NULL
ds7$kmcCluster = NULL


# 2.2 
sort(tail(sort(colMeans(ds1[-1]))))
sort(tail(sort(colMeans(ds2[-1]))))
sort(tail(sort(colMeans(ds3[-1]))))
sort(tail(sort(colMeans(ds4[-1]))))
sort(tail(sort(colMeans(ds5[-1]))))
sort(tail(sort(colMeans(ds6[-1]))))
sort(tail(sort(colMeans(ds7[-1]))))


table(clusterGroups, KMC$cluster)
