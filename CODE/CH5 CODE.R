# The innerworking of agglomerative clustering, from p. 82
rownames(life.scaled) = life$country
a=hclust(dist(life.scaled))
par(mfrow=c(1,2))
plot(a, hang=-1, xlab="Case number", main = "Euclidean")
## p. 83
a = hclust(dist(life.scaled, method= "manhattan"))
plot(a, hang=-1, xlab="Case number", main = "Manhattan")
## p. 85
A1 = c(2,3,5,7,8,10,20,21,23)
A2 = A1
A3 = A1

if(!require("scatterplot3d")) install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(A1,A2,A3, angle = 25, type = "h")

demo = hclust(dist(cbind(A1,A2,A3)))
plot(demo)
# Exploring the results of votes, from p. 86
swiss_votes = read.table("swiss_votes.dat", sep = "\t", 
   header = T)
## p. 87
dist_matrix = dist(swiss_votes[2:11])
clust_compl  = hclust(dist_matrix)
clust_single  = hclust(dist_matrix, method = "single")
clust_ave  = hclust(dist_matrix, method = "average")
par(mfrow = c(3,1))
plot(clust_compl, labels=swiss_votes$Canton, hang = -1, 
   main = "Complete linkage", xlab = "Canton", 
   ylab = "Distance")
plot(clust_single, labels=swiss_votes$Canton, hang = -1, 
   main = "Single linkage", xlab = "Canton", 
   ylab = "Distance")
plot(clust_ave, labels=swiss_votes$Canton, hang = -1, 
   main = "Average linkage", xlab = "Canton", 
   ylab = "Distance")
## from p. 89
clusters=cutree(clust_compl, k = 4)
cbind(clusters,swiss_votes[1])
par(mfrow = c(1,1)) #resetting to 1 plot 
plot(swiss_votes$Protection,swiss_votes$Taxes2, 
   pch=15, col=gray.colors(4)[clusters], 
   xlab="Protection and support services for peace", 
   ylab = "Tax on non-renewable energy")

round(aggregate(swiss_votes[2:11], list(clusters), mean),1)

# The use of hierarchical clustering on binary attributes, from p. 92
if(!require("vcd")) install.packages("vcd")
library(vcd)
data(Trucks)
head(Trucks)
## p. 93
Trucks.wd<- Trucks[rep(1:nrow(Trucks),Trucks$Freq),]
Trucks.rm = Trucks.wd[, -(c(1,5))]

set.seed(456)
Trucks.sample = Trucks.rm[sample(nrow(Trucks.rm), 100), ]

Trucks.onoff = data.frame(matrix(nrow =
   nrow(Trucks.sample), ncol = ncol (Trucks.sample)))
for (i in 1:nrow(Trucks.sample)) {
   for (j in 1:ncol(Trucks.sample)) {
      if (Trucks.sample[i,j] != Trucks.sample[1,j]) 
          Trucks.onoff[i,j] = 0
      else  Trucks.onoff[i,j] = 1
   }
}
names(Trucks.onoff)=names(Trucks.sample)
## p. 94
b = hclust(dist(Trucks.onoff, method= "binary"))
plot(b)

Trucks.onoff[c(96,92,76,52,5,23),]

