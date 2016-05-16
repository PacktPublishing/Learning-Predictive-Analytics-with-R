# Setting the centroids, from p. 66
set.random.clusters = function (numrows, k) {
   clusters = sample(1:k, numrows, replace=T)
}

compute.centroids = function (df, clusters) {
    means = tapply(df[,1], clusters, mean)
    for (i in 2:ncol(df)) {
        mean.case = tapply(df[,i], clusters, mean)
        means=rbind(means, mean.case)
    }
    centroids = data.frame(t(means))
    names(centroids) = names(df)
    centroids
} 

# Computing distances to centroids, p. 67
euclid.sqrd = function (df, centroids) {
    distances = matrix(nrow=nrow(df), ncol=nrow(centroids))
    for (i in 1:nrow(df)) {
        for (j in 1:nrow(centroids)) {
            distances[i,j]  = sum((df[i,]-centroids[j,])^2)
        }
    }
    distances
}

assign= function (distances) {
   clusters=data.frame(cbind(c(apply(distances, 1, which.min))))
   if(nrow(unique(clusters))<ncol(distances)){  
   #precaution in case of empty cluster
   clusters=set.random.clusters(nrow(distances),ncol(distances))
   }
   clusters
} 
# Tasks performed by the main function, from p. 68
kay.means = function (df, k) {
    clusters = set.random.clusters(nrow(df),k)
    ss.old = 1e100
    ss = 1e99
    while(ss!=ss.old) {
   
        centroids = compute.centroids(df, clusters)
        distances = euclid.sqrd(df, centroids)
        ss.old=ss
        ss = sum(distances)
        clusters = assign(distances)
    }
    names(clusters) = "Clusters"
    clusters
}
# Internal validation, from p. 69
set.seed(1)
irisClust = cbind(iris, kay.means(iris[1:4], 3))
tableClust=table(unlist(irisClust[5]), unlist(irisClust[6]))
tableClust

if(!require("psych")) install.packages("psych")
library(psych)

irisClust = cbind(irisClust, rep(0,nrow(irisClust)))
names(irisClust[7]) = "Species.recode"
irisClust[7][irisClust[5]=="setosa"] = 3
irisClust[7][irisClust[5]=="versicolor"] = 2
irisClust[7][irisClust[5]=="virginica"] = 1
kappa=cohen.kappa(cbind(irisClust[6],irisClust[7]))

if(!require("flexclust")) install.packages("flexclust")
library(flexclust)
randIndex(tableClust)
# Using k-means with public datasets, p. 71
if(!require("cluster.datasets")) install.packages("cluster.datasets")
library(cluster.datasets)
# Understanding the data, from p. 71
data(all.us.city.crime.1970)
crime = all.us.city.crime.1970

ncol(crime)
names(crime)
summary(crime)


plot(crime[5:10])
round(cor(crime[5:10]),3)
## From p. 72
crime.scale = data.frame(scale(crime[5:10]))
set.seed(234)
TwoClusters = kmeans(crime.scale, 2, nstart = 25) 
plot(crime[5:10],col=as.factor(TwoClusters$cluster), main = "2-cluster solution")
ThreeClusters = kmeans(crime.scale, 3, nstart = 25) 
plot(crime[5:10],col=as.factor(ThreeClusters$cluster), main = "3-cluster solution")
FourClusters= kmeans(crime.scale, 4, nstart = 25) 
plot(crime[5:10],col=as.factor(FourClusters$cluster), main = "4-cluster solution")
FiveClusters = kmeans(crime.scale, 5, nstart = 25) 
plot(crime[5:10],col=as.factor(FiveClusters$cluster), main = "5-cluster solution")

## From p. 75
v=rep(0,4)
v[1] = TwoClusters[[6]]/TwoClusters[[3]]
v[2] = (ThreeClusters[[6]]/ThreeClusters[[3]]) - v[1]
v[3] = (FourClusters[[6]]/FourClusters[[3]]) - sum(v[1:2])
v[4] = (FiveClusters[[6]]/FiveClusters[[3]]) - sum(v[1:3])
plot(v, xlab = "Number of clusters ", 
   ylab = "Ratio difference")
# Finding the best number of clusters, from p. 77
data(life.expectancy.1971)
life.expectancy.1971

life = life.expectancy.1971[-c(23,24,27),]
life$f50 = unlist(lapply(life$f50, as.numeric))
life.temp = cbind(life, life$m0/life$f0, life$m25/life$f25, life$m50/life$f50, life$m75/life$f75)

life.scaled = as.matrix(data.frame( scale(life.temp[-(c(1,2))]) ))
# External validation, p. 79
if(!require("NbClust")) install.packages("NbClust") 
library(NbClust)
NbClust(life.scaled, method = "kmeans")
