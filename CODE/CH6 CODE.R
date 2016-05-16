# The inner workings of PCA, from p. 98
## p. 100
myPCA = function (df) {
   eig = eigen(cov(df))
   means = unlist(lapply(df,mean))
   scores = scale(df, center = means) %*% eig$vectors 
   list(values = eig$values,vectors = eig$vectors, scores = scores)
}


my_pca = myPCA(iris[1:4])
my_pca
## p. 101
pca = princomp(iris[1:4], scores = T)
cbind(unlist(lapply(my_pca[1], sqrt)), pca$sdev)

summary(pca)
## p.102
scores = cbind(matrix(unlist(my_pca[3]),ncol = 4), pca$scores)
round(cor(scores)[1:4,5:8],3)
# Learning PCA in R, from p. 103
if(!require("psych")) install.packages("psych")
library(psych)
data(msq)
motiv = msq[,1:72]
# Dealing with missing values, from p. 104
apply(is.na(motiv),2,sum)

head(cbind(names(motiv)),5)
ToSuppress = c(5, 15, 37, 38, 66)
names(motiv[ToSuppress])
# Selecting how many components are relevant, from p. 105
Pca = princomp(na.omit(motiv[,-ToSuppress]))
plot(Pca$sdev^2)
# Naming the components using the loadings, from p. 107

if(!require("GPArotation")) install.packages("GPArotation")
Pca2 = principal(motiv[,-ToSuppress],nfactors = 5, 
rotate = "varimax", missing = T, scores = T)
print.psych(Pca2, sort =T)
# Assessing the PCA scores, from p. 109
round(cor(Pca2$scores),3)
nrow(Pca2$scores) == nrow(msq)
bound = cbind(msq,Pca2$score)
# PCA scores for analysis, from p. 110
Correl = cor(na.omit(cbind(bound[80:84],bound[93:97])))

if(!require("Hmisc")) install.packages("Hmisc")
library(Hmisc)
Correl_and_sig =    
   rcorr(as.matrix(na.omit(cbind(bound[80:84],bound[93:97]))),
      type = "pearson")
Correl = Correl_and_sig[[1]]
Sig = Correl_and_sig[[3]] 
round(Correl, digits = 2)[6:10,1:5]
# PCA diagnostics, from p. 112
M = na.omit(motiv[-ToSuppress])
cortest.normal(cor(M), n1 = nrow(M))
KMO(motiv)[1]
