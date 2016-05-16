# Understanding k-NN, from p. 176
distances = dist(iris[1:4], upper = T, diag = T)

if(!require("reshape2")) install.packages("reshape2")
library(reshape2)
distances.df  <- melt(as.matrix(distances))
## from p. 178
k = 5
N = length(levels(as.factor(distances.df$Var2)))
Nearest = matrix(nrow= N, ncol = k)
level_count = length(levels(as.factor(iris[[5]])))
classif = rep(0,N)
for (i in 1:N) {
   temp = subset(distances.df, Var2 == i)
   nearest = 
   unlist(head(temp[order(temp$Var2,temp$value),],k)[1])
   votes = iris[0,5]
   for (j in 1:length(nearest)) {
        votes[j] = iris[5][nearest[j],]
        classif[i]= which.max(table(votes))
    }
   }
iris$Species_class[classif == 1] = "setosa"
iris$Species_class[classif == 2] = "versicolor"
iris$Species_class[classif == 3] = "virginica"
table(iris$Species,iris$Species_class)

rownames(iris)[iris$Species != iris$Species_class]

if(!require("class")) install.packages("class")
library(class)

iris$knn_class = knn(iris[1:4],iris[1:4],iris[[5]], 5)

sum(iris$knn_class == iris$Species_class)
# orking with k-NN in R, from p. 179
if(!require("mlbench")) install.packages("mlbench")
library(mlbench)
data(Ozone)
Oz = na.omit(Ozone)

Oz$AprilToSeptember = rep(0,length(Oz[,1]))
Oz$AprilToSeptember[as.numeric(Oz[[1]])>=4 &
as.numeric(Oz[[1]])<=9] = 1

Oz$classif = knn(Oz[2:13],Oz[2:13],Oz[[14]], 3)
table(Oz$classif,Oz[[14]])

Oz$classif2 = knn.cv(Oz[2:13],Oz[[14]], 3)

table(Oz$classif2,Oz[[14]])
# How to select k, from p. 181
Accur = rep(0,20)
for (i in 1:20) {
   classification = knn.cv(Oz[2:13],Oz[[14]], i)
   Accur[i] = sum(classification == Oz[[14]])/203
   }

which.max(Accur)
# Understanding Naïve Bayes, from p. 182
DiseaseZ = read.table("DiseaseZ.txt", header = T, sep="\t")
Sick = subset(DiseaseZ, DiseaseZ=="YES")
NotSick = subset(DiseaseZ, DiseaseZ=="NO")
dim(Sick)[1]
dim(NotSick)[1]

prob.Sick = colSums(Sick[,1:6]== "YES")/6
prob.NotSick  = colSums(NotSick[,1:6]== "NO")/4
## From p. 184
if(!require("e1071")) install.packages("e1071")
library(e1071)

Classify = naiveBayes(DiseaseZ[1:10,1:6], 
   DiseaseZ[1:10,7])         
Classify

predict(Classify, DiseaseZ[11,1:6])

# Working with Naïe Bayes in R, from p. 186
Titanic.df_weighted = data.frame(Titanic)
## From p. 188
### creating empty data frame to be populated
Titanic.df = Titanic.df_weighted[0,1:4]

### populating the data frame
k=0
for (i in 1:nrow(Titanic.df_weighted)){
   if (Titanic.df_weighted[i,5]>0) {
      n = Titanic.df_weighted[i,5]
      for (j in 1:n) {
         k = k + 1
         Titanic.df [k,] =  
            unlist(Titanic.df_weighted[i,1:4])
      }
   }
}

table(Titanic.df) == Titanic

set.seed(1)
Titanic.df$Filter= sample(c("TRAIN","TEST"), 
   nrow(Titanic.df), replace = T)
TRAIN = subset (Titanic.df, Filter == "TRAIN")
TEST = subset (Titanic.df, Filter == "TEST")

Classify = naiveBayes(TRAIN[1:3],TRAIN[[4]])
Classify 

TEST$Classified = predict(Classify,TEST[1:3])
table(TEST$Survived,TEST$Classified)
