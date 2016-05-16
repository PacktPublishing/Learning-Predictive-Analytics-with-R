# Performing cross-validation in R with caret, from p. 264
if(!require("caret")) library (caret)
CtrlCV = trainControl(method = "cv", number = 10) 

if(!require("klaR")) install.packages("klaR")
modelNB = train(Species ~ ., data = iris, trControl = CtrlCV, method = "nb")

library(RWeka)
modelC45 = train(Species ~ ., data = iris, trControl = CtrlCV, method = "J48")

modelC50 =  train(Species ~ ., data = iris, trControl = CtrlCV, method = "C5.0")

modelCART = train(Species ~ ., data = iris, trControl = CtrlCV, method = "rpart")
# From p. 266
modelRF = train(Species ~ ., data = iris, trControl = CtrlCV, method = "rf")

names(getModelInfo())

modelNB

CtrlBoot = trainControl(method="boot", number=1000)

# Performing bootstrapping in R with caret, from p. 267
### you will get warning messages that you can simply ignore
modelNBboot = train(Species ~ ., data = iris, trControl = CtrlBoot, method = "nb") 

modelC45boot = train(Species ~ ., data = iris, trControl = CtrlBoot, method = "J48")

modelC50boot =  train(Species ~ ., data = iris, trControl = CtrlBoot, method = "C5.0")

modelCARTboot = train(Species ~ ., data = iris, trControl = CtrlBoot, method = "rpart")

modelRFboot = train(Species ~ ., data = iris, trControl = CtrlBoot, method = "rf")

# Predicting new data, from p. 268
forCV = createDataPartition(iris$Species, p=0.75, list=FALSE)
CVset = iris[forCV,]
NEWset = iris[-forCV,]

model = train(Species ~ ., data = CVset, trControl = CtrlCV, method = "nb")

Predictions = predict(model, NEWset)

# A brief description of the structure of PMML objects, from p. 269
if(!require("pmml")) install.packages("pmml")
library(pmml)
model = lm (Sepal.Length ~ Sepal.Width, data = iris)
model

pmml(model)
# Exporting k-means objects, from p. 271
iris.kmeans = kmeans(iris[1:4],3)
pmml_kmeans = pmml(iris.kmeans)
saveXML(pmml_kmeans, data=iris, "iris_kmeans.PMML")

# Hierarchical clustering, from p. 272
DF = cbind(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4)), rep(c(1,2,3,4),4), rep(c(rep(1,2),rep(2,2),rep(3,2),rep(4,2)),2))
DF.hclust = hclust(dist(DF))
plot(DF.hclust)

Cut = cutree(DF.hclust, k = 2)
centroids = aggregate(DF, list(Cut), mean)
pmml_hclust = pmml(DF.hclust, centers = centroids)
saveXML(pmml_hclust, data=DF, "DF_hclust.PMML")

# Exporting association rules, p. 274
library(arules)
data(Adult,AdultUCI) 
names(AdultUCI)

Adult.Apriori = apriori(Adult)
saveXML(pmml(Adult.Apriori), "Adult_Apriori.PMML")

# Exporting Naïve Bayes objects, p. 274
library(e1071)
iris.NaiveBayes = naiveBayes(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
saveXML(pmml(iris.NaiveBayes,dataset = iris, predictedField = "Species"), "iris_NaiveBayes.pmml")
# Exporting decision trees, from p. 274
iris.rpart = rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
iris.rpart
saveXML(pmml(iris.rpart), data = iris, "iris_rpart.pmml")

# Exporting random forest objects, p. 275
library(randomForest)
iris.RandomForest = randomForest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
saveXML(pmml(iris.RandomForest), data = iris, "iris_randomForest.pmml")

# Exporting logistic regression objects, p. 275
set.seed(1234)
y = c(rep(0,50),rep(1,50))
x = rnorm(100)
x[51:100] = x[51:100] + 0.2
glm.model = glm(y ~ x, family = "binomial")
saveXML(pmml(glm.model), "glm_model.PMML")

# Exporting SVM objects, p. 276
iris.svm = svm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
saveXML(pmml(iris.svm), "iris_svm.PMML")
