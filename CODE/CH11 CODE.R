# Information gain, from p. 197
# Preparing the dataset
Titanic.Weights = as.data.frame(Titanic)
Titanic.df = Titanic.Weights[rep(1:nrow(Titanic.Weights), Titanic.Weights$Freq),]
Titanic.df = Titanic.df[,1:4]
table(Titanic.df$Survived)
EntropAll = -(1490/2201) * log2(1490/2201) -(711/2201) * log2(711/2201) #= 0.9076514
table(Titanic.df$Sex,Titanic.df$Survived)
EntropM = -(1364/1731) * log2(1364/1731) -(367/1731) * log2(367/1731) # = 0.745319
EntropF = -(126/470) * log2(126/470) -(344/470) * log2(344/470)  # = 0.8387034
EntropSex = ( (1731/ 2201) * EntropM ) + ((470/ 2201) * EntropF ) # = 0.7652602
InformationGain = EntropAll - EntropSex  # = 0.1423912
# Installing the required packages, from p. 202
if(!require("RWeka")) install.packages("RWeka")
library(RWeka)
if(!require("C50")) install.packages("C50") 
library(C50)
if(!require("rpart")) install.packages("rpart")
library(rpart)
if(!require("rpart.plot")) install.packages("rpart.plot")
library(rpart.plot)
if(!require("randomForest")) install.packages("randomForest")
library(randomForest)
if(!require("Formula")) install.packages("Formula")


# Loading and preparing the data from p. 203
if(!require("partykit")) install.packages("partykit")
library(Formula)
library(partykit)
if(!require("arules")) install.packages("arules")
library(arules)


data(AdultUCI)
summary(AdultUCI)

ADULT = na.omit(AdultUCI)[,-c(3,5)]

if(!require("caret")) install.packages("caret", dependencies = (c("Suggests","Depends"))) 
library(caret)
set.seed(123)
TrainCases = createDataPartition(ADULT$income, p = .5, list=F)

TrainTemp = ADULT[TrainCases,]
AdultTrainSmallIncome = TrainTemp[TrainTemp$income == "small",]
AdultTrainLargeIncome = TrainTemp[TrainTemp$income == "large",]
Oversample = sample(nrow(AdultTrainLargeIncome), nrow(AdultTrainSmallIncome), replace = TRUE)
AdultTrain = rbind(AdultTrainSmallIncome, AdultTrainLargeIncome[Oversample,])
AdultTest = ADULT[-TrainCases,]
# The unpruned tree, from p. 204
C45tree = J48(income ~ . , data= AdultTrain, control= Weka_control(U=TRUE))

C45tree
summary(C45tree)

Predictions = data.frame(matrix(nrow = nrow(AdultTest), ncol=0))
Predictions$C45 = predict(C45tree, AdultTest)

# The pruned tree, from p. 205
C45pruned = J48(income ~ . , data= AdultTrain, control= Weka_control(U=FALSE))

summary(C45pruned)
Predictions$C45pr = predict(C45pruned, AdultTest)
# C50, from p. 206
C50tree = C5.0(y = AdultTrain$income, x = AdultTrain[,-13], Trials = 10)
summary(C50tree)

TabC5.0= as.table(matrix(c(10061,1980,714,10613), byrow=T, nrow = 2))
library(psych)
cohen.kappa(TabC5.0)
Predictions$C5.0 = predict(C50tree, AdultTest)

# CART, from p. 207
CARTtree = rpart(income ~. , data= AdultTrain)
summary(CARTtree)
rpart.plot(CARTtree, extra = 1)

ProbsCART = predict(CARTtree, AdultTrain)
PredictCART = rep(0, nrow(ProbsCART))
PredictCART[ProbsCART[,1] <=.5] = "small"
PredictCART[ProbsCART[,1] >.5] = "large"
TabCART = table(AdultTrain$income, PredictCART)
TabCART
cohen.kappa(TabCART)

# Pruning, from p. 208
CARTtreePruned = prune(CARTtree, cp=0.03)
rpart.plot(CARTtreePruned, extra = 1)
ProbsCARTtest = predict(CARTtreePruned, AdultTest)
Predictions$CART[ProbsCARTtest[,1] <=.5] = "small"
Predictions$CART[ProbsCARTtest[,1] >.5] = "large"

# Random forests in R from p. 210
RF = randomForest(y = AdultTrain$income, x = AdultTrain[,-13])
RF
Predictions$RF = predict(RF, AdultTest)

# Examining the predictions on the testing set, from p. 211
values = data.frame(matrix(ncol = ncol(Predictions), nrow = 6))
rownames(values) = c("True +", "True -", "False +", "False -", "Accuracy", "Kappa")
names(values) = names(Predictions)
for (i in 1:ncol(Predictions)) {
   tab = table(AdultTest$income,Predictions[,i])
   values[1,i] = tab[1,1]
   values[2,i] = tab[2,2]
   values[3,i] = tab[1,2]
   values[4,i] = tab[2,1]
   values[5,i] = sum(diag(tab))/sum(tab)
   values[6,i] = cohen.kappa(tab)[1] 
}
round(values,2)

# Conditional inference trees in R, from p. 212
set.seed(999)
TitanicRandom = Titanic.df[sample(nrow(Titanic.df)),]
TitanicTrain = TitanicRandom[1:1100,]
TitanicTest = TitanicRandom[1101:2201,]
CItree <- ctree(Survived ~ Class + Sex + Age, data=TitanicTrain)
plot(CItree)
CIpredictTrain = predict(CItree, TitanicTrain)
CIpredictTest = predict(CItree, TitanicTest)
TabCI_Train = table(TitanicTrain$Survived,CIpredictTrain)
TabCI_Test = table(TitanicTest$Survived,CIpredictTest) 
TabCI_Train
TabCI_Test

