# Loading the corpus, from p. 239
URL = "http://www.cs.cornell.edu/people/pabo/movie-review-data/review_polarity.tar.gz"
download.file(URL,destfile = "reviews.tar.gz")
untar("reviews.tar.gz") 
setwd("txt_sentoken")
## From p. 240
if(!require("tm")) install.packages("tm")
library(tm)
SourcePos = DirSource(file.path(".", "pos"), pattern="cv")
SourceNeg = DirSource(file.path(".", "neg"), pattern="cv")
pos = Corpus(SourcePos) 
neg = Corpus(SourceNeg)
pos
neg

reviews = c(pos, neg)
reviews

# Processing and inspecting the data, from p. 241
if(!require("SnowballC")) install.packages("SnowballC")
preprocess = function(corpus, stopwrds = 
  stopwords("english")){ 
  library(SnowballC)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, content_transformer(removeNumbers))
  corpus = tm_map(corpus, removeWords, stopwrds)
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, stemDocument)
  corpus
}

processed = preprocess(reviews)
### If you get the error message 
### Error in match.fun(FUN) : could not find 
### function content_transformer, reinstalling R will solve the issue.

term_documentFreq = TermDocumentMatrix(processed)

asMatrix = t(as.matrix(term_documentFreq))
Frequencies = colSums(asMatrix)
head(Frequencies[order(Frequencies, decreasing=T)], 5)

## From p. 243
Present = data.frame(asMatrix)
Present [Present>0] = 1

DocFrequencies = colSums(Present)
head(DocFrequencies[order(DocFrequencies, decreasing=T)], 5)

DocFrequencies[DocFrequencies > 1400]

total = ncol(asMatrix)
moreThanOnce = sum(DocFrequencies != Frequencies)
prop = moreThanOnce / total
moreThanOnce
total
prop

term_documentTfIdf= TermDocumentMatrix(processed,
   control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))

SparseRemoved = as.matrix(t(removeSparseTerms(
  term_documentTfIdf, sparse = 0.8)))
ncol(SparseRemoved)

sum(rowSums(as.matrix(SparseRemoved)) == 0)
colnames(SparseRemoved) 

# Computing new attributes, p. 245
quality = c(rep(1,1000),rep(0,1000))
lengths = colSums(as.matrix(TermDocumentMatrix(processed)))
# Creating the training and testing data, p. 245
DF = as.data.frame(cbind(quality, lengths, SparseRemoved))

set.seed(123)
train = sample(1:2000,1000)
TrainDF = DF[train,]
TestDF = DF[-train,]

# Document classification with k-NN, from p. 45
library(class) # knn() is in the class packages
library(caret) # confusionMatrix is in the caret package
library(e1071)
set.seed(975)
Class3n = knn(TrainDF[,-1], TrainDF[,-1], TrainDF[,1], k = 3)
Class5n = knn(TrainDF[,-1], TrainDF[,-1], TrainDF[,1], k = 5)
confusionMatrix(Class3n,as.factor(TrainDF$quality))

confusionMatrix(Class5n,as.factor(TrainDF$quality))

set.seed(975)
Class3nTest = knn(TrainDF[,-1], TestDF[,-1], TrainDF[,1], k = 3)
confusionMatrix(Class3nTest,as.factor(TestDF$quality))

# Document classification with Naïve Bayes, from p. 247
library(e1071)
set.seed(345)
model <- naiveBayes(TrainDF[-1], as.factor(TrainDF[[1]]))
classifNB = predict(model, TrainDF[,-1])
confusionMatrix(as.factor(TrainDF$quality),classifNB)

classifNB = predict(model, TestDF[,-1])
confusionMatrix(as.factor(TestDF$quality),classifNB)

# Classification using logistic regression, from p. 249
model = glm(quality~ lengths, family = binomial)
summary(model)
# From p. 250
exp(0.0018276)
Prob1 = exp(-0.6383373 + lengths * 0.0018276) / 
  (1 + exp(-0.6383373 + lengths * 0.0018276))
Prob2 = model$fitted

classif = Prob1
classif[classif>0.5] = 1
classif[classif<=0.5] = 0
table(classif, quality)

cohen.kappa(table(classif, quality))

model2 = glm(quality ~ ., family = binomial, data = TrainDF)

TrainDF$classif = fitted.values(model2, type= "response")
TrainDF$classif[TrainDF$classif>0.5] = 1
TrainDF$classif[TrainDF$classif<=0.5] = 0

confusionMatrix(TrainDF$quality, TrainDF$classif)

TestDF$classif = predict(model2, TestDF, type = "response")
TestDF$classif[TestDF$classif>0.5] = 1
TestDF$classif[TestDF$classif<=0.5] = 0
confusionMatrix(TestDF$quality, TestDF$classif)

# Document classification with SVM, from o. 252
library(e1071)
modelSVM = svm (quality ~ ., data = TrainDF)
probSVMtrain = predict(modelSVM, TrainDF[,-1])
classifSVMtrain = probSVMtrain
classifSVMtrain[classifSVMtrain>0.5] = 1
classifSVMtrain[classifSVMtrain<=0.5] = 0
confusionMatrix(TrainDF$quality, classifSVMtrain)

probSVMtest = predict(modelSVM, TestDF[,-1])
classifSVMtest = probSVMtest
classifSVMtest[classifSVMtest>0.5] = 1
classifSVMtest[classifSVMtest<=0.5] = 0
confusionMatrix(TestDF$quality, classifSVMtest)
# we go back to initial directory using the line below
setwd("../")

#A successful document classification, from p. 253
Strands = read.csv("StrandsPackt.csv", header = T)
colSums(Strands[1])

Seasonal = subset(Strands, SEASONAL.FLU == 1)
FreqSeasonal = colSums(Seasonal)[-1]
head(FreqSeasonal[order(FreqSeasonal, decreasing=T)], 20)

Other = subset(Strands, SEASONAL.FLU == 0)
FreqOther = colSums(Strands[Strands[,1] == 0])[-1]
head(FreqOther[order(FreqOther, decreasing=T)], 20)

## From p. 255
set.seed(1234)
TrainCases = sample(1:nrow(Strands),1086)
TrainPredictions = matrix(ncol=1086,nrow=100)
TestPredictions = matrix(ncol=1085,nrow=100)

for (i in 1:100) {
  UNUSED = sample(2:ncol(Strands), ncol(Strands)-300)
  Strands2 = Strands[,-UNUSED]
  StrandsTrain = Strands2[TrainCases,]
  StrandsTest = Strands2[-TrainCases,]
  model = glm(StrandsTrain$SEASONAL.FLU~., 
     data = StrandsTrain, family="binomial")
  TrainPredictions[i,] = t(predict(model,StrandsTrain, 
     type="response"))
  TestPredictions[i,] = t(predict(model,StrandsTest, 
     type="response"))
}

PredsTrain = colMeans(TrainPredictions)
PredsTrain[PredsTrain< .5] = 0
PredsTrain[PredsTrain>= .5] = 1
PredsTest = colMeans(TestPredictions)
PredsTest[PredsTest< .5] = 0
PredsTest[PredsTest>= .5] = 1

confusionMatrix(PredsTrain,StrandsTrain$SEASONAL.FLU)

confusionMatrix(PredsTest,StrandsTest$SEASONAL.FLU)

# Extracting the topics of the articles, from p. 257
Seasonal = subset(Strands,SEASONAL.FLU ==1)[,-1]
Non.Seasonal = subset(Strands,SEASONAL.FLU ==0)[,-1]

if(!require("qdap")) install.packages("qdap")
library(qdap)
seasonal.tm <- as.dtm(as.wfm(t(Seasonal)))
non.seasonal.tm <- as.dtm(as.wfm(t(Non.Seasonal)))
if(!require("topicmodels")) install.packages("topicmodels")
library(topicmodels)
Topics.seasonal = LDA(seasonal.tm, 2)
Topics.non.seasonal = LDA(non.seasonal.tm, 2)
Terms.seasonal = terms(Topics.seasonal, 20)
Terms.non.seasonal = terms(Topics.non.seasonal, 20)
Terms.seasonal
Terms.non.seasonal


# Collecting news articles in R from the NY Times article search API, from p. 259
if(!require("tm.plugin.webmining")) install.packages("tm.plugin.webmining")
library(tm.plugin.webmining)
nytimes_appid = "YOUR_KEY_HERE"
NYtimesNews <- WebCorpus(NYTimesSource("Euro", appid = nytimes_appid))
NYtimesNews 
## From p. 261
preprocessedNews = preprocess(NYtimesNews)
tdmNews = TermDocumentMatrix(preprocessedNews)
loaded_tdm = dget("tdmNews")
findFreqTerms(loaded_tdm, low = 100)
Assocs = findAssocs(loaded_tdm, terms = c("bank", "greek"), corlim =  c(0.5, 0.45))
Assocs
barplot(Assocs[[1]], ylim = c(0,1))
