# Understanding simple regression, from p. 148
plot(iris$Petal.Length ~ iris$Petal.Width, 
   main = "Relationship between petal length and petal width", 
   xlab = "Petal width", ylab = "Petal length")
iris.lm = lm(iris$Petal.Length ~ iris$Petal.Width)
abline(iris.lm)
# Computing the intercept and slope coefficient, from p. 150
SlopeCoef = cor(iris$Petal.Length,iris$Petal.Width) * 
(sd(iris$Petal.Length) / sd(iris$Petal.Width))
SlopeCoef

coeffs = function (y,x) {
   ( (length(y) * sum( y*x)) - 
      (sum( y) * sum(x)) )  / 
      (length(y) * sum(x^2) - sum(x)^2)
}
coeffs(iris$Petal.Length, iris$Petal.Width)
## p. 151
iris.lm

regress =  function (y,x) {
   slope = coeffs(y,x)
   intercept = mean(y) - (slope * mean(x))
   model = c(intercept, slope)
   names(model) = c("intercept", "slope")
   model
}
model = regress(iris$Petal.Length, iris$Petal.Width)
model
# Obtaining the residuals, from p. 151
resids = function (y,x, model) {
	y - model[1] - (model[2] * x)
}

Residuals = resids(iris$Petal.Length, iris$Petal.Width, model)

head(round(Residuals,2))

plot(iris.lm)
# Computing the significance of the coefficients, from p. 154
Significance = function (y, x, model) {
   SSE = sum (resids(y,x,model)^2)
   DF = length(y)-2
   S = sqrt ( SSE / DF)
   SEslope = S / sqrt(sum( (x-mean(x))^2 ))
   tslope = model[2] / SEslope
   sigslope = 2*(1-pt(abs(tslope),DF))
   SEintercept = S * sqrt((1/length(y) + 
   mean(x)^2 / sum( (x- mean(x))^2)))
   tintercept = model[1] / SEintercept
   sigintercept = 2*(1-pt(abs(tintercept),DF))
   RES = c(SEslope, tslope, sigslope, SEintercept,
      tintercept, sigintercept)
   names(RES) = c("SE slope", "T slope", "sig slope", 
      "SE intercept", "t intercept", "sig intercept")
   RES
} 

round(Significance(iris$Petal.Length,iris$Petal.Width, model), 3)

summary(iris.lm)
# First steps in the data analysis, from p. 157
library(psych)
if(!require("MASS")) install.packages("MASS")
library(MASS)
matcov = unlist(read.csv("matcov.txt", header=F))
covs = matrix(matcov, 6, 6)
means = c(4.47,14.95,4.87,36.08,5,1.88) 
set.seed(987)
nurses = data.frame(mvrnorm(n=40, means, covs))
colnames(nurses)= c("Commit","Exhaus","Depers","Accompl",
   "WorkSat","WFC")
corr.test(nurses)

plot(nurses)
# Performing the regression, from p. 160
model1 = lm(Commit ~ Exhaus + Depers + Accompl, data = nurses)

summary(model1)
# hecking for the normality of residuals, from p. 161
hist(resid(model1), main="Histogram of residuals", 
   xlab="Residuals")

shapiro.test(resid(model1))

# Checking for variance inflation, from p. 162
if(!require("HH")) install.packages("HH")
library(HH)
vif(Commit ~ Exhaus + Depers + Accompl, data = nurses)

# Examining potential mediations nd comparing models, from p. 163
model2 = lm(Commit ~ Exhaus + Depers + Accompl + WorkSat, 
   data = nurses)
anova(model1,model2)

summary(model2)

model3 = lm(WorkSat ~ Exhaus + Depers + Accompl, data = nurses)
summary(model3)
# p. 165
if(!require("bda")) install.packages("bda")
library(bda)
mediation.test(nurses$WorkSat,nurses$Exhaus,nurses$Commit)
# Predicting new data, from p. 166
matcov2 = unlist(read.csv("matcov2.txt", header = F))
covs2 = matrix(matcov2, 6, 6)
means2 = c(4.279, 13.152, 5.156, 39.28, 5.153, 1.875)
set.seed(987)
nurses2 = data.frame(mvrnorm(n=40, means2, covs2))
colnames(nurses2)= c("Commit","Exhaus","Depers","Accompl",
   "WorkSat","WFC")
predicted = predict.lm(model1, nurses2)
cor.test(predicted, nurses2$Commit)

residuals_test = nurses2$Commit - predicted

# From p. 168
ComputeF = function(predicted, observed, npred) {
   DFModel = npred  # the number of predictors
   DFError = length(observed) - DFModel -1
   SSModel = sum((predicted - mean(observed))^2)
   SSError = sum((observed - predicted)^2)
   MSModel = SSModel / DFModel
   MSError = SSError / DFError
   F = MSModel / MSError 
   F
}

ComputeF(unlist(model1[5]), nurses$Commit, 3)
ComputeF(predicted, nurses2$Commit, 3)

qf(.95, df1=3, df2=36) 
# Robust regression, from p. 169
model1.rr = rlm(Commit ~ Exhaus + Depers + Accompl, data = nurses)
summary(model1.rr)

# Bootstrapping, from p. 170
ret = data.frame(matrix(nrow=0, ncol=6))
set.seed(567)
for (i in 1:2000) {
  data = nurses[sample(nrow(nurses), 40, replace = T),]
  model_i <- lm(Commit ~ Exhaus + Depers + Accompl, 
   data = data)
  ret = rbind(ret,c(coef(model_i),summary(model_i)$r.square, 
     summary(model_i)$fstatistic[1]))
} 
names(ret) = c("Intercept","Exhaus","Depers",
   "Accomp","R2","F")
round(head(ret), 3)

set.seed(567)
sample(nrow(nurses), 40, replace = T)
## p. 172
qnorm(0.975)
CIs = data.frame(matrix(nrow = ncol(ret), ncol = 2))
for (j in 1:ncol(ret)) {
   M = mean(ret[,j])
   SD = sd(ret[,j])
   lowerb = M - (1.96* (SD / sqrt(2000)))
   upperb = M + (1.96* (SD / sqrt(2000)))  
   CIs[j,1] = round(lowerb,3)
   CIs[j,2] = round(upperb,3)
}
names(CIs) = c("95% C.I.lower bound", "95% C.I.upper bound")
rownames(CIs) = colnames(ret)
CIs

