# Random intercepts and random slopes, from p. 219
library(MASS)
set.seed(999)
Covariances = read.table("Covariances.dat", sep = "\t", header=T)
df = data.frame(matrix(nrow=0,ncol=4))
colnames(df) = c("Accomp","Depers","Exhaus","WorkSat")
for (i in 1:17){
  if(i == 1) {start_ln = 1}
  else start_ln = 1+((i-1)*4)
  end_ln = start_ln + 3
  covs = Covariances[start_ln:end_ln, 3:6]
  rownames(covs)=Covariances[start_ln:end_ln,2]
  dat=mvrnorm(n=100, c(rep(0,4)), covs)
  df = rbind(df,dat)
}
df$hosp = as.factor(c(rep(1,100), rep(2,100), rep(3,100),   
   rep(4,100), rep(5,100), rep(6,100),    
   rep(7,100),rep(8,100),rep(9,100), 
   rep(10,100),rep(11,100),rep(12,100),
   rep(13,100),rep(14,100),rep(15,100),
   rep(16,100),rep(17,100)))
library(lattice)
attach(df)
xyplot(WorkSat~Depers | hosp, panel = function(x,y) { 
  panel.xyplot(x,y) 
  panel.lmline(x,y)
})

# Multilevel modeling in R, from p. 221
NursesML = read.table("NursesML.dat", header = T, sep = " ")

# The null model, from p. 221
means = aggregate(NursesML[,4], by=list(NursesML[,5]), 
FUN=mean)[2]
var(unlist(means)) #at the hospital level
var(NursesML[,4]) #at the observation level

if(!require("lme4")) install.packages("lme4")
library(lme4)
NursesML$hosp = factor(NursesML$hosp)
null = lmer(WorkSat ~ 1 + (1|hosp), data=NursesML)
summary(null)
mean(NursesML[,4])
coef(null)
means
ranef(null)

# Random intercepts and fixed slopes, from p. 225
NursesMLtrain = read.table("NursesMLtrain.dat", header = T, sep = " ")
NursesMLtest = read.table("NursesMLtest.dat", header = T, sep = " ")
NursesMLtrain$hosp = factor(NursesMLtrain$hosp)
NursesMLtest$hosp = factor(NursesMLtest$hosp)
model = lmer(WorkSat ~ Accomp + Depers + Exhaust + (1|hosp), 
data=NursesMLtrain, REML = F)
null = lmer(WorkSat ~ 1 +  (1|hosp), data=NursesMLtrain, REML = F)
anova(null, model)

## From p. 226
if(!require("MuMIn")) install.packages("MuMIn")
library(MuMIn)
r.squaredLR(model,null)
summary(model)

tvals = coef(summary(model))[,3]
tvals.p <- 2 * (1 - pnorm(abs(tvals)))
round(tvals.p,3)
confint.merMod(model)

# Random intercepts and random slopes, from p. 228
modelRS = lmer(WorkSat ~ Accomp + Depers + Exhaust + 
   (1+Accomp+Depers+Exhaust|hosp), data=NursesMLtrain, REML = F)
anova(null, modelRS)

if(!require("sjPlot")) install.packages("sjPlot")
library(sjPlot)
sjp.lmer(modelRS, type = "re.qq")

## From p. 230
qqnorm(resid(modelRS))
r.squaredLR(model,null)
summary(modelRS)

tvals = coef(summary(modelRS))[,3]
tvals.p <- 2 * (1 - pnorm(abs(tvals)))
round(tvals.p,3)

## From p. 232
# if(!require("languageR")) install.packages("languageR")
### Package LanguageR is not available through install.packages() anymore. 
### The reader is advised to download it here and install it manually 
### (as explained in chapter 1):
### https://cran.r-project.org/web/packages/languageR/index.html

library(languageR)
par(mfrow=c(1,3))
plotLMER.fnc(modelRS)
# Using the predict() function, from p. 233
NursesMLtest$predicted = predict(modelRS, NursesMLtest, 
allow.new.levels = F)

# Assessing the prediction quality, from p. 234
correls = matrix(nrow=17,ncol=3)
colnames(correls) = c("Correlation", "p value", "r squared")
for (i in 1:17){
   dat = subset(NursesMLtest, hosp == i)
   correls[i,1] = cor.test(dat$predicted, dat$WorkSat)[[4]]
   correls[i,2] = cor.test(dat$predicted, dat$WorkSat)[[3]]
   correls[i,3] = correls[i,1]^2
}
round(correls, 3)

## p. 235
NursesMLtest$predicted = NursesMLtest$predicted - mean(NursesMLtest$predicted)
nullPred = lmer(WorkSat ~ 1 + (1|hosp), data=NursesMLtest, REML = F)
modelPred = lmer(WorkSat ~ predicted + (1+predicted|hosp), 
data=NursesMLtest, REML = F)

anova(nullPred,modelPred)
r.squaredLR(modelPred,nullPred)

