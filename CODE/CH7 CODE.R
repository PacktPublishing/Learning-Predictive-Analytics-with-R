# Analyzing data using apriori in R, from p. 119
if (!require("arules")) install.packages("arules")
library(arules)
data(Groceries)
# Using apriori for basic analysis, from p. 119
rules = apriori(Groceries)

rules = apriori(Groceries, parameter = list(support = 0.05, confidence = .1))
inspect(rules)
# Detailed analysis with priori, from p. 122
if (!require("vcdExtra")) install.packages("vcdExtra")
library(vcdExtra)
data(ICU)
summary(ICU)
# Preparing the data, from p. 123
ICU2 = ICU[-4]
ICU2$age = cut(ICU2$age, breaks = 4)
ICU2$systolic = cut(ICU2$systolic, breaks = 4)
ICU2$hrtrate = cut(ICU2$hrtrate, breaks = 4)
ICU_tr = as(ICU2, "transactions")
# Analyzing the data, from p. 123
rules = apriori (ICU_tr, parameter = list(support = .85, confidence = .95))
inspect(rules)
## p. 125
IM = interestMeasure(rules, "fishersExactTest", ICU_tr)
round(IM, digits=2)
# Cohercing association rules to a data frame, from p. 127
rulesDeath = apriori(ICU_tr, 
   parameter = list(confidence = 0.3,support=.1), 
   appearance = list(rhs = c("died=Yes"), default="lhs"))

rulesDeath.df = as(rulesDeath,"data.frame")
rulesDeath.df.sorted = rulesDeath.df[order(rulesDeath.df$lift,decreasing = T),]
head(rulesDeath.df.sorted)

rulesDeath.sorted = sort(rulesDeath, by ="lift")
inspect(head(rulesDeath.sorted,5))
# Visualizing association rules, from p. 128
if (!require("arulesViz")) install.packages("arulesViz")
library(arulesViz)

morerules = apriori(ICU_tr, parameter=list(confidence=.95, support=.5, minlen=2,maxlen=2))
plot(morerules, method = "grouped") 

