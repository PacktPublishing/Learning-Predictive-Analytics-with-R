# Histograms and bar plots, from p. 18
set.seed(1)
drawn = sample(0:36, 100, replace = T)
hist(drawn, main = "Frequency of numbers drawn",
   xlab = "Numbers drawn", breaks=37)
## p. 20
   buildDf = function(howmany) {
     Matrix=matrix(rep(0, howmany * 14), nrow=howmany,ncol=14)
     DF=data.frame(Matrix)
     names(DF)=c("number","position","isRed","isBlack",
       "isOdd","isEven","is1to18","is19to36","is1to12",
       "is13to24","is25to36","isCol1","isCol2","isCol3")
     return(DF)
}


attributes = function(howmany,Seed=9999) { 
   if (Seed != 9999) set.seed(Seed)
   DF = buildDf(howmany)
   drawn = sample(0:36, howmany, replace = T)
   DF$number=drawn
   numbers = c(0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27, 
      13, 36, 11, 30, 8, 23, 10, 5, 24, 16, 33, 1, 20, 14, 
      31, 9, 22, 18, 29, 7, 28, 12, 35, 3, 26)

## p. 21
   for (i in 1:nrow(DF)){
     DF$position[i]= match(DF$number[i],numbers)
     if (DF$number[i] != 0) { if (DF$position[i]%%2) {
        DF$isBlack[i] = 1} else {DF$isRed[i] = 1}
     if (DF$number[i]%%2) { DF$isOdd[i]=1} 
     else {DF$isEven[i]=1}
     if (DF$number[i] <= 18){ DF$is1to18[i]=1} 
     else { DF$is19to36[i]=1}
     if(DF$number[i] <= 12){ DF$is1to12[i]=1} 
     else if (DF$number[i]<25) { DF$is13to24[i] = 1} 
        else { DF$is25to36[i] = 1}
     if(!(DF$number[i]%%3)){ DF$isCol3[i] = 1} 
     else if ((DF$number[i] %% 3 ) == 2) {
       DF$isCol2[i] = 1}  
       else { DF$isCol1[i] = 1}
       }
     }
   return(DF)
}

## from p. 22

Data=attributes(1000,2)

par(mfrow = c(2,3))
barplot(mean(subset(Data, isCol1 == 1)$isRed), ylim=(c(0,1)), 
   main = "Prop. of red in Col. 1")
barplot(mean(subset(Data, isCol2 == 1)$isRed), ylim=(c(0,1)), 
   main = "Prop. of red in Col. 2")
barplot(mean(subset(Data, isCol3 == 1)$isRed), ylim=(c(0,1)), 
   main = "Prop. of red in Col. 3")
barplot(mean(subset(Data, isCol1 == 1)$isEven), ylim=(c(0,1)), 
   main = "Prop. of even numbers in Col. 1")
barplot(mean(subset(Data, isCol2 == 1)$isEven), ylim=(c(0,1)),
   main = "Prop. of even numbers in Col. 2")
barplot(mean(subset(Data, isCol3 == 1)$isEven), ylim=(c(0,1)), 
   main = "Prop. of even numbers in Col. 3")
## p.24
for (i in 1:nrow(Data)){
   if(Data$isCol1[i]== 1){ Data$Column[i]=1 } 
      else if (Data$isCol2[i] == 1 ) { Data$Column[i] = 2 } 
      else if (Data$isCol3[i] == 1 ) { Data$Column[i] = 3 } 
      else {Data$Column[i] = 0 }
}

Data$isRed = factor(Data$isRed, levels = c(1,0))
Data$isEven = factor(Data$isEven, levels = c(1,0))
par(mfrow = c(2,1))
barplot(table(Data$isRed,Data$Column), 
   main = "Red numbers in Columns 1, 2 and 3", 
   names.arg = (c("0","Column 1", "Column 2", "Column 3")) )
barplot(table(Data$isEven,Data$Column), 
   main = "Even numbers in Columns 1, 2 and 3", 
   names.arg = (c("0","Column 1", "Column 2", "Column 3")) )
 
# Scatterplots, from p. 25
proportions = function(n = 100) {
   DF=attributes(n)
   return(data.frame(t(colMeans(DF[3:ncol(DF)]))))
}

multisample = function(n=100,k=100, Seed=3){
   set.seed(Seed)
   ColMeans.df=proportions(n)
   for (i in 1:k-1){
      ColMeans.df=rbind(ColMeans.df,
          proportions(n))
   }
   return(ColMeans.df)
}
## p.27
samples = multisample()
par(mfrow=c(1,1))
plot(samples$isOdd,samples$isRed, 
   main = "Relationship between attributes Red and Even ", 
   xlab = "Proportion of Even numbers", 
   ylab = "Proportion of Red numbers")
abline(lm(samples$isOdd~samples$isRed))

# Boxplots, from p. 28
boxplot(samples)
# Lineplots, from p. 29
par(mfrow=c(4,3), oma = rep(0.1,4), mar = rep(4,4))
names=colnames(samples)
for (i in 1:ncol(samples)){ 
   plot(samples[,i], xlab="Sample", ylab=names[i],
      type = "l")
}
# Application - Outlier detection, from p. 31
samples$isZero = 1-(samples$isRed+samples$isBlack)
Mean = mean(samples$isZero)
Mean

upper = Mean+(3*sd(samples$isZero))
lower = Mean-(3*sd(samples$isZero))
par(mfrow=c(1,1))
plot(samples$isZero, main = "Proportion of zeros", 
   xlab = "sample", ylab= "", ylim = c(0,1))
abline(h=upper)
abline(h=lower)

# Formatting plots, from p. 32
plot( 
  jitter(Data$position, factor=4),jitter(Data$number, factor=4),
  cex = 0.5,
  main = "Relationship between number and position on the 
  wheel", xlab = "Position", 
  ylab="Number",
  col=as.factor(Data$isRed))
 
