# Discrete uniform distribution, from p.132
rolls = sample(6, size = 1000000, replace = TRUE)
hist(rolls)
# The normal distribution, from p. 133
curve(dnorm(x, 0 ,1), lwd = 2, xlim=c(-3,3), xlab="", ylab="", 
main = "The standard normal distribution")
# p. 135
if(!require("HistData")) install.packages("HistData")
library(HistData)
hist(Galton$parent, xlab="Height", 
   main="Height of adults in inches")

x1 = runif(1000)
x2 = rnorm(1000)
shapiro.test(x1)

shapiro.test(x2) 

# The student distribution, p. 136
curve(dt(x, 14), col = "black", lwd = 2, xlim=c(-3,3), xlab="",   
   ylab="", main = "The t distribution")
curve(dt(x, 199), col = "grey", lwd = 2, add=T)
# The binomial distribution, from p. 137
p = 1/6
N = 100
n = 1
v = rep(1,40)
for (n in 0:40) {
 v[n] = choose(N, n) * (p^n) * (1 - p)^(N-n)
}
plot(v, type="l", xlab = "Exact number of successes", ylab = 
"Probability")
# Pearson's correlation, from p. 142
data(anscombe)
c1=cor(anscombe$x1, anscombe$y1); 
c2=cor(anscombe$x2, anscombe$y2); 
c3=cor(anscombe$x3, anscombe$y3); 
c4=cor(anscombe$x4, anscombe$y4)
c1; c2; c3; c4


par(mfcol=c(1,4))
plot(anscombe$x1, anscombe$y1); 
plot(anscombe$x2, anscombe$y2); 
plot(anscombe$x3, anscombe$y3); 
plot(anscombe$x4, anscombe$y4)
cor.test(anscombe$x1, anscombe$y1)
# Spearman's correlation, from p. 145
A = c(3,4,2,6,7)
B = c(4,3,1,6,5)

RankA = rank(A);  RankB = rank(B)
cor(A,B,method = "spearman")
cor.test(A,B, method = "spearman")
