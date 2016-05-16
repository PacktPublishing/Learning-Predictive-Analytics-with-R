# A quick look at the file menu, from p. 4
print("Hi again, world")
# A quick look at the misc menu, from p. 5
repeat(a = 1)
b = c(1,2,3)
b[2]
c = list(a,b)
c[[1]]
c[[2]][1]
M = matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)
M[1,]
M[,2]
M[2,1]
f = data.frame(c("a","b","c"),c(1,2,3))
f[,1]
f[1]
f[1,2]
f[2:3,1]
# Packages, from p. 8
library(lib = .Library)
objects(package:stats)
# Installing packages in R, from p. 9

### The if(!require()) statements was 
### added here and elsewhere to make it easy 
### to run the whole code several times 
### without having to download the packages
### each time.
if(!require("animation")) install.packages("animation")
# Loading packages in R, from p. 11
library(animation)
df=data.frame(c(-3,3),c(3,-3))
saveHTML({
   for (i in 1:20)  {
      plot(df)
      df = rbind(df,c(rnorm(1),rnorm(1)))
   }
},
img.name = "plot", 
imgdir = "unif_dir", 
htmlfile = "test.html", 
autobrowse = FALSE,
title = "Animation test", 
description = "Testing the animation package for the first time.")


if(!require(prob)) install.packages("prob")
library(prob)
objects(package:prob)
