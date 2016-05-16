# loading and discovering the lattice package, from .36
if(!require("lattice")) install.packages("lattice") 
library(lattice)
search()
ls(2)
# Discovering multipanel conditioning using xyplot(), from p. 37
xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris)
# Discovering other lattice plots, from p. 39
histogram(~Temp | factor(Month), data = airquality, 
xlab = "Temperature", ylab = "Percent of total")
# Stacked bars, from p. 41
salesdata = as.data.frame (matrix(rep(0, 200), nrow = 50, ncol = 4))
names(salesdata) = c("Year", "Branch", "Dept", "Sales")
salesdata$Year = c(rep(2004,25),rep(2014,25)) 
salesdata$Branch = rep(c("Branch 1", "Branch 2", 
   "Branch 3", "Branch 4", "Branch 5"), 5)
salesdata$Dept = c(rep("Movies",5),rep("TVSeries",5),
   rep("Documentary",5),rep("Music", 5), rep("Instructional", 5))
salesdata$Sales = c(50.795, 25.469, 30.241, 100.658, 36.412, 45.632, 
   30.541, 31.421, 70.212, 25.412, 5.124, 3.124, 4.065, 10.258, 0.82, 
   10.658, 5.474, 6.541, 10.698, 76.584, 1.021, 0.504, 0.76, 0.15, 0.3, 
   203.18, 101.876, 120.964, 402.632, 145.648, 182.528, 122.164, 125.684, 
   280.848, 101.648, 20.496, 12.496, 16.26, 41.032, 3.28, 42.632, 21.896, 
   26.164, 42.792, 306.336, 4.084, 2.016, 3.04, 0, 0)

barchart(Dept ~ Sales | Branch, groups = Year, data = salesdata, 
   auto.key = list(space = 'right'), stack = TRUE)
# Dotplots, from p. 43
dotplot(Dept ~ Sales | Branch, groups = Year, data = salesdata, 
   cex = 2, auto.key = list(space = "right"), 
   main = "Sales by department, branch and year") 
# Displaying data points as text, from p. 45
fertility=swiss
fertility$Mortality[(fertility$Infant.Mortality > 
   mean(swiss$Infant.Mortality)) == TRUE] = "High infant mortality" 
fertility$Mortality[(fertility$Infant.Mortality >    

   mean(swiss$Infant.Mortality)) == FALSE] = "Low infant mortality"
fertility$Rural[(fertility$Agriculture > 
   mean(swiss$Agriculture)) == TRUE] = "Rural"
fertility$Rural[(fertility$Agriculture> 
   mean(swiss$Agriculture)) == FALSE] = "Non-rural"
fertility$District = rownames(fertility)

## p. 46
xyplot(Fertility ~ Education | Mortality * Rural, data = fertility,
   groups = as.character(District),
   panel = function(x, y, subscripts, groups) {
      ltext(x, y, labels = groups[subscripts], cex=.4)},
      main = "Fertility and education in 1888 Occidental Switzerland")
# Updating graphics, from p. 47
xyplot(weight ~ Time | Diet, data=ChickWeight)

Graph = xyplot(weight ~ Time | Diet, groups = Chick,
   data=ChickWeight)

Graph = update(Graph, 
   main = "Chicken growth by diet",
   ylab = "Weight of the chicken",
   xlab = "Days since birth")
Graph = update(Graph, index.cond = list(c(3,4,1,2)))
Graph

# Case study, from p. 50
if(!require("latticeExtra")) install.packages("latticeExtra")
library(latticeExtra)
head(USCancerRates, 3)
summary(USCancerRates)

set.seed(987)
subsample=USCancerRates[sample(1:nrow(USCancerRates),75),c(7,4,1)]
parallelplot(subsample, horizontal.axis=F, groups=subsample$state)

parallelplot(~USCancerRates[,c(4,1)]|USCancerRates$state,   
   horizontal.axis=F,data=USCancerRates, varnames =c("F","M"))
## from p.53
xyplot(rate.male ~ rate.female | state, 
   data=USCancerRates, main = "Death due to cancer",
   ylab= "Rates in males", 
   xlab = "Rates in females",
   panel = function(x, y, ...) {
      panel.xyplot(x,y)
      panel.abline(lm(y~x))
   }
)
# Integrating supplementary data, from p. 55
NoInsur = c(15.1, 16.3, 24.3, 21.6, 21.2, 15.6, 11.8, 13.7, 
   18.7, 17.6, 17.3, 12.9, 12.1, 11, 11.1, 14.8, 19.8, 
   13.2, 13.8, 11.8, 11.2, 9.6, 19.6, 12.1, 17.6, 10.4, 
   18.1, 10.9, 16.5, 22, 17.2, 15.5, 13.1, 11.1, 17.7, 
   14.3, 10, 10, 16.4, 11.9, 13.9, 24.4, 13.1, 10.1, 
   13.1, 12.4, 16.5, 9.4, 15.3)

state = c("Alabama", "Alaska", "Arizona", "Arkansas", 
   "California", "Colorado", "Connecticut", "Delaware", 
   "Florida", "Georgia", "Idaho", "Illinois", "Indiana", 
   "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
   "Maryland", "Massachusetts", "Michigan", "Minnesota", 
   "Mississippi", "Missouri", "Montana", "Nebraska", 
   "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
   "New York", "North Carolina", "North Dakota", "Ohio", 
   "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
   "South Carolina", "South Dakota", "Tennessee", "Texas", 
   "Utah", "Vermont", "Virginia", "Washington", 
   "West Virginia", "Wisconsin", "Wyoming")
CentLong = c(-86.68, 21.5, -111.93, -92.12, -119.27, -105.56, 
   -72.75, -75.40, -83.81, -83.18, -114.13, -89.50, -86.43, 
   -93.37, -98.54, -85.77, -91.43, -69.01, -77.25, -71.72, 
   -86.43, -93.38, -89.88, -92.44, -110.04, -99.68, -117.03, 
   -71.58, -74.71, -106.03, -75.82, -79.88, -100.30, -82.67, 
   -98.72, -120.52, -77.60, -71.52, -80.94, -100.24, -85.98, 
   -99.58, -111.53, -72.53, -79.47, -120.84, -80.19, -89.83, 
   -107.55)
CentLat = c(32.63, 61.38, 34.17, 34.75, 37.27, 39.00, 
   41.53, 39.15, 27.75, 32.68, 45.50, 39.75, 39.81, 41.93, 
   38.50, 37.88, 30.97, 45.22, 38.81, 42.04, 44.99, 46.44, 
   32.50, 38.31, 46.68, 41.50, 38.50, 44.03, 40.14, 34.17, 
   42.76, 35.23, 47.47, 40.20, 35.31, 44.13, 40.99, 41.58, 
   33.61, 44.21, 35.83, 31.17, 39.50, 43.86, 38.00, 47.27, 
   38.92, 44.81, 43.00)

DF = data.frame(state, CentLong, CentLat, NoInsur)

xyplot(DF$NoInsur~DF$CentLat, 
   main = "State latitude and health insurance coverage", 
   xlab = "Latitude", 
   ylab = "Percent of noninsured individuals")
## from p. 57
CLong = rep(0,nrow(USCancerRates))
CLat = rep(0,nrow(USCancerRates))
NInsur = rep(0,nrow(USCancerRates))
cancer = data.frame(USCancerRates[c(7,1,4)],CLong, CLat, NInsur)
for (i in 1:nrow(cancer)) {
   for (j in 1:nrow(DF)) {
      if (cancer[i,1] == DF[j,1]) {
            cancer[i,4:6] = DF[j,2:4]
            next
      }
   }
}

xyplot((rate.female + rate.male)/2 ~ NInsur | 
      ifelse(cancer$CLat>median(cancer$CLat),"North","South"),
   data = cancer,
   index.cond=list(c(2,1)),
   panel = function(x, y, ...) {
      panel.xyplot(x,y)
      panel.abline(lm(y~x))
      panel.loess(x,y, col = "Red") },
   xlab = "Percent of noninsured individuals",
   ylab = "Mortality due to cancer",
   main = "Health insurance coverage and Mortality due to 
      cancer by latitude"
)
