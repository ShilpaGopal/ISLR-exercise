auto =read.csv("/Users/gshilpa/Documents/DS/r-sample/Auto.csv", header = T,na.strings = "?")
summary(auto)
dim(auto)
##remove the null values 
auto=na.omit(auto)
dim(auto)

## Which of the predictors are quantitative and qualitative

qualitative_col=c(2,8,9)
quantitative_col=c(1,3,4,5,6,7)

## What is the range of each quantitative_predictors

sapply(auto[,-qualitative_col], range)
sapply(auto[, -qualitative_col], mean)
sapply(auto[, -qualitative_col], sd)

## Remove 10th through 85th observation
sapply(auto[-seq(10,85),-qualitative_col],range)
sapply(auto[-seq(10,85),-qualitative_col],mean)
sapply(auto[-seq(10,85),-qualitative_col],sd)

# scaterplot
pairs(auto[,-qualitative_col])
plot(auto$year, auto$mpg)

# plot relationship between mpg and other predictors
plot(as.factor(auto$cylinders), auto$mpg)
plot(as.factor(auto$origin), auto$mpg)         
