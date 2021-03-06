Auto=read.table("/Users/gshilpa/Documents/DS/r-sample/Auto.data",header=T,na.strings="?")
#fix(Auto)
attach(Auto)
cylinders=as.factor(cylinders)
plot(cylinders,mpg,col="red", varwidth=T, hosizontal=T, xlab="cylinders",ylab="mpg")
hist(mpg,col=2,breaks=15)
pairs(Auto)
pairs(~mpg+displacement+horsepower+weight+acceleration,Auto)
plot(horsepower,mpg)
identify(horsepower,mpg,name)
summary(Auto)
summary(mpg)
