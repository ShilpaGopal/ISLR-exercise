library(MASS)
install.packages("corrplot")
install.packages("ggplot2")
install.packages("plotly")
#headers
names(Boston)
all_correlation=cor(Boston)
print(all_correlation[,14])

#correlation
library(corrplot)
cor_matrix = cor(Boston)
corrplot(cor_matrix, type="upper")

# scaterplotes
cols=c(14,13,6,11,3,10)
pairs(Boston[,cols])

attach(Boston)
plot(age,crim)
plot(Boston[, 1], Boston[, 9])

# Relationship between crime rate and other predictors value

plot(crim~medv, data=Boston, main="Relation between crim rate and predictors", xlab="median value of owner-occupied", ylab="crim rate")
str(Boston)
summary(crim)
plot(tax,crim)
hist(crim)

#summarys of suburbs with particularly high crime rate
quantile(crim,.90)
high_crime=subset(Boston,crim > quantile(crim,.90))
sum(crim > quantile(crim, .90))
summary(high_crime)
require(plotly)
require(ggplot2)
plot_ly(data = Boston, x = ~lstat, y = ~crim)
plot_ly(Boston,x=~tax,y=~crim)

#Histogram of crim
plot_ly(Boston,x=~crim, type="histogram")

#Comparing attributes between suburbs with 90th percentile crime rate and Boston:

plot_ly(Boston, y=~lstat, name ="Boston", type="box") %>%
  add_boxplot(high_crime, y=~lstat,name="Area between 90% of crime rate", type="box")

plot_ly(Boston, y = ~medv, name = "Boston", type="box")  %>%
  add_boxplot(high_crime, y= ~medv, name = "Area with 90th percentile lstat", type="box")

## portion of population with lower status in Boston

plot_ly(data=Boston, x=~lstat, y=~medv)
plot_ly(Boston, x=~lstat, y=~rm)
plot_ly(Boston, x=~lstat, y=~age)

plot_ly(Boston, x=~lstat, type="histogram")

#what do surburbs with 90th percentile lstat (hlstat) look like

hlstat=subset(Boston, lstat>quantile(lstat,.90))

sum(lstat>quantile(lstat,.90))
summary(hlstat, Boston)

#Comparing attributes between surburbs with very high lstat and Boston:

plot_ly(Boston, y=~rm, name=Boston, type="box") %>%
  add_boxplot(hlstat, y=~rm, name="Area with 90th percentile lstat",type="box")

plot_ly(data=Boston, y = ~crim, name = "Boston", type="box")  %>%
  add_boxplot(data=hlstat, y= ~crim, name = "Area with 90th percentile lstat", type="box")
plot_ly(data=Boston, y = ~age, name = "Boston", type="box")  %>%
  add_boxplot(data=hlstat, y= ~age, name = "Area with 90th percentile lstat", type="box")


#House Value Prediction
any(is.na(Boston))
data("Boston")
smp_size = floor(0.75*nrow(Boston))
print(smp_size)
set.seed(12)
train_ind=sample(seq_len(nrow(Boston)),size=smp_size)

train = Boston[train_ind,]
test<-Boston[-train_ind, ]


lm.fit=lm(medv~lstat,data=train)
summary(lm.fit)

install.packages("Metrics")
require(Metrics)

evaluate=predict(lm.fit,test)
rmse(evaluate,test[,14])
dat <- data.frame(lstat = (1:35),
                  medv = predict(lm.fit, data.frame(lstat = (1:35))))

plot_ly() %>% 
  add_trace(x=~lstat, y=~medv, type="scatter", mode="lines", data = dat, name = "Predicted Value") %>%
  add_trace(x=~lstat, y=~medv, type="scatter", data = test, name = "Actual Value")

plot(lm.fit)

lm.fit=lm(medv~lstat+I(lstat^2),data=train)
dat <- data.frame(lstat = (1:40),
                  medv = predict(lm.fit, data.frame(lstat = (1:40))))
plot_ly() %>% 
  add_trace(x=~lstat, y=~medv, type="scatter", mode="lines", data = dat, name = "Predicted Value") %>%
  add_trace(x=~lstat, y=~medv, type="scatter", data = test, name = "Actual Value")

summary(lm.fit)

evaluate<-predict(lm.fit, test) 
rmse(evaluate,test[,14])
















