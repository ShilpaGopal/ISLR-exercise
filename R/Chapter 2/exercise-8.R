college = read.csv("./data/College.csv")
fix(college)
rownames(college)=college[,1]
fix(college)
college=college[,-1]
fix(college)
summary(college)
pairs(college[,1:10])
college[1,]
attach(college)
plot(Outstate,Private)
Elite=rep("No",nrow(college))
Elite[Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)

summary(college)
boxplot(Outstate,Elite)
par(mfrow=c(2,2))
college[which.max(Top10perc),]
acceptance_rate <- college$Accept/college$Apps
college[which.min(acceptance_rate),]