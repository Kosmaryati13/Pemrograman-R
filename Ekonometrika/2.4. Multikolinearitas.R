setwd("D:\\KULIAH\\2017-2018\\Semester 1\\3_Ekonometrika\\Pemrograman_R\\multikolinear")
data2 <- read.table("data_multiko.txt",header=TRUE)

regres1 <- lm(Y ~ X1 + X2 + X3 +X4 +X5 +TIME, data=data2)
summary(regres1)

#multikolinearitas check
cor(data2[,c(-1,-2)])

#check VIF
library(car)
vif(regres1)

#aux reg, check only R^2
summary(lm(Y ~ X1 + X2 + X3 +X4 +X5 +TIME, data=data2))
summary(lm(X1 ~ X2 + X3 +X4 +X5 +TIME, data=data2))
summary(lm(X2 ~ X1 + X3 +X4 +X5 +TIME, data=data2))
summary(lm(X3 ~ X1 + X2 +X4 +X5 +TIME, data=data2))
summary(lm(X4 ~ X1 + X2 +X3 +X5 +TIME, data=data2))
summary(lm(X5 ~ X1 + X2 +X3 +X4 +TIME, data=data2))
summary(lm(TIME ~ X2 + X3 +X4 +X5 , data=data2))


#solving multicollinearity
X10=data2$X2/data2$X1
regres2 <- lm(Y ~ X10 + X4 +X5 , data=data2)
summary(regres2)
vif(regres2)



