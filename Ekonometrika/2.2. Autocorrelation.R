setwd("D:\\KULIAH\\2017-2018\\Semester 1\\3_Ekonometrika\\Pemrograman_R\\autokorelasi")
data=read.table("data_autokorelasi.txt",header=TRUE)

#======= regression and data plot
plot(data$X,data$Y)
abline(lm(data$Y~data$X),col="blue")

regdata=lm(Y~X,data=data)
summary(regdata)

#======= autocorrelation test
library(lmtest)
dwtest(regdata)

# ======== menyelesaikan problem autocorrelation 
# ======== two step Durbin Watson D Stat
dwstat=dwtest(regdata)$statistic
rho=1-(dwstat/2)
n=length(data$Y)
Xstar=data$X[2:n]-rho*data$X[1:(n-1)]
Ystar=data$Y[2:n]-rho*data$Y[1:(n-1)]
regstardata=lm(Ystar~Xstar)
summary(regstardata)
dwtest(regstardata)


