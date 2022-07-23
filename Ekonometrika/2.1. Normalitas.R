data1 = read.table(file.choose(), header=TRUE)
data1

out<-lm(Y~X2+X3+X4+X5,data=data1)
summary(out)

out2<-lm(Y~X2+X4+X5,data=data1)
summary(out2)

#histogram dan plot densitas
hist(out2$residuals, probability=T,  main="Histogram of data")
lines(density(out2$residuals),col="red")

#QQnorm
qqnorm(out2$residuals,main="QQ plot of data",pch=19)
qqline(out2$residuals, col="red")

#uji normalitas
#shapiro wilk test
shapiro.test(out2$residuals)

#Jika data tidak normal maka salah satu alternatifnya dapat dilakukan transfromasi variabel dependen.
