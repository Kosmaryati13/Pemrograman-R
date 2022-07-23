data1 = read.table(file.choose(), header=TRUE)
data1

#regresi dengan konstanta
out<-lm(Y~X2+X3+X4+X5,data=data1)
summary(out)

#regresi tanpa konstanta
out1<-lm(Y~X2+X3+X4+X5-1,data=data1)
summary(out1)

#backward
out2<-lm(Y~X2+X4+X5,data=data1)
summary(out2)

#forward
out3<-lm(Y~X4,data=data1)
summary(out3)

out4<-lm(Y~X2+X4,data=data1)
summary(out4)

out4<-lm(Y~X2+X4,data=data1)
summary(out4)

out5<-lm(Y~X2+X4+X5,data=data1)
summary(out5)

out6<-lm(Y~X2+X3+X4+X5,data=data1)
summary(out6)
