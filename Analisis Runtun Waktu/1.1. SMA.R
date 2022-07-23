#Single MA (Moving Average)
X=scan()
n=length(X)
k=5

MA=array(NA,dim=c(n))
for(i in 1:n){
  MA[i+k]=mean(X[i:(i+(k-1))])
}
MA


##MSE##
e=array(NA,dim=c(n))
for(i in 1:n){
  e[i]=(X[i]-MA[i])^2
}
e
SSE=sum(e,na.rm=TRUE)
SSE
MSE=mean(e,na.rm=TRUE)
MSE

##MAPE##
PE=array(NA, dim=c(n))
for(i in 1:n){
  PE[i]=abs((X[i]-MA[i])/X[i])
}
PE
MAPE=mean(PE,na.rm=TRUE)
MAPE

##data time series##
X=ts(X, start=c(2006,1), end=c(2009,12), freq=12)
X
MA5=ts(MA, start=c(2006,1), end=c(2009,12), freq=12)
MA5
pred=ts(MA[49], start=c(2010,1),freq=12)
pred

##plot tipe titik##
plot(X, type="p", col="red",lwd=2, xlim=c(2006,2010), ylim=c(1500,5000), xlab="tahun", ylab="jumlah pasien", main="Plot Data Asli dan Ramalan MA(5)")
lines(MA5, col="blue", lwd=2, type="p")
limitDate=end(X)[1]+(end(X)[2]-1)/frequency(X)
abline(v=limitDate ,lty=4)
lines(pred,col="green", lwd=2, type="p")
legend("topleft", c("Asli", "prediksi", "Ramalan"), pch=21, bty="n", lwd=2, col=c("red", "blue","green"))


##plot tipe garis#
pred=ts(MA[48:49], start=c(2009,12), end=c(2010,1), freq=12)
pred

plot(X, type="l", col="red",lwd=2, xlim=c(2006,2010), ylim=c(1500,5000), xlab="Tahun", ylab="Jumlah Pasien", main="Plot Data Asli dan Ramalan MA(5)")
lines(MA5, col="blue", lwd=2)
limitDate=end(X)[1]+(end(X)[2]-1)/frequency(X)
abline(v=limitDate ,lty=4)
lines(pred,col="green", lwd=2)
legend("topleft", c("Asli", "prrdiksi", "Ramalan"), bty="n", lwd=2, col=c("red", "blue","green"))

