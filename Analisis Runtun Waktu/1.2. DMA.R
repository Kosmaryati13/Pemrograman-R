#Double MA (Moving Average)
X=scan()
n=length(X)
k=5

MA=array(NA,dim=c(n))
for(i in 1:n){
  MA[i+(k-1)]=mean(X[i:(i+(k-1))])
}
MA

m=3
DMA=array(NA,dim=c(n))
for(i in 1:n){
  DMA[i+(m-1)+(k-1)]=mean(MA[(i+(k-1)):(i+(m-1)+(k-1))])
}
DMA

a=array(NA,dim=c(n))
for(i in 1:n){
  a[i+(m-1)+(k-1)]=2*MA[i+(m-1)+(k-1)]-DMA[i+(m-1)+(k-1)]
}
a

b=array(NA,dim=c(n))
for(i in 1:n){
  b[i+(m-1)+(k-1)]=(2/(m-1))*(MA[i+(m-1)+(k-1)]-DMA[i+(m-1)+(k-1)])
}
b

#Prediksi
Prediksi=array(NA,dim=c(n))
for(i in 1:n){
  Prediksi[i+(m-1)+(k-1)+1]=a[i+(m-1)+(k-1)]+b[i+(m-1)+(k-1)]
}
Prediksi

#error
e=array(NA,dim=c(n))
for(i in 1:n){
  e[i]=(X[i]-Prediksi[i])^2
}
MSE=mean(e,na.rm=TRUE)
MSE


PE=array(NA, dim=c(n))
for(i in 1:n){
  PE[i]=abs((X[i]-Prediksi[i])/X[i])
}
PE
MAPE=mean(PE,na.rm=TRUE)
MAPE


#Forecast
F=array(NA,dim=c(n))
for(i in 1:n){
  if(h==1){
    F[n+h]=a[n]+b[n]*h 
  }else{
    F[n+h]=a[n]+b[n]*h 
  }}
F
Forecast=function(h){
  a[n]+b[n]*h 
}
Ramalan=Forecast(1:5)
Ramalan


######## data time series#######
Ramalan=c(Prediksi[48],Ramalan)
X=ts(X, start=c(2006,1), end=c(2009,12), freq=12)
Prediksi=ts(Prediksi, start=c(2006,1), end=c(2009,12), freq=12)
Ramalan=ts(Ramalan, start=c(2009,12), end=c(2010,5), freq=12)


########## plot data ################
plot(X, type="l", col="red",lwd=2, xlim=c(2006,2011), ylim=c(1500,5000), xlab="Tahun", ylab="Banyak Pasien", main="Plot Data Asli dan Ramalan MA(3x5)")
lines(Prediksi, col="blue", lwd=2)
limitDate=end(X)[1]+(end(X)[2]-1)/frequency(X)
abline(v=limitDate ,lty=4)
lines(Ramalan,col="green", lwd=2)
legend("topleft", c("Asli", "Prediksi", "Ramalan"), bty="n", lwd=2, col=c("red", "blue","green"))

