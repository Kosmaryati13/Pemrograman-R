#Volatility
x=scan()

vola<-function(x,tau){
  n<-length(x)
  y<-array(NA,dim=c(n,2)) ###membuat kolom kosong ---->>kebawah
  
  for(t in 2:n){    #2:n--->> dimulai dari baris ke-2
  ##Kolom pertama
    y[t,1]=log(x[t]/x[t-1])}  ##menghitung log return
  
  Rt_bar=mean(y[,1],na.rm = TRUE)  ###nilai rata2 kolom pertama
  ##Kolom ke 2
  y[,2]=(y[,1]-Rt_bar)^2
  
  jumlah=sum(y[,2],na.rm = TRUE) ###na.rm = TRUE -----> NA di remove
  sigma=sqrt((1/(n-1))*jumlah)
  volatilitas=sigma*sqrt(tau)
  return(volatilitas)   ##mengeluarkan nilai volatillitas
    
}

####Contoh kasus
vola(x,252) ###data saham indofood (daily)
plot(x,type="1")

vola(x,52) ###data saham microsoft (weekly)
