setwd("E:\\Sekolah\\Kuliah\\Semester 6\\Pengantar Statistik Keuangan") 
#Single MA
x=scan()
analisis.teknikal<-function(num,x,n){
  nx<-length(x)
y<-numeric(nx) ###sama dengan array--->kesamping
switch(num,
       satu={
         for(i in n:nx){
           y[i]=mean(x[(i-n+1):i])}
           return(data.frame(cbind(x,SMA=y)))
         },
       dua={
         SMA<-numeric(nx)
         for(i in n:n){
           SMA[i]=mean(x[(i-n+1):i])}
         a=2/(n+1)
         y[n-1]=SMA[n]
         for(i in n:nx){
           y[i]=a*(x[i]-y[i-1])+y[i-1]}
       y[n-1]=0
       return(data.frame(cbind(x,EMA=y)))
       },
       tiga={
         for(i in n:nx){
           y[i]=2*sum((n:1)*x[i:(i-n+1)])/(n*(n+1))}
         return(data.frame(cbind(x,WMA=y)))
       }
      )
       
      }

SMA=analisis.teknikal(1,x,10)
EMA=analisis.teknikal(2,x,10)
WMA=analisis.teknikal(3,x,10)
Hasil=data.frame(SMA,EMA$EMA,WMA$WMA)
Hasil
View(Hasil)
#-------------------------------------------------
SMA
x[10:1]
