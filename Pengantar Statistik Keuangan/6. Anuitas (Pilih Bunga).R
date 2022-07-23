setwd("E:\\Materi Kuliah\\Script R\\Pengantar Statistik Keuangan") 
##ANUITAS AKHIR
require(stats)
#Nilai PV(an)
Anuitas<-function(num,k,i,t,m=TRUE)
  switch(num, 
         satu = {
           j=i/m
           n=t*m
           v=(1/(1+j))
           an_anuitasAkhir = k*((1-((v)^n))/j)
           sn_anuitasAkhir =k*((((1+j)^n)-1)/j)
           cat("Nilai an Anuitas Akhir",an_anuitasAkhir,"\n")
           cat("Nilai an Anuitas Akhir",sn_anuitasAkhir)
         },
         dua = {
           j=i/m
           n=t*m
           v=(1/(1+j))
           an_anuitasAwal= k*((1-((v)^n))/(j*v))
           sn_anuitasAwal =k*((((1+j)^n)-1)/(j*v))
           cat("Nilai an Anuitas Awal",an_anuitasAwal,"\n")
           cat("Nilai an Anuitas Awal",sn_anuitasAwal)
           
         })
Anuitas(1,2000,0.08,4,m=2)
Anuitas(2,2000,0.08,4,m=2)

Anuitas(1,2000,0.08,4)
Anuitas(2,2000,0.08,4)
