setwd("E:\\Sekolah\\Kuliah\\Semester 6\\Pengantar Statistik Keuangan") 
##ANUITAS AKHIR
require(stats)
#Nilai K (present value/PV)
Angsuran<-function(num,nilai,i,t,m=TRUE)
  switch(num, 
         satu = {
           j=(i/m)
           n=(t*m)
           v=(1/(1+j))
           k_an_anuitasAkhir =nilai/((1-(v^n))/j)
           cat("Nilai Angsuran an anuitas Akhir",k_an_anuitasAkhir)
         },
         dua = {
           j=(i/m)
           n=(t*m)
           v=(1/(1+j))
           k_sn_anuitasAkhir = nilai/((((1+j)^n)-1)/j)
           cat("Nilai Angsuran sn anuitas Akhir",k_sn_anuitasAkhir)
         },
         tiga = {
           j=(i/m)
           n=(t*m)
           v=(1/(1+j))
           k_an_anuitasAwal = nilai/((1-(v^n))/(j*v))
           cat("Nilai Angsuran an anuitas Awal",k_an_anuitasAwal)
         },
         empat = {
           j=(i/m)
           n=(t*m)
           v=(1/(1+j))
           k_sn_anuitasAwal = nilai/((((1+j)^n)-1)/(j*v))
           cat("Nilai Angsuran sn anuitas Akhir",k_sn_anuitasAwal)
           
         })
Angsuran(1,8000,0.08,5,m=6)
Angsuran(2,8000,0.08,5,m=6)
Angsuran(3,8000,0.08,5,m=6)
Angsuran(4,8000,0.08,5,m=6)
