setwd("E:\\Materi Kuliah\\Script R\\Pengantar Statistik Keuangan")
####Mencari Present Value 
#3. Modifikasi fungsi sehingga tercipta suatu fungsi yang pilihannya menginput (3 jenis bunga)
#mengandung switch
PilihBunga<- function(num,A,i,t,m=TRUE)
  switch(num, 
         satu = {
           Tunggal = A/(1+i*t)
           cat("Nilai Present Value",Tunggal)
         },
         dua = {
           MajemukNominal = A/(1+i/m)^(m*t)
           cat("Nilai Present Value",MajemukNominal)
         },
         tiga = {
           MajemukKontinu = A/(exp(i*t))
           cat("Nilai Present Value",MajemukKontinu)
         }
  )

## Diketahui kasus
#A=3500; i=0.07; t=4 thn; m=2










#___________________________________________________________________
PilihBunga("satu",3500,0.07,4)
PilihBunga("dua",3500,0.07,4,m=1) #tahunan
PilihBunga("dua",3500,0.07,4,m=2) #semesteran
PilihBunga("tiga",3500,0.07,4,m=2)
#--------------------------------------------------------------------
PilihBunga("tiga",10000,0.06931472,20) #soal 6
