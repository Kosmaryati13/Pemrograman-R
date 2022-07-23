setwd("D:\\Kuliah\\Semester 6\\Pengantar Statistika Keuangan\\Syntax R") 
##NEW FUNCTION##
#Fungsi Bunga (i)#

fungsibunga <- function(num, A, k, t, m=TRUE)
  #A: Nilai Akumulasi, k : modal, t : tahun, m : banyaknya pemberi bunga dalam satu tahun
  #fungsibunga : pilhan(1: bunga tunggal, 2 : bunga majemuk nominal, 3 : bunga majemuk kontinu)
  
  switch(num, 
         satu = {
           bungatunggal = ((A/k)-1)/t
           cat("Bunga Tunggal : ", bungatunggal)
         },
         dua = {
           bungamajemuknominal = (((A/k)^(1/(m*t)))-1)*m
           cat("Bunga Majemuk Nominal : ", bungamajemuknominal)
         },
         tiga = {
           bungamajemukkontinu = (log(A/k))/t
           cat("Bunga Majemuk Kontinu : ", bungamajemukkontinu)
         })

#contoh diket A : 3500, k:2000, t:4, m:2 -> opsional jika menggunakan majemuk nominal semesteran