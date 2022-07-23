setwd("E:\\Materi Kuliah\\Script R\\Pengantar Statistik Keuangan")
#1.Buatlah fungsi nilai akumulasi bunga majemuk(tahunan)
NilaiAkumulasiMajemukTahunan<- function(k,i,t){
  N.Akumulasi.MajemukTahunan = k*(1+i)^t
  return(N.Akumulasi.MajemukTahunan)
}
NilaiAkumulasiMajemukTahunan(2500,0.14,3)

#2.modifikasi fungsi tersebut sehingga dapat digunakakn untuk menentukan nilai akumulasi bunga majemuk nominal
#note:ingat bahwa majemuk
NilaiAkumulasiMajemukTahunanNom<- function(k,i,t,m){
  N.Akumulasi.MajemukTahunanNom = k*(1+(i/m))^(m*t)
  return(N.Akumulasi.MajemukTahunanNom)
}
NilaiAkumulasiMajemukTahunanNom(100,0.06,0.5,4)

#3. Modifikasi fungsi sehingga tercipta suatu fungsi yang pilihannya menginput (3 jenis bunga)
#mengandung switch
PilihBunga<- function(num, k,t,i,m=TRUE)
  switch(num, 
         satu = {
           Tunggal = k*(1+(i*t))
           cat("Nilai Akumulasi Bunga Tunggal",Tunggal)
         },
         dua = {
           MajemukNominal = k*(1+(i/m))^(m*t)
           cat("Nilai Akumulasi Bunga majemuk Nominal",MajemukNominal)
         },
         tiga = {
           MajemukKontinu = k*(exp(i*t))
           cat("Nilai Akumulasi Bunga Majemuk Kontinu",MajemukKontinu)
         }
  )

PilihBunga("satu",2000,4,0.08)
PilihBunga("dua",2000,4,0.08,m=1)
PilihBunga("tiga",2000,4,0.08,m=1)
