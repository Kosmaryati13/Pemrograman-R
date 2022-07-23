# RAKL (Rangkaian Acak Kelompok)

# Percobaan pemberian ransum (4 jenis: A, B, C, D) pada pertambahan bobot domba jantan. Dengan kelompok umur dibagi menjadi 4. 


perlakuan = c(rep('A',4),rep('B',4),rep('C',4),rep('D',4))
kelompok = c(rep(c('1','2','3','4'),4))

respon = c(2,3,3,5,5,4,5,5,8,7,10,9,6,5,5,2)


percobaan = data.frame(perlakuan, kelompok, respon)


hasil = aov(respon~perlakuan+kelompok, data = percobaan)
summary(hasil)

# Box plot respon VS perlakuan dan kelompok

par(mfrow = c(1,2)) #membuat 2 gambar 1 tempat

plot(respon~perlakuan, data = percobaan)

plot(respon~kelompok, data = percobaan)


# Uji perbandingan ganda

TukeyHSD(hasil, ordered = TRUE, "perlakuan")

plot(TukeyHSD(hasil, ordered = TRUE, "perlakuan"))

