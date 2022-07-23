#RBSL (Rancangan Bujur Sangkar Latin)


perlakuan = c('A','B','C','D','B','C','D','A','C','D','A','B','D','A','B','C')

baris = c(rep(c('1','2','3','4'),4))
kolom = c(rep('Aljabar',4), rep('Geometri',4), rep('Statistika',4), rep('Kalkulus',4))

respon = c(84,91,59,75,79,82,70,91,63,80,77,75,97,93,80,68)


kuliah = data.frame(perlakuan, baris, kolom, respon)


hasil = aov(respon~perlakuan+baris+kolom, data = kuliah)
summary(hasil)


# Uji Kecukupan Model (Homogenitas, normalitas)

bartlett.test(respon~perlakuan, data = kuliah)

shapiro.test(hasil$residuals)


# Uji perbandingan Ganda untuk Perlakuan

TukeyHSD(hasil,'perlakuan',ordered = TRUE)

plot(TukeyHSD(hasil,'perlakuan',ordered = TRUE))

