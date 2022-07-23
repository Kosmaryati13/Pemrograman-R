#Rancangan Bujur Sangkar Latin
#uji tukey
aktifitas=c("A","D","B","C","C","A","D","B","B","C","A","D","D","B","C","A")
nilai=c(1.8,8.2,6.8,1.6,1.3,2.4,7.2,5.8,4.5,2.4,3.2,3.4,1.2,5.2,2.7,1.9)
waktu=c(rep(c("Pagi","Siang","Malam","TengahMalam"),4))
umur=c(rep('1',4),rep('2',4),rep('3',4),rep('4',4))
bobot_Kosmar=data.frame(aktifitas,nilai,waktu,umur)
bobot_Kosmar

hasil=aov(nilai~aktifitas+waktu+umur,data=bobot_Kosmar)
summary(hasil)

TukeyHSD(hasil,'aktifitas',ordered = TRUE)
plot(TukeyHSD(hasil,'aktifitas',ordered = TRUE))
sum(nilai)
mean(nilai)
hasil$residuals

#TABEL TUKEY R
qtukey(0.95,4,6)
