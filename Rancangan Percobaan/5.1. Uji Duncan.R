#Uji Perbandingan Ganda #uji Duncan
# package Agricolae

library(agricolae)
aktifitas=c("A","D","B","C","C","A","D","B","B","C","A","D","D","B","C","A")
nilai=c(1.8,8.2,6.8,1.6,1.3,2.4,7.2,5.8,4.5,2.4,3.2,3.4,1.2,5.2,2.7,1.9)
waktu=c(rep(c("Pagi","Siang","Malam","TengahMalam"),4))
umur=c(rep('1',4),rep('2',4),rep('3',4),rep('4',4))
bobot_Kosmar=data.frame(aktifitas,nilai,waktu,umur)
bobot_Kosmar

hasil1=aov(nilai~aktifitas+waktu+umur,data=bobot_Kosmar)
summary(hasil1)
out1=duncan.test(hasil1,'aktifitas',group = TRUE,console=TRUE)#kalau groupnya diganti false maka akan muncul p-value nya, pembacaan perlakuan berdasarkan p-value
                                                              #kalau groupnya true, berarti baca nya berdasarkan groupnya bukan p-value nya, bacanya di plot, perlakukan satu group sama 
                                                              #kalau console true
                                                              #kalau console false 
plot(out1,variation="IQR")
