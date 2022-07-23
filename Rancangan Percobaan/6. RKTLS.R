#RKTLS (Rancangan Kelompok Tak Lengkap Seimbang)// BIBD               

perlakuan=c('A', 'C', 'D', 'A', 'B', 'C', 'B', 'C', 'D', 'A', 'B', 'D')
respon=c(73,73,75,74,75,75,67,68,72,71,72,75)
kelompok=c(rep("1",3),rep("2",3), rep("3",3), rep("4",3)) 
bobot=data.frame(perlakuan,respon,kelompok)
bobot

hasil=aov(respon~kelompok+perlakuan,data=bobot) #tidak boleh terbalik
summary(hasil)

#BIBD untuk perlakuan dan kelompok terkoreksi
hasil1=aov(respon~perlakuan+kelompok, data=bobot) #jika dibalik sudah sama hasilnya
drop1(hasil1, test ="F") #F disini bukan False tetapi pengujian menggunakan uji F, dan functionnya drop1
#jika menggunakan drop1 boleh kelompok&perlakuannya boak-bolik
