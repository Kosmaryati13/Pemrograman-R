##Rancangan Faktorial RAL##

respon=c(130,74,155,180,150,159,188,126,138,168,110,160,
         34,80,40,75,136,106,122,115,174,150,120,139,
         20,82,70,58,25,58,70,45,96,82,104,60)
sum(respon)
baris=c(rep(c(rep('1',4),rep('2',4), rep('3',4)),3)) #material
kolom=c(rep('t15',12), rep('t70',12), rep('t125',12)) #temperature
baterai=data.frame(respon,kolom,baris)
baterai

#Faktorial RAL (Fixed Model)
hasil=aov(respon~baris*kolom, data=baterai) #bisa juga --> (respon~baris+kolom+baris*kolom)
summary(hasil)

kolom1=factor(kolom, levels= c("t15","t70", "t125")) #mengurutkan kolom dari temperature 15,70,125
with(baterai, interaction.plot(kolom, baris, respon, type= "b", pch=19, fixed=TRUE, xlab="Temperature", ylab = "Avergafe life")) 
plot.design(respon~baris*kolom, data=baterai)
