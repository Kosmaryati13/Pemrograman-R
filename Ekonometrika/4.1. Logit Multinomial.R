library(foreign)###data dari SAS
library(nnet) ##multinom
library(ggplot2)
library(reshape2)
##dta---> memanggil data dari sas
Data=read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
Data
##deskripsi dari data
#berapa sisiwa yang mengambil program general berdasarkan status ekonomi
with(Data,table(ses,prog))
##write --->numerik
#berapa nilai mean dan SD dari writeing score dari program general,,,dst(masing2 program)
with(Data,do.call(rbind,tapply(write,prog,function(x)c(M=mean(x),SD=sd(x)))))
#do.call-->esekusi program,, x adalah write

Data$prog2=relevel(Data$prog,ref="academic")##relevel-->reference(pembagi)
##academic sebagai rujukan (reference)
Data$prog2 ##Levels: academic general vocation (telah diseting rujukan(reference))
Data$prog ###Levels: general academic vocation
##mengandung koefisien jadi variabel dummynya hanya keluar 
test=multinom(prog2~ses+write, data=Data)
test=multinom(prog2~ses+write-1, data=Data)
summary(test)
##Residual Deviance: 359.9635 ,, AIC: 375.9635 (membandingkan model terbaik)

test1=multinom(prog~ses+write, data=Data)
summary(test1)
z=summary(test)$coefficients/summary(test)$standard.errors
z
p=(1-pnorm(abs(z),0,1))*2 ###0,1-->normal standar 
p
fitted(test) ###y_het --->error
##contoh data baru
dses=data.frame(ses=c("low","middle","high"),write=mean(Data$write))
dses
##probabilitas
predict(test,newdata=dses,"probs")
dwrite=data.frame(ses=rep(c("low","middle","high"),each=41),write=rep(c(30:70),3))
dwrite
pp.write=cbind(dwrite,predict(test,newdata=dwrite,type="probs"))
pp.write
lpp=melt(pp.write, id.vars=c("ses","write"),value.name="probability")
##melt-->menggabungkan
lpp
ggplot(lpp,aes(x=write,y=probability,colour=ses))+geom_line()+facet_grid(variable~.,scales="free")
##penggabungan
ggplot(lpp,aes(x=write,y=probability,colour=ses))+geom_line()
ggplot(lpp,aes(x=write,y=probability,colour=ses))+geom_point()
ggplot(lpp,aes(x=write,y=probability,colour=ses))+geom_line()+facet_grid(.~variable,scales="free")
