datadec=scan()

#mengubah data menjadi data time series
series=ts(datadec, start=c(2010,1),freq=12) #freq=12 karena data bulanan
series
#Membuat Plot #---> setiap kenaikan data 
ts.plot(series,col="blue",main="time series plot")

####Additive(additive-->jumlahan seluruh indeks musiman nol)
datadec=decompose(series,type="additive") #jika multiplicative diganti type="multiplicative"
plot(datadec)
datadec$seasonal
datadec$trend
datadec$random

#meramalkan beberapa priode kedepan
library(forecast)
sindexf(datadec, 12)

##Mengkombinasi antara 2 metode
seasadj(datadec)#data yg indeks musimanya telah dibuang (data non musiman)
##di hydman data yang diatas yg diramalkan(data tanpa indeks musiman) dengan menggunakan
metode non musiman(MA, double MA, ARIMA)
ts.plot(series,col="blue")
lines(seasadj(datadec),col="red")

DES=HoltWinters(seasadj(datadec), alpha=NULL, beta=NULL, gamma=FALSE)
DES
forecast_seasadj=predict(DES,12)
forecast_seasadj

###mengcombine(peramalan non musiman+peramalan musiman)
forecast_datadec=forecast_seasadj+sindexf(datadec,12)
forecast_datadec

ts.plot(series,col="blue", xlim=c(2010,2017), ylim=c(400,2000))
lines(forecast_datadec,col="red")