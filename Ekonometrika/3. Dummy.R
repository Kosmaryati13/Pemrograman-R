data=read.table(file.choose(), header=TRUE)
data

#membuat regresikan variabel dummy dengan variabel X3

#mempertahankan intersep
model.matrix(Y~X3, data=data) #mendefinisikan variabel X3 dengan Variabel dummy

##mengurangi intersep
model.matrix(Y~X3-1, data=data)

#meregresikan var dummy ----->> Y terhadap var X3
out1=lm(Y~X3, data=data)
out1
summary(out1)

#Menghilangkan intersep
out2=lm(Y~X3-1, data=data)
summary(out2)

#Kategori
data$X2=factor(data$X2, levels=0:1, labels=c("Pria","Wanita"))
data

out3=lm(Y~X1+X2+X3, data=data)
out3
summary(out3)

out4=lm(Y~X2+X3, data=data)
out4
summary(out4)
out5=lm(Y~X3, data=data)
summary(out5)