###ARIMA (p,d,q)
## p=AR ; q=MA>>Acf ; d=differencing
Data=scan()
Data
Data_ts=ts(Data, start=c(2008,1),freq=12)
Data_ts
View(Data)
library("tseries")
library("forecast")
ts.plot(Data_ts, main="Plot Data Wisatawan", col=10)
ls("package:tseries")
adf.test(Data_ts) #adf.test (menguji kestasioneran (H0: data tdk stasioner))
par(mfrow=c(1,2)) #membagi plot menjadi dua bagian
acf(Data_ts, lag.max=24) #lag kelipatan 12 (12,24,36)
##lag data keluar dari garis (maks 4 lag yg keluar dari garis)
## defferencing nol>>
> belum stasioner maka data harus di stasionerkan dengan diff

pacf(Data_ts, lag.max=24)
par(mfrow=c(1,1))

##mengatasi data stasioner
Data.diff1=diff(Data_ts,1)
ts.plot(Data.diff1, main="TS : Data Wisatawan (Diferensi orde 1)")

#Uji adf
adf.test(Data.diff1) 
par(mfrow=c(1,2))
acf(Data.diff1, lag.max=24)
##Q>>MA>> 2
pacf(Data.diff1, lag.max=24)
##P>>AR>> 1
##d=1(perlakuan differncing) 

#Estimasi Parameter
#model_1
model1=arima(Data.diff1, order=c(0,1,3))
model1
#model_2
model2=arima(Data.diff1, order=c(0,1,1))
model2
#model_3
model3=arima(Data.diff1, order=c(2,1,3))
model3
#model_4
model4=arima(Data.diff1, order=c(2,1,1))
model4
#model_5
model5=arima(Data.diff1, order=c(0,1,2))
model5
#model_6
model6=arima(Data.diff1, order=c(1,1,3))
model6
printstatarima(model6)

#printstatarima(melihat p-value dari tiap ordo)
printstatarima <- function (x, digits = 4,se=T,...){
  if (length(x$coef) > 0) {
    cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
      ses <- rep(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      statt <- coef[1,]/ses
      pval  <- 0.5*pt(abs(statt), df=length(x$residuals)-1, lower.tail = FALSE)
###### 2 nya bisa diganti 0.5 (untuk satu sisi)
      coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,digits=digits))
      coef <- t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}
printstatarima(model1)
printstatarima(model2)
printstatarima(model3)
printstatarima(model4)
printstatarima(model5)

#pilih nilai Aic paling kecil
#model yang diperoleh untuk uji diagnostik adalah model ARIMA(1,1,1)
#uji diagnostic
#uji normalitas ACF of Residuals
#uji no autokorelasi
#ljung-box diatas garis putus2 artinya gagal tolak H0=tdk mengandung auto
tsdiag(model6)


#forecast
library("forecast") 
pred.Data=predict(model4,n,ahead=4)
pred.Data
fitted(model5)
acfStat(model5)


