####### SARIMA #######
#terdapat evek musiman ada bebeapa bulan yang lebih tinggi dibanding bulan lainnya dan berulang setiap tahunnnya
turis=read.csv('E:\\ARW\\spain.txt')
turis=ts(turis,start=c(1970,1),frequency=12)
turis
ts.plot(turis, col='blue', main="TS: Turis Spanyol", lwd=2)
###sudah dapat diketahui data bukan stasioner

library(tseries)
library(forecast)

### Uji stasioneritas data ###
adf.test(turis) #H0 : data mengandung unit root (tidak stationer)##lebih kepada trendnya
kpss.test(turis, null="Level") #H0 : data tidak mengandung unit root (stasioner)

### Melihat adanya musiman (selain dari plot) ###
par(mfrow=c(1,2)) 
Acf(turis, lag.max = 36)
Pacf(turis, lag.max = 36) #kalau mengandung musiman maka, ACF dan PACFnya bergelombang
##data mengandung musiman grafik bergelobang
### Jika data mengandung musiman maka, dilakukan differencing musiman ###
turis.dslog = diff(log(turis), differences=1, lag=12) ##differencing awal->>menghilangkan musiman
##dilog kan karena datanya besar-->>sampe jutaan ,, pnjang orde 12
par(mfrow=c(1,1))
ts.plot(turis.dslog, col="red", main="TS: Turis dslog")

### Uji stationeritas data setelah differencing musiman ###
adf.test(turis.dslog)

### Melihat efek musiman setelah di-differencing ###
par(mfrow=c(1,2))
Acf(turis.dslog, lag.max=37)
Pacf(turis.dslog, lag.max=37)

### Data belum stasioner maka, dilakukan differensing lagi yang non-musiman ###
turis.ddslog =  diff(turis.dslog, differences=1)

### Uji stationeritas data differencing musiman dan non-musiman ###
adf.test(turis.ddslog)

### Melakukan estimasi model dengan melihat plot ACF dan PACF ###
Acf(turis.ddslog, lag.max=37) ### 
Pacf(turis.ddslog, lag.max=37)

### Fitting Model ###
###musiman P D Q
model1 = Arima(log(turis), order = c(0,1,1), seasonal = list(order = c(2,1,3), period = 12), include.mean = FALSE)
summary(model1) ###hasil ada NaN maka model yg dibentuk salah

model2 = Arima(log(turis), order = c(0,1,1), seasonal = list(order = c(2,1,1), period = 12), include.mean = FALSE)
summary(model2)

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
      pval  <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = FALSE)
      coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,digits=digits))
      coef <- t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}

printstatarima(model2) #Model2 tidak sig. pada sar 1 dan sar 2 maka buat model lagi tanpa sar 1 dan sar 2

model2.1 = Arima(log(turis), order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE)
summary(model2.1)
printstatarima(model2.1)

model3 = Arima(log(turis), order = c(0,1,1), seasonal = list(order = c(0,1,2), period = 12), include.mean = FALSE)
summary(model3)
printstatarima(model3)

model4 = Arima(log(turis), order = c(0,1,1), seasonal = list(order = c(0,1,3), period = 12), include.mean = FALSE)
summary(model4)
printstatarima(model4)

### Uji diagnostik ###
tsdiag(model2.1)
jarque.bera.test(model2.1$residuals)##normalitas (engga dipake engga papa)

### Peramalan ###
turis.pred=predict(model2.1, n.ahead = 5)
turis.pred

### Membalikkan ke data awal ###
turis.pred2 = exp(turis.pred$pred)
turis.pred2

### Nilai prediksi ###
turis.fitted2 = exp(fitted(model2.1))
turis.fitted2

### Grafik data asli dan data prediksi ##
ts.plot(turis, col="blue")
lines(turis.fitted2, col="red")