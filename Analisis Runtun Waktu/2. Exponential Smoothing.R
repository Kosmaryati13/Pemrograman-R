#Simple Exponential Smoothing

data=scan()
series=ts(data, start=c(2015,1), frequency=12)

SES=HoltWinters(series, alpha=0.1, beta=FALSE, gamma=FALSE)
SES$fitted  
SES$SSE
forecast=predict(SES, n.ahead=1)

plot(series, main="Time Series Plot", type="o", col="blue", pch=15, lty=1)
lines(SES$fitted[,2], type="o", col="red", pch=16, lty=2)
legend("topleft", c("Data Aktual","Prediksi SES"), bty="n", cex=0.8, col=c("blue","red"), pch=15:16, lty=1:2)

#Double Exponential Smoothing

data=scan()
series=ts(data, start=c(1995,1), frequency=12)

DES=HoltWinters(series, alpha=0.1, beta=0.1, gamma=FALSE)
DES$fitted  
DES$SSE
forecast=predict(DES, n.ahead=6)

plot(series, main="Time Series Plot", type="o", col="blue", pch=15, lty=1)
lines(DES$fitted[,2], type="o", col="red", pch=16, lty=2)
legend("topleft", c("Data Aktual","Prediksi DES"), bty="n", cex=0.8, col=c("blue","red"), pch=15:16, lty=1:2)

#Triple Exponential Smoothing

data=scan()
series=ts(data, start=c(2015,1), frequency=4)

TES=HoltWinters(series, alpha=0.1, beta=0.1, gamma=0.2, seasonal="additive")
TES$fitted  
TES$SSE
forecast=predict(TES, n.ahead=4)

plot(series, main="Time Series Plot", type="o", col="blue", pch=15, lty=1)
lines(TES$fitted[,2], type="o", col="red", pch=16, lty=2)
legend("topleft", c("Data Aktual","Prediksi TES"), bty="n", cex=0.8, col=c("blue","red"), pch=15:16, lty=1:2)

