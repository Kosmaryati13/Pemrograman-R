setwd("D:\\KULIAH\\2017-2018\\Semester 1\\3_Ekonometrika\\Pemrograman_R\\heteroskedastik")
data1 <- read.table("data_heteroskedastik.txt",header=TRUE)

lm1 <- lm(MPG ~ SP + HP + WT, data=data1)
summary(lm1)

#breusch Pagan test
library(lmtest)
bptest(lm1, studentize=FALSE, data=data1)

#coba WT sebagai weight (Mengatasi Heteros)
lm2 <- lm(MPG ~ SP + HP + WT, data=data1,weights=1/WT)
summary(lm2)
bptest(lm2, studentize=FALSE, data=data1)

