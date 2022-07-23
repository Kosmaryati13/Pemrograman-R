nilai=read.table(file.choose(),header=TRUE)
nilai
logit1 <- glm(Nilai~IPK+M1,data=nilai, family=binomial(link="logit"))
summary(logit1)

logit2 <- glm(Nilai~IPK,data=nilai, family=binomial(link="logit"))
summary(logit2)

#Odds ratio
thetahat <- logit2$fitted.values
odds_ratio <- logit2$fitted.values/(1-logit2$fitted.values)
cbind(nilai$Nilai,Fitted=round(thetahat,3),Odds=round(odds_ratio,3))

#Goodness of fit
logit20 <- glm(Nilai~1,data=nilai, family=binomial(link="logit")) #constanta model
pseudo=1-as.vector(logLik(logit2)/logLik(logit20)) # pseudo R2
pseudo

#predicted table korkondasi 
table(true=nilai$Nilai, pred=round(fitted(logit2)))


