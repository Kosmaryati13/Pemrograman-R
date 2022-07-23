Panel=read.csv(file.choose(),header=TRUE)
Panel
head(Panel) #memanggil 6 data pertama

##Uji Hausman (menentukan model tetap atau acak)
library(plm)
#independen+konstanta (jumlah variabel independen)
##Model1
gf=plm(Bantuan~PAD+SDO+YCAP+POP+BHPBP, data=Panel,model="within")#model tetap
gr=plm(Bantuan~PAD+SDO+YCAP+POP+BHPBP, data=Panel,model="random")#model random
phtest(gf,gr)

##Model 2
gf=plm(Bantuan~PAD+SDO+YCAP+POP, data=Panel,model="within")
gr=plm(Bantuan~PAD+SDO+YCAP+POP, data=Panel,model="random")
phtest(gf,gr)

##Model 3
gf=plm(Bantuan~PAD+SDO+YCAP, data=Panel,model="within")
gr=plm(Bantuan~PAD+SDO+YCAP, data=Panel,model="random")
phtest(gf,gr)

##Model 3.1
gf=plm(Bantuan~PAD+SDO+POP, data=Panel,model="within")
gr=plm(Bantuan~PAD+SDO+POP, data=Panel,model="random")
phtest(gf,gr)

##Model 4
gf=plm(Bantuan~PAD+SDO, data=Panel,model="within")
gr=plm(Bantuan~PAD+SDO, data=Panel,model="random")
phtest(gf,gr)

#UJI BREUSCH PAGAN
##Model1
g=plm(Bantuan~PAD+SDO+YCAP+POP+BHPBP, data=Panel,model="pooling")
plmtest(g,effect="twoways",type="bp") ##2 arah
plmtest(g,effect="individual",type="bp") ##cross section
plmtest(g,effect="time",type="bp")

##Model2
g=plm(Bantuan~PAD+SDO+YCAP+POP, data=Panel,model="pooling")
plmtest(g,effect="twoways",type="bp") 
plmtest(g,effect="individual",type="bp") ##cross section ##breusch pagan
plmtest(g,effect="time",type="bp")

##Model3
g=plm(Bantuan~PAD+SDO+POP, data=Panel,model="pooling")
plmtest(g,effect="twoways",type="bp") 
plmtest(g,effect="individual",type="bp") ##cross section ##breusch pagan
plmtest(g,effect="time",type="bp")

##Model4
g=plm(Bantuan~PAD+SDO, data=Panel,model="pooling")
plmtest(g,effect="twoways",type="bp") 
plmtest(g,effect="individual",type="bp") ##cross section ##breusch pagan
plmtest(g,effect="time",type="bp")


#ESTIMASI MODEL
##Model1
g=plm(Bantuan~PAD+SDO+YCAP+POP+BHPBP, data=Panel,model="within",effect="time")
summary(g)

##Model1.1
g1=plm(Bantuan~SDO+YCAP+POP+BHPBP, data=Panel,model="within",effect="time")
summary(g1)

##Model1.2
g2=plm(Bantuan~SDO+POP+BHPBP, data=Panel,model="within",effect="time")
summary(g2)

##Model1.3
g3=plm(Bantuan~SDO+POP, data=Panel,model="within",effect="time")
summary(g3)
fixef(g3,type="level")

#ESTIMASI MODEL
##Model2 
g=plm(Bantuan~PAD+SDO+YCAP+POP, data=Panel,model="pooling")
summary(g)
##Model2.1
g1=plm(Bantuan~PAD+SDO+POP, data=Panel,model="pooling")
summary(g1)

##Model3
g=plm(Bantuan~PAD+SDO+POP, data=Panel,model="pooling")
summary(g)

##Model4
g=plm(Bantuan~PAD+SDO, data=Panel,model="within", effect="individual")
summary(g)
##Model4.1
g1=plm(Bantuan~SDO, data=Panel,model="within", effect="individual")
summary(g1)
fixef(g1,type="level")

##Uji Diagnostik
#uji korelasi serial
##Model1.2
g2=plm(Bantuan~SDO+POP+BHPBP, data=Panel,model="within",effect="time")
pbgtest(g2)
##Model2.1
g1=plm(Bantuan~PAD+SDO+POP, data=Panel,model="pooling")
pbgtest(g1)
##Model3
g=plm(Bantuan~PAD+SDO+POP, data=Panel,model="pooling")
pbgtest(g)
#model4
g4.1=plm(Bantuan~SDO, data=Panel,model="within", effect="individual")
summary(g4.1)
fixef(g1,type="level")

## estimasi yg bebas robust thdp heteros (heteros)
library(lmtest)
##Model2.1
g1=plm(Bantuan~PAD+SDO+POP, data=Panel,model="pooling")
coeftest(g1,vcovHC)
summary (g1)
#nilai estimasi coeftest dan biasa sama maka model tersebut baik


