# RAL (Rangkaian Acak Lengkap) dengan ulangan berbeda

# Menguji homogenitas mesin tenun, yang diukur adalah kekuatan kain yang dihasilkan oleh keempat mesin


perlakuan = c(rep('M1',4), rep('M2',3),rep('M3',3),rep('M4',2))

respon = c(98,97,99,96,91,90,93,96,95,97,95,96)


percobaan = data.frame(perlakuan, respon)

plot(respon~perlakuan, data = percobaan)


result = aov(respon~perlakuan, data = percobaan)

summary(result)
