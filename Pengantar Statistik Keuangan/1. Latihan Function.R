##13 maret 2018 (Pengantar statistik keuangan)
#membuat directory file
setwd("E:\\Materi Kuliah\\Script R\\Pengantar Statistik Keuangan")
luassegitiga <- function(a, t){
  luas = 0.5*a*t
  return(luas)
}
LuasSegitiga=luassegitiga(4,8) ## bisa juga langsung ->> luassegitiga(4,8)
LuasSegitiga
luassegitiga(4,8)

#c dan b opstional
perkalian <- function(a, b, c = TRUE, d = TRUE){
  kali = a*b*c/d
  return(kali)
}

perkalian(4, 3, d = 2)


#Looping dalam R
kalimat <- "Cerdas bermedos dengan menangkal hoax"
num <- 3

while (num <= 5){
  print(kalimat)
  num = num + 1
}

for (i in 1:4){
  print("Alay boleh, asal taat aturan")
}

###if (boolean)
a <- 22.2

if (is.numeric(a)){
  cat("Variabel a adalah suatu angka:", a)
} ###cat=mirip print namaun dapat menggabungkan kaliamat dengan perulangan nilai a nya

##contoh yang lain
a <- "Hello"

if (is.numeric(a)){
  cat("Variabel a adalah suatu angka:", a)
} ###cat=mirip print namaun dapat menggabungkan kaliamat dengan perulangan nilai a nya
###tidak ada keluaran karena FALSE langsung berhenti

##if...else
a <- "Nom...nom"

if (is.numeric(a)){
  cat("Variabel a adalah suatu angka:", a)
} else {
  cat("Variabel a bukan angka:", a)
}

#if...else bertingkat
a <- 7

if (a>10){
  print("Statistics ENTHUSIASTICS")
} else if (a>0 & a<= 10) {
  print("Data analis yang antusias dan berintegritas")
} else {
  print("Lima konsentrasi")
}
##a>10
##0<a<=10
##a<=0

#switch
pilih <- switch(3, "Bahasa R", "Bahasa Python", "Bahasa C")
print(pilih)

#mengandung switch
pilih <- function(num, a, b)
  switch(num, 
         satu = {
           kali = a*b
           print(kali)
         },
         dua = {
           bagi = a/b
           print(bagi)
         }
  )

pilih("satu", 2, 5)
