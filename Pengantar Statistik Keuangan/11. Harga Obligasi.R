setwd("E:\\Sekolah\\Kuliah\\Semester 6\\Pengantar Statistik Keuangan")

##Membuat Function Harga Obligasi (P) (Untuk m kali)
#f=nilai nominal; r=kupon; i=tingkat yield; t=banyaknya waktu pembayaran hingga jatuh tempo;
#m=banyaknya pembayaran kupon dalam 1 tahun; 
obligasi<-function(f,r,i,m,t){
  r_bintang=r/m
  i_bintang=i/m
  n=t*m
  v=1/(1+i_bintang)
  an=(1-(v^n))/i_bintang
  P=f*r_bintang*an+f*(v^n)
  return(P)
}
