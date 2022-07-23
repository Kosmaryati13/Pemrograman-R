setwd("E:\\Sekolah\\Kuliah\\Semester 6\\Pengantar Statistik Keuangan")
#t=banyaknya kupon; f=nilai nominal; r=kupon; i=tingkat yield
obligasi<-function(f,i,r,ttm,m){
  t=ceiling(ttm*m+1)
  k=1-(ttm*m-floor(ttm*m))
  rbin=r/m
  ibin=i/m
  v=1/(1+ibin)
  an=(1-v^t)/ibin
  Bt=f*rbin*an+f*v^t
  
  Bf_1=Bt*(1+ibin)^k
  frk_1=f*rbin*((1+ibin)^k-1)/ibin
  Bm_1=Bf_1-frk_1
  cat("Obligasi Theoritical Method",Bm_1,"\n")
  
  Bf_2=Bt*(1+k*ibin)
  frk_2=k*f*rbin
  Bm_2=Bf_2-frk_2
  cat("Obligasi Partical Method",Bm_2,"\n")
  
  Bf_3=Bt*(1+ibin)^k
  frk_3=k*f*rbin
  Bm_3=Bf_3-frk_3
  cat("Obligasi Semi-Theoritical Method",Bm_3,"\n")
}
