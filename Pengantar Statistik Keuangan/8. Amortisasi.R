amortisasi<-function(PV=NA,i,m,t,k=FALSE)
{
  j=i/m;
  n=t*m;
  v=1/(1+j);
  if(nargs()==5&is.na(PV)==TRUE){
    PV=k*(1-v^n)/j;
  }
  k=PV/((1-v^n)/j);
  b=PV;
  output=array(dim=c(n,5))
  output[1,1]=1
  output[1,2]=k
  output[1,3]=j*b
  output[1,4]=k-output[1,3]
  output[1,5]=b-output[1,4]
  for(i in 2:n){  #dari baris ke 2 karena baris ke 1 udh diisi
    output[i,1]=i
    output[i,2]=round(k,digits = 3)
    output[i,3]=round(j*output[i-1,5],digits = 3)
    output[i,4]=round(k-output[i,3],digits = 3)
    output[i,5]=round(output[i-1,5]-output[i,4],digits = 3)
  }
  periode=c(output[,1])
  payment=c(output[,2])
  interest=c(output[,3])
  principal=c(output[,4])
  balance=c(output[,5])
  SCHEDULE=data.frame(periode,payment,interest,principal,balance)
  
  SCHEDULE
  }

jadwal=amortisasi(20000,0.189,1,12)
jadwal
