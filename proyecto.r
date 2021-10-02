m=5000
n=2000
L = 2000
ugestad=rep(0,500)
psi=function(x){
  if(x>0){return(1)}
  return(0)
}
h=function(vx,vy){
  return(psi(abs(vy[1]-vy[2])-abs(vx[1]-vx[2])))
}
h10=function(vx,vy){
  a=psi(abs(vy[1]-vy[2])-abs(vx[1]-vx[2]))
  b=psi(abs(vy[3]-vy[4])-abs(vx[1]-vx[3]))
  return(a*b)
}
h01=function(vx,vy){
  a=psi(abs(vy[1]-vy[2])-abs(vx[1]-vx[2]))
  b=psi(abs(vy[1]-vy[3])-abs(vx[3]-vx[4]))
  return(a*b)
}
simu=function(X,Y,M,r,s,f){
  estad=rep(0,M)
  for(j in 1:M){
    ixstar=rep(0,r)
    iystar=rep(0,s)
    while(any(ixstar %in% iystar)){
      ixstar=sample(ix,r,replace=F)
      iystar=sample(iy,s,replace=F)
    }
    vx=X[ixstar]
    vy=Y[iystar]
    estad[j]=f(vx,vy)
  }
  esti=mean(estad)
  desvi=sd(estad)
  return(c(esti,desvi))
}

for(i in 1:500){
  X=rgamma(m,shape=1.5,scale=2)
  Y=rgamma(n,shape=1.5,scale=2)
  ix=1:m
  iy=1:n
  N=m+n
  lambda=m/N
  z01=simu(X,Y,L,4,3,h01)[1]-(1/4)
  z10=simu(X,Y,L,3,4,h10)[1]-(1/4)
  sigma=4*(z10/lambda+z01/(1-lambda))
  u=simu(X,Y,L,2,2,h)[1]
  ugorro=sqrt(N)*(u-(1/2))/sigma
  ugestad[i]=ugorro
  print(i)
}

simu2=function(X,M,r,s,f){
  estad=rep(0,M)
  for(j in 1:M){
    ixstar=rep(0,r)
    iystar=rep(0,s)
    while(any(ixstar %in% iystar)){
      ixstar=sample(ix,r,replace=F)
      iystar=sample(iy,s,replace=F)
    }
    vx=X[ixstar]
    vy=X[iystar]
    estad[j]=f(vx,vy)
  }
  esti=mean(estad)
  desvi=sd(estad)
  return(c(esti,desvi))
}

percentage=functio
n(ugestad, percentile){
  c = 0
  for(i in 1:500){
    if(ugestad[i] > percentile){
      c = c+1
    }
    if(ugestad[i] < percentile*(-1)){
      c = c+1
    }
  }
  return(c/500)
}

percentage(ugestad,1.96)

ugestad2=rep(0,500)
for(i in 1:500){
  X=rgamma(m,shape=1.5,scale=2)
  Y=rgamma(n,shape=1.5,scale=3)
  ix=1:m
  iy=1:n
  N=m+n
  lambda=m/N
  z01=simu2(X,L,4,3,h01)[1]-(1/4)
  z10=simu2(X,L,3,4,h10)[1]-(1/4)
  sigma=4*(z10/lambda+z01/(1-lambda))
  u=simu(X,Y,L,2,2,h)[1]
  ugorro=sqrt(N)*(u-(1/2))/sigma
  ugestad2[i]=ugorro
  print(i)
}