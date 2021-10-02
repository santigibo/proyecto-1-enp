# Función característica para números positivos
psi=function(x){
  if(x>0){return(1)}
  return(0)
}

# Kernel
h=function(vx,vy){
  return(psi(abs(vy[1]-vy[2])-abs(vx[1]-vx[2])))
}

# Cálculo de kernel dejando un X quieto 
h10=function(vx,vy){
  a=psi(abs(vy[1]-vy[2])-abs(vx[1]-vx[2]))
  b=psi(abs(vy[3]-vy[4])-abs(vx[1]-vx[3]))
  return(a*b)
}

# Cálculo de kernel dejando un Y quieto
h01=function(vx,vy){
  a=psi(abs(vy[1]-vy[2])-abs(vx[1]-vx[2]))
  b=psi(abs(vy[1]-vy[3])-abs(vx[3]-vx[4]))
  return(a*b)
}

# Simulación para obtener las 2000 iteraciones del cálculo 
simu=function(X,Y,M,r,s,f){
  estad=rep(0,M)
  for(j in 1:M){
    ixstar=rep(0,r)
    iystar=rep(0,s)
    # Verificar que las muestras que se sacan no tienen números iguales
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
  return(c(esti,desvi)) # Vector con la media y desviación estándar de las M iteraciones
}

# Porcentaje de datos en el ugestad que están por fuera del percentil dado
percentage=function(ugestad, percentile){
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

# Simulación para la parte 2, tomando solo la muestra X
simu2=function(X,M,r,s,f){
  estad=rep(0,M)
  for(j in 1:M){
    ixstar=rep(0,r)
    iystar=rep(0,s)
    # Verificar que las muestras que se sacan no tienen números iguales
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
  return(c(esti,desvi)) # Vector con la media y desviación estándar de las M iteraciones
}

# U estadístico para la parte 1, con las dos muestras y los mismos parámetros de distribución
u_statistic=function(m, n, L){
    ugestad = rep(0,500)
    for(i in 1:500){
        X=rgamma(m,shape=1.5,scale=2) # Muestra X
        Y=rgamma(n,shape=1.5,scale=2) # Muestra Y
        ix=1:m
        iy=1:n
        N=m+n
        lambda=m/N
        z01=simu(X,Y,L,4,3,h01)[1]-(1/4) # Cálculo ecuación (2)
        z10=simu(X,Y,L,3,4,h10)[1]-(1/4) # Cálculo ecuación (2)
        sigma=4*(z10/lambda+z01/(1-lambda))
        u=simu(X,Y,L,2,2,h)[1] # Media del resultado de la simulación
        ugorro=sqrt(N)*(u-(1/2))/sigma # Cálculo de U gorro con la fórmula dada
        ugestad[i]=ugorro
    }
    return(ugestad)
}

u_statistic2=function(m, n, L){
    ugestad = rep(0,500)
    for(i in 1:500){
        X=rgamma(m,shape=1.5,scale=2) # Muestra X
        Y=rgamma(n,shape=1.5,scale=3) # Muestra Y
        ix=1:m
        iy=1:n
        N=m+n
        lambda=m/N
        z01=simu2(X,L,4,3,h01)[1]-(1/4) # Cálculo ecuación (2) con solo muestra X
        z10=simu2(X,L,3,4,h10)[1]-(1/4) # Cálculo ecuación (2) con solo muestra X
        sigma=4*(z10/lambda+z01/(1-lambda))
        u=simu(X,Y,L,2,2,h)[1] # Media del resultado de la simulación 2
        ugorro=sqrt(N)*(u-(1/2))/sigma # Cálculo de U gorro con la fórmula dada
        ugestad[i]=ugorro
    }
    return(ugestad)
}