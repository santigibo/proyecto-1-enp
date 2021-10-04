source('functions.r')
library(goftest)

m  = 5000
n = 2000
L = 2000

# Primera parte para caclular bajo la hipótesis nula H_0

ugestad = u_statistic(m,n,L) # Vector de los 500 valores independientes
mean_1 = mean(ugestad)
sd_1 = sd(ugestad)

qqnorm(ugestad) # q-q plot comparando con la distribución normal
cvm_test = cvm.test(ugestad, "pnorm", mean=0, sd=1) # Cramér-Von Mises test

per = percentage(ugestad, 1.96) # Porcentaje de datos afuera del percentil 95

# Segunda parte para calcular bajo la hipótesis alternativa

ugestad2 = u_statistic2(m,n,L) # Vector de los 500 valores independientes
mean_2 = mean(ugestad2)
sd_2 = sd(ugestad2)

per2 = percentage(ugestad2, 1.96) # Porcentaje de datos afuera del percentil 95

ugestad3 = u_statistic(m,n,m+n) # El mismo ugestad 1 pero con N iteraciones de montecarlo
qqnorm(ugestad3)
cvm_test2=cvm.test(ugestad3) # Vemos la normalidad de este caso
