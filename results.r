source('functions.r')
library(goftest)

m  = 5000
n = 2000
L = 2000

# Primera parte para caclular bajo la hipótesis nula H_0

ugestad = u_statistic(m,n,L) # Vector de los 500 valores independientes
mean_1 = mean(ugestad)
sd_1 = sd(ugestad)

cvm_test = cvm.test(ugestad, "pnorm", mean=0, sd=1) # Cramér-Von Mises test

per = percentage(ugestad, 1.96) # Porcentaje de datos afuera del percentil 95

# Calculo bajo la hipótesis nula pero con 7000 iteraciones 

ugestad_7000 = u_statistic(m,n,7000) # Vector de los 500 valores independientes
mean_7000 = mean(ugestad_7000)
sd__7000 = sd(ugestad_7000)

cvm_test_7000 = cvm.test(ugestad, "pnorm", mean=0, sd=1) # Cramér-Von Mises test

per_7000 = percentage(ugestad_7000, 1.96) # Porcentaje de datos afuera del percentil 95

# Segunda parte para calcular bajo la hipótesis alternativa

ugestad2 = u_statistic2(m,n,L) # Vector de los 500 valores independientes
mean_2 = mean(ugestad2)
sd_2 = sd(ugestad2)

per2 = percentage(ugestad2, 1.96) # Porcentaje de datos afuera del percentil 95

Titles <- c("", "Hipótesis nula", "", "Hipótesis alternativa")
Iterations <- c("Iteraciones", 2000, 7000, 2000)
Means <- c("Media", mean_1, mean_7000,mean_2)
SD <- c("Desviación Estándar", sd_1, sd__7000, sd_2)
CVM <- c("Omega CVM test", cvm_test[["statistic"]][["omega2"]], cvm_test_7000[["statistic"]][["omega2"]], "-")
Percentage <- c("Porcentaje fuera de 5% y 95%", per, 0, per2)

df = data.frame(Titles, Iterations, Means, SD, CVM, Percentage)

write.csv(df, "Results.csv")

