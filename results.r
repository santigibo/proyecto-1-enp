source('functions.r')
library(goftest)

m  = 5000
n = 2000
L = 2000

# First part to calculate statistic under H_0 hypothesis

ugestad = u_statistic(m,n,L) # 500 array of independent u_statistics
mean_1 = mean(ugestad)
sd_1 = sd(ugestad)

cvm_test = cvm.test(ugestad, "pnorm", mean=1, sd=0) # Cramér-Von Mises test

per = percentage(ugestad, 1.96) # Percentage of data outside percentile

# Second part to calculate statistic under alternative

ugestad2 = u_statistic2(m,n,L) # 500 array of independent u_statistics
mean_2 = mean(ugestad2)
sd_2 = sd(ugestad2)

per2 = percentage(ugestad2, 1.96) # Percentage of data outside percentile
