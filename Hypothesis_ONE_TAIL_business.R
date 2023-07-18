#Hypothesis: The true mean # of business in EU is greater than 15
# Null hypothesis u0 > 15
# Alternative hypothesis u1 < 15
library(ggplot2)

u0 <- 15
alpha <- 0.05
n <- 5
sd <- 3.856129
mean <- 14.79048

z <- (mean - u0)/(sd/sqrt(n))
View(z)

#z = -0.1214951 > -1.96 => we do not reject u0
#We do have enough evidence to conclude that 
#the true mean # of business in EU is greater than 15
