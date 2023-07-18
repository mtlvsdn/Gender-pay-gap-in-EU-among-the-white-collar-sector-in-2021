#Hypothesis: The true mean # of gender pay gap in EU is equal to 15
# Null hypothesis u0 = 15
# Alternative hypothesis u1 != 15
library(ggplot2)

u0 <- 15
alpha <- 0.05
n <- 9
sd <- 5.433823
mean <- 13.18889

z <- (mean - u0)/(sd/sqrt(n))
View(z)

#z = -0.9999093 > -1.96 => we do not reject u0
#We do have enough evidence to conclude that 
#there is sufficient evidence that the mean
#number of gender pay gap in EU is equal to 15