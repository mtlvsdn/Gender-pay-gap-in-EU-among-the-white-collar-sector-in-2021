#Hypothesis: The true mean # of education in EU is lower than 9
# Null hypothesis u0 < 9
# Alternative hypothesis u1 >= 9
library(ggplot2)

u0 <- 9
alpha <- 0.05
n <- 5
sd <- 3.941181
mean <- 11.79048

z <- (mean - u0)/(sd/sqrt(n))
View(z)

#z = 1.583206 < 1.96 => we do not reject u0
#We do have enough evidence to conclude that 
#the true mean # of education in EU is lower than 9