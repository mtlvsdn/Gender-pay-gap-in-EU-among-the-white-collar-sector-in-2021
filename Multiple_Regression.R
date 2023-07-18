library(tidyverse)
library(ggpubr)

GapPoverty <- read.csv("E:\\ASE\\Y1\\S2\\{ Statistica }\\Proiect_R\\Multiple_Regression.csv")

model2 <- lm(Average.gap ~ prate + Urban_population, data = GapPoverty)
model2
summary(model2)

#confint(model2)
#sigma(model2)*100/mean(GapPoverty$Average.gap)