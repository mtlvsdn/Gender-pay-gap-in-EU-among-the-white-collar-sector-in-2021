library(tidyverse)
library(ggpubr)

GapPoverty <- read.csv("E:\\ASE\\Y1\\S2\\{ Statistica }\\Proiect_R\\Simple_Regression.csv")

model <- lm(Average.gap ~ prate, data = GapPoverty)
model
summary(model)

#confint(model)
#sigma(model)*100/mean(GapPoverty$Average.gap)