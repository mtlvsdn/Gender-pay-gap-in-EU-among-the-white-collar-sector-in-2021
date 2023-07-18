#1.	Stratified random sampling(confidence intervals for population mean and total 
#taking into account different scenarios for the margin of error or significance level);
library(readxl)
library(ggplot2)
data <- read_excel("C:/MATEI/ASE/SEMESTRUL II/Statistica/PROIECT/DATABASE/1.Stratified random sampling/Dataset_VAR_FINALA.xlsx")
View(data)

population_total <- 27 #TOTAL POPULATION
population_west <- 14 #STRATA POPULATION WEST
population_east <- 13 #STRATA POPULATION EAST
var_sm <- 5/1.96 #VARIANCE OF THE SAMPLE MEAN

#URBAN POPULATION
#urban_ss = data[-c(12,15,26,18,16), ]
urban_ss <- data["Education"] #DATA URBAN
urban = urban_ss[-c(12,15,26,18,16), ]
#urban <- data["Education)"]
var_urban <- var(urban, na.rm = TRUE)
sd_urban <- sqrt(var_urban) #STANDARD DEVIATION WESTERN COUNTRIES
ss <- (population_total * var_urban)/((population_total - 1) * var_sm + var_urban) #SAMPLE SIZE TOTAL
urban <- as.numeric(urban[!is.na(urban)])
mean1 <- mean(urban, na.rm = FALSE) #MEAN WEST

View(urban)
View(var_urban)
View(sd_urban)
View(ss)
View(mean1)
