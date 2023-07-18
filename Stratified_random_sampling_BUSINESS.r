#1.	Stratified random sampling(confidence intervals for population mean and total 
#taking into account different scenarios for the margin of error or significance level);
library(readxl)
library(ggplot2)
data <- read_excel("C:/MATEI/ASE/SEMESTRUL II/Statistica/PROIECT/DATABASE/1.Stratified random sampling/Dataset_VAR_FINALA.xlsx")
View(data)

population_total <- 27 #TOTAL POPULATION
population_west <- 14 #STRATA POPULATION WEST
population_east <- 13 #STRATA POPULATION EAST
strata_west <- data[-c(3,4,5,6,8,12,13,16,17,21,23,24,25), ]
strata_east <- data[-c(1,2,7,9,10,11,14,15,18,19,20,22,26,27), ]
var_sm <- 5/1.96 #VARIANCE OF THE SAMPLE MEAN

#URBAN POPULATION
urban <- data["Business"] #DATA URBAN
col_urban_west <- sample_west["Business"] #DATA WEST
col_urban_east <- sample_east["Business"] #DATA EAST

var_urban <- var(urban, na.rm = TRUE)

var_urban_west <- var(col_urban_west, na.rm = TRUE) #VARIANCE WESTERN COUNTRIES
var_urban_east <- var(col_urban_east, na.rm = TRUE) #VARIANCE EASTERN COUNTRIES
sd_urban_west <- sqrt(var_urban_west) #STANDARD DEVIATION WESTERN COUNTRIES
sd_urban_east <- sqrt(var_urban_east) #STANDARD DEVIATION EASTERN COUNTRIES

ss <- (population_total * var_urban)/((population_total - 1) * var_sm + var_urban) #SAMPLE SIZE TOTAL
ss_west <- (population_west * var_urban_west)/(population_west * var_urban_west + population_east * var_urban_east) * ss #SAMPLE SIZE WEST
ss_east <- (population_east * var_urban_east)/(population_east * var_urban_east + population_west * var_urban_west) * ss #SAMPLE SIZE EAST

View(ss) #!!!!!!!!!!!!!!
View(ss_west) #!!!!!!!!!!!!!!
View(ss_east) #!!!!!!!!!!!!!!

sample_west <- strata_west[-c(1,2,3,4,6,7,9,10,11,13,14), ] #WE CHOOSE A SAMPLE FOR WEST STRATA RANDOMLY
sample_east <- strata_east[-c(1,2,4,5,6,7,8,9,10,11,12), ] #WE CHOOSE A SAMPLE FOR EAST STRATA RANDOMLY
sample_west_urban <- sample_west["Business"]
sample_east_urban <- sample_east["Business"]

View(sample_west_urban)
View(sample_east_urban)

me_west <- 1.96 * (sd_urban_west/sqrt(population_west)) #MARGIN OF ERROR WEST
me_east <- 1.96 * (sd_urban_east/sqrt(population_east)) #MARGIN OF ERROR EAST

sample_west_urban <- as.numeric(sample_west_urban[!is.na(sample_west_urban)])
mean_west <- mean(sample_west_urban, na.rm = FALSE) #MEAN WEST
sample_east_urban <- as.numeric(sample_east_urban[!is.na(sample_east_urban)])
mean_east <- mean(sample_east_urban, na.rm = TRUE) #MEAN EAST

lower_limit_west <- mean_west - me_west #LOWER LIMIT WEST
upper_limit_west <- mean_west + me_west #UPPER LIMIT WEST
lower_limit_east <- mean_east - me_east #LOWER LIMIT EAST
upper_limit_east <- mean_east + me_east #UPPER LIMIT EAST

View(lower_limit_west)
View(upper_limit_west)
View(lower_limit_east)
View(upper_limit_east)
View(mean_east)
View(mean_west)
View(me_west)
View(me_east)
View(sample_west_urban)
View(sample_east_urban)

# DISTRIBUTION OF SAMPLE WEST
hist(sample_west_urban, breaks = 3, main = "Distribution of Pay Gap in the \nBusiness sector\n in Western EU in 2021", xlab = "Urbanization Rate", col = 'lightblue')
View(sample_west_urban)
# DISTRIBUTION OF SAMPLE EAST
View(sample_east_urban)
hist(sample_east_urban, breaks = 2, main = "Distribution of Pay Gap in the \nBusiness sector\n in Eastern EU in 2021", xlab = "Urbanization Rate", col = 'orange')

results <- data.frame(
  Mean = c(mean_west, mean_east),
  Margin_Of_Error = c(me_west, me_east),
  Lower_Limit = c(lower_limit_west, lower_limit_east),
  Upper_Limit = c(upper_limit_west, upper_limit_east),
  row.names = c("Western Countries", "Eastern Countries")
)
View(results)

library("plotrix")

# CONFIDENCE INTERVAL PLOT WEST AND EAST
mean_values <- c(mean_west, mean_east)
standard_errors <- c(me_west, me_east)
group_names <- c("Western Countries", "Eastern Countries")

plotCI(
  x = 1:2,               # X-axis position of the point
  y = mean_values,      # Mean value
  uiw = standard_errors, # Width of the confidence interval (twice the standard error)
  main = "Confidence Intervals for Pay Gap \nin the Business sector\n in Western and Eastern EU in 2021"
)

plotCI(
  x = 1:2,                   # X-axis positions of the points
  y = mean_values,           # Mean values
  uiw = 2 * standard_errors, # Widths of the confidence intervals (twice the standard errors)
  ylim = c(0, max(mean_values + 2 * standard_errors)),  # Set the y-axis limits
  xlim = c(0.5, 3.5),        # Set the x-axis limits
  xaxt = "n",                # Remove the x-axis labels
  xlab = "Group",            # X-axis label
  ylab = "Mean",             # Y-axis label
  pch = 19,                  # Use filled circles as data points
  col = "blue",              # Use blue color for the data points and confidence intervals
  ci.label = TRUE,           # Display the confidence interval labels
  ci.label.pos = "out",      # Position the confidence interval labels outside the plot
  ci.label.size = 0.8,       # Set the font size of the confidence interval labels
  ci.label.font = 2,         # Set the font style of the confidence interval labels (2 for bold)
  ci.label.fontface = "italic", # Set the font face of the confidence interval labels (italic)
  main = "Confidence Intervals for Pay Gap \nin the Business sector\n in Western and Eastern EU in 2021",  # Main plot title
  names = group_names        # Group names for x-axis labels
)