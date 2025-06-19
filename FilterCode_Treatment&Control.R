###COMMENTS EXPLAINING THE PURPOSE OF THE R SCRIPT

##This code was written in order to aggregate all the data contained in the newly created 'filtered_crime_data' CSV file into 
##one filtered CSV file that contains only our treatment groups, BOCA and NUÑEZ, as well as our control groups, CABALLITO", ALMAGRO, 
##and VILLA URQUIZA. 
##This new CSV file named 'treatment_and_control_crime_data.csv' filtered the roughly 86000  observations from the CSV file named
##'filtered_crime_data_superclasico.csv' down to 12000 observations. This made the analysis much easier to carry out.

# Load necessary libraries
library(dplyr)
library(readr)

# Load the filtered crime data
filtered_crime_data <- read_csv("/Users/octavius/Desktop/ECON 114 R CODE/FINAL PROJECT IDEA 1/filtered_crime_data_superclasico.csv")

# Define treatment and control barrios
treatment_barrios <- c("BOCA", "NUÑEZ")
control_barrios <- c("CABALLITO", "ALMAGRO", "VILLA URQUIZA")

# Filter the dataset for treatment and control groups
treatment_and_control_data <- filtered_crime_data %>%
  filter(toupper(barrio) %in% c(treatment_barrios, control_barrios))

# Save the filtered data to a new CSV file
write_csv(treatment_and_control_data, "treatment_and_control_crime_data.csv")
