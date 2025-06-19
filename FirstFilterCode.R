###COMMENTS EXPLAINING THE PURPOSE OF THE R SCRIPT

##This code was written in order to aggregate all the data contained in the 2016-2022 'delitos' CSV files into one filtered CSV file. 
##This new CSV file named 'filtered_crime_data_superclasico.csv' filtered roughly 1 millions observations down to 87000 observations.
##These 87000 observations included all publicly available data for crime in Buenos Aires with a 1 week before and after the Superclásico buffer period. 

# Load necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readr")) install.packages("readr")

library(dplyr)
library(lubridate)
library(readr)

# Define file paths for crime data CSVs (adjust paths as necessary)
file_paths <- c("/Users/octavius/Desktop/ECON 114 R CODE/FINAL PROJECT IDEA 1/DELITOS FILES/delitos_2016.csv", "/Users/octavius/Desktop/ECON 114 R CODE/FINAL PROJECT IDEA 1/DELITOS FILES/delitos_2017.csv", 
                "/Users/octavius/Desktop/ECON 114 R CODE/FINAL PROJECT IDEA 1/DELITOS FILES/delitos_2018.csv", "/Users/octavius/Desktop/ECON 114 R CODE/FINAL PROJECT IDEA 1/DELITOS FILES/delitos_2019.csv", 
                "/Users/octavius/Desktop/ECON 114 R CODE/FINAL PROJECT IDEA 1/DELITOS FILES/delitos_2020.csv", "/Users/octavius/Desktop/ECON 114 R CODE/FINAL PROJECT IDEA 1/DELITOS FILES/delitos_2021.csv", 
                "/Users/octavius/Desktop/ECON 114 R CODE/FINAL PROJECT IDEA 1/DELITOS FILES/delitos_2022.csv")

# Function to read each file, converting 'franja' to character
preprocess_file <- function(file_path) {
  df <- read_csv(file_path, col_types = cols(
    franja = col_character()  # Specify 'franja' as character
  ))
  return(df)
}

# Load, preprocess, and combine the datasets
crime_data <- lapply(file_paths, preprocess_file) %>% bind_rows()

# Define 'Superclásico' match dates
superclasico_dates <- as.Date(c("2016-03-06", "2016-04-24", "2016-12-11",
                                "2017-05-14", "2017-11-05", "2018-09-23", "2018-11-11",
                                "2019-09-01", "2019-10-01", "2019-10-22", "2021-01-02",
                                "2021-03-14", "2021-05-16", "2022-03-20", "2022-09-11"))

# Filter the data
filtered_crime_data <- crime_data %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(sapply(fecha, function(date) {
    any(sapply(superclasico_dates, function(match_date) {
      date >= (match_date - 7) && date <= (match_date + 7)
    }))
  }))

# Save the filtered dataset
write_csv(filtered_crime_data, "filtered_crime_data_superclasico.csv")

