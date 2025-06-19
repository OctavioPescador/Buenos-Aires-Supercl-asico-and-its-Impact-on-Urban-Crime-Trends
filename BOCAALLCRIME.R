# Loading libraries
library(dplyr)
library(fastDummies)
library(lmtest) 
library(sandwich) 

# Load the dataset
crime_dataDD <- read.csv("/Users/octavius/Desktop/ECON 114 R CODE/RESEARCH PAPER/AGGREGATED CSV FILES/filtered_crime_data_superclasico.csv")

# Convert 'fecha' to Date format
crime_dataDD$fecha <- as.Date(crime_dataDD$fecha, format="%Y-%m-%d")

# Define 'Superclásico' match dates
match_dates <- as.Date(c("2016-04-24", 
                         "2017-05-14", "2018-09-23", 
                         "2018-11-11", 
                         "2019-10-22", "2021-01-02", "2021-03-14", 
                         "2021-05-16", 
                         "2022-09-11"), format="%Y-%m-%d")

# Defining the treatment/control based on 'barrio'
crime_dataDD$group <- ifelse(crime_dataDD$barrio %in% c("BOCA"), "treatment",
                             ifelse(crime_dataDD$barrio %in% c("AGRONOMIA", "ALMAGRO", "BALVANERA", "BARRACAS", "BELGRANO", "BOEDO", 
                                                               "CABALLITO", "CHACARITA", "COGHLAN", "COLEGIALES", "CONSTITUCION", 
                                                               "FLORES", "FLORESTA", "LINIERS", "MATADEROS", "MONSERRAT", 
                                                               "MONTE CASTRO", "NUEVA POMPEYA", "PALERMO", "PARQUE AVELLANEDA", 
                                                               "PARQUE CHACABUCO", "PARQUE CHAS", "PARQUE PATRICIOS", "PATERNAL", 
                                                               "PUERTO MADERO", "RECOLETA", "RETIRO", "SAAVEDRA", "SAN CRISTOBAL", 
                                                               "SAN NICOLAS", "SAN TELMO", "VELEZ SARSFIELD", "VERSALLES", 
                                                               "VILLA CRESPO", "VILLA DEL PARQUE", "VILLA DEVOTO", "VILLA GRAL. MITRE", 
                                                               "VILLA LUGANO", "VILLA LURO", "VILLA ORTUZAR", "VILLA PUEYRREDON", 
                                                               "VILLA REAL", "VILLA RIACHUELO", "VILLA SANTA RITA", "VILLA SOLDATI", 
                                                               "VILLA URQUIZA"), "control", NA))

# Making Sure 'group' is a factor 
crime_dataDD$group <- factor(crime_dataDD$group)
crime_dataDD$group <- droplevels(crime_dataDD$group)

# Making Binary Variable for Each Crime Type
crime_dataDD$is_robo <- as.numeric(crime_dataDD$tipo == "Robo")
crime_dataDD$is_lesiones <- as.numeric(crime_dataDD$tipo == "Lesiones")
crime_dataDD$is_hurto <- as.numeric(crime_dataDD$tipo == "Hurto")
crime_dataDD$is_vialidad <- as.numeric(crime_dataDD$tipo == "Vialidad")
crime_dataDD$is_amenazas <- as.numeric(crime_dataDD$tipo == "Amenazas")

# 7 Day Buffer Check Relative to the 'Superclásico' matches
crime_dataDD$match_period <- sapply(crime_dataDD$fecha, function(date) {
  if (date %in% match_dates) {
    return("match-day")
  } else if (any(date >= match_dates - 7 & date < match_dates)) {
    return("pre-match")
  } else if (any(date > match_dates & date <= match_dates + 7)) {
    return("post-match")
  } else {
    return(NA)
  }
})

# Making 'match_period' sfactor
crime_dataDD$match_period <- factor(crime_dataDD$match_period, levels = c("pre-match", "match-day", "post-match"))
crime_dataDD$match_period <- droplevels(crime_dataDD$match_period)

# Running the Differences-in-Differences models 
DiD_model_robo <- lm(is_robo ~ match_period * group, data = crime_dataDD)
DiD_model_lesiones <- lm(is_lesiones ~ match_period * group, data = crime_dataDD)
DiD_model_hurto <- lm(is_hurto ~ match_period * group, data = crime_dataDD)
DiD_model_vialidad <- lm(is_vialidad ~ match_period * group, data = crime_dataDD)
DiD_model_amenazas <- lm(is_amenazas ~ match_period * group, data = crime_dataDD)


# Robust standard errors for the DiD model
coeftest(DiD_model_robo, vcov. = vcovHC(DiD_model_robo, type = "HC1"))

stargazer(DiD_model_robo, type = "text")

stargazer(DiD_model_lesiones, type = "text")

stargazer(DiD_model_hurto, type = "text")

stargazer(DiD_model_vialidad, type = "text")

stargazer(DiD_model_amenazas, type = "text")


# Making Cool Table
models_nunez <- list(DiD_model_robo, DiD_model_lesiones, DiD_model_hurto, DiD_model_vialidad, DiD_model_amenazas)

stargazer(models_nunez, type = "text", out = "/Users/octavius/Desktop/ECON 114 R CODE/RESEARCH PAPER/TABLES:IMAGES/DiD_analysis_BOCA.txt", 
          title = "Differences-in-Differences Analysis for Boca", 
          model.names = FALSE, 
          align = TRUE)

