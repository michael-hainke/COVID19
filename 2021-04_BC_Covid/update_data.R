# Libraries
library(googlesheets4)

# Set Directory
setwd("~/GitHub/COVID19/2021-04_BC_Covid/Shiny_App")

# Load BC CDC Case Data
read.csv("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Dashboard_Case_Details.csv") %>%
saveRDS("case_data.rds")

# Load BC CDC Situation Report Data (This Google Sheet is updated manually by me from Situation Report)
read_sheet("https://docs.google.com/spreadsheets/d/1aVTXh9bvKB4HtIdL3AVLHkfN6XR0xbEO8-KW57AhGPk/edit?usp=sharing") %>%
saveRDS("sitrep_data.rds")

# Load BC CDC Long Term Care Data (This Google Sheet is updated manually by me from LTC Report)
read_sheet("https://docs.google.com/spreadsheets/d/1p-n-yOfZqbSxamwQ8F7j5eqZPAcMGcksrgEACrEKteY/edit?usp=sharing") %>%
saveRDS("ltc_data.rds")


