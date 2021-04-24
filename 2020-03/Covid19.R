# Packages
library(plyr)
library(tidyverse)

# Read list of .csv files
setwd("~/GitHub/COVID-19/csse_covid_19_data")
mydir = "csse_covid_19_daily_reports"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)

# Split at Mar 21.  Later files have different columns
df1 = ldply(myfiles[1:60], read_csv) %>%
      mutate(Active = NA,
             FIPS = NA,
             County = NA) %>%
      select(County,
             Province_State = `Province/State`,
             Country_Region = `Country/Region`,
             FIPS,
             Latitude,
             Longitude,
             Confirmed,
             Deaths,
             Recovered,
             Active,
             Last_Update = `Last Update`)
       
df2 = ldply(myfiles[61:length(myfiles)], read_csv) %>%
      select(County = Admin2,
             Province_State,
             Country_Region,
             FIPS,
             Latitude = Lat,
             Longitude = Long_,
             Confirmed,
             Deaths,
             Recovered,
             Active,
             Last_Update)

df = rbind(df1, df2)
rm(df1)
rm(df2)

# Fix Date
df$Last_Update <- gsub(" .+", "",df$Last_Update)

for (i in 1:length(df$Last_Update)) {
  if (grepl("^\\d+/\\d+/\\d+", df$Last_Update[i])) {
    month <- sprintf("%02d", as.numeric(gsub("/\\d+/\\d+$", "",df$Last_Update[i])))
    day <- sprintf("%02d", as.numeric(gsub("^\\d+/|/\\d+$", "",df$Last_Update[i])))
    year <- as.numeric(gsub("^\\d+/\\d+/", "",df$Last_Update[i]))
    year <- if(year<2000) { year + 2000 } else { year }
    df$Last_Update[i] <- paste0(year,"-",month,"-",day)
  }
}

df$Last_Update <- as.Date(df$Last_Update)

# Join Population Data
pop <- read.csv('population.csv') %>%
       select(-n)
df <- left_join(df, pop)

# Fix some Country Data
df <- mutate(df, Country_Region = case_when(Country_Region == 'Korea, South' ~ 'South Korea',
                                            Country_Region == 'United Kingdom' ~ 'UK',
                                            Country_Region %in% c('Hong Kong','Hong Kong SAR', 'Mainland China') ~ 'China',
                                            Country_Region == 'Iran (Islamic Republic of)' ~ 'Iran',
                                            TRUE ~ Country_Region))
df$Province_State[df$Province_State %in% c('UK','United Kingdom')] <- NA

# Add Population Adjusted Data
df <- mutate(df, Confirmed_per_mil = Confirmed / (Population/1000000),
                 Deaths_per_mil = Deaths / (Population/1000000),
                 Recovered_per_mil = Recovered / (Population/1000000),
                 Active_per_mil = Recovered / (Population/1000000))



# Test
test <- filter(df, Province_State == "British Columbia")


