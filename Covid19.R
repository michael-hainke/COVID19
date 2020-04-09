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
test$Last_Update <- gsub(" .+", "",test$Last_Update)

for (i in 1:length(test$Last_Update)) {
  if (grepl("^\\d+/\\d+/\\d+", test$Last_Update[i])) {
    month <- sprintf("%02d", as.numeric(gsub("/\\d+/\\d+$", "",test$Last_Update[i])))
    day <- sprintf("%02d", as.numeric(gsub("^\\d+/|/\\d+$", "",test$Last_Update[i])))
    year <- as.numeric(gsub("^\\d+/\\d+/", "",test$Last_Update[i]))
    year <- if(year<2000) { year + 2000 } else { year }
    test$Last_Update[i] <- paste0(year,"-",month,"-",day)
  }
}

test$Last_Update <- as.Date(test$Last_Update)

