# Libraries

library(tidyverse)
library(lubridate)


# Load BC CDC Case Data
df_case <- read.csv("BCCDC_COVID19_Dashboard_Case_Details.csv") %>%
           mutate(Reported_Date = as.Date(Reported_Date),
                  yr = year(as.Date(Reported_Date)),
                  mth = month(as.Date(Reported_Date)),
                  wk = week(as.Date(Reported_Date))) %>%
           mutate(year_week = as.character(ymd(paste0(yr,"-01-07")) + weeks(wk-1))) %>%
           filter(!wk %in% 53)

# Load BC Population Data
df_pop <- read.csv("BC_Population_Demographics.csv")

# Summarize Weekly Case Data
df_summary <- df_case %>%
              filter(!Age_Group %in% c('Unknown')) %>%
              group_by(year_week, Age_Group) %>%
              summarise(cases = n()) %>%
              ungroup() %>%
              complete(year_week, Age_Group, fill = list(cases = 0)) %>%
              left_join(df_pop, by = c("Age_Group"="age_group")) %>%
              mutate(case_rate = cases / population * 100000) %>%
              mutate(case_rate_bin = cut(case_rate,
                                         c(-1,5,10,15,20,35,50,100,150,200,300,600,Inf),
                                         labels = c("0-5",">5-10",">10-15",">15-20",">20-35",">35-50",">50-100",">100-150",">150-200",">200-300",">300-600",">600")))

# Case Heatmap 
p <- ggplot(df_summary, aes(year_week, Age_Group, fill= case_rate_bin)) + 
     geom_tile() +
     geom_text(size = 2, aes(label = round(case_rate, 0))) +
     scale_fill_brewer(type="seq",palette = "Spectral", direction = -1) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") + 
     labs(fill = "cases per 100k population",
          x = "Age Group",
          y = "Week",
          title = "BC Weekly COVID19 Cases per 100,000 Population")



