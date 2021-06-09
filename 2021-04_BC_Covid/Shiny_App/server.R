
### PACKAGES ###
################

library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)  # rolling averages
library(scales) # to access breaks/formatting functions


### FUNcTIONS ###
#################

# Daily Cases - 7 day Rolling Average #
#######################################

cases_by_age <- function(ha, age_group) {
  
  df <- NULL
  
  for (i in 1:length(age_group)) {
    dates <- data.frame(Reported_Date = seq(ymd('2020-10-01'), max(df_case$Reported_Date), by = 'day'),
                        Age_Group = age_group[i])
    
    df_age <- filter(df_case, HA == ha) %>%
      group_by(HA, Age_Group, Reported_Date) %>%
      summarise(cases = n())
    
    tmp <- dates %>%
      left_join(df_age, by=c("Reported_Date","Age_Group")) %>%
      mutate(cases = replace_na(cases, 0)) %>%
      mutate(rolling_mean = rollmean(cases, k=7, align="right", fill=NA))
    
    if (is.null(df)) {
      df <- tmp
    } else (df <- rbind(df, tmp))
    
  }
  
  df   
  
}

daily_case_plot <- function(ha, age_groups) {

# Load BC CDC Case Data
df_case <- df_cases %>%
    mutate(Reported_Date = as.Date(Reported_Date)) %>%
    mutate(year_week = case_when(epiweek(Reported_Date) != epiweek(Reported_Date+1) ~ as.character(Reported_Date),
                                 epiweek(Reported_Date) != epiweek(Reported_Date+2) ~ as.character(Reported_Date+1),
                                 epiweek(Reported_Date) != epiweek(Reported_Date+3) ~ as.character(Reported_Date+2),
                                 epiweek(Reported_Date) != epiweek(Reported_Date+4) ~ as.character(Reported_Date+3),
                                 epiweek(Reported_Date) != epiweek(Reported_Date+5) ~ as.character(Reported_Date+4),
                                 epiweek(Reported_Date) != epiweek(Reported_Date+6) ~ as.character(Reported_Date+5),
                                 epiweek(Reported_Date) != epiweek(Reported_Date+7) ~ as.character(Reported_Date+6),
                                 TRUE ~ "")) %>%
    filter(!Age_Group %in% c('Unknown'),
           !HA %in% c('Out of Canada'))

  
df_ha <- cases_by_age(ha, age_groups)
  
  
ggplot() + 
    geom_line(data = df_ha, aes(x=Reported_Date, y=rolling_mean, group=Age_Group, colour=Age_Group), size=1) +
    geom_point(data = df_ha, aes(x=Reported_Date, y=cases, group=Age_Group, colour=Age_Group)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") + 
    scale_x_date(date_breaks = "weeks" , date_labels = "%Y-%m-%d") +
    labs(y = "Number of Cases",
         x = "Date",
         title = paste0("BC ",ha," Interior COVID19 Cases (7 Day Rolling Average)"),
         caption = "Chart: Michael Hainke  Data: BC CDC")
  
}


# Situation Report Data: Hospitalizations, ICU, Deaths #
########################################################
sitrep_plot <- function(measure) {

# Summarize data
df <- df_situation %>%
      filter(metric == measure) %>% 
      left_join(filter(df_pop, HA == "BC"), by = c("Age_Group"="age_group")) %>%
      mutate(rate = weekly_count / population * 1000000) %>%
      mutate(rate_bin = cut(rate,
                            c(-100,5,10,15,20,35,50,100,150,300,600,Inf),
                            labels = c("0-5",">5-10",">10-15",">15-20",">20-35",">35-50",">50-100",">100-150",">150-300",">300-600",">600")))

# Plot Heatmap    
ggplot(df, aes(year_week, Age_Group, fill= rate_bin)) + 
       geom_tile() +
       geom_text(size = 3, aes(label = round(rate, 0))) +
       scale_fill_brewer(type="seq",palette = "Spectral", direction = -1) +
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") + 
       labs(fill = paste0(measure," per 1m age group population"),
            y = "Age Group",
            x = "Week",
            title = paste0("BC Weekly COVID19 ",measure," per 1,000,000 Age Group Population"),
            caption = "Chart: Michael Hainke Data: BC CDC Situation Report")
    
}

# BC CDC Cases Data #
#####################
cases_plot <- function(ha) {

# Filter Partial Epi-week
if(epiweek(max(df_case$Reported_Date)) == epiweek(max(df_case$Reported_Date) + 1)) {
  df_case <- df_case %>%
             filter(!(year(df_case$Reported_Date) == year(max(df_case$Reported_Date)) & epiweek(df_case$Reported_Date) == epiweek(max(df_case$Reported_Date))))
}

# Summarize BC Weekly Case Data
df_summary_bc <- df_case %>%
    group_by(year_week, Age_Group) %>%
    summarise(cases = n()) %>%
    mutate(HA = "BC") %>%
    select(HA, year_week, Age_Group, cases)

# Summarize Weekly Case Data
df_summary_ha <- df_case %>%
    group_by(HA, year_week, Age_Group) %>%
    summarise(cases = n()) %>%
    select(HA, year_week, Age_Group, cases)

df_summary <- rbind(df_summary_bc, df_summary_ha) %>%
    ungroup() %>%
    complete(HA, year_week, Age_Group, fill = list(cases = 0)) %>%
    left_join(df_pop, by = c("Age_Group"="age_group", "HA"="HA")) %>%
    mutate(case_rate = cases / population * 100000) %>%
    mutate(case_rate_bin = cut(case_rate,
                               c(-1,5,10,15,20,35,50,100,150,200,300,600,Inf),
                               labels = c("0-5",">5-10",">10-15",">15-20",">20-35",">35-50",">50-100",">100-150",">150-200",">200-300",">300-600",">600")))
    
# Plot Heatmap
theme_set(theme_gray(base_size = 15))

ggplot(filter(df_summary, HA == ha), aes(year_week, Age_Group, fill= case_rate_bin)) + 
    geom_tile() +
    geom_text(size = 3, aes(label = round(case_rate, 0))) +
    scale_fill_brewer(type="seq",palette = "Spectral", direction = -1) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") + 
    labs(fill = "cases per 100k population",
         y = "Age Group",
         x = "Week",
         title = paste0(ha," Weekly COVID19 Cases per 100,000 Population"),
         caption = "Chart: Michael Hainke Data: BCCDC Case Data")

}

# Plot LTC cases #
##################
ltc_plot <- function() {
  
  # Load LTC data
  df_ltc <- readRDS("ltc_data.rds") %>%
    mutate(outbreak_start_date = parse_date_time(outbreak_start_date, orders = c('mdy', 'ymd')),
           outbreak_end_date = parse_date_time(outbreak_end_date, orders = c('mdy', 'ymd'))) %>%
    mutate(yr = year(as.Date(outbreak_start_date)),
           wk = week(as.Date(outbreak_start_date))) %>%
    mutate(year_week = as.character(ymd(paste0(yr,"-01-07")) + weeks(wk-1)))
  
  # Tidy LTC data
  df_tidy <- rename(df_ltc, LTC_cases = cases_residents, LTC_deaths = deaths_residents, LTC_staff_cases = cases_staff, LTC_staff_deaths = deaths_staff) %>%
    pivot_longer(cols = c(LTC_cases, LTC_deaths, LTC_staff_cases, LTC_staff_deaths),
                 names_to = "metric",
                 values_to = "count") %>%
    group_by(metric, year_week) %>%
    summarise(cases = sum(count)) %>%
    select(metric, year_week, cases) %>%
    filter(metric %in% c("LTC_cases", "LTC_staff_cases"))
  
  # Summarize BC Weekly Case Data
  df_summary_bc <- df_case %>%
    mutate(metric = case_when(Age_Group %in% c("70-79","80-89","90+") ~ "70+ Total",
                              Age_Group %in% c("60-69") ~ "60-69",
                              TRUE ~ "Total Under 60")) %>%
    group_by(metric, year_week) %>%
    summarise(cases = n()) %>%
    select(metric, year_week, cases) %>%
    filter(metric %in% c("70+ Total", "60-69"))
  
  df_tidy <- rbind(df_tidy, df_summary_bc)
  
  p <- ggplot(df_tidy, aes(x=year_week, y=cases, group=metric, colour=metric)) + 
    geom_line(size=1) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") + 
    labs(y = "Number of Cases",
         x = "Week",
         title = "BC Weekly COVID19 Cases",
         caption = "Chart: Michael Hainke  Data: BC CDC")
  p

}



### LOAD DATA ###
#################

# Load BC CDC Case Data
df_cases <- readRDS("case_data.rds") 


df_case <- df_cases %>%
           mutate(Reported_Date = as.Date(Reported_Date)) %>%
           mutate(year_week = case_when(epiweek(Reported_Date) != epiweek(Reported_Date+1) ~ as.character(Reported_Date),
                                        epiweek(Reported_Date) != epiweek(Reported_Date+2) ~ as.character(Reported_Date+1),
                                        epiweek(Reported_Date) != epiweek(Reported_Date+3) ~ as.character(Reported_Date+2),
                                        epiweek(Reported_Date) != epiweek(Reported_Date+4) ~ as.character(Reported_Date+3),
                                        epiweek(Reported_Date) != epiweek(Reported_Date+5) ~ as.character(Reported_Date+4),
                                        epiweek(Reported_Date) != epiweek(Reported_Date+6) ~ as.character(Reported_Date+5),
                                        epiweek(Reported_Date) != epiweek(Reported_Date+7) ~ as.character(Reported_Date+6),
                                        TRUE ~ "")) %>%
           filter(!Age_Group %in% c('Unknown'),
                  !HA %in% c('Out of Canada'))

# Load BC Situation Report Data
df_situation <- readRDS("sitrep_data.rds") %>%
      mutate(date = as.Date(year_week, format="%d/%m/%Y"),
             year_week = as.character(as.Date(year_week, format="%d/%m/%Y"))) %>%
      filter(!year_week %in% c("2020-05-07","2020-10-17","2021-01-09")) 

# Load BC Population Data
df_pop <- readRDS("population_data.rds")


### SHINY OUTPUTS ###
#####################

shinyServer(function(input, output) {

    output$cases <- renderPlot({
        cases_plot(input$ha)
    })
    
    output$daily_cases <- renderPlot({
        daily_case_plot(input$ha_daily, input$age_daily)
    })
    
    output$hospitalizations <- renderPlot({
        sitrep_plot("Hospitalizations")
    })
    
    output$icu <- renderPlot({
        sitrep_plot("ICU")
    })

    output$deaths <- renderPlot({
        sitrep_plot("Deaths")
    })
    
    output$ltc <- renderPlot({
        ltc_plot()
    })


})
