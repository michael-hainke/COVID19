
library(shiny)
library(shinythemes)
library(shinycssloaders)

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage("BC COVID19", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
             tabPanel("Cases",
                      fluidPage(tabsetPanel(
                        tabPanel("Weekly Heatmaps",
                                 br(),
                                 fluidRow(column(4, selectInput(inputId = "ha",
                                                                label = "Select Health Authority:",
                                                                choices = c("BC",
                                                                            "Fraser",
                                                                            "Vancouver Coastal",
                                                                            "Interior",
                                                                            "Northern",
                                                                            "Vancouver Island")))),
                                 fluidRow(column(10, withSpinner(plotOutput("cases"), type = 2)))),
                        tabPanel("Daily",
                                 br(),
                                 fluidRow(column(4, selectInput(inputId = "ha_daily",
                                                                label = "Select Health Authority:",
                                                                choices = c("Fraser",
                                                                            "Vancouver Coastal",
                                                                            "Interior",
                                                                            "Northern",
                                                                            "Vancouver Island")))),
                                 fluidRow(column(6, checkboxGroupInput(inputId = "age_daily",
                                                                       label = "Select Age Groups:",
                                                                       choices = c("<10","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"),
                                                                       inline = TRUE,
                                                                       selected = "20-29"))),
                                 fluidRow(column(10, withSpinner(plotOutput("daily_cases"), type = 2))))
                      ))),
             tabPanel("Hosps / ICU / Death",
                      fluidRow(column(10, br(),
                                      plotOutput("hospitalizations"),
                                      plotOutput("icu"),
                                      plotOutput("deaths")
                      ))),
             tabPanel("Long Term Care",
                      fluidRow(column(10, withSpinner(plotOutput("ltc", height = "600px"), type = 2))))
  )
)
  
