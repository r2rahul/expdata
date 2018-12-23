library(shiny)
library(shinydashboard)
source("helpers_shiny.R")
source("main_source.R")
# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  dashboardHeader(title = "EDA explorer"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
    column(width = 2,
    box(
      title = "Upload Data", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = NULL,
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      checkboxInput("header", "Header", TRUE),
      tags$hr()
    ),
    box(
      title = "Variable Selector", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = NULL, height = "100%",
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      checkboxInput("header", "Header", TRUE),
      tags$hr()
    )),
    column(width = 10, height = "100%",
    tabBox(
      title = "DataExplorer",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      width = NULL,
      tabPanel("Overview", "Overview"),
      tabPanel("NumericVariable", "Numeric Variables"),
      tabPanel("CharFactorVariables", "Character/Factor/Logical Variables"),
      tabPanel("DateVariables", "Date Variables")
     )
    )
  )
)
)
