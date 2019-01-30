library(shiny)
library(shinydashboard)
library(hflights)
library(plotly)
library(GGally)
library(GoodmanKruskal)
library(corrplot)
library(data.table)
library(tidyverse)
library(moments)
library(lubridate)
source("helpers_shiny.R")
source("main_source.R")
# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  dashboardHeader(title = "EDA explorer"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
    column(width = 3,
    box(
      title = "Upload Data", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = NULL,
      fileInput("file1", "Choose a CSV File",
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      checkboxInput("header", "Header", TRUE),
      h4("Enter Space Separated column names to parse as dates"),
      checkboxInput("pdate", "Columns to Parse as date", FALSE),
      textInput("colsdate", label = h4("Column Names"), value = "Space Separated Columns"),
      actionButton("go", "submit"),
      tags$hr()
    ),
    box(
      title = "Meta Data", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = NULL, height = "100%",
      verbatimTextOutput("fbasic"),
      tags$hr(),
      DT::dataTableOutput('varmeta')
    )),
    column(width = 9, height = "100%",
    tabBox(
      title = "DataExplorer",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      #collapsible = TRUE,
      width = NULL,
      tabPanel("Overview", plotlyOutput("pairs_plot")),
      tabPanel("NumericVariable", "Numeric Variables"),
      tabPanel("CharFactorVariables", "Character/Factor/Logical Variables"),
      tabPanel("DateVariables", "Date Variables"),
      tabPanel("Association", checkboxInput("asso", "Correlation/GoodmanKruskal", TRUE), plotOutput("cor_plot"))
     ),
    tabBox(
      title = "DataView",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      width = NULL,
      tabPanel("Overview", DT::dataTableOutput('contents')),
      tabPanel("NumericVariable", DT::dataTableOutput('num_s')),
      tabPanel("CharFactorVariables", DT::dataTableOutput('char_s')),
      tabPanel("DateVariables", DT::dataTableOutput('date_s')),
      tabPanel("InputData", DT::dataTableOutput('orig_data'))
    )
    )
  )
)

