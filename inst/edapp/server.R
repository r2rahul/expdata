library(shiny)
library(shinydashboard)
library(hflights)
library(plotly)
library(data.table)
library(tidyverse)
library(moments)
library(lubridate)
source("helpers_shiny.R")
source("main_source.R")
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

data_dt <- reactiveValues(data = data.table(hflights))

observeEvent(input$go, {
  data_dt$data <- fread(input$file1$datapath, 
                 header = input$header,
                 stringsAsFactors = FALSE,
                 logical01 = TRUE)
      })
    
  
input_data <- reactive({
  complete_summary(data_dt$data) %>% arrange(type)
})
  
output$orig_data <- DT::renderDataTable({
    DT::datatable(
      data_dt$data, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames= FALSE
    )
    
  })
  
  output$contents <- DT::renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    DT::datatable(
      input_data(), options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames= FALSE
    )
    
  })

output$num_s <- DT::renderDataTable({
  dt <- input_data() %>% filter(type == "numeric")
  if(nrow(dt) == 0){
    out <- data.table(info = "No Numeric Variables in the Data")
  }else{
    out <- dt
  }
    DT::datatable(
      out, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames= FALSE
    )
 })

output$char_s <- DT::renderDataTable({
  dt <- input_data() %>% filter(type %in% c("character", "factor", "logical"))
  if(nrow(dt) == 0){
    out <- data.table(info = "No character or factor or logical Variables in the Data")
  }else{
    out <- dt %>% select(var_name, type, num_row, miss_num, "miss %", zero_count, distinct)
  }
  
    DT::datatable(
      out, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames= FALSE
    )
})

output$date_s <- DT::renderDataTable({
  dt <- input_data() %>% filter(type %in% c("date"))
  if(nrow(dt) == 0){
   out <- data.table(info = "No date Variables in the Data")
  }else{
    out <- dt
  }
  
  DT::datatable(
    out, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames= FALSE
    )
})
}