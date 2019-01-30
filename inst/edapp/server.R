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
library(parsedate)
source("helpers_shiny.R")
source("main_source.R")
options(shiny.maxRequestSize = 100*1024^2) 
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

data_dt <- reactiveValues(data = data.table(iris))

observeEvent(input$go, {
  withProgress(message = 'Reading Data', value = 0, {
  if(input$pdate){
    data_dt$data <- fread(input$file1$datapath, 
                header = input$header,
                stringsAsFactors = FALSE,
                logical01 = TRUE)[, (input$colsdate) := lapply(.SD, parse_date), .SDcols = input$colsdate]
  }else{
    data_dt$data <- fread(input$file1$datapath, 
                          header = input$header,
                          stringsAsFactors = FALSE,
                          logical01 = TRUE)
  }
  incProgress(detail = paste("Reading Data", input$file1$datapath))
    
    })
})
    
  
input_data <- reactive({
  complete_summary(data_dt$data) %>% arrange(type)
})

output$varmeta <- DT::renderDataTable({ 
  meta <- input_data() %>% select(var_name, type, num_row)
  DT::datatable(
    meta, options = list(
      pageLength = 10,
      scrollX = TRUE
    ),
    rownames= FALSE
  )
})

output$fbasic <- renderPrint({
  features <- ncol(data_dt$data) %>% as.character()
  rows <- nrow(data_dt$data) %>% as.character()
  a <- paste0("No of Columns: ", features)
  b <- paste0("No of Rows: ", rows)
  paste(a, b, sep = " ")
  
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

output$pairs_plot <- renderPlotly({
  pairs_plot(input_data())
})

output$cor_plot <- renderPlot({
  asso_plots(input_data())
})


}


