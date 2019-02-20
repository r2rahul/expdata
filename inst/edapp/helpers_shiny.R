#' Returns a pair plot.
#'
#' This function takes input data.
#' The function calculates the number of missing values in the vector.
#'
#'@import GGally
#'@import plotly
#'@param data: The data table
#'@param vars: String name of the variable.
#'@return A plotly
#'@export
pairs_plot <- function(data, vars = NULL){
  if(is.null(vars)){
    p <- (ggpairs(iris) + theme_minimal()) %>% ggplotly()
  }else{
    p <- data %>% select(vars) %>% ggpairs() + theme_minimal()
    p <- ggplotly(p)
  }
  
  return(p)
}

#' Returns a pair plot.
#'
#' This function takes input data.
#' The function calculates the number of missing values in the vector.
#' M1 <- GKtauDataframe(data)
#'@import corrplot
#'@param data: The data table
#'@param vars: String name of the variable.
#'@return A R graph object
#'@export
asso_plots <- function(data, vars = NULL){
  data <- data.table(iris)
  if(is.null(vars)){
    M <- cor(data[, .SD, .SDcols = sapply(data, is.numeric)])
  }else{
    M <- data %>% select(vars) %>% cor()
  }
  p <- corrplot(M, method = "ellipse", type = "lower")
  return(p)
}
#' Returns a density plot object.
#'
#' This function takes input data.
#' The function calculates the number of missing values in the vector.
#'
#'@import rmarkdown
#'@param data: The data frame
#'@param filename: String name for the report name.
#'@param output_format: Output Format.
#'@return A Report for the Data Analysis
#'@export
exploratory_report <- function(data, filename = NULL, output_format = "html_document"){

  wd <- getwd()
  if(is.null(filename)){
    filename <- "eda_report.html"
  }
  pkg_path <- system.file(package = "expdata", "inst")
  fname <- "/inst/template_rmarkdown.Rmd"
  md_path <- paste0(pkg_path, fname)
  #file.copy(md_path, wd)
  rmarkdown::render(md_path, 
                    params = list(data = data), 
    output_format = output_format, 
    output_file = filename, 
    output_dir = wd,
    intermediates_dir = wd,
    knit_root_dir = wd
    )
}

#' Check Date Type
#'
#' This function checkes if the input column is of date type.
#'
#' @importFrom stats IQR median quantile sd
#' @import data.table
#' @param x: A single numeric column to get top n and min n numbers
#' @param n: count of top numbers to return
#' @return returns a data.frame with two columns with min and max numbers
#' @export
top_min_n <- function(data, x_var, n = 5){
  out <- data[order(-get(x_var))][,get(x_var)]
  max_numbers <- head(out, n)
  min_numbers <- tail(out, n)
  dd <- data.frame(max_numbers = max_numbers, min_numbers = min_numbers)
  return(dd)
}

#' Preprocess the data
#'
#' This function checkes if the input column is of date type.
#'
#' @importFrom stats IQR median quantile sd
#' @import data.table
#' @param x: A single numeric column to get top n and min n numbers
#' @param n: count of top numbers to return
#' @return returns a data.frame with two columns with min and max numbers
#' @export
preprocess_data <- function(fpath, hflag = TRUE, dcolumn = NULL, dformat = NULL, fcolumn = NULL, store = TRUE){
  out <- fread(fpath, 
               header = hflag,
               stringsAsFactors = FALSE,
               logical01 = TRUE)
  
  if(!is.null(dcolumn)){
    if(!is.null(dformat)){
      out[, (dcolumn) := lapply(.SD, function(x){parse_date_time(x, dformat)}), .SDcols = dcolumn]
    }else{
      out[, (dcolumn) := lapply(.SD, parse_date), .SDcols = dcolumn]
    }
  }
  
  
 if(!is.null(fcolumn)){
   out[, (fcolumn) := lapply(.SD, as.factor), .SDcols = fcolumn]
 }
  
  tstamp <- paste0("_", format(now(), "%Y%m%d_%H%M", "GMT"))
  fname <- paste0(gsub(".csv", tstamp, fpath), ".fst")
  if(store){
    write_fst(out, fname)
  }
  return(out)
}



