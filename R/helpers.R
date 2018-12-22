#' Returns a scatter plot object.
#'
#' This function takes input data.
#' The function calculates the number of missing values in the vector.
#'
#'@import rbokeh
#'@param data: The data frame
#'@param x_var: String name of the variable.
#'@param y_var: String name of the variable.
#'@return A rbokeh plot object
#'@export
scatter_plot <- function(data, x_var, y_var){
  p <- figure() %>%
    ly_points(eval(x_var), eval(y_var), data = data,
              hover = list(eval(x_var), eval(y_var)))
  return(p)
}

#' Returns a bar plot object.
#'
#' This function takes input data.
#' The function calculates the number of missing values in the vector.
#'
#'@import rbokeh
#'@param data: The data frame
#'@param x_var: String name of the variable.
#'@param y_var: String name of the variable.
#'@return A rbokeh plot object
#'@export
bar_plot <- function(data, x_var, y_var){
  p <- figure() %>%
    ly_bar(eval(x_var), eval(y_var), data = data,
              hover = list(eval(x_var), eval(y_var)))
  return(p)
}

#' Returns a hist plot object.
#'
#' This function takes input data.
#' The function calculates the number of missing values in the vector.
#'
#'@import rbokeh
#'@param data: The data frame
#'@param x_var: String name of the variable.
#'@return A rbokeh plot object
#'@export
his_plot <- function(data, x_var){
  p <- figure() %>%
    ly_hist(eval(x_var), data = data) %>% 
    ly_density(eval(x_var), data = data)
  return(p)
}

#' Returns a density plot object.
#'
#' This function takes input data.
#' The function calculates the number of missing values in the vector.
#'
#'@import rbokeh
#'@param data: The data frame
#'@param x_var: String name of the variable.
#'@return A rbokeh plot object
#'@export
den_plot <- function(data, x_var){
  p <- figure() %>% 
    ly_density(eval(x_var), data = data)
  return(p)
}

#' Returns a density plot object.
#'
#' This function takes input data.
#' The function calculates the number of missing values in the vector.
#'
#'@import rbokeh
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

#' Find Max and Min of the Numeric Columns
#'
#' This function finds max n and min n data of the numeric columns
#'
#' @importFrom stats IQR median quantile sd
#' @import data.table
#' @param data: Input data table
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

#' Group Column based on bin size
#'
#' This function groups columns based on bin size of numeric data. 
#' Ideally The breaks should start from -Inf and end at +Inf to avoid Nan's. 
#' Otherwise we can see see Nan's. Default bin's are calculated based on quartiles of the data
#'
#' @importFrom stats IQR median quantile sd
#' @import data.table
#' @param data Input data table
#' @param x: A single numeric column to get top n and min n numbers
#' @param y: Column to summarize default is input column
#' @param break: bin size of data
#' @return returns a data.frame with two columns with min and max numbers
#' @export
group_bin <- function(data, x, y = NULL, breaks = NULL, ...){
  if(is.null(breaks)){
    breaks <- quantile(data[, get(x)], probs = c(0.25, 0.50, 0.75))
    breaks <- c(-Inf, breaks, Inf)
  }
  
  if(is.null(y)){
    out <- data[, 
                .(count = .N, avg = mean(get(x)), std = sd(get(x))), 
                by = cut(get(x), breaks = breaks)]
  }else{
    out <- data[, 
                .(count = .N, avg = mean(get(y)), std = sd(get(y))), 
                by = cut(get(x), breaks = breaks, ordered_result = TRUE)]
  }
  setnames(out, old = "cut", new = "bin_groups")
return(out)
}