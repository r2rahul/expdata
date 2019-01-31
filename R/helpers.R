#' Returns a report.
#'
#' This function takes input data.
#' The function calculates the number of missing values in the vector.
#'
#'@import knitr
#'@import rmarkdown
#'@param data: The data frame
#'@param filename: String name for the report name.
#'@param output_format: Output Format.
#'@return A Report for the Data Analysis
#'@export
exploratory_report <- function(data = iris, filename = NULL, output_format = "html_document"){

  wd <- getwd()
  if(is.null(filename)){
    tstamp <- format(Sys.time(), "%Y%m%d_%H%M")
    filename <- paste0("eda_report", tstamp, ".html")
  }
  md_path <- system.file("template_rmarkdown.Rmd", package = "expdata")
  render(md_path,
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
#' This a helper function to summarize more than one columns for group_bin function. 
#' Ideally The breaks should start from -Inf and end at +Inf to avoid Nan's. 
#' Otherwise we can see see Nan's. Default bin's are calculated based on quartiles of the data
#'
#' @importFrom stats IQR median quantile sd
#' @import data.table
#' @param data Input data table
#' @param x: A single numeric column to get top n and min n numbers
#' @param y: Column to summarize default is input column
#' @param break: bin size of data
#' @return returns a list containing summary data.frame with two columns with min and max numbers
#' @export
helper_bin <- function(data, x, y, breaks, ...){
  
  dt <- data[, .SD, .SDcols = c(x, y)]
  out <- list()
  num_s <- dt[, 
              .(var_name = names(.SD),
                type = "numeric",
                count = .N,
                miss_num = sapply(.SD, miss_sum, count = 1),
                "miss %" = sapply(.SD, miss_sum, count = .N),
                zero_count = sapply(.SD, zeros_sum, count = 1),
                "zero %" = sapply(.SD, zeros_sum, count = .N),
                "quant 0%" = sapply(.SD, quantile, probs = 0, na.rm = TRUE),
                "quant 25%" = sapply(.SD, quantile, probs = 0.25, na.rm = TRUE),
                "quant 75%" = sapply(.SD, quantile, probs = 0.75, na.rm = TRUE),
                "quant 100%" = sapply(.SD, quantile, probs = 1, na.rm = TRUE),
                var_av = sapply(.SD, mean, na.rm = TRUE),
                var_sd = sapply(.SD, sd, na.rm = TRUE),
                var_med = sapply(.SD, function(x) as.double(median(x, na.rm = TRUE))),
                var_min = sapply(.SD, function(x) as.double(min(x, na.rm = TRUE))),
                var_max = sapply(.SD, function(x) as.double(max(x, na.rm = TRUE))),
                var_sum = sapply(.SD, sum, na.rm = TRUE),
                var_kurtosis = sapply(.SD, skewness, na.rm = TRUE),
                var_skewness = sapply(.SD, kurtosis, na.rm = TRUE),
                var_iqr = sapply(.SD, IQR, na.rm = TRUE)),
              by = cut(get(x), breaks = breaks, ordered_result = FALSE),
              .SDcols = sapply(dt, function(x) is.numeric(x) || is.double(x))]
  setnames(num_s, old = "cut", new = "bin_groups")
  out[['num_summary']] <- copy(num_s)
  rm(num_s)
  
  if(nrow(dt[, .SD, .SDcols = sapply(dt, is.character)]) != 0){
    char_s <- dt[, 
                 .(var_name = names(.SD),
                   type = 'character',
                   count = .N,
                   miss_num = sapply(.SD, miss_sum, count = 1),
                   "miss %" = sapply(.SD, miss_sum, count = .N),
                   zero_count = sapply(.SD, zeros_sum, count = 1),
                   "zero %" = sapply(.SD, zeros_sum, count = .N),
                   distinct = sapply(.SD, unique_sum)),
                 by = cut(get(x), breaks = breaks, ordered_result = FALSE),
                 .SDcols = sapply(dt, is.character)]
    setnames(char_s, old = "cut", new = "bin_groups")
    out[["char_summary"]] <- char_s
    rm(char_s)
  }
  
  if(nrow(dt[, .SD, .SDcols = sapply(dt, is.factor)]) != 0){
    factor_s <- dt[, 
                 .(var_name = names(.SD),
                   type = 'factor',
                   count = .N,
                   miss_num = sapply(.SD, miss_sum, count = 1),
                   "miss %" = sapply(.SD, miss_sum, count = .N),
                   zero_count = sapply(.SD, zeros_sum, count = 1),
                   "zero %" = sapply(.SD, zeros_sum, count = .N),
                   distinct = sapply(.SD, unique_sum)),
                 by = cut(get(x), breaks = breaks, ordered_result = FALSE),
                 .SDcols = sapply(dt, is.factor)]
    setnames(factor_s, old = "cut", new = "bin_groups")
    out[["factor_summary"]] <- factor_s_s
    rm(factor_s)
  }
  
  if(nrow(dt[, .SD, .SDcols = sapply(data, is_date)]) != 0){
    date_s <- dt[,
               .(var_name = names(.SD),
                 type = "date",
                 num_row = .N,
                 miss_num = sapply(.SD, miss_sum, count = 1),
                 "miss %" = sapply(.SD, miss_sum, count = .N),
                 zero_count = sapply(.SD, zeros_sum, count = 1),
                 "zero %" = sapply(.SD, zeros_sum, count = .N),
                 distinct = sapply(.SD, unique_sum),
                 var_min = sapply(.SD, min, na.rm = TRUE),
                 var_max = sapply(.SD, max, na.rm = TRUE))]
    out[['date_summary']] <- date_s
    rm(date_s)
  }
  return(out)
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
    breaks <- quantile(data[, get(x)], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
    breaks <- c(-Inf, breaks, Inf)
  }
  
  if(is.null(y)){
    out <- data[, 
                .(count = .N, avg = mean(get(x)), std = sd(get(x))), 
                by = cut(get(x), breaks = breaks)]
    setnames(out, old = "cut", new = "bin_groups")
  }else{
    out <- helper_bin(data, x, y, breaks, ...)
  }
  
return(out)
}

#' The functions executes the dashboard using shiny to explore the data set. 
#' 
#' @inheritParams shiny::runApp
#' 
#' @export
start_shiny <- function(port = getOption("shiny.port")) {
  appDir <- system.file("edapp", package = "expdata")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `expdata`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal", port = port)
}