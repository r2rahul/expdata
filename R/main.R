#' Check Date Type
#'
#' This function checkes if the input column is of date type.
#'
#' @importFrom stats IQR median quantile sd
#' @import data.table
#' @import moments
#' @param x: A single column of data table
#' @return Logical True if the input column is of date type else False
#' @export
is_date <- function(x){
  return(inherits(x, "Date") | inherits(x, "POSIXct") | inherits(x, "POSIXlt") | 
           inherits(x, "POSIXt"))
}

#' Perform full join between two data tables
#'
#' This function takes input as two data tables and perform full join between the two.
#' The join keys are all the common columns between the two data table
#'
#' @param dt1: The first data table
#' @param dt2: The second data table
#' @return A data.table
#' @export
call_merge <- function(dt1, dt2){
  com_cname <- intersect(colnames(dt1), colnames(dt2))
  setkeyv(dt1, com_cname)
  setkeyv(dt2, com_cname)
  out <- merge(dt1, dt2, all = TRUE)
  return(out)
}

#' Compares two data tables
#'
#' This function takes input as two data tables.
#' The function checks if any one data.table is empty and returns the non-empty data.table
#' If both the data tables are non-empty then it call the call_merge to perform full join on the two data tables.
#'
#' @param dt1: The first data table
#' @param dt2: The second data table
#' @return A data.table
#' @export
join_data <- function(dt1, dt2){

  if (is.null(dt1)){
    return(dt2)
  }else if (is.null(dt2)){
    return(dt1)
  }else{
    return(call_merge(dt1, dt2))
  }
}

#' Returns the number or percentage of zeros value
#'
#' This function takes input vector.
#' The function calculates the number of zeros values in the vector.
#'
#' @param x: The vector
#' @param count: Scalar quantity either 1 or number of rows in the data.
#' @return A vector
#' @export
zeros_sum <- function(x, count){
  if (count == 1){
    return(length(x[x == 0]))
  }else{
    temp <- length(x[x == 0])
    out <- temp / count
    return(out)
  }
}

#' Returns the number or percentage of missing value
#'
#' This function takes input vector.
#' The function calculates the number of missing values in the vector.
#'
#' @param x: The vector
#' @param count: Scalar quantity either 1 or number of rows in the data.
#' @return A vector
#' @export
miss_sum <- function(x, count){
  if (count == 1){
    return(length(x[is.na(x)]))
  }else{
    temp <- length(x[is.na(x)])
    out <- temp / count
    return(out)
  }
}

#' Returns number of unique characters
#'
#' This function takes input as vector or column of data table or data frame.
#' The function returns the number of unique elements in the vector.
#'
#' @param x: A character, date, or factor vector
#' @return A vector
#' @export
unique_sum <- function(x){
  length(unique(x))
}

#' Creates summary for character variable
#'
#' This function takes input as a data table or data frame.
#' The function creates summary for the character variable.
#'
#' @param data: The data table or data frame
#' @return A data frame
#' @export
char_summary <- function(data){
  data <- data.table(data)
  df_char <- data[, .SD, .SDcols = sapply(data, is.character)]
  if (ncol(df_char) == 0){
    warning("The data table has no character columns")
    return(NULL)
  }

  out_summary <- df_char[,
  .(var_name = names(df_char),
    type = "character",
    num_row = .N,
    miss_num = sapply(.SD, miss_sum, count = 1),
    "miss %" = sapply(.SD, miss_sum, count = .N),
    zero_count = sapply(.SD, zeros_sum, count = 1),
    "zero %" = sapply(.SD, zeros_sum, count = .N),
    distinct = sapply(.SD, unique_sum))]
  return(out_summary)
}

#' Creates summary for factor variable
#'
#' This function takes input as a data table or data frame.
#' The function creates summary for the factor variable.
#'
#' @param data: The data table or data frame
#' @return A data frame
#' @export
factor_summary <- function(data){
  data <- data.table(data)
  df_fac <- data[, .SD, .SDcols = sapply(data, is.factor)]
  if (ncol(df_fac) == 0){
    warning("The data table has no factor columns")
    return(NULL)
  }

  out_summary <- df_fac[,
                         .(var_name = names(df_fac),
                           type = "factor",
                           num_row = .N,
                           miss_num = sapply(.SD, miss_sum, count = 1),
                           "miss %" = sapply(.SD, miss_sum, count = .N),
                           zero_count = sapply(.SD, zeros_sum, count = 1),
                           "zero %" = sapply(.SD, zeros_sum, count = .N),
                           distinct = sapply(.SD, unique_sum))]
  return(out_summary)
}

#' Creates summary for numeric variable
#'
#' This function takes input as a data table or data frame.
#' The function creates summary for the numeric variable.
#'
#' @param data: The data table or data frame
#' @return A data frame
#' @export
num_summary <- function(data){
  data <- data.table(data)
  df_num <- data[, .SD, .SDcols = sapply(data, is.numeric)]
  if (ncol(df_num) == 0){
    warning("The data table has no numeric columns")
    return(NULL)
  }

  out_summary <- df_num[,
  .(var_name = names(df_num),
    type = "numeric",
    num_row = .N,
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
    var_iqr = sapply(.SD, IQR, na.rm = TRUE))]
  return(out_summary)
}

#' Creates summary for date variable
#'
#' This function takes input as a data table or data frame.
#' The function creates summary for the date variable.
#'
#' @param data: The data table or data frame
#' @return A data frame
#' @export
date_summary <- function(data){
  data <- data.table(data)
  df_date <- data[, .SD, .SDcols = sapply(data, is_date)]
  if (ncol(df_date) == 0){
    warning("The data table has no date columns")
    return(NULL)
  }

  out_summary <- df_date[,
  .(var_name = names(df_date),
    type = "date",
    num_row = .N,
    miss_num = sapply(.SD, miss_sum, count = 1),
    "miss %" = sapply(.SD, miss_sum, count = .N),
    zero_count = sapply(.SD, zeros_sum, count = 1),
    "zero %" = sapply(.SD, zeros_sum, count = .N),
    distinct = sapply(.SD, unique_sum),
    var_min = sapply(.SD, min, na.rm = TRUE),
    var_max = sapply(.SD, max, na.rm = TRUE))]
  return(out_summary)
}

#' Creates summary for logical variable
#'
#' This function takes input as a data table or data frame.
#' The function creates summary for the logical variable.
#'
#' @param data: The data table or data frame
#' @return A data frame
#' @export
log_summary <- function(data){
  data <- data.table(data)
  df_log <- data[, .SD, .SDcols = sapply(data, is.logical)]
  if (ncol(df_log) == 0){
    warning("The data table has no logical columns")
    return(NULL)
  }

  out_summary <- df_log[,
  .(var_name = names(df_log),
    type = "logical",
    num_row = .N,
    miss_num = sapply(.SD, miss_sum, count = 1),
    "miss %" = sapply(.SD, miss_sum, count = .N),
    zero_count = sapply(.SD, zeros_sum, count = 1),
    "zero %" = sapply(.SD, zeros_sum, count = .N))]
  return(out_summary)
}

#' Creates the complete summary of data
#'
#' This function takes input as a data table or data frame.
#' The function creates complete summary of the data.
#'
#' @param data: The data table or data frame
#' @return A frame
#' @export
complete_summary <- function(data){
 comp_summ <- list(
   num_var_summary = num_summary(data),
   char_var_summary = char_summary(data),
   date_var_summary = date_summary(data),
   log_var_summary = log_summary(data),
   fac_var_summary = factor_summary(data)
 )

 out <- Reduce(join_data, comp_summ)
return(out)
}

#' Returns a data frame with maximium 5 and minimum 5.
#'
#' This function takes input data.
#' The function calculates the number of missing values in the vector.
#'
#' @param x: The data frame
#' @param variable: String name of the variable.
#' @return A data frame
#' @export
#' 
maxmin_count <- function(data, var){
  y <- data[, .(eval(var))][order(eval(var))]
  out <- data.frame(var_name = var, max5 = y[1:5], min5 = tail(y, 5))
  return(out)
}

#' Returns a data frame with requenct count of date, factor, and character variables.
#'
#' This function takes input data.
#' The function calculates the number of missing values in the vector.
#'
#' @param x: The data frame
#' @param variable: String name of the variable.
#' @return A data table
#' @export
#' 
frequent_count <- function(data, var){
  out <- data[, .(freq = .N), by = .(eval(var))][order("freq")]
  return(out)
}