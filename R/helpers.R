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
#'@import rbokeh, rmarkdown
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