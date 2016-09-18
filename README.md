## expdata: A light weight exploratory data analysis tool using data.table in R.


## Installation

- The package has three dependencies:
 - data.table 
 - moments
 - devtools


- Next, use the following commands to install `expdata`:

  ```s
  library(devtools)
  install_github("r2rahul/expdata")
  ```

- Alternatively, you can download the source code as zip or tar.gz file. Next, unzip or untar the compressed file in the local directory and use `load_all` from devtool package:

  ```s
  library(devtools)
  # Assuming the package directory is in current directory 
  #else provide the complete path
  load_all("expdata")
  ```