expdata: A light weight exploratory data analysis tool using data.table in R.
================

- [expdata: A light weight exploratory data analysis tool using data.table in R.](#expdata-a-light-weight-exploratory-data-analysis-tool-using-datatable-in-r)
  - [Introduction](#introduction)
  - [Installation](#installation)
  - [Example](#example)
  - [Upcoming Features](#upcoming-features)
  - [Footnote](#footnote)

Introduction
------------

The package provides simple methods to get first look into the tidy data, which is organized in the form where each column is a feature and row a unit of observation about the features. The motivation to build the package came from personal use case to get a quick snapshot of data during developing research problem and later to create a standard template inside team to start the discussion about potential business insights (At BlackRock our goal was to brainstorm possible investment insights.) that can be derived from the data. The objective was to create a unified template, which each team member is familiar. So, we can kick start the brainstorming session to access the potential impact of the data analysis. Accordingly, the package will fit the use case where:

+ A standard template for initial snapshots of the data, like missing values, trend, distribution, and similar data attributes.  
+ Using package codes, which are self-contained for individual workflow as per own use case. 
+ A quick visual snapshot of the data, the package now offers a Shiny App to explore the data.  

The methods are implemented using **data.table** package so that the package can handle large data sets. The implemented methods can automatically detect variable type and create the corresponding summary as a data frame. The currently implemented methods detect following variable types:

-   Numeric

-   Character

-   Factor

-   Logical

-   Date

The package is under active development, check upcoming section for next list of features. Please refer to Example section for usage example and vignettes for detail notes on the package usage. 

Installation
------------

-   The package has three dependencies:
-   data.table
-   moments
-   devtools

-   Next, use the following commands to install `expdata`:

``` r
library(devtools)
install_github("r2rahul/expdata")
# To build Vignettes at the time of installation
install_github("r2rahul/expdata", build_vignettes = TRUE)
```

-   Alternatively, you can download the source code as zip or tar.gz file. Next, unzip or untar the compressed file in the local directory and use `load_all` from devtool package:

``` r
library(devtools)
# Assuming the package reside in current directory 
#else provide the complete path
load_all("expdata")
```

Example
-------

Let us explore the package using `hflights` data.

``` r
library(expdata)
library(hflights)
```

To generate numerical variable summary.

``` r
num_summary(hflights)
```

    ##              var_name    type num_row miss_num     miss % zero_count
    ##  1:              Year numeric  227496        0 0.00000000          0
    ##  2:             Month numeric  227496        0 0.00000000          0
    ##  3:        DayofMonth numeric  227496        0 0.00000000          0
    ##  4:         DayOfWeek numeric  227496        0 0.00000000          0
    ##  5:           DepTime numeric  227496     2905 0.01276946       2905
    ##  6:           ArrTime numeric  227496     3066 0.01347716       3066
    ##  7:         FlightNum numeric  227496        0 0.00000000          0
    ##  8: ActualElapsedTime numeric  227496     3622 0.01592116       3622
    ##  9:           AirTime numeric  227496     3622 0.01592116       3622
    ## 10:          ArrDelay numeric  227496     3622 0.01592116      10585
    ## 11:          DepDelay numeric  227496     2905 0.01276946      20792
    ## 12:          Distance numeric  227496        0 0.00000000          0
    ## 13:            TaxiIn numeric  227496     3066 0.01347716       3066
    ## 14:           TaxiOut numeric  227496     2947 0.01295407       2947
    ## 15:         Cancelled numeric  227496        0 0.00000000     224523
    ## 16:          Diverted numeric  227496        0 0.00000000     226847
    ##         zero % quant 0% quant 25% quant 75% quant 100%       var_av
    ##  1: 0.00000000     2011      2011      2011       2011 2.011000e+03
    ##  2: 0.00000000        1         4         9         12 6.513662e+00
    ##  3: 0.00000000        1         8        23         31 1.573745e+01
    ##  4: 0.00000000        1         2         6          7 3.947691e+00
    ##  5: 0.01276946        1      1021      1801       2400 1.395756e+03
    ##  6: 0.01347716        1      1215      1953       2400 1.578254e+03
    ##  7: 0.00000000        1       855      2755       7290 1.961663e+03
    ##  8: 0.01592116       34        77       165        575 1.293237e+02
    ##  9: 0.01592116       11        58       141        549 1.081423e+02
    ## 10: 0.04652829      -70        -8        11        978 7.094334e+00
    ## 11: 0.09139501      -33        -3         9        981 9.444951e+00
    ## 12: 0.00000000       79       376      1042       3904 7.877832e+02
    ## 13: 0.01347716        1         4         7        165 6.098855e+00
    ## 14: 0.01295407        1        10        18        163 1.509110e+01
    ## 15: 0.98693164        0         0         0          1 1.306836e-02
    ## 16: 0.99714720        0         0         0          1 2.852797e-03
    ##           var_sd var_med var_min var_max   var_sum var_kurtosis
    ##  1: 0.000000e+00    2011    2011    2011 457494456          NaN
    ##  2: 3.417676e+00       7       1      12   1481832 -0.003416037
    ##  3: 8.782705e+00      16       1      31   3580206  0.004665638
    ##  4: 1.990272e+00       4       1       7    898084  0.027977358
    ##  5: 4.483222e+02    1416       1    2400 313474149 -0.027920683
    ##  6: 4.724017e+02    1617       1    2400 354207569 -0.378208712
    ##  7: 1.430793e+03    1696       1    7290 446270518  0.748694257
    ##  8: 5.928584e+01     128      34     575  28952224  0.871237158
    ##  9: 5.655523e+01     107      11     549  24210257  0.971752501
    ## 10: 3.070852e+01       0     -70     978   1588237  5.038974613
    ## 11: 2.880361e+01       0     -33     981   2121251  5.972389671
    ## 12: 4.536806e+02     809      79    3904 179217537  0.914340659
    ## 13: 3.961069e+00       5       1     165   1368766  5.578754943
    ## 14: 7.740373e+00      14       1     163   3388691  3.262540818
    ## 15: 1.135678e-01       0       0       1      2973  8.575192568
    ## 16: 5.333546e-02       0       0       1       649 18.642317153
    ##     var_skewness var_iqr
    ##  1:          NaN       0
    ##  2:     1.822607       5
    ##  3:     1.809957      15
    ##  4:     1.790369       4
    ##  5:     1.911806     780
    ##  6:     2.826492     738
    ##  7:     3.034459    1900
    ##  8:     5.223369      88
    ##  9:     5.841207      83
    ## 10:    58.570951      19
    ## 11:    74.899972      12
    ## 12:     6.322568     666
    ## 13:    89.040041       3
    ## 14:    29.987101       8
    ## 15:    74.533928       0
    ## 16:   348.535989       0

For complete summary

``` r
complete_summary(hflights)
```

    ## Warning in date_summary(data): The data table has no date columns

    ## Warning in log_summary(data): The data table has no logical columns

    ## Warning in factor_summary(data): The data table has no factor columns

    ##              var_name      type num_row miss_num     miss % zero_count
    ##  1: ActualElapsedTime   numeric  227496     3622 0.01592116       3622
    ##  2:           AirTime   numeric  227496     3622 0.01592116       3622
    ##  3:          ArrDelay   numeric  227496     3622 0.01592116      10585
    ##  4:           ArrTime   numeric  227496     3066 0.01347716       3066
    ##  5:  CancellationCode character  227496        0 0.00000000          0
    ##  6:         Cancelled   numeric  227496        0 0.00000000     224523
    ##  7:         DayOfWeek   numeric  227496        0 0.00000000          0
    ##  8:        DayofMonth   numeric  227496        0 0.00000000          0
    ##  9:          DepDelay   numeric  227496     2905 0.01276946      20792
    ## 10:           DepTime   numeric  227496     2905 0.01276946       2905
    ## 11:              Dest character  227496        0 0.00000000          0
    ## 12:          Distance   numeric  227496        0 0.00000000          0
    ## 13:          Diverted   numeric  227496        0 0.00000000     226847
    ## 14:         FlightNum   numeric  227496        0 0.00000000          0
    ## 15:             Month   numeric  227496        0 0.00000000          0
    ## 16:            Origin character  227496        0 0.00000000          0
    ## 17:           TailNum character  227496        0 0.00000000          0
    ## 18:            TaxiIn   numeric  227496     3066 0.01347716       3066
    ## 19:           TaxiOut   numeric  227496     2947 0.01295407       2947
    ## 20:     UniqueCarrier character  227496        0 0.00000000          0
    ## 21:              Year   numeric  227496        0 0.00000000          0
    ##              var_name      type num_row miss_num     miss % zero_count
    ##         zero % quant 0% quant 25% quant 75% quant 100%       var_av
    ##  1: 0.01592116       34        77       165        575 1.293237e+02
    ##  2: 0.01592116       11        58       141        549 1.081423e+02
    ##  3: 0.04652829      -70        -8        11        978 7.094334e+00
    ##  4: 0.01347716        1      1215      1953       2400 1.578254e+03
    ##  5: 0.00000000       NA        NA        NA         NA           NA
    ##  6: 0.98693164        0         0         0          1 1.306836e-02
    ##  7: 0.00000000        1         2         6          7 3.947691e+00
    ##  8: 0.00000000        1         8        23         31 1.573745e+01
    ##  9: 0.09139501      -33        -3         9        981 9.444951e+00
    ## 10: 0.01276946        1      1021      1801       2400 1.395756e+03
    ## 11: 0.00000000       NA        NA        NA         NA           NA
    ## 12: 0.00000000       79       376      1042       3904 7.877832e+02
    ## 13: 0.99714720        0         0         0          1 2.852797e-03
    ## 14: 0.00000000        1       855      2755       7290 1.961663e+03
    ## 15: 0.00000000        1         4         9         12 6.513662e+00
    ## 16: 0.00000000       NA        NA        NA         NA           NA
    ## 17: 0.00000000       NA        NA        NA         NA           NA
    ## 18: 0.01347716        1         4         7        165 6.098855e+00
    ## 19: 0.01295407        1        10        18        163 1.509110e+01
    ## 20: 0.00000000       NA        NA        NA         NA           NA
    ## 21: 0.00000000     2011      2011      2011       2011 2.011000e+03
    ##         zero % quant 0% quant 25% quant 75% quant 100%       var_av
    ##           var_sd var_med var_min var_max   var_sum var_kurtosis
    ##  1: 5.928584e+01     128      34     575  28952224  0.871237158
    ##  2: 5.655523e+01     107      11     549  24210257  0.971752501
    ##  3: 3.070852e+01       0     -70     978   1588237  5.038974613
    ##  4: 4.724017e+02    1617       1    2400 354207569 -0.378208712
    ##  5:           NA      NA      NA      NA        NA           NA
    ##  6: 1.135678e-01       0       0       1      2973  8.575192568
    ##  7: 1.990272e+00       4       1       7    898084  0.027977358
    ##  8: 8.782705e+00      16       1      31   3580206  0.004665638
    ##  9: 2.880361e+01       0     -33     981   2121251  5.972389671
    ## 10: 4.483222e+02    1416       1    2400 313474149 -0.027920683
    ## 11:           NA      NA      NA      NA        NA           NA
    ## 12: 4.536806e+02     809      79    3904 179217537  0.914340659
    ## 13: 5.333546e-02       0       0       1       649 18.642317153
    ## 14: 1.430793e+03    1696       1    7290 446270518  0.748694257
    ## 15: 3.417676e+00       7       1      12   1481832 -0.003416037
    ## 16:           NA      NA      NA      NA        NA           NA
    ## 17:           NA      NA      NA      NA        NA           NA
    ## 18: 3.961069e+00       5       1     165   1368766  5.578754943
    ## 19: 7.740373e+00      14       1     163   3388691  3.262540818
    ## 20:           NA      NA      NA      NA        NA           NA
    ## 21: 0.000000e+00    2011    2011    2011 457494456          NaN
    ##           var_sd var_med var_min var_max   var_sum var_kurtosis
    ##     var_skewness var_iqr distinct
    ##  1:     5.223369      88       NA
    ##  2:     5.841207      83       NA
    ##  3:    58.570951      19       NA
    ##  4:     2.826492     738       NA
    ##  5:           NA      NA        5
    ##  6:    74.533928       0       NA
    ##  7:     1.790369       4       NA
    ##  8:     1.809957      15       NA
    ##  9:    74.899972      12       NA
    ## 10:     1.911806     780       NA
    ## 11:           NA      NA      116
    ## 12:     6.322568     666       NA
    ## 13:   348.535989       0       NA
    ## 14:     3.034459    1900       NA
    ## 15:     1.822607       5       NA
    ## 16:           NA      NA        2
    ## 17:           NA      NA     3320
    ## 18:    89.040041       3       NA
    ## 19:    29.987101       8       NA
    ## 20:           NA      NA       15
    ## 21:          NaN       0       NA
    ##     var_skewness var_iqr distinct

Upcoming Features
-----------------

-   A Dashboard to:

    - Visualize data summary

Footnote
-----------------
There are other packages, which offers this kind of functionality. For example, [Skimr](https://github.com/ropensci/skimr) provided a feature to take a snapshot of the data quickly. Another very useful package [Hmsic](https://github.com/harrelfe/Hmisc) also has the functionality to generate pdf reports. My thought process to build this kind of package started during post-doctoral work, when I realized I am using the same set of codes to get an initial assessment of the data. So, decide to wrap around into a package for personal use.  R was under of rapid adoption during my postdoctoral time, and no package fit my use case. So first this was developed for own lab use and later decide to make it open. As I chart along jobs, I will have a universal copy of codes for the intital exploratory data analysis. 
