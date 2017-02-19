## ----init_libs-----------------------------------------------------------
library(nycflights13)
library(expdata)

## ----f_sum---------------------------------------------------------------
sum_out <- complete_summary(weather)
knitr::kable(sum_out)

## ----num_out-------------------------------------------------------------
num_out <- num_summary(flights)
knitr::kable(num_out)

## ----char_out------------------------------------------------------------
char_out <- char_summary(flights)
knitr::kable(char_out)

## ----date_out------------------------------------------------------------
date_out <- date_summary(flights)
knitr::kable(date_out)

## ----log_out-------------------------------------------------------------
log_out <- log_summary(flights)
knitr::kable(log_out)

## ----fac_out-------------------------------------------------------------
fac_out <- factor_summary(flights)
knitr::kable(fac_out)

