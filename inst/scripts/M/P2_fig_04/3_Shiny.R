##  The "bivariate local trigonometric"-example with individual hase
##  adjustment, from P2_fig_04.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_04_S3.4")
data_dir <-
    c(ts.dir = "dmt_bivariate_79e20882a99a22d412f6d372310bb0ad",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
