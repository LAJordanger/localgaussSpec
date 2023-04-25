##  The "bivariate local trigonometric"-example with constant phase
##  adjustment, from P2_fig_02.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_02_S3.3")
data_dir <-
    c(ts.dir = "dmt_bivariate_10b7b108a597d6c54a0ab04dc7e36ff9",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
