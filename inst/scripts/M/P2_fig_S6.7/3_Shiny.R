##  The "bivariate local trigonometric"-example with constant phase
##  adjustment, from P2_fig_S6.7

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_S6.7")
data_dir <-
    c(ts.dir = "dmt_bivariate_67cced467f2a10c59a6833285e6b3e4b",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
