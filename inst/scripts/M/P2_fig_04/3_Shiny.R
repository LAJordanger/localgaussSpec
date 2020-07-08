##  The "local trigonometric" example from P2_fig_04.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_04.05")
data_dir <-
    c(ts.dir = "dmt_bivariate_6a6f6d4cbc41e96083d61d26469b7ac6",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
