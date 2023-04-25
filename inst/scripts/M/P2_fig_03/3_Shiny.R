##  The "local trigonometric" example from P2_fig_03.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_03_S6.3")
data_dir <-
    c(ts.dir = "dmt_bivariate_0fa441a13f46c6a5f53484556b81ef25",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
