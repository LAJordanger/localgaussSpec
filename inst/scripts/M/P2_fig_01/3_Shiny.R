##  The "bivariate cosine" example from P2_fig_01.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_01_S6.2")
data_dir <-
    c(ts.dir = "dmt_bivariate_9b04a420df4b6e7ed7e68a96b1b2fc4b",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
