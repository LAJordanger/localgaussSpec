##  The "local trigonometric" example from P1_fig_07.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_07.G1")
data_dir <-
    c(ts.dir = "dmt_3a0010ae2dde5bfcd94c7fee2611292a",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
