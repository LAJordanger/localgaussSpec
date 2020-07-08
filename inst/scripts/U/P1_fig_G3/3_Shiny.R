##  The "local trigonometric" example from P1_fig_G3.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_G3")
data_dir <-
    c(ts.dir = "dmt_b2fecf9f8c798c0c058df84ca025c944",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)

