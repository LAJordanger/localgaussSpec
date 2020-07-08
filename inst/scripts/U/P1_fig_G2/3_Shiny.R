##  The "local trigonometric" example from P1_fig_G2.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_G2")
data_dir <-
    c(ts.dir = "dmt_de89eb62d39c815b116675e1605cb8c6",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
