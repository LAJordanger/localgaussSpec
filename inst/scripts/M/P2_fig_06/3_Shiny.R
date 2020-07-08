##  The "local trigonometric" example from P2_fig_06.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_06")
data_dir <-
    c(ts.dir = "dmt_bivariate_73ef14e659ac59cb4c93690a16bc158c",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
