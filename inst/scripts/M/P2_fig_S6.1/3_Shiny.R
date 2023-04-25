##  The bivariate Gaussian example from P2_fig_S6.1.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_S6.1")
data_dir <-
    c(ts.dir = "rmvnorm_1b6348699b1f40c0505b754c9603d93a",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
