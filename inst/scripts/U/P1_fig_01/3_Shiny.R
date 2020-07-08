##  The GARCH(1,1) example from P1_fig_01.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_01")
data_dir <-
    c(ts.dir = "rugarch_3c6864d4791f01f627d73827ecac24f2",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
