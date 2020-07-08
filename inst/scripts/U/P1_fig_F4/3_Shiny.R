##  The "local trigonometric" example from P1_fig_F4.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_F4")
data_dir <-
    c(ts.dir = "ebf2ecaa1791c56724dd4490ad408a9e",
      approx.dir = "Approx__1",
      boot.approx.dir = "Boot_Approx_60")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
