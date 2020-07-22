##  The Gaussian white noise example from P1_fig_05.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_05")
data_dir <-
    c(ts.dir = "rnorm_1adb5cb875e8f852b5b33946234d9801",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
