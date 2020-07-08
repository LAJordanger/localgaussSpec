##  The cGARCH-example from P2_fig_11.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_11.12")
data_dir <-
    c(ts.dir = "sample_rmgarch_3ba0572c315f69b5b54c81752ff6068c",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
