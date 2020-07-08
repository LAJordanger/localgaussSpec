##  The bivariate Gaussian example from P2_fig_01.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_01")
data_dir <-
    c(ts.dir = "rmvnorm_41b3559fddd457ca16acb737b7b9d85d",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
