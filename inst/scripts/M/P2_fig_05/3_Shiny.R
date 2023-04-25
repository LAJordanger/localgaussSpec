##  The "local trigonometric" example from P2_fig_05.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_05")
data_dir <-
    c(ts.dir = "dmt_bivariate_74ddf479f01f769040ce7c626e35cca1",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
