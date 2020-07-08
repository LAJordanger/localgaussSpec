##  The "bivariate cosine" example from P2_fig_02.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_02.03")
data_dir <-
    c(ts.dir = "dmt_bivariate_e65da95d163c4f4df65813aee851ec99",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
