##  The EuStockMarkets-example from P2_fig_08.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_08_S3.10")
data_dir <-
    c(ts.dir = "9e59e59f271b88315be95f9e40025f04",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
