##  The EuStockMarkets-example from P2_fig_S3.6

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_S3.6_S3.7_S3.8_S3.9")
data_dir <-
    c(ts.dir = "6c8070689177015432b618c37bce0d69",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
