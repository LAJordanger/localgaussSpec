##  The EuStockMarkets-example from P2_fig_07, focusing on the DAX-
##  and CAC-components of the logreturns.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_07_S3.1_S3.2")
data_dir <-
    c(ts.dir = "6c8070689177015432b618c37bce0d69",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
