##  The EuStockMarkets-example from P2_fig_S5.7.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_S5.7_S5.8")
data_dir <-
    c(ts.dir = "df53fd2fa601bb8edc2f610a40d91b5b",
      approx.dir = "Approx__1",
      boot.approx.dir = "Boot_Approx_60")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
