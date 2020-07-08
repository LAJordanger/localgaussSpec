##  The cosine with some noise example from P1_fig_06.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_06")
data_dir <-
    c(ts.dir = "dmt_48189c8a2232981eaa19be8c457777ae",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
