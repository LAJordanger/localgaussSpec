##  The apARCH(2,3)-example from P1_fig_11.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_09.11.F1")
data_dir <-
    c(ts.dir = "rugarch_3e08382d945f0567d9f8f5d8e171aa53",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
