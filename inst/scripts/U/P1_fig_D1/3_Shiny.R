##  The  apARCH(2,3)-example from P1_fig_D1.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_D1.D2")
data_dir <-
    c(ts.dir = "rugarch_ea795134c2c77f252a7abefd2a30cf82",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
