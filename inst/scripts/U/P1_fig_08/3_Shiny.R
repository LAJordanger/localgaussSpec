##  The "local trigonometric" example from P1_fig_08.


library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_08.D4")
data_dir <-
    c(ts.dir = "dmt_11bc47958fb4ad5c83f29d7267694368",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
