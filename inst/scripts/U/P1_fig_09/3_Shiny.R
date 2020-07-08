##  The dmbp-example from P1_fig_09.

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_09.11.F1")
data_dir <-
    c(ts.dir = "0fb42549ce13fce773c12b77463bdca8",
      approx.dir = "Approx__1",
      boot.approx.dir = "Boot_Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
