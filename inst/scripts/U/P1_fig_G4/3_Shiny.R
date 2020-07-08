##  The cosine with a tiny bit of noise example from P1_fig_G4.

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_G4")
data_dir <-
    c(ts.dir = "dmt_4d532713e9b2866675cf31b64942ce70",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
