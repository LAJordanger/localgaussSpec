##  The "bivariate local trigonometric"-example with constant phase
##  adjustment, from P2_fig_S6.4

library(localgaussSpec)

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_S6.4_S6.5_S6.6")
data_dir <-
    c(ts.dir = "dmt_bivariate_9cebf62295edc7c1de29a65cf086652a",
      approx.dir = "Approx__1")

LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir)
