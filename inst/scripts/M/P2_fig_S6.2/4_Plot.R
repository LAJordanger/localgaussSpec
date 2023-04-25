###----------------------------------------------------------------###
##  The "bivariate cosine" example from P2_fig_S6.2.

##  This script investigates a complex valued presentation of the data
##  from P2_fig_01.

###----------------------------------------------------------------###

##  In order for this script to work, it is necessary that the script
## '2_Data.R' from P2_fig_01 has been used first.

##  Warning: The code below assumes that '2_Data.R' was used with its
##  initial arguments, i.e. an adjustment of the script that includes
##  additional points might require a modification of this script.

##  Note: The '..TS' value given below originates from the
##  'digest::digest'-function.  This is used in order to keep track of
##  the different simulations, and it is in particular used to avoid
##  the re-computation of a script that has already been executed.  It
##  might alas be the case that this value can be influenced by the
##  random number generator used during the computation, so if the
##  scrips has been used without modifications and the code below
##  returns an error, then it might be necessary to update the
##  '..TS'-value in this script by the one created by the
##  data-generating script.

###----------------------------------------------------------------###

##  Load the required libraries.

library(localgaussSpec)
library(grid)

#####------------------------------------------------------------#####

##  Specify the key arguments that identifies where the data to be
##  investigated can be found.

..main_dir <- c("~", "LG_DATA_scripts", "P2_fig_01_S6.2")
..TS <- "dmt_bivariate_9b04a420df4b6e7ed7e68a96b1b2fc4b"
..Approx <- "Approx__1"

###----------------------------------------------------------------###

##  For this example it is of interest to show the situation for
##  trigonometric example at the frequency where a peak should occur
##  in the local Gaussian spectral density.  In order to get hold of
##  the frequency of interest (without using internal functions) it is
##  necessary to first create a plot related to this case -- since it
##  then is possible to extract the desired frequency from the
##  formals-attribute.  A plot of the time series suffices for this
##  purpose.

.tmp <- LG_plot_helper(
    main_dir = ..main_dir, 
    input = list(
        TCS_type = "T",
        TS = ..TS,
        TS_type_or.pn = "pseudo-normalised",
        TS_restrict = list(
            content = 1)))

##  Extract information about the frequency of interest, and convert
##  it to the desired scale.

.omega <- 1 / (2*pi) * attributes(.tmp)$details$fun_formals$first_dmt$alpha
rm(.tmp)

##  Create a list with the three plots of interest, i.e. one plot
##  showing Cartesian-based confidence intervals, one plot showing
##  polar-based confidence intervals, and one zoomed in version of the
##  point-cloud of interest.

..plot <- list()

for (.cpz in c("c", "p", "z")) {

    input <- list(TCS_type = "S",
                  window = "Tukey",
                  Boot_Approx = NA_character_,
                  confidence_interval = "95",
                  levels_Diagonal = 1L,
                  bw_points = "0.6",
                  cut = 10L,
                  type = "par_five",
                  levels_Horizontal = 2,
                  TS = ..TS,
                  S_type = "LS_c_Co",
                  levels_Line = 2,
                  point_type = "on_diag",
                  Approx = ..Approx,
                  Vi = "Y1",
                  Vj = "Y2",
                  levels_Vertical = 2,
                  global_local = "local",
                  complex = TRUE,
                  complex_c_or_p_or_z = .cpz,
                  complex_frequency = .omega,
                  complex_frequency_print_digits = 3)

    ..plot[[.cpz]] <- LG_plot_helper(
        main_dir = ..main_dir,
        input = input,
        input_curlicues= list(
            NC_value = list(
                short_or_long_label = "short")))

}
rm(..Approx, .cpz, .omega, input)

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P2_fig_S6.2.pdf")
rm(..main_dir, ..TS)

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(1, 3)))
print(..plot$p,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 1))
print(..plot$c,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 2))
print(..plot$z,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 3))

dev.off()

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)
