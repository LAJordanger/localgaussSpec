###----------------------------------------------------------------###
##  The "bivariate local trigonometric"-example with constant phase
##  adjustment, from P2_fig_S6.4 and P2_fig_S6.5

##  This script generates a plot that combines heatmap-based plot for
##  the Co- and Quad-spectra with the corresponding distance-based
##  plot.  The point of interest in this plot is the effect on the
##  estimated local Gaussian spectra as the point v varies along the
##  diagonal.  This example differs from P2_fig_02 since it only
##  consider points in the lower tail.

##  Note that this script in addition to P2_fig_S6.4 also creates
##  P2_fig_S6.5. The default is that P2_fig_S6.4 is created.  In order
##  to create P2_fig_S6.5, change the '.plot_type'-argument from
##  "Cartesian" to "Polar"

#####------------------------------------------------------------#####

##  In order for this script to work, it is necessary that the script
## '2_Data.R' has been used first.

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

#####------------------------------------------------------------#####

##  Specify the packages required for this script.

library(localgaussSpec)
library(ggplot2)
library(grid)

#####------------------------------------------------------------#####

##  Define the directory- and file components needed for the
##  extraction of the data.  The path to the main directory is given
##  as a vector since '.Platform$file.sep' depends on the OS.  Note
##  that these values must correspond to those that are used in the
##  script '2_Data.R', so any modifications there must be mirrored in
##  this script.

..main_dir <- c("~", "LG_DATA_scripts", "P2_fig_S6.4_S6.5_S6.6")
..TS  <- "dmt_bivariate_9cebf62295edc7c1de29a65cf086652a"
..Approx <- "Approx__1"

##  Select what kind of plot that should be produced.

.plot_type <- "Cartesian"  ## "Polar"

if (.plot_type  == "Cartesian") {
    ..S_type_pair <- c("LS_c_Co", "LS_c_Quad")
    .save_file_name <- "P2_fig_S6.4.pdf"  ## Co- and Quad-spectra
} else {
    ..S_type_pair <- c("LS_c_amplitude", "LS_c_phase")
    .save_file_name <- "P2_fig_S6.5.pdf"  ## Amplitude- and Phase-spectra
}

#####------------------------------------------------------------#####

for (..S_type in ..S_type_pair) {

    ##  Define the 'input'-list that specifies the content of the plot.
    ##  Some of the information in this list is redundant for the present
    ##  plot, but it is necessary to update the plot-function before those
    ##  parts can be removed from the list below.

    input <- list(TCS_type = "S",
                  window = "Tukey",
                  Boot_Approx = "Nothing here to select",
                  confidence_interval = "95",
                  levels_Diagonal = 1L,
                  bw_points = "0.4",
                  cut = 10L,
                  frequency_range = c(0, 0.5),
                  type = "par_five",
                  levels_Horizontal = 100,
                  TS = "dmt_bivariate_9cebf62295edc7c1de29a65cf086652a",
                  S_type = ..S_type,
                  levels_Line = 100,
                  point_type = "on_diag",
                  Approx = ..Approx,
                  Vi = "Y1",
                  Vj = "Y2",
                  levels_Vertical = 100,
                  global_local = "local",
                  heatmap = TRUE,
                  heatmap_b_or_v = "v",
                  spectra_f_or_F = "f",
                  drop_annotation = TRUE)

#####------------------------------------------------------------#####
#####------------------------------------------------------------#####
#####------------------------------------------------------------#####

    ##  This part deals with the heatmap-plot.

    heatmap_plot <- LG_plot_helper(
        main_dir = ..main_dir,
        input = input,
        input_curlicues = list(
            NC_value = list(short_or_long_label = "short"),
            title = list(
                label = "Heatmap for the local Gaussian autospectrum",
                element_text = list(
                    size = 8))))

    ##  The present plot is given without annotations.  Those must be
    ##  extracted from the attributes, and added later on after a bit of
    ##  tweaking.  (It is possible to define everything in the
    ##  'input_curlicues' argument, but it is easier to experiment with
    ##  the details by using this approach.)

    annotate_heatmap <- attributes(heatmap_plot)$curlicues$text

    ##  Adjust the limit for this rather extreme case.
    library(ggplot2)
    heatmap_plot <-
        heatmap_plot +
        coord_cartesian(ylim=c(0, 0.035))

    ## Reminder: Do not use 'ylim' to change the axis, since that removes
    ## the percentages from the axis.


    ##--------------------------------------------------------------------
    ##  Code only relevant for the trigonometric examples.  Extract
    ##  information about the frequencies and phase-adjustments.

    alpha <- attributes(heatmap_plot)$details$fun_formals$first_dmt$alpha
    phase_adjustment <- attributes(heatmap_plot)$details$fun_formals$phase_adjustment

    ##  Add the alpha-value (frequencies) as vertical lines
    heatmap_plot <- heatmap_plot +
        geom_vline(
            xintercept = alpha/(2*pi),
            lty = 2,
            lwd = 0.2,
            alpha = .5)
    rm(alpha)
    ##  End of part specific for the local trigonometric examples.
    ##--------------------------------------------------------------------

    ##  Adjust the title manually (update code later on), including
    ##  adjustments of the axes.
    heatmap_plot <-
        heatmap_plot +
        theme(plot.title = element_text(hjust = 0.5,
                                        vjust = 0,
                                        size = 8,
                                        colour = "brown")) +
        annotate(geom = "text",
                 label = "omega",
                 parse = TRUE,
                 x = Inf,
                 y = -Inf,
                 size = 2,
                 hjust = "inward",
                 vjust = "inward") +
        annotate(geom = "text",
                 label = "v",
                 parse = TRUE,
                 x = -Inf,
                 y = Inf,
                 size = 2,
                 hjust = "inward",
                 vjust = "inward") +
        xlab(label = NULL) +
        ylab(label = NULL)  +
        theme(axis.ticks = element_line(linewidth = 0.3),
              axis.ticks.length = unit(.06, "cm"),
              axis.text = element_text(size = 6))

    ##  Add the annotations to the plot.  This requires a bit of tweaking
    ##  of the details since the result should look reasonable when the
    ##  plots are included in a grid and then saved to file.  Note that it
    ##  is the saved file that should be inspected in order to figure out
    ##  if the tweaking has produced a reasonable result.

    ## Tweak the position of the plot stamp.

    annotate_heatmap$annotated$vjust[1] <- 2 * annotate_heatmap$annotated$vjust[1]

    ##  Tweak the size of the annotated text so it looks decent after the
    ##  grid-plot has been saved.

    .scale <- 0.4

    annotate_heatmap$annotated$size <- 
        .scale * annotate_heatmap$annotated$size

    heatmap_plot <-
        heatmap_plot +
        eval(annotate_heatmap$annotated)
    rm(.scale, annotate_heatmap)

    heatmap_plot_list[[..S_type]] <- heatmap_plot

}
rm(..Approx)


#####------------------------------------------------------------#####
#####------------------------------------------------------------#####
#####------------------------------------------------------------#####

##  This part deals with the distance-based plot.  Note: The present
##  incarnation of the code primarily aims at removing internal
##  functions from the scripts, and it is thus not possible to tweak
##  the annotations of this plot in the same manner used for the
##  heatmap-plot.

input$heatmap <- NULL
input$heatmap_b_or_v <- NULL

input$L2_distance_plot <- TRUE
input$L2_distance_vbmL <- "v"

distance_plot <- LG_plot_helper(
    main_dir = ..main_dir,
    input = input,
    input_curlicues = list(
        NC_value = list(short_or_long_label = "short"),
        limits = list(xlim = c(0, 0.035))))  +
    theme(axis.ticks = element_line(linewidth = 0.3),
          axis.ticks.length = unit(.06, "cm"),
          axis.text = element_text(size = 6))
## ## ## rm(input)

#####------------------------------------------------------------#####
#####------------------------------------------------------------#####
#####------------------------------------------------------------#####

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        .save_file_name)
rm(..main_dir, ..TS, .save_file_name)

##  This part is used to tweak the ratios between the subplots when
##  they are collected in the grid.

.x <- 25
.y <- 6
.z <- 4

.heatmap.pos.row <- 2:.y
.distance.pos.row <- .y + 1:.z

##  REMINDER: Create a hyprid solution for the

pdf(.save_file)

grid.newpage()
pushViewport(viewport(
    gp = gpar("col" = "brown"),
    layout = grid.layout(1+.x,6)))
for (.i in seq_along(heatmap_plot_list)) {
    print(heatmap_plot_list[[.i]]  +
          ggtitle(label = NULL) +
          theme(legend.key.width = unit(0.15, "cm"),
                legend.text = element_text(size = 4.5)),
          vp = viewport(
              layout.pos.row = .heatmap.pos.row,
              layout.pos.col = 1:2 + 2*(.i-1)))
}
rm(.i)
print(distance_plot,
      vp = viewport(
          layout.pos.row = .distance.pos.row,
          layout.pos.col = 1:4))

.grid_text <- sprintf("Heatmap and distance plot: %s",
                      paste(gsub(pattern = "LS_c_",
                                 replacement = "",
                                 ..S_type_pair),
                            collapse = " + "
                            ))
grid.text(label = .grid_text,
          vp = viewport(
              layout.pos.row = 1,
              layout.pos.col = 1:4))

dev.off()
rm(.x, .y, .z, .heatmap.pos.row, .distance.pos.row)

##  Crop the resulting file.  This code works on a Linux-based OS, and
##  it requires that 'pdfcrop' has been installed on the system.

.crop_code <- sprintf("pdfcrop --margins 5 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)
