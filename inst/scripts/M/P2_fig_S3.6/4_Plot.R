###----------------------------------------------------------------###
##  The dmbp-example from P2_fig_S3.6, P2_fig_S3.7 and P2_fig_S3.9

##  This script creates plots that combine the heatmap-based plot with
##  the corresponding distance-based plot.  The point of interest in
##  this plot is the effect on the estimated local Gaussian spectra as
##  the bandwidth b varies.

##  Note that this script in addition to P2_fig_S3.6 also creates
##  P2_fig_S3.7 and P2_fig_S3.9. The default is that P2_fig_S3.6 and
##  P2_fig_S3.9 are created.  In order to create P2_fig_S3.7, change
##  the '.plot_type'-argument from "Cartesian" to "Polar"

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

###----------------------------------------------------------------###

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

..main_dir <- c("~", "LG_DATA_scripts", "P2_fig_S3.6_S3.7_S3.8_S3.9")
..TS  <- "6c8070689177015432b618c37bce0d69"
..Approx <- "Approx__1"

##  Select what kind of plot that should be produced.

.plot_type <- "Cartesian"  ## "Polar"

if (.plot_type  == "Cartesian") {
    ..S_type_pair <- c("LS_c_Co", "LS_c_Quad")
    .save_file_name <- "P2_fig_S3.6.pdf"  ## Co- and Quad-spectra
} else {
    ..S_type_pair <- c("LS_c_amplitude", "LS_c_phase")
    .save_file_name <- "P2_fig_S3.7.pdf" ## Amplitude- and Phase-spectra
}

heatmap_plot_list <- list()

#####------------------------------------------------------------#####

for (..S_type in ..S_type_pair) {
    
    ##  Define the 'input'-list that specifies the content of the plot.
    ##  Some of the information in this list is redundant for the present
    ##  plot, but it is necessary to update the plot-function before those
    ##  parts can be removed from the list below.

    input_common <- list(
        window = "Tukey",
        Boot_Approx = "Nothing here to select", 
        TCS_type = "S",
        confidence_interval = "95",
        bw_points = "0.25",
        cut = 10L,
        frequency_range = c(0, 0.5),
        type = "par_five",
        TS = ..TS,
        S_type = ..S_type,
        point_type = "on_diag", 
        Approx = "Approx__1",
        Vi = "Y1",
        Vj = "Y3",
        global_local = "local",
        spectra_f_or_F = "f",
        drop_annotation = TRUE)

    ##  Initate the lists used to store the different plots, and the
    ##  annotations that is needed.  Note that the annotation is dropped
    ##  in the initially created plots, since it might be necessary to
    ##  adjust the size when they are included in a grid-based setting.

    .names <- c("lower", "center", "upper")
    heatmap_plots <- list()
    heatmap_annotations <- list()
    distance_plots <- list()

    .levels <- 1:3
    .levels_Diagonal <- 1

    for (.levels_Diagonal in .levels) {
        .name <- .names[.levels_Diagonal]
        
        heatmap_plots[[.name]]  <- LG_plot_helper(
            main_dir = ..main_dir,
            input = c(input_common,
                      list(
                          heatmap = TRUE,
                          heatmap_b_or_v = "b",
                          levels_Diagonal = .levels_Diagonal)),
            input_curlicues = list(
                NC_value = list(short_or_long_label = "short")))

        ##  Adjust the title.
        heatmap_plots[[.name]] <-
            heatmap_plots[[.name]] +
            ## ## ## ggtitle(label = "Heatmap for the local Gaussian autospectra") +
            theme(plot.title = element_text(hjust = 0.5,
                                            vjust = 0,
                                            size = 8,
                                            colour = "brown")) +
            ##  And add a horizontal line for the values used as default.
            geom_hline(yintercept = 0.6,
                       lty = 2,
                       lwd = 0.2,
                       alpha = .5)
        
        ##  Store the annotated values.
        heatmap_annotations[[.name]] <- attributes(
            x = heatmap_plots[[.name]])$curlicues$text
        
        ##  The next part deals with the distance-based plot.  Note:
        ##  The present incarnation of the code primarily aims at
        ##  removing internal functions from the scripts, and it is
        ##  thus not possible to tweak the annotations of this plot in
        ##  the same manner used for the heatmap-plot.

        distance_plots[[.name]] <- LG_plot_helper(
            main_dir = ..main_dir,
            input = c(input_common,
                      list(L2_distance_plot = TRUE,
                           L2_distance_vbmL = "b",
                           levels_Diagonal = .levels_Diagonal)),
            input_curlicues = list(
                NC_value = list(short_or_long_label = "short"),
                distance_plot = list(
                    add_points_at_bws = "0.6",
                    size = .7,
                    shape = 1,
                    colour = "blue")))  +
            theme(axis.ticks = element_line(linewidth = 0.3),
                  axis.ticks.length = unit(.06, "cm"),
                  axis.text = element_text(size = 6))

    }


###----------------------------------------------------------------###

    ##  Ensure that the limit on the y-axis is the same for all the
    ##  distance-plots (not an issue for the heatmap-plots).  Procedure:
    ##  For each plot, identify the ranges of the (relevant parts of the)
    ##  data-frames that is included, store this in a list and then
    ##  compute the ranges from this.

    .range_list <- lapply(
        X = distance_plots,
        FUN = function(x) {
            ggplot_build(x)$layout$panel_params[[1]]$y.range
        })

    .range <- range(.range_list)

    ##  Use this to ensure the same ylimit for the plots of the same type.

    for (i in seq_along(distance_plots))
        distance_plots[[i]]$coordinates$limits$y <- .range 
    rm(.range, i, .range_list)


###----------------------------------------------------------------###

    ##  Tweak the size of the annotated stuff so it looks decent after the
    ##  grid-plot has been saved.

    .scale <- 0.4
    for (.name in .names)  {
        heatmap_annotations[[.name]]$annotated$size <- 
            .scale * heatmap_annotations[[.name]]$annotated$size
                                        #
        heatmap_plots[[.name]] <- 
            heatmap_plots[[.name]] +
            eval(heatmap_annotations[[.name]]$annotated)
    }
    rm(.scale)

###----------------------------------------------------------------###

    ##  Adjust the placement of omega and b for the axes for the heatmap
    ##  (in order to get the grid-plot better looking).

    for (.name in .names)  {
        v_just_b <-
            heatmap_annotations[[.name]]$annotated_df["m_value", "vjust"]
        v_just_omega <- - v_just_b
        heatmap_plots[[.name]] <-
            heatmap_plots[[.name]] +
            annotate(geom = "text",
                     label = "omega",
                     parse = TRUE,
                     x = Inf,
                     y = -Inf,
                     size = 2,
                     hjust = "inward",
                     ## vjust = "inward") +
                     vjust = v_just_omega) + 
            annotate(geom = "text",
                     label = "b",
                     parse = TRUE,
                     x = -Inf,
                     y = Inf,
                     size = 2,
                     hjust = "inward",
                     vjust = v_just_b) +
            xlab(label = NULL) +
            ylab(label = NULL) +
            theme(axis.ticks = element_line(linewidth = 0.3),
                  axis.ticks.length = unit(.06, "cm"),
                  axis.text = element_text(size = 6))
    }
    rm(.name, .names, v_just_b, v_just_omega, heatmap_annotations)

    heatmap_plot_list[[..S_type]] <- list(
        heatmap_plots = heatmap_plots,
        distance_plots = distance_plots)
}
rm(..Approx)



#####------------------------------------------------------------#####
#####------------------------------------------------------------#####
#####------------------------------------------------------------#####

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.


.save_file1 <- file.path(paste(c(..main_dir, ..TS),
                               collapse = .Platform$file.sep),
                         .save_file_name)
.save_file2 <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                         "P2_fig_S3.9.pdf")
rm(..main_dir, ..TS, .save_file_name)

##  This part is used to tweak the ratios between the subplots when
##  they are collected in the grid.

.x <- 25
.y <- 6 
.z <- 4

.heatmap.pos.row <- 2:.y
.distance.pos.row <- .y + 1:.z

##  REMINDER: Create a hyprid solution for the grid-title.

pdf(.save_file1)

grid.newpage()
pushViewport(viewport(
    gp = gpar("col" = "brown"),
    layout = grid.layout(1+.x,6)))
for (.i in seq_along(heatmap_plot_list)) {
    print(heatmap_plot_list[[.i]]$heatmap_plots$lower  +
          ggtitle(label = NULL) +
          theme(legend.key.width = unit(0.15, "cm"),
                legend.text = element_text(size = 4.5)),
          vp = viewport(
              layout.pos.row = .heatmap.pos.row,
              layout.pos.col = 1:2 + 2*(.i-1)))
}
rm(.i)
print(heatmap_plot_list[[1]]$distance_plot$lower,
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

.crop_code <- sprintf("pdfcrop --margins 5 %s %s", .save_file1, .save_file1)
system(.crop_code)
rm(.crop_code, .save_file1)


###----------------------------------------------------------------###
###----------------------------------------------------------------###

##  The case P2_fig_S3.9:

pdf(.save_file2)

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(16, 1)))
print(distance_plots$upper,
      vp = viewport(
          layout.pos.row = 1:3,
          layout.pos.col = 1))
print(distance_plots$center,
      vp = viewport(
          layout.pos.row = 4:6,
          layout.pos.col = 1))
print(distance_plots$lower,
      vp = viewport(
          layout.pos.row = 7:9,
          layout.pos.col = 1))

dev.off()

##  Crop the resulting file.  This code works on a Linux-based OS, and
##  it requires that 'pdfcrop' has been installed on the system.

.crop_code <- sprintf("pdfcrop --margins 5 %s %s", .save_file2, .save_file2)
system(.crop_code)
rm(.crop_code, .save_file2)
