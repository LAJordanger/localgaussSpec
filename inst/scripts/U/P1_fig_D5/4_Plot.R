###----------------------------------------------------------------###
##  The dmbp-example from P1_fig_D5 and P1_fig_D7

##  This script creates plots that combine the heatmap-based plot with
##  the corresponding distance-based plot.  The point of interest in
##  this plot is the effect on the estimated local Gaussian spectra as
##  the bandwidth b varies.

##  Note that this script in addition to P1_fig_D5 also creates
##  P1_fig_D7.

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

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_D5.D6.D7")
..TS  <- "0fb42549ce13fce773c12b77463bdca8"
..Approx <- "Approx__1"

#####------------------------------------------------------------#####

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
    TS = "0fb42549ce13fce773c12b77463bdca8", 
    S_type = "LS_a",
    point_type = "on_diag", 
    Approx = "Approx__1",
    Vi = "Y",
    Vj = "Y",
    global_local = "local",
    spectra_f_or_F = "f",
    drop_annotation = TRUE)
rm(..Approx)

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
        ggtitle(label = "Heatmap for the local Gaussian autospectra") +
        theme(plot.title = element_text(hjust = 0.5,
                                        vjust = 0,
                                        size = 8,
                                        colour = "brown")) +
        ##  And add a horizontal line for the values used as default.
        geom_hline(yintercept = 0.5,
                   lty = 2,
                   size = 0.2,
                   alpha = .5)
        
    ##  Store the annotated values.
    heatmap_annotations[[.name]] <- attributes(
        x = heatmap_plots[[.name]])$curlicues$text
    
    ##  The next part deals with the distance-based plot.  Note: The
    ##  present incarnation of the code primarily aims at removing
    ##  internal functions from the scripts, and it is thus not
    ##  possible to tweak the annotations of this plot in the same
    ##  manner used for the heatmap-plot.

    distance_plots[[.name]] <- LG_plot_helper(
        main_dir = ..main_dir,
        input = c(input_common,
                  list(L2_distance_plot = TRUE,
                       L2_distance_vbmL = "b",
                       levels_Diagonal = .levels_Diagonal)),
        input_curlicues = list(
            NC_value = list(short_or_long_label = "short"),
            distance_plot = list(
                add_points_at_bws = "0.5",
                size = .7,
                shape = 1,
            colour = "blue")))  +
        theme(axis.ticks = element_line(linewidth = 0.3),
              axis.ticks.length = unit(.06, "cm"),
              axis.text = element_text(size = 6))

}
rm(.levels_Diagonal, .levels, input_common)

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

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

##  This script need two save files.

.save_file1 <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_D5.pdf")
.save_file2 <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_D7.pdf")
rm(..main_dir, ..TS)

###----------------------------------------------------------------###
###----------------------------------------------------------------###

##  The case P1_fig_D5:

##  Some tweaking that governs the ratios between the subplots.

.x <- 25
.y <- 6
.z <- 4

.heatmap.pos.row <- 1:.y
.distance.pos.row <- .y + 1:.z

pdf(.save_file1)

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(.x,3)))
print(heatmap_plots$lower  + 
    theme(legend.key.width = unit(0.15, "cm"),
          legend.text = element_text(size = 4.5)),
      vp = viewport(
          layout.pos.row = .heatmap.pos.row,
          layout.pos.col = 1:2))
print(distance_plots$lower,
      vp = viewport(
          layout.pos.row = .distance.pos.row,
          layout.pos.col = 1:2))

dev.off()
rm(.x, .y, .z, .heatmap.pos.row, .distance.pos.row)

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file1, .save_file1)
system(.crop_code)
rm(.crop_code, .save_file1)

###----------------------------------------------------------------###
###----------------------------------------------------------------###

##  The case P1_fig_D7:

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

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file2, .save_file2)
system(.crop_code)
rm(.crop_code, .save_file2)
