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

##  WARNING: The distance-part of this script contains solutions based
##  on internal code from the 'localgaussSpec'-package.  This implies
##  that it almost certainly will have to be updated at some point.
##  The plan is to implement a proper function for this task, and the
##  mess in this script will then be replaced with proper code.

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

##  Initate the lists used to store the different plots, and the
##  annotations that is needed.  Note that the annotation is dropped
##  in the initially created plots, since it might be necessary to
##  adjust the size when they are included in a grid-based setting.


.names <- c("lower", "center", "upper")
heatmap_plots <- list()
heatmap_annotations <- list()
distance_plots <- list()
distance_annotations <- list()

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
    
    ##  The solution for the distance-based plots is a bit messy,
    ##  since it is based on code extracted from other functions.
    ##  This will hopefully be resolved in a future update.

    .extracted <- localgaussSpec:::LG_plot_helper_extract_data_only(
            main_dir = ..main_dir,
            input =
                c(input_common,
                  list(L2_distance_normal = TRUE,
                       L2_inspection_vbmL = "b",
                       levels_Diagonal = .levels_Diagonal)),
            input_curlicues = list(
                x.label_low_high = c(0, 1.5),
                b_value = list(include = FALSE),
                NC_value = list(short_or_long_label = "short")))

    distance_annotations[[.name]] <-
        .extracted$look_up$curlicues$text

    ##  Adjust the extracted environment so the internal function
    ##  'LG_spectrum_norm' can digest it.

    .env <- .extracted$..env
    .env$look_up <- .extracted$look_up
    look_up <- .env$look_up
    rm(.extracted)

    ##  Use the norm-function to compute the different norms and
    ##  distances.
    
    ## .L2_norm <- localgaussSpec:::LG_spectrum_norm(
    ##                                  C1_env = .env,
    ##                                  W1 = localgaussSpec:::myWindows[[look_up$window]](.cut = look_up$cut))

    ## ##  REMINDER: This is not used yet in the script below, it is
    ## ##  included here as a placeholder for the time when it is due to
    ## ##  adjust this mess.
    
    #####  Attempting at geting stuff done in the same way as for the
    #####  levels/points versus the L2-norm.

    cache <- look_up$cache

    ##  For bandwidth-versus-norm: The '.env'-targets of interest are
    ##  contained in ".LGC_array_local" and cache$weights_f.  The structure
    ##  of 'weights_f' imply that the restriction of the data should be
    ##  taken with regard to the selected m-truncation.  Reminder: The
    ##  names in the '.weight'-list refer to the 'cut'-value, which is
    ##  one higher than the 'm'-value.
    ###############
    ##  Need the global data too, in order for the plot to contain a
    ##  horisontal line that shows the norm of the global spectrum.
    ###############

    ##  First: Extract the correlations of interest, with a
    ##  restriction to one of the points of interest.
    
    ..lag_values <- names(.env[[cache$weights_f]][[as.character(look_up$cut)]])
    ..restrict <- list(variable = "rho",
                       levels = look_up$levels_point,
                       lag = ..lag_values)
    .data <- leanRcoding::restrict_array(
        .arr = .env[["LGC_array_local"]]$on_diag,
        .restrict = ..restrict,
        .drop = TRUE,
        .never_drop = c("lag", "bw_points"))
    .global_data <- leanRcoding::restrict_array(
        .arr = .env[["LGC_array_global"]],
        .restrict = list(TS = "TS_for_analysis",
                         lag = ..lag_values),
        .drop = TRUE,
        .never_drop = c("lag", "content"))
    rm(..lag_values, ..restrict)

    ## Next: Compute the product of the correlations with the weights.

    .weighted_data <- leanRcoding::multiply_arrays(
        .arr1 = .data,
        .arr2 = .env[[cache$weights_f]][[as.character(look_up$cut)]])
    .global_weighted_data <- leanRcoding::multiply_arrays(
        .arr1 = .global_data,
        .arr2 = .env[[cache$weights_f]][[as.character(look_up$cut)]])

    ##  Next: Compute the norms.  In this "on_diagonal"-case for local
    ##  Gaussian autocorrelations, the squared norms are obtained by
    ##  squaring the '.weighted_data', summing over the lags,
    ##  multiplying with 2 (due to folding) and add 1 (to include the
    ##  lag-zero-term).  Take the square root to get the norms.

    .the_norms <- sqrt(
        1 + 2 * leanRcoding::my_apply(
                    X = .weighted_data^2,
                    MARGIN = "bw_points",
                    FUN = sum))
    .the_global_norm <- sqrt(
        1 + 2 * leanRcoding::my_apply(
                    X = .global_weighted_data^2,
                    MARGIN = "content",
                    FUN = sum))

    ##  Create the data-frame needed for the plot.

    .df <- reshape2::melt(data = .the_norms)

    ##  Specify stuff to be included in the plot.
    .plot_title <- sprintf("Bandwidth vs. norm for the m=%s local Gaussian autospectrum",
                           look_up$m_selected)
    .aes_mapping <- aes(x = bw_points,
                        y = value)
    .xlab_expression <- "b"
    .ylab_expression <- ""

    library(ggplot2)

    ##  Create the plot.
    .plot <- ggplot(data=.df,
                    mapping = .aes_mapping) +
        geom_line(size = 0.1,
                  colour = "brown") +    
        ##  Adjust legend and labels.
        labs(fill = NULL) +
        xlab(.xlab_expression) +
        ylab(.ylab_expression) +
        ##  Add the title
        ggtitle(label = .plot_title) +
        theme(plot.title = element_text(hjust = 0.5,
                                        vjust = 0,
                                        size = 8,
                                        colour = "brown")) +
        ##  Add a line for the norm of the global spectrum.
        geom_hline(yintercept = .the_global_norm,
                   size = 0.1,
                   colour = "red",
                   lty = 1)

    ###  Add a point that shows the 'b'-value used in most of the
    ###  plots of the local Gaussian spectral densities.

    .plot <-
        .plot +
        annotate(geom = "point",
                 x = 0.5,
                 y = .df$value[which(.df$bw_points == "0.5")],
                 size = .7,
                 shape = 1,
                 colour = "blue")

    ##  Add information about the value of the global norm.
    .global_label <- sprintf("D*(f^'%s'*(omega)) == '%s'",
                             look_up$m_selected,
                             .the_global_norm)
    
    .plot <- .plot +
        annotate(
            geom = "text",
            x = 0,
            y = .the_global_norm,
            size = 2,
            label = .global_label,
            col = "red",
            vjust = -0.2,
            hjust = "inward",
            parse = TRUE)
    ##  Update the 'distance_plots'
    distance_plots[[.name]] <- .plot
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
    distance_annotations[[.name]]$annotated$size <- 
        .scale * distance_annotations[[.name]]$annotated$size
    #
    heatmap_plots[[.name]] <- 
        heatmap_plots[[.name]] +
        eval(heatmap_annotations[[.name]]$annotated)
    distance_plots[[.name]] <- 
        distance_plots[[.name]] +
        eval(distance_annotations[[.name]]$annotated)
}

###----------------------------------------------------------------###

##  Adjust the placement of omega and b for the axes for the heatmap
##  (in order to get the grid-plot better looking).

for (.name in .names)  {
    v_just_b <-
        heatmap_annotations[[.name]]$annotated_df["m_value", "vjust"]
    v_just_omega <- - v_just_b
        ## heatmap_annotations[[.name]]$annotated_df["NC_value", "vjust"]
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
        theme(axis.ticks = element_line(size = 0.3),
              axis.ticks.length = unit(.06, "cm"),
              axis.text = element_text(size = 6))
}


###----------------------------------------------------------------###

##  Adjust the placement of omega and b for the axes for the
##  distance-plot (in order to get the grid-plot better looking).

for (.name in .names)  {
    size_b <-
        heatmap_annotations[[.name]]$annotated_df["NC_value", "size"]
    v_just_b <- 
        heatmap_annotations[[.name]]$annotated_df["NC_value", "vjust"]
        ## heatmap_annotations[[.name]]$annotated_df["NC_value", "vjust"]
    distance_plots[[.name]] <-
        distance_plots[[.name]] +
        annotate(geom = "text",
                 label = "b",
                 parse = TRUE,
                 x = Inf,
                 y = -Inf,
                 size = 2,
                 hjust = "inward",
                 vjust = v_just_b) + 
        xlab(label = NULL) +
        theme(axis.ticks = element_line(size = 0.3),
              axis.ticks.length = unit(.06, "cm"),
              axis.text = element_text(size = 6))
}

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
