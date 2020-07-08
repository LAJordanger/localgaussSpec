###----------------------------------------------------------------###
##  The  apARCH(2,3)-example from P1_fig_D1.

##  This script generates a plot that combines the heatmap-based plot
##  with the corresponding distance-based plot.  The point of interest
##  in this plot is the effect on the estimated local Gaussian spectra
##  as the point v varies along the diagonal.

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

##  Specify the packaces required for this script.

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

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_D1.D2")
..TS  <- "rugarch_ea795134c2c77f252a7abefd2a30cf82"
..Approx <- "Approx__1"

#####------------------------------------------------------------#####

##  Define the 'input'-list that specifies the content of the plot.
##  Some of the information in this list is redundant for the present
##  plot, but it is necessary to update the plot-function before those
##  parts can be removed from the list below.

input <- list(TCS_type = "S",
              window = "Tukey",
              Boot_Approx = "Nothing here to select", 
              confidence_interval = "95",
              levels_Diagonal = 21L, 
              bw_points = "0.5",
              cut = 10L,
              frequency_range = c(0, 0.5), 
              type = "par_five",
              levels_Horizontal = 46,
              TS = ..TS, 
              S_type = "LS_a",
              levels_Line = 46,
              point_type = "on_diag", 
              Approx = ..Approx,
              Vi = "Y",
              Vj = "Y",
              levels_Vertical = 46, 
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

##  Add horizontal lines at the 10%, 50% and 90% percentiles, since those
##  were used for the basic plots in the paper.

heatmap_plot <- heatmap_plot +
    geom_hline(
        yintercept = c(0.1, 0.5, 0.9),
        lty = 2,
        size = 0.2,
        alpha = .5)

##  Adjust the title manually (update code later on), including
##  adjustments of the axes.

heatmap_plot <-
    heatmap_plot +
    ggtitle(label = "Heatmap for the local Gaussian autospectrum") +
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
    theme(axis.ticks = element_line(size = 0.3),
          axis.ticks.length = unit(.06, "cm"),
          axis.text = element_text(size = 6))

##  The annotations must be added back to the plot, but it is better
##  to deal with that part when the other plot also has been created.


#####------------------------------------------------------------#####
#####------------------------------------------------------------#####
#####------------------------------------------------------------#####

##  This part deals with the distance-based plot.  Note that it is a
##  bit messy for the time being.  A latter iteration of the package
##  will contain a function that internalises this.

##  Strategy: Extract the data.

.extracted <- LG_plot_helper(main_dir = ..main_dir, 
                             input = input,
                             input_curlicues = list(
                                 x.label_low_high = c(0, 1),
                                 NC_value = list(short_or_long_label = "short")),
                             .extract_LG_data_only = TRUE)

##  Get hold of some internal details.

.env <- .extracted$..env
.env$look_up <- .extracted$look_up
look_up <- .env$look_up
rm(.extracted)

##  Extract the details related to the annotation of the plot.

annotate_norm <- look_up$curlicues$text

##--------------------------------------------------------------------
## ##  Use the norm-function to compute the different norms and
## ##  distances.
## .L2_norm <- localgaussSpec:::LG_spectrum_norm(
##         C1_env = .env,
##         W1 = localgaussSpec:::myWindows[[look_up$window]](.cut = look_up$cut))
## ##  REMINDER: This is not used yet in the script below, it is included
## ##  here as a placeholder for the time when it is due to adjust this mess.
##--------------------------------------------------------------------

##  Extract the correlations of interest, with a restriction to one of
##  the points of interest.  OBS: The package 'leanRcoding' lives on
##  github, but it is installed when localgaussSpec is installed.

..lag_values <- names(.env[[look_up$cache$weights_f]][[as.character(look_up$cut)]])
..restrict <- list(variable = "rho",
                   bw_points = look_up$bw_points,
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

## Compute the product of the correlations with the weights.

.weighted_data <- leanRcoding::multiply_arrays(
    .arr1 = .data,
    .arr2 = .env[[look_up$cache$weights_f]][[as.character(look_up$cut)]])
.global_weighted_data <- leanRcoding::multiply_arrays(
    .arr1 = .global_data,
    .arr2 = .env[[look_up$cache$weights_f]][[as.character(look_up$cut)]])

##  Next: Compute the norms.  In this "on_diagonal"-case for local
##  Gaussian autocorrelations, the squared norms are obtained by
##  squaring the '.weighted_data', summing over the lags,
##  multiplying with 2 (due to folding) and add 1 (to include the
##  lag-zero-term).  Take the square root to get the norms.

.the_norms <- sqrt(
    1 + 2 * leanRcoding::my_apply(
                X = .weighted_data^2,
                MARGIN = "levels",
                FUN = sum))
.the_global_norm <- sqrt(
    1 + 2 * leanRcoding::my_apply(
                X = .global_weighted_data^2,
                MARGIN = "content",
                FUN = sum))

##  Need to adjust the dimension-names for '.the_norms', i.e. we want
##  to have them replaced with the percentages they correspond to.
##  Recycling som code from 'LG_plot_heatmap'.

.quantile_levels <- vapply(
    X = strsplit(x = dimnames(.the_norms)$levels, 
                 split = "_"), FUN = function(..x) pnorm(as.numeric(..x[1])), 
    FUN.VALUE = numeric(1))
dimnames(.the_norms)$levels <- .quantile_levels
rm(.quantile_levels)

##  Create the data-frame needed for the plot.

.df <- reshape2::melt(data = .the_norms)

##  Specify stuff to be included in the plot.

distance_plot_title <- sprintf("Percentiles vs. norm for the m=%s local Gaussian autospectrum",
                       look_up$m_selected)
.aes_mapping <- aes(x = levels,
                    y = value)
.xlab_expression <- "v = diagonal point/percentage"
.ylab_expression <- ""

##  Create the plot.

distance_plot <- ggplot(data=.df,
                mapping = .aes_mapping) +
    geom_line(size = 0.1,
              colour = "brown") +    
    ##  Adjust legend and labels.
    labs(fill = NULL) +
    xlab(.xlab_expression) +
    ylab(.ylab_expression) +
    ##  Add the title
    ggtitle(label = distance_plot_title) +
    theme(plot.title = element_text(hjust = 0.5,
                                    vjust = 0,
                                    size = 8,
                                    colour = "brown")) +
    ##  Adjust the x-axis.  The values will in this case always be
    ##  percentiles between 0 and 1
    ggplot2::scale_x_continuous(
                 limits = c(0,1),
                 labels = scales::percent)

###  Add points that shows the 'v'-values used in most of the plots of
###  the local Gaussian spectral densities.

.levels <- c(0.10, 0.50, 0.90)
distance_plot <-
    distance_plot +
    annotate(geom = "point",
             x = .levels,
             y = .df$value[which(.df$levels %in% .levels)],
             size = .7,
             shape = 1,
             colour = "blue")
rm(.levels)

##  Add information about the value of the global norm.

.global_label <- sprintf("D*(f^'%s'*(omega)) == '%s'",
                         look_up$m_selected,
                         .the_global_norm)

distance_plot <- distance_plot +
    ##  Add a line for the norm of the global spectrum.
    geom_hline(yintercept = .the_global_norm,
               size = 0.1,
               colour = "red",
               lty = 1)  +
    ##  Add the label with the value.
    annotate(
        geom = "text",
        x = 0,
        y = .the_global_norm,
        size = 2,
        label = .global_label,
        col = "red",
        vjust = -0.3,
        hjust = "inward",
        parse = TRUE)

#####------------------------------------------------------------#####
#####------------------------------------------------------------#####
#####------------------------------------------------------------#####

##  It is time to add the annotations to the plot.  This requires a
##  bit of tweaking of the details since the result should look
##  reasonable when the plots are included in a grid and then saved to
##  file.  Note that it is the saved file that should be inspected in
##  order to figure out if the tweaking has produced a reasonable
##  result.

## Tweak the position of the plot stamp.

annotate_heatmap$annotated$vjust[1] <- 2 * annotate_heatmap$annotated$vjust[1]

##  Tweak the size of the annotated text so it looks decent after the
##  grid-plot has been saved.

.scale <- 0.4

annotate_heatmap$annotated$size <- 
    .scale * annotate_heatmap$annotated$size

annotate_norm$annotated$size <- 
    .scale * annotate_norm$annotated$size

heatmap_plot <-
    heatmap_plot +
    eval(annotate_heatmap$annotated)

##  Adjust the stamp for the distance-based plot.

annotate_norm$annotated$label[1] <-
    sprintf("D*(%s)",
            annotate_norm$annotated$label[1])

distance_plot <-
    distance_plot +
    eval(annotate_norm$annotated)

##  Adjust details related to the axis ticks/text, and also adjust the
##  way the axis-labels are given.

size_v <- annotate_norm$annotated_df["NC_value", "size"] * .scale
v_just_v <- annotate_norm$annotated_df["NC_value", "vjust"]

distance_plot <- distance_plot +
    annotate(geom = "text",
             label = "v",
             parse = TRUE,
             x = Inf,
             y = -Inf,
             size = size_v,
             hjust = "inward",
             vjust = v_just_v) + 
        xlab(label = NULL) +
        theme(axis.ticks = element_line(size = 0.3),
              axis.ticks.length = unit(.06, "cm"),
              axis.text = element_text(size = 6))

#####------------------------------------------------------------#####
#####------------------------------------------------------------#####
#####------------------------------------------------------------#####

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_D1.pdf")
rm(..main_dir, ..TS)


##  This part is used to tweak the ratios between the subplots when
##  they are collected in the grid.

.x <- 25
.y <- 6 
.z <- 4

.heatmap.pos.row <- 1:.y
.plot.pos.row <- .y + 1:.z

pdf(.save_file)

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(.x,3)))
print(heatmap_plot  + 
    theme(legend.key.width = unit(0.15, "cm"),
          legend.text = element_text(size = 4.5)),
      vp = viewport(
          layout.pos.row = .heatmap.pos.row,
          layout.pos.col = 1:2))
print(distance_plot,
      vp = viewport(
          layout.pos.row = .plot.pos.row,
          layout.pos.col = 1:2))

dev.off()

##  Crop the resulting file.  This code works on a Linux-based OS, and
##  it requires that 'pdfcrop' has been installed on the system.
.crop_code <- sprintf("pdfcrop --margins 5 %s %s", .save_file, .save_file)
system(.crop_code)
