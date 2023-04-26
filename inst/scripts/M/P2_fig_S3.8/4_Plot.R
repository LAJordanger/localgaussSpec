###----------------------------------------------------------------###
##  The EuStockMarkets-example from P2_fig_S3.8, focusing on the DAX-
##  and CAC-components of the logreturns.

##  This script generates a heatmap-based plot that investigates the
##  effect on the estimated local Gaussian cross-correlations as the
##  bandwidth varies.

#####------------------------------------------------------------#####

##  In order for this script to work, it is necessary that the script
## '2_Data.R' from P2_fig_S3.6 has been used first.

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

..main_dir <- c("~", "LG_DATA_scripts", "P2_fig_S3.6_S3.7_S3.8_S3.9")
..TS  <- "6c8070689177015432b618c37bce0d69"
..Approx <- "Approx__1"

#####------------------------------------------------------------#####

##  Define the 'input'-list that specifies the content of the plot.
##  Some of the information in this list is redundant for the present
##  plot, but it is necessary to update the plot-function before those
##  parts can be removed from the list below.

input_common <- list(
    window = "Tukey",
    Boot_Approx = "Nothing here to select", 
    TCS_type = "C",
    confidence_interval = "95",
    bw_points = "0.25",
    cut = 10L,
    frequency_range = c(0, 0.5),
    type = "par_five",
    TS = ..TS, 
    S_type = "LS_a",
    point_type = "on_diag", 
    Approx = ..Approx,
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
        ggtitle(label = "Heatmap for the local Gaussian cross-correlations") +
        theme(plot.title = element_text(hjust = 0.5,
                                        vjust = 0,
                                        size = 8,
                                        colour = "brown")) +
        ##  And add a horizontal line for the values used as default.
        geom_hline(yintercept = 0.5,
                   lty = 2,
                   lwd = 0.2,
                   alpha = .5)
        
    ##  Store the annotated values.
    heatmap_annotations[[.name]] <- attributes(
        x = heatmap_plots[[.name]])$curlicues$text
}

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

#####-----------------------------------------------------------------
##  Adjust the placement of h and b for the axes for the heatmap (in
##  order to get the grid-plot better looking).

for (.name in .names)  {
    v_just_b <-
        heatmap_annotations[[.name]]$annotated_df["m_value", "vjust"]
    v_just_omega <- - v_just_b
    heatmap_plots[[.name]] <-
        heatmap_plots[[.name]] +
        annotate(geom = "text",
                 label = "h",
                 parse = TRUE,
                 x = Inf,
                 y = -Inf,
                 size = 2,
                 hjust = "inward",
                 vjust = "inward") +
        annotate(geom = "text",
                 label = "b",
                 parse = TRUE,
                 x = -Inf,
                 y = Inf,
                 size = 2,
                 hjust = "inward",
                 vjust = "inward") +
                 ## vjust = v_just_b) +
        xlab(label = NULL) +
        ylab(label = NULL) +
        theme(axis.ticks = element_line(linewidth = 0.3),
              axis.ticks.length = unit(.06, "cm"),
              axis.text = element_text(size = 6))
}

#####------------------------------------------------------------#####

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P2_fig_S3.8.pdf")

pdf(.save_file)

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(30, 1)))
print(heatmap_plots$lower  + 
      theme(legend.key.width = unit(0.15, "cm"),
            legend.key.height = unit(.5, "cm"),
            legend.text = element_text(size = 4.5)) ,
      vp = viewport(
          layout.pos.row = 1:7,
          layout.pos.col = 1))


dev.off()

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)
