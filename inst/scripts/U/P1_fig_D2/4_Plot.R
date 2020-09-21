###----------------------------------------------------------------###
##  The apARCH(2,3)-example from P1_fig_D2.

##  This script generates a heatmap-based plot that investigates the
##  effect on the estimated local Gaussian autocorrelations as the
##  point v varies along the diagonal.

#####------------------------------------------------------------#####

##  In order for this script to work, it is necessary that the script
## '2_Data.R' from P1_fig_D1 has been used first.

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

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_D1.D2")
..TS <- "rugarch_ea795134c2c77f252a7abefd2a30cf82"
..Approx <- "Approx__1"

#####------------------------------------------------------------#####

##  Define the 'input'-list that specifies the content of the plot.
##  Some of the information in this list is redundant for the present
##  plot, but it is necessary to update the plot-function before those
##  parts can be removed from the list below.

input <- list(TCS_type = "C",
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
              drop_annotation = TRUE)

#####------------------------------------------------------------#####
#####------------------------------------------------------------#####
#####------------------------------------------------------------#####

##  Compute the heatmap.

heatmap_plot <- LG_plot_helper(
    main_dir = ..main_dir, 
    input = input,
    input_curlicues = list(
        NC_value = list(short_or_long_label = "short"),
        title = list(
            label = "Heatmap for the local Gaussian autospectrum",
            element_text = list(
                size = 8))))

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
    ggtitle(label = "Heatmap for the local Gaussian autocorrelations") +
    theme(plot.title = element_text(hjust = 0.5,
                                    vjust = 0,
                                    size = 8,
                                    colour = "brown")) +
    annotate(geom = "text",
             label = "h",
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

##  It is time to add the annotations to the plot.  This requires a
##  bit of tweaking of the details since the result should look
##  reasonable when the plot is included in a grid and then saved to
##  file.  Note that it is the saved file that should be inspected in
##  order to figure out if the tweaking has produced a reasonable
##  result.

## Tweak the position of the plot stamp.

annotate_heatmap$annotated$vjust[1] <- 1.5 * annotate_heatmap$annotated$vjust[1]

## Tweak the size of the annotated stuff so it looks decent after the
##  grid-plot has been saved.

.scale <- 0.4

annotate_heatmap$annotated$size <- 
    .scale * annotate_heatmap$annotated$size

heatmap_plot <-
    heatmap_plot +
    eval(annotate_heatmap$annotated)

#####------------------------------------------------------------#####
#####------------------------------------------------------------#####
#####------------------------------------------------------------#####

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_D2.pdf")
rm(..main_dir, ..TS)

pdf(.save_file)

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(30, 1)))
print(heatmap_plot  + 
      theme(legend.key.width = unit(0.15, "cm"),
            legend.key.height = unit(.5, "cm"),
            legend.text = element_text(size = 4.5)) ,
      vp = viewport(
          layout.pos.row = 1:7,
          layout.pos.col = 1))

dev.off()

## ##  Crop the result.  This approach requires that 'pdfcrop' is
## ##  available on the system.

## .crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
## system(.crop_code)
## rm(.crop_code, .save_file)
##  Crop the result
.crop_code <- sprintf("pdfcrop --margins 5 %s %s", .save_file, .save_file)
system(.crop_code)
