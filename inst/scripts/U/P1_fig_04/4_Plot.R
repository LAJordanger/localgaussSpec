###----------------------------------------------------------------###
##  The dmbp-example from P1_fig_04.

##  This script contains the code needed in order to reproduce the
##  plot that shows the 200 first local Gaussian autocovariances for
##  points in the lower tail, center, and upper tail of the dmbp-data.

###----------------------------------------------------------------###

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

##  Load the required libraries.

library(localgaussSpec)
library(ggplot2)
library(grid)

###----------------------------------------------------------------###

##  Specify the key arguments that identifies where the data to be
##  investigated can be found.

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_04.D8")
..TS <- "0fb42549ce13fce773c12b77463bdca8"
..Approx <- "Approx__1"

###----------------------------------------------------------------###

##  Initiate a list to store the plot-values

..plot <- list()

##  Loop over the desired vertical levels 1,2,3.  If the underlying
##  script in the file "2_Data.R" has not been modified, then this
##  should select diagonal points corresponding to the lower tail,
##  center and upper tail.

.names <- c("lower.tail", "center", "upper.tail")
..line.size <- 0.1

for (.point in 1:3) {
    .name <- .names[.point]
    ##  Specify input values for the selected point.
    .input <-
        list(TCS_type = "C",
             window = "Tukey",
             Boot_Approx = "Nothing here to select", 
             confidence_interval = "95",
             levels_Diagonal = .point,
             bw_points = "0.5",
             cut = 10L,
             frequency_range = c(0, 0.5),
             type = "par_five",
             levels_Horizontal = 2,
             TS = ..TS,
             S_type = "LS_a",
             levels_Line = 2,
             point_type = "on_diag",
             Approx = ..Approx,
             Vi = "Y",
             Vj = "Y",
             levels_Vertical = 2,
             global_local = "local",
             drop_annotation = TRUE)
    
    ..plot[[.name]] <- LG_plot_helper(
        main_dir = ..main_dir, 
        input = .input,
        input_curlicues= list(
            NC_value = list(
                short_or_long_label = "short"),
            spectra_plot = list(
                WN_line  = list(
                    size = ..line.size),
                global = list(
                    line.size = ..line.size),
                local = list(
                    line.size = ..line.size))))
}
rm(.point, .name, .names, .input), ..line.size)

##  Ensure that the limit on the y-axis is the same for all the plots.

.range_list <- lapply(
    X = ..plot,
    FUN = function(x) {
        range(x$data$orig)
    })
.range <- range(.range_list)
for (i in seq_along(..plot))
    ..plot[[i]]$coordinates$limits$y <- .range 
rm(.range, i, .range_list)

##  The use of 'drop_annotation=TRUE' in the 'input'-argument of
##  'LG_plot_helper' prevented the annotated text to be added to the
##  plots in the list '..plot'.  The information to add them on later
##  on (with an adjusted size-value) can be extracted from the
##  attributes, and can be stored in a separate list.

annotated_text <- lapply(
    X = ..plot,
    FUN = function(x)
        attributes(x)$curlicues$text)

.scaling_for_annotated_text <- 0.6

for (.name in names(annotated_text)) {
    ##  Adjust the size of all the annotated text.
    annotated_text[[.name]]$annotated$size <-
        annotated_text[[.name]]$annotated$size *
        .scaling_for_annotated_text

    ##  Additional tweaking in order for the grid-based shrinked plots
    ##  to look a bit more decent.  The plots now have a stamp
    ##  describing the content, so it is feasible to ditch the title.


    size_h <- annotated_text[[.name]]$annotated_df["NC_value", "size"]  *
        .scaling_for_annotated_text
    
    ##  Add the annoted text to the plots, and fix other stuff at the
    ##  same time.

    ..plot[[.name]] <-
        ..plot[[.name]] +
        eval(annotated_text[[.name]]$annotated) +
        annotate(geom = "text",
                 label = "h",
                 parse = TRUE,
                 x = Inf,
                 y = -Inf,
                 size = size_h,
                 hjust = "inward",
                 vjust = "inward") + 
        xlab(label = NULL) +
        ggtitle(label = NULL) +
        theme(axis.ticks = element_line(linewidth = 0.35),
              axis.ticks.length = unit(.08, "cm"),
              axis.text = element_text(size = 6))
}
rm(.name, size_h, .scaling_for_annotated_text, annotated_text)

###----------------------------------------------------------------###
##  Add a red dotted line to emphasise the truncation level 'm' that
##  is used later on.  Note that the line is shifted '+.5' to avoid
##  overlaying the local Gaussian autocorrelation at that lag.

m <- 10
for (i in seq_along(..plot))
    ..plot[[i]] <- ..plot[[i]] + 
        geom_vline(xintercept = m + .5,
                   linetype = 3,
                   col = "red",
                   alpha = 0.8,
                   lwd = 0.5)
rm(i,m)

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_04.pdf")
rm(..main_dir, ..TS)

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(9, 1)))
print(..plot$lower.tail,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 1))
print(..plot$center,
      vp = viewport(
          layout.pos.row = 2,
          layout.pos.col = 1))
print(..plot$upper.tail,
      vp = viewport(
          layout.pos.row = 3,
          layout.pos.col = 1))

dev.off()

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)

###----------------------------------------------------------------###

## ##  This part is not needed in order to create the plot, but it has
## ##  been included to show how to extract the information that each
## ##  plot contains about its content.

## ##  This gives the information seen in the shiny-application, cf. the
## ##  documentation of 'LG_explain_plot' for further details.
## .explanations_of_plots <- lapply(
##     X = ..plot,
##     FUN = LG_explain_plot)

## ##  It is also possible to extract information directly from the
## ##  stored attributes if that should be of interest:

## .b <- attributes(..plot[[1]])$details$bandwidth
## .CI <- attributes(..plot[[1]])$details$CI_percentage
## .N <- attributes(..plot[[1]])$details$N
## .nr.samples <-
##     if (attributes(..plot[[1]])$details$is_block) {
##         attributes(..plot[[1]])$details$nr_simulated_samples
##     } else
##         attributes(..plot[[1]])$details$nb
## ##  Only relevant when bootstrapping
## .block.length <- attributes(..plot[[1]])$details$block_length
