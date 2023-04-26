###----------------------------------------------------------------###
##  The EuStockMarkets-example from P2_fig_08.

##  This scripts investigates the first 200 local Gaussian
##  cross-correlations, based on the pseudo-normalised version of the
##  log-returns of the EuStockMarkets, focusing on the DAX and CAC
##  variables.

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

..main_dir <- c("~", "LG_DATA_scripts", "P2_fig_08_S3.10")
..TS <- "9e59e59f271b88315be95f9e40025f04"
..Approx <- "Approx__1"
..Boot_Approx  <- "Nothing here to select"

###----------------------------------------------------------------###

##  Initiate a list to store the plot-values

..plot <- list()

##  Loop over the cases of interest.  If the underlying script in the
##  file "2_Data.R" has not been modified, then this should return the
##  desired plot.

##  Define the list with the points to investigate.

.point_list <- list(
    '10_10' = list(levels_Horizontal = 1L,
                 levels_Vertical = 1L),
    '50_50' = list(levels_Horizontal = 2L,
                   levels_Vertical = 2L),
    '90_90' = list(levels_Horizontal = 3L,
                   levels_Vertical = 3L))

..line.size <- 0.1
for (.point in names(.point_list)) {
    .point_type <- ifelse(
        test = {.point_list[[.point]]$levels_Horizontal ==
                    .point_list[[.point]]$levels_Vertical},
        yes  = "on_diag",
        no   = "off_diag")

    ##  Specify input values for the selected point.
    .input <- list(
        TCS_type = "C", 
        window = "Tukey",
        Boot_Approx = ..Boot_Approx,
        confidence_interval = "95",
        levels_Diagonal = .point_list[[.point]]$levels_Horizontal,
        bw_points = "0.6", 
        cut = 10L,
        frequency_range = c(0, 0.5),
        type = "par_five", 
        levels_Horizontal = .point_list[[.point]]$levels_Horizontal,
        TS = ..TS,
        S_type = NA_character_,
        levels_Line = .point_list[[.point]]$levels_Horizontal,
        point_type = .point_type, 
        Approx = ..Approx,
        Vi = "Y1",
        Vj = "Y3",
        levels_Vertical = .point_list[[.point]]$levels_Vertical, 
        global_local = "local",
        drop_annotation = TRUE)
    ##  Add plots to '..plot_list'.
    ..plot[[.point]] <-
        LG_plot_helper(
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
rm(..Approx, ..Boot_Approx, .input, .point, .point_type, .point_list,
   ..line.size)

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

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P2_fig_08.pdf")
rm(..main_dir, ..TS)

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(8, 1)))
for (i in seq_along(..plot)) {
    print(..plot[[i]],
          vp = viewport(
              layout.pos.row = i,
              layout.pos.col = 1))
}

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
