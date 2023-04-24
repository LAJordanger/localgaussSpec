###----------------------------------------------------------------###
##  The "local trigonometric" example from P1_fig_G3.

##  This scripts investigates the local Gaussian auto-spectra for a
##  "local trigonometric" example, detect the elusive component for a
##  point in the lower tail.  See also P1_fig_07.

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

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_G3")
..TS <- "dmt_b2fecf9f8c798c0c058df84ca025c944"

###----------------------------------------------------------------###

##  Create the plot of interest.

input <- list(
    TCS_type = "S", 
    window = "Tukey",
    Boot_Approx = NA_character_,
    confidence_interval = "95",
    levels_Diagonal = 1L,
    bw_points = "0.4", 
    cut = 10L,
    frequency_range = c(0, 0.5),
    type = "par_five", 
    levels_Horizontal = 1,
    TS = ..TS, 
    S_type = "LS_a",
    levels_Line = 1,
    point_type = "on_diag", 
    Approx = "Approx__1",
    Vi = "Y",
    Vj = "Y",
    levels_Vertical = 1, 
    global_local = "local",
    drop_annotation = TRUE)

..line.size <- 0.1
.the_plot <- LG_plot_helper(
    main_dir = ..main_dir, 
    input = input,
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
rm(input, ..line.size)

###----------------------------------------------------------------###

##  Code only relevant for the trigonometric examples: Extract
##  information about the frequencies.

alpha <- attributes(.the_plot)$details$fun_formals$alpha

##  Add the alpha-values as vertical lines.

.the_plot <- .the_plot +
    geom_vline(xintercept = alpha/(2*pi),
               linetype = c(1, 3, 3, 3),
               col = c("orange", "black", "black", "black"),
               alpha = 0.8,
               lwd = 0.3)
rm(alpha)

##  End of part specific for the trigonometric examples.

###----------------------------------------------------------------###

##  The use of 'drop_annotation=TRUE' in the 'input'-argument of
##  'LG_plot_helper' prevented the annotated text to be added to the
##  plots in the list '..plot'.  The information to add them on later
##  on (with an adjusted size-value) can be extracted from the
##  attributes, and can be stored in a separate list.

annotated_text <- attributes(.the_plot)$curlicues$text

.scaling_for_annotated_text <- 0.6

##  Adjust the size of all the annotated text.
annotated_text$annotated$size <-
    annotated_text$annotated$size *
    .scaling_for_annotated_text

##  Additional tweaking in order for the grid-based shrinked plots
##  to look a bit more decent.  The plots now have a stamp
##  describing the content, so it is feasible to ditch the title.

size_omega <- annotated_text$annotated_df["n_R_L_value", "size"] *
    .scaling_for_annotated_text

##  Add the annoted text to the plots, and fix other stuff at the
##  same time.

.the_plot <-
    .the_plot +
    eval(annotated_text$annotated) +
    annotate(geom = "text",
             label = "omega",
             parse = TRUE,
             x = Inf,
             y = -Inf,
             size = size_omega,
             hjust = "inward",
             vjust = "inward") + 
    xlab(label = NULL) +
    ggtitle(label = NULL) +
    theme(axis.ticks = element_line(linewidth = 0.25),
          axis.ticks.length = unit(.04, "cm"),
          axis.text = element_text(size = 4.5))
rm(annotated_text, .scaling_for_annotated_text, size_omega)

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_G3.pdf")
rm(..main_dir, ..TS)

pdf(file = .save_file)

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(7, 1)))
print(.the_plot,
      vp = viewport(
          layout.pos.row = 1,
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

## .b <- attributes(.the_plot)$details$bandwidth
## .CI <- attributes(.the_plot)$details$CI_percentage
## .N <- attributes(.the_plot)$details$N
## .nr.samples <-
##     if (attributes(.the_plot)$details$is_block) {
##         attributes(.the_plot)$details$nr_simulated_samples
##     } else
##         attributes(.the_plot)$details$nb
## ##  Only relevant when bootstrapping
## .block.length <- attributes(.the_plot)$details$block_length
