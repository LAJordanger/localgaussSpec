###----------------------------------------------------------------###
##  The cosine with some noise example from P1_fig_G4.

##  This scripts investigates the local Gaussian auto-spectra for a
##  cosine with small noise.  See also P1_fig_05.

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

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_G4")
..TS <- "dmt_4d532713e9b2866675cf31b64942ce70"

###----------------------------------------------------------------###

##  Initiate a list to store the plot-values

..plot <- list()

##  Loop over the three cases of interest.  If the underlying script
##  in the file "2_Data.R" has not been modified, then this should
##  return the desired plot.

.names <- c("first", "second", "third")

.points <- structure(
    .Data = c(1 , 2, 1),
    .Names = .names)
.cut_values <- structure(
    .Data = c(10L, 10L, 20L),
    .Names = .names)

..line.size <- 0.1

for (.name in .names) {
    ##  Specify input values for the selected point.
    .input <-
        list(TCS_type = "S",
             window = "Tukey",
             Boot_Approx = NA_character_,
             confidence_interval = "95",
             levels_Diagonal = .points[.name],
             bw_points = "0.5",
             cut = .cut_values[.name],
             frequency_range = c(0, 0.5),
             type = "par_five",
             levels_Horizontal = 2,
             TS = ..TS,
             S_type = "LS_a",
             levels_Line = 2,
             point_type = "on_diag",
             Approx = "Approx__1",
             Vi = "Y", Vj = "Y",
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
rm(.name, .names, .input, ..line.size)

##  Ensure that the limit on the y-axis is the same for all the plots,
##  and that it is based on the smallest natural range for the
##  selected m-truncation.

.range_list <- lapply(
    X = ..plot,
    FUN = function(x) {
        attributes(x)$ylim_list$ylim_restricted
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


    size_omega <- annotated_text[[.name]]$annotated_df["NC_value", "size"] *
        .scaling_for_annotated_text
    
    ##  Add the annoted text to the plots, and fix other stuff at the
    ##  same time.

    ..plot[[.name]] <-
        ..plot[[.name]] +
        eval(annotated_text[[.name]]$annotated) +
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
        theme(axis.ticks = element_line(size = 0.25),
              axis.ticks.length = unit(.04, "cm"),
              axis.text = element_text(size = 4.5))
}
rm(.name, size_omega)

###----------------------------------------------------------------###

##  Code only relevant for the trigonometric examples: Extract
##  information about the frequencies from the info-file.

alpha <- attributes(..plot[[1]])$details$fun_formals$alpha

##  Add the alpha-value as a vertical lines to all the plots, and
##  update the names of the layers so the size of the line can be
##  adjusted properly in the code below.

for (i in seq_along(..plot)) {
    ..plot[[i]] <- ..plot[[i]] +
    geom_vline(xintercept = alpha/(2*pi),
               linetype = 3,
               col = "black",
               alpha = 0.8,
               lwd = 0.3)
    names(..plot[[i]]$layers) <- c(
        head(x = names(..plot[[i]]$layers),
             n = -1),
        "alpha_lines")
}
rm(alpha, i)

##  End of part specific for the trigonometric examples.

###----------------------------------------------------------------###

##  We need an example with the pseudo-normalised plot of the time
##  series under investigation.  In case the investigation is based on
##  a number of simulations from a parametric model, then the first
##  sample will be used.

##  Strategy: Specify a reasonable label, extract the time series
##  sample of interest, and create a plot of it.

.TS_label <- "cosine and a tiny bit of noise, 100 observations"
.TS_path <- file.path(
    paste(..main_dir,
          collapse = .Platform$file.sep),
    ..TS,
    localgaussSpec:::LG_default$global["TS"])

##  Load the object into the present workflow, and reduce to the case
##  of interest.

localgaussSpec:::LG_load(.file = .TS_path, .name = ".TS_example")
.TS_example_pn <- attributes(.TS_example)$TS_for_analysis[1,,]

##  Extract information to be used when adding '.TS_label'.

.TS_annotation <- annotated_text[[1]]$annotated_df["v_value", ]

##  Only plot 100 values in this case, since it is extremely periodic.

..plot$TS_example <- ggplot(
    data = data.frame(
        x = seq_along(.TS_example_pn[1:100]),
        y = .TS_example_pn[1:100]),
    mapping = aes(x = x, y = y)) + 
    geom_line(
        mapping = aes(x = x, y = y),
        alpha = 0.8,
        lwd = 0.03) + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) + 
    annotate(geom = "text",
             x = 0,
             y = Inf,
             size = .TS_annotation$size *
                 .scaling_for_annotated_text,
             label =  .TS_label,
             col = .TS_annotation$col,
             alpha = 1,
             vjust = .TS_annotation$vjust,
             hjust = .TS_annotation$hjust)  +
    theme(axis.ticks = element_line(size = 0.25),
          axis.ticks.length = unit(.04, "cm"),
          axis.text = element_text(size = 4.5))

##  This plot is shrinked a lot later on, and then the tiny `lwd`
##  gives a decent view.

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_G4.pdf")
rm(..main_dir, ..TS)

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(7, 2)))
print(..plot$TS_example  +
      geom_hline(
          yintercept = qnorm(p = c(0.1, 0.5, 0.9)),
          linetype = 2,
          col = "brown",
          alpha = 0.8,
          lwd = 0.4),
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 1))
print(..plot$first,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 2))
print(..plot$second,
      vp = viewport(
          layout.pos.row = 2,
          layout.pos.col = 1))
print(..plot$third,
      vp = viewport(
          layout.pos.row = 2,
          layout.pos.col = 2))

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
