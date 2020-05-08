## This script contains the code needed in order to recreate the
## single cosine with white noise example in the univariate paper.  In
## order for this script to work, it is necessary that the script that
## generates the data has been used first.  NOTE: This script is based
## on code that picks out three different bandwidths, and this that an
## unexpected result might occur if the data-generating script was
## modified before it was used.

library(localgaussSpec)
library(ggplot2)
library(grid)

..main_dir <- "~/LG_DATA"
..TS <- "dmt_3a0010ae2dde5bfcd94c7fee2611292a"
##  Note: The 'TS' value is based on the 'digest::digest'-function.
##  This is used in order to keep track of the different simulations,
##  and it is used to avoid the re-computation of a script that has
##  already been executed.  It might alas be cases where this value
##  can be influenced by the machine used for the computation, in
##  which case it might be necessary to update the 'TS'-value.

## ## ## data_dir_for_LG_shiny <-
## ## ##     c(ts.dir = ..TS,
## ## ##       approx.dir = "Approx__1")



## ## ## shiny::runApp(LG_shiny(
## ## ##            main_dir = ..main_dir,
## ## ##            data_dir = data_dir_for_LG_shiny))


#########################
##  Initiate a list to store the plot-values

..plot <- list()  ##  Initiate list for plots.

##  Loop over the desired vertical levels 1,2,3.  If the underlying
##  script has not been modified, then this should select diagonal
##  points corresponding to the lower tail, center and upper tail.


.names <- c("lower.tail", "center", "upper.tail")
..line.size <- 0.1

for (.point in 1:3) {
    .name <- .names[.point]
    ##  Specify input values for the selected point.
    .input <-
        list(TCS_type = "S",
             window = "Tukey",
             Boot_Approx = NA_character_,
             TS_key = "WNG",
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
rm(.point, .name, .names, .input)

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

################################################################################





#####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

## ##  To be used when experimenting with the settings:
## copy_plots <- ..plot
## ..plot <- copy_plots


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


################################################################################
##  Code only relevant for the trigonometric examples.  Extract
##  information about the frequencies from the info-file and add as
##  vertical lines to the plots.

.info_path <- file.path(
    paste(..main_dir,
          collapse = .Platform$file.sep),
    ..TS,
    localgaussSpec:::LG_default$info_file_name)


##  Load the object into the present workflow, and reduce to the
##  component of interest.

localgaussSpec:::LG_load(.file = .info_path, .name = "info_TS")
info_TS <- info_TS$TS_info$TS_data
rm(.info_path)

##  Extract the relevant information.
alpha <- formals(info_TS$fun)$alpha
rm(info_TS)


##  Add the alpha-value (frequencies) as a vertical lines to all the
##  plots, with a tweak that highlights the frequency that we
##  anticipate to see for the specified combintation of tuning
##  parameters for the estimation algorithm.


.linetype  <- structure(
    .Data = list(c(3,1,3,3),
                 c(3,3,1,3),
                 c(3,3,3,1)),
    .Names = names(..plot))

.col  <- structure(
    .Data = list(c("black", "orange", "black", "black"),
                 c("black", "black", "orange", "black"),
                 c("black", "black", "black", "orange")),
    .Names = names(..plot))

for (i in names(..plot)) {
    ..plot[[i]] <- ..plot[[i]] +
        geom_vline(xintercept = alpha/(2*pi),
                   linetype = .linetype[[i]],
                   col = .col[[i]],
                   alpha = 0.8,
                   lwd = 0.3)
}
rm(i, .linetype, .col)

##  End of part specific for the trigonometric examples.






## #####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#####
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
## ##############################  OLD CODE

################################################################################
##  Next up: We need an example with the pseudo-normalised plot of the
##  time series under investigation.  In case the investigation is
##  based on a number of simulations from a parametric model, then the
##  first sample will be used.

##  Strategy: Specify a reasonable label, extract the time series
##  sample of interest, and create a plot of it.
.TS_label <- "artificial trigonometric example"
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


..plot$TS_example <- ggplot(
    data = data.frame(
        x = seq_along(.TS_example_pn),
        y = .TS_example_pn),
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
##  REMINDER: This plot is shrinked a lot later on, and then the
##  tiny `lwd` gives a decent view.

#####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#####




##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.


## TODO: Adjust save file later on, for now update plot to be used in
## paper.
.save_file <- file.path("~/Dropbox/arXiv/Article_1_JASA/figure", "trigonometric-1.pdf")

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
print(..plot$lower.tail,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 2))
print(..plot$center,
      vp = viewport(
          layout.pos.row = 2,
          layout.pos.col = 1))
print(..plot$upper.tail,
      vp = viewport(
          layout.pos.row = 2,
          layout.pos.col = 2))

dev.off()

##  Crop the result
.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)






