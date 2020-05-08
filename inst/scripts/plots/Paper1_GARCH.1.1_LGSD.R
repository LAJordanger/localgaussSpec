## This script contains the code needed in order to recreate the
## motivating GARCH(1,1)-example in paper 1.

## In order for this script to work, it is necessary that the script
## that generates the required data has been used first.  Warning: The
## code below also assumes that these scripts where run with their
## initial arguments.

##  Note: The 'TS' value given below originates from the
##  'digest::digest'-function.  This is used in order to keep track of
##  the different simulations, and it is in particular used to avoid
##  the re-computation of a script that has already been executed.  It
##  might alas be the case that this value can be influenced by the
##  random number generator used during the computation, so if the
##  scrips has been used without modifications and the code below
##  returns an error, then it might be necessary to update the
##  'TS'-value in this script the one created by the data-generating
##  script.


library(localgaussSpec)
library(ggplot2)
library(grid)

##  TODO: Run script over again with target 'LG_DATA'

..main_dir <- c("~", "LG_DATA_paper1_revision")  
..TS <- "rugarch_3c6864d4791f01f627d73827ecac24f2"


## ## ## data_dir_for_LG_shiny <-
## ## ##     c(ts.dir = ..TS,
## ## ##       approx.dir = "Approx__1")



## ## ## shiny::runApp(LG_shiny(
## ## ##            main_dir = ..main_dir,
## ## ##            data_dir = data_dir_for_LG_shiny))


#########################
##  Initiate a list to store the plot-values

..plot <- list()  ##  Initiate list for plots.

## Loop over the two desired variants, i.e. "global" and "local", and
## skip the pointwise confidence intervals for this particular plot.

.names <- c("global", "local")
..line.size <- 0.1

for (.name in .names) {
    ##  Specify input values for the selected point.
    .input <- list(TCS_type = "S", 
                  window = "Tukey",
                  Boot_Approx = NA_character_,
                  TS_key = "rugarch", 
                  confidence_interval = "95",
                  levels_Diagonal = 1L,
                  bw_points = "0.4", 
                  cut = 10L,
                  frequency_range = c(0, 0.5),
                  type = "par_five", 
                  levels_Horizontal = 2,
                  TS = ..TS,
                  S_type = "GS_a",
                  levels_Line = 2,
                  point_type = "on_diag", 
                  Approx = "Approx__1",
                  Vi = "Y",
                  Vj = "Y",
                  levels_Vertical = 2, 
                  global_local = .name,
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
                    line.include = {.name == "global"},
                    linetype = 1,
                    ribbon.include = FALSE,
                    line.size = ..line.size),
                local = list(
                    line.include = {.name == "local"},
                    linetype = 1,
                    ribbon.include = FALSE,
                    line.size = ..line.size))))
}
rm(.name, .names, .input)


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

    size_omega <- annotated_text[[.name]]$annotated_df["n_R_L_values", "size"] *
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
rm(.name, size_omega, .scaling_for_annotated_text)

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





##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.


## TODO: Adjust save file later on, for now update plot to be used in
## paper.
.save_file <- file.path("~/Dropbox/arXiv/Article_1_JASA/figure", "GARCH_1_1_intro_short.pdf")

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(6, 2)))
print(..plot$global,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 1))
print(..plot$local,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 2))

dev.off()

##  Crop the result
.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)



