###----------------------------------------------------------------###
## The bivariate Gaussian example from P2_fig_S6.1.

##  This script investigates the local Gaussian cross-spectra for
##  bivariate Gaussian white noise.

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

..main_dir <- c("~", "LG_DATA_scripts", "P2_fig_S6.1")
..TS <- "rmvnorm_1b6348699b1f40c0505b754c9603d93a"
..Approx <- "Approx__1"

###----------------------------------------------------------------###

##  Initiate a list to store the plot-values

..plot <- list()

##  Loop over the cases of interest.  If the underlying script in the
##  file "2_Data.R" has not been modified, then this should return the
##  desired plot.

##  Define the list with the points to investigate.

.point_list <- list(
    '10_50' = list(levels_Horizontal = 1L,
                 levels_Vertical = 2L),
    '50_50' = list(levels_Horizontal = 2L,
                   levels_Vertical = 2L))

##  Specify the values needed in order to create the cospectra,
##  quadrature spectra and phase spectra.

.S_type_values <- c("LS_c_Co", "LS_c_Quad", "LS_c_phase" )

..line.size <- 0.1
for (.point in names(.point_list)) {
    .point_type <- ifelse(
        test = {.point_list[[.point]]$levels_Horizontal ==
                    .point_list[[.point]]$levels_Vertical},
        yes  = "on_diag",
        no   = "off_diag")
    for (.S_type in .S_type_values) {
        ##  Specify input values for the selected point.
        .input <- list(
            TCS_type = "S", 
            window = "Tukey",
            Boot_Approx = NA_character_,
            confidence_interval = "95",
            levels_Diagonal = .point_list[[.point]]$levels_Horizontal,
            bw_points = "0.6", 
            cut = 10L,
            frequency_range = c(0, 0.5),
            type = "par_five", 
            levels_Horizontal = .point_list[[.point]]$levels_Horizontal,
            TS = ..TS,
            S_type = .S_type,
            levels_Line = .point_list[[.point]]$levels_Horizontal,
            point_type = .point_type, 
            Approx = ..Approx,
            Vi = "Y1",
            Vj = "Y2",
            levels_Vertical = .point_list[[.point]]$levels_Vertical, 
            global_local = "local",
            drop_annotation = TRUE)
        ##  Add plots to '..plot_list'.
        ..plot[[.S_type]][[.point]] <-
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
}
rm(..Approx, .input, .point, .point_type, .point_list, .S_type,
   .S_type_values, ..line.size)

###----------------------------------------------------------------###

##  Ensure that the limit on the y-axis is the same for all the plots
##  of the same type, and that should also be based on the smallest
##  natural range for the selected m-truncation.

.range_list <- lapply(
    X = ..plot,
    FUN = function(x) {
        range(lapply(
            X = x,
            FUN = function(.x) {
                attributes(.x)$ylim_list$ylim_restricted
            }))
    })
for (i in seq_along(..plot)) {
    for (j in seq_along(..plot[[i]])) {
        ..plot[[c(i,j)]]$coordinates$limits$y <- .range_list[[i]]
    }
}
rm(i, j, .range_list)

###----------------------------------------------------------------###

##  The use of 'drop_annotation=TRUE' in the 'input'-argument of
##  'LG_plot_helper' prevented the annotated text to be added to the
##  plots in the list '..plot'.  The information to add them on later
##  on (with an adjusted size-value) can be extracted from the
##  attributes, and can be stored in a separate list.

annotated_text <- lapply(
    X = ..plot,
    FUN = function(x)
        lapply(
            X = x,
            FUN = function(.x)
                attributes(.x)$curlicues$text))

.scaling_for_annotated_text <- 0.6

for (.name in names(annotated_text)) {
    for (.name2 in names(annotated_text[[.name]])) {
        .bm <- c(.name, .name2)
        ##  Adjust the size of all the annotated text.
        annotated_text[[.bm]]$annotated$size <-
            annotated_text[[.bm]]$annotated$size *
            .scaling_for_annotated_text

        ##  Additional tweaking in order for the grid-based shrinked plots
        ##  to look a bit more decent.  The plots now have a stamp
        ##  describing the content, so it is feasible to ditch the title.

        size_omega <- annotated_text[[.bm]]$annotated_df["NC_value", "size"] *
            .scaling_for_annotated_text
        
        ##  Add the annoted text to the plots, and fix other stuff at the
        ##  same time.
        
        ..plot[[.bm]] <-
            ..plot[[.bm]] +
            eval(annotated_text[[.bm]]$annotated) +
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
    }
}
rm(.name, .name2, .bm, size_omega, .scaling_for_annotated_text,
   annotated_text)

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P2_fig_S6.1.pdf")
rm(..main_dir, ..TS)

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(8, length(..plot[[1]]))))
##  Loop over '..plot_list', such that information about the same
##  point is in one column.
for (i in seq_along(..plot)) {
    for (j in seq_along(..plot[[i]])) {
        print(..plot[[c(i,j)]],
              vp = viewport(
                  layout.pos.row = i,
                  layout.pos.col = j))
    }
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
##     FUN = function(x) {
##         lapply(
##             X = x,
##             FUN = LG_explain_plot)
##     })

## ##  It is also possible to extract information directly from the
## ##  stored attributes if that should be of interest:

## .b <- attributes(..plot[[c(1,1)]])$details$bandwidth
## .CI <- attributes(..plot[[c(1,1)]])$details$CI_percentage
## .N <- attributes(..plot[[c(1,1)]])$details$N
## .nr.samples <-
##     if (attributes(..plot[[c(1,1)]])$details$is_block) {
##         attributes(..plot[[c(1,1)]])$details$nr_simulated_samples
##     } else
##         attributes(..plot[[c(1,1)]])$details$nb
## ##  Only relevant when bootstrapping
## .block.length <- attributes(..plot[[c(1,1)]])$details$block_length
