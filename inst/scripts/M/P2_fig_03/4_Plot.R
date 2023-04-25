###----------------------------------------------------------------###
##  The "local trigonometric" example from P2_fig_03.

##  This script investigates the local Gaussian cross-spectra for a
##  "local trigonometric" example, based on P1_fig_07, with a common
##  phase shift for all the four cosine-components.  Inspection along
##  diagonal points.

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

..main_dir <- c("~", "LG_DATA_scripts", "P2_fig_03_S6.3")
..TS <- "dmt_bivariate_0fa441a13f46c6a5f53484556b81ef25"
..Approx <- "Approx__1"

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
   .S_type_values)

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

##  Code only relevant for the trigonometric examples.  Extract
##  information about the frequencies and phase-adjustments.

alpha <- attributes(..plot[[c(1,1)]])$details$fun_formals$first_dmt$alpha
phase_adjustment <- attributes(..plot[[c(1,1)]])$details$fun_formals$phase_adjustment

##  Add the alpha-values as vertical lines to all the plots.

for (i in names(..plot)) {
    for (j in names(..plot[[i]])) {
        .bm <- c(i,j)
        ..plot[[.bm]] <- ..plot[[.bm]] +
            geom_vline(xintercept = alpha/(2*pi),
                       linetype = 3,
                       col = "black",
                       alpha = 0.8,
                       lwd = .1)
    }
}
rm(i, j, .bm)

##  Add the phase_adjustment-values as horizontal lines to the
##  phase-plots, with an additional point at the intersection of the
##  relevant frequency alpha, given that the inspection is based on
##  an diagonal point of the lag h pairs.

##  WARNING: The present setup for the phase is not generic.  It
##  should work for this particular example, but that is all.

##  The strategy is to loop over '..plot', and add things only when an
##  on diagonal cross-phase-spectrum is encountered.  The details
##  needed in order to decide this is available in the
##  'details'-attribute of the plots.


##  There might be a need for a sign-adjustment in the result, so a
##  minor help-function is needed in order to get that placed
##  correctly.

.adjust_helper <- function(.details) {
    Vi <- as.integer(gsub(pattern = "Y",
                          replacement = "",
                          x = .details$Vi))
    Vj <- as.integer(gsub(pattern = "Y",
                          replacement = "",
                          x = .details$Vj))
    ##  Return -1 if Vi < Vj, else return 1.
    ifelse(
        test = Vi < Vj,
        yes  = -1,
        no   = 1)
}

##  Update the relevant plots.

for (i in seq_along(..plot)) {
    for (j in seq_along(..plot[[i]])) {
        .bm <- c(i,j)
        .details <- attributes(..plot[[.bm]])$details
        .adjust <- .adjust_helper(.details)
        ##  Go to next if not a relevant target.
        if (!all(.details$is_cross_pair,
                 .details$spectrum_variant == "phase",
                 .details$is_on_diagonal))
            next
        ##  Add horizontal lines when relevant.
        ..plot[[.bm]] <- ..plot[[.bm]] +
            geom_hline(yintercept = .adjust * phase_adjustment,
                       linetype = 3,
                       col = "black",
                       alpha = 0.8,
                       lwd = 0.1) +
            annotate(geom = "point",
                     x = alpha[j+1]/(2*pi),
                     ##  The subindexing by 'j+1' is OK for this
                     ##  particular investigation.
                     y = .adjust * phase_adjustment,
                     alpha = 0.5,
                     shape = 21,
                     colour = "black",
                     size = .5,
                     stroke = 0.4)
    }
}
rm(i, j, .bm, .details, .adjust, .adjust_helper)

##  End of part specific for the trigonometric examples.

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P2_fig_03.pdf")
rm(..main_dir, ..TS)

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(6, length(..plot[[1]]))))
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
