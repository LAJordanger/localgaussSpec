###----------------------------------------------------------------###
##  The combined dmbp and apARCH(2,3) example from P1_fig_F1.

##  This script enables a comparison of the local Gaussian spectra for
##  an original data sample (the dmbp-data) with those generated from
##  a parametric model (aparch) fitted to the data.

#####------------------------------------------------------------#####

##  In order for this script to work, it is necessary to first use the
##  script '2_Data.R' for both P1_fig_09 and P1_fig_11.

##  Warning: The code below assumes that the two '2_Data.R'-files were
##  used with their initial arguments, i.e. an adjustment of the
##  script that includes additional points might require a
##  modification of this script.

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

##  The present setup requires that the two local Gaussian
##  investigations must have been performed on the respective cases of
##  interest, and it is in particular based on the respective scripts
##  for the dmbp-example and the aparch-example. This script assumes
##  that the two other scripts have been used as they are given in the
##  template, i.e. without any adjustments of the points of
##  investigation, the bandwidth and so on.

#####------------------------------------------------------------#####

##  Define the directory- and file components needed for the
##  extraction of the data.  The path to the main directory is given
##  as a vector since '.Platform$file.sep' depends on the OS.  Note
##  that these values must correspond to those that are used in the
##  script '2_Data.R', so any modifications there must be mirrored in
##  this script.

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_09.11.F1")

TS_aparch <- "rugarch_3e08382d945f0567d9f8f5d8e171aa53"
TS_dmbp <- "0fb42549ce13fce773c12b77463bdca8"

..TS <- TS_dmbp

###----------------------------------------------------------------###

##  Create two lists: One with the aparch-plots, and one with the data
##  for the dmbp-plots.  These lists will be filled by the help of a
##  double loop over c(0.1, 0.5, 0.9).  Note that this assumes that
##  only those points have been investigated when the data was
##  created!

.plots_aparch <- list()
.plots_dmbp <- list()

##  Specify the 'curlicues'-data.  This will restrict the attention to
##  the local data only (global data will be ignored), and it will
##  also reduce the size of text and lines.  The latter part is
##  required in order for these plots to look decent when all of them
##  are included in a single grid-based plot.

##  The tweaked line size needs to be inserted several places.

..line.size <- 0.2
.title_text.size <- 7

##  It is necessary to save the final grid-plot to file in order to
##  see the final efect of these values.

input_curlicues_aparch = list(
    title = list(
        include = TRUE,
        element_text = list(
            size = .title_text.size)),
    NC_value = list(
        include = TRUE,
        short_or_long_label = "short"),
    spectra_plot = list(
        WN_line  = list(
            include = TRUE,
            size = ..line.size,
            alpha = .8),
        global = list(
            line.include = FALSE,
            ribbon.include = FALSE),
        local = list(
            line.include = TRUE,
            ribbon.include = TRUE,
            line.size = ..line.size,
            line.alpha = .8,
            ribbon.alpha = .3)))
rm(.title_text.size)

##  Restrict to only the part having the ordinary setup, since that is
##  what will be extracted later on.  Moreover, include an order to
##  extract the information about 'data' and 'mapping' into a
##  'data_env'-environment attribute of the plots.

input_curlicues_dmbp = list(
    spectra_plot = list(
        global = list(line.include = FALSE,
                      ribbon.include = FALSE),
        local = list(ribbon.include = FALSE)),
    data_extraction = list(.geom_line_local = TRUE))

##  Specify the cut-value to be used.

.cut <- 10L

##  Specify the coordinates of the points to be investigated.  The
##  vector below gives a total of nine points.

.LGp <- c(0.1, 0.5, 0.9)

##  Compute an array with the node names of interest.

.node_names <- outer(
    X = .LGp,
    Y = .LGp,
    FUN = function(x,y)
        sprintf("%s_%s", x, y))


## Compute an array that specifies the spectra to be used.

..S_types <- outer(
    X = .LGp,
    Y = .LGp,
    FUN = function(x,y)
        sprintf(
            "LS_a_%s",
            ifelse(test = {x == y},
                   yes  = "auto",
                   no   = ifelse(test = {x > y},
                                 yes  = "Co",
                                 no   = "Quad"))))

##  The above setup gives a diagonal with the local Gaussian
##  autospectra computed at the diagonal points specified, and these
##  are always real.  The lower part of the matrix contains the
##  cospectra (real parts) and the upper part contains the
##  quadrature-spectra (negative imaginary parts).  The ylimits of
##  these plots will be adjusted later on so the grid of plots are
##  easier to compare, and with regard to the comparison of these
##  plots it is necessary to scale the relevant real and imaginary
##  parts accordingly.

.re_im_info <- outer(
    X = .LGp,
    Y = .LGp,
    FUN = function(x,y)
        ifelse(test = x >= y,
               yes  = "Re",
               no   = "Im"))

for (v1 in seq_along(.LGp)) {
    for (v2 in seq_along(.LGp)) {
        ##  Find the node name for the list, and print it to keep
        ##  track of the progression of this loop.
        .node_name <- .node_names[v1, v2]
        print(.node_name)  
        ##  Fint the plot type for the point.
        ..S_type <-  ..S_types[v1, v2]
        ##  Specify the input-list for aparch.
        input_aparch <- list(
            TCS_type = "S", 
            window = "Tukey",
            Boot_Approx = NA_character_,
            confidence_interval = "95",
            bw_points = "0.5", 
            cut = .cut,
            frequency_range = c(0, 0.5),
            type = "par_five", 
            levels_Horizontal = v1,
            TS = TS_aparch, 
            S_type = ..S_type,
            point_type = "off_diag", 
            Approx = "Approx__1",
            Vi = "Y",
            Vj = "Y",
            levels_Vertical = v2, 
            global_local = "local",
            drop_annotation = TRUE)
        ##  Specify the input-list for dmbp.
        input_dmbp <- list(
            TCS_type = "S",
            window = "Tukey",
            Boot_Approx = "Select a bootstrap approximation",
            confidence_interval = "95",
            bw_points = "0.5",
            cut = .cut,
            frequency_range = c(0, 0.5),
            type = "par_five",
            levels_Horizontal = v1,
            TS = "0fb42549ce13fce773c12b77463bdca8",
            S_type = ..S_type,
            point_type = "off_diag",
            Approx = "Approx__1",
            Vi = "Y",
            Vj = "Y",
            levels_Vertical = v2, 
            global_local = "local")
        ##  Create the aparch-plot.
        .plots_aparch[[.node_name]]  <- LG_plot_helper(
            main_dir = ..main_dir,
            input = input_aparch,
            input_curlicues = input_curlicues_aparch)
        ##  Create the dmbp-plot.
        .plots_dmbp[[.node_name]] <- LG_plot_helper(
            main_dir = ..main_dir,
            input = input_dmbp,
            input_curlicues = input_curlicues_dmbp)
    }
}
rm(..S_type, ..S_types, .cut, .LGp, .node_name, input_aparch,
   input_curlicues_aparch, input_dmbp, input_curlicues_dmbp,
   TS_aparch, TS_dmbp, v1, v2)

##  The use of 'drop_annotation=TRUE' as removed all the annotated
##  text from the plots in the list '.plots_aparch'.  Extract the data
##  to be used from the attributes, and store those in a separate
##  list.

annotated_text <- lapply(
    X = .plots_aparch,
    FUN = function(x)
        attributes(x)$curlicues$text)

###----------------------------------------------------------------###

##  Create a copy of the aparch-plots, then extract relevant data from
##  the dmbp-plots, and add these to the aparch-plots.

new_plots <- .plots_aparch
.range_list <- list()

for (.name in names(new_plots)) {
    ##  Add a pointer to the 'data_env'-environment that is an
    ##  attribute of the dmbp-plots (exported due to the
    ##  'data_extraction'-node in 'input_curlicues_dmbp').
    .data_env <- attributes(.plots_dmbp[[.name]])$data_env
    ##  Update the plots with the dmbp-data.
    new_plots[[.name]] <-
        new_plots[[.name]] +
        geom_line(
            mapping = .data_env$.geom_line_local$mapping, #aes(x = omega, y = orig),
            data = .data_env$.geom_line_local$data , #.orig,
            col = "black",
            size = ..line.size,
            lty = 1)
    ##  Find the 'ylim' that best fit the selected data.
    .range_list[[.name]] <- range(
        attributes(new_plots[[.name]])$ylim_list$ylim_restricted,
        attributes(.plots_dmbp[[.name]])$ylim_list$ylim_restricted)
}
rm(.name, .data_env, ..line.size)
## rm(.plots_aparch, .plots_dmbp)

##  Update the ylimits based on the plot type used.
for (.re_im_type in unique(as.vector(.re_im_info))) {
    ##  Identify the nodes having 're_im_type'.
    .re_im_type_nodes <- .node_names[which(.re_im_info %in% .re_im_type)]
    ##  Find the desired ylimit in this case.
    .ylim <- range(.range_list[.re_im_type_nodes])
    ##  Update the specified plots
    for (.node in .re_im_type_nodes) {
        new_plots[[.node]] <-
            new_plots[[.node]] +
            coord_cartesian(ylim = .ylim,
                            default = TRUE)
    }
    rm(.re_im_type, .re_im_type_nodes, .ylim, .node)
}
rm(.node_names, .range_list, .re_im_info)

###----------------------------------------------------------------###

.scaling_for_annotated_text <- 0.3

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
    v_just_omega <- annotated_text[[.name]]$annotated_df["NC_value", "vjust"]
    
    ##  Add the annoted text to the plots, and fix other stuff at the
    ##  same time.

    new_plots[[.name]] <-
        new_plots[[.name]] +
        eval(annotated_text[[.name]]$annotated) +
        annotate(geom = "text",
                 label = "omega",
                 parse = TRUE,
                 x = Inf,
                 y = -Inf,
                 size = size_omega,
                 hjust = "inward",
                 vjust = v_just_omega) + 
        xlab(label = NULL) +
        ggtitle(label = NULL) +
        theme(axis.ticks = element_line(size = 0.25),
              axis.ticks.length = unit(.04, "cm"),
              axis.text = element_text(size = 4.5))
}
rm(.name, .scaling_for_annotated_text, annotated_text)

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_F1.pdf")
rm(..main_dir, ..TS)

##  Two minor helper functions that puts the plots in the desired
##  positions of the grid, i.e. positioning them in the order that
##  lower levels should occur at the bottom/left part.

.row_pos <- function(i) {
    (12 - i) %/% 3
}

.col_pos <- function(i) {
    .res <- i %% 3
    ifelse(test = {.res == 0},
           yes  = 3,
           no   = .res)
}

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(5, 3)))
for (i in seq_along(new_plots)) {
    print(new_plots[[i]] +
          theme(axis.title.x = element_blank()),
          vp = viewport(
              layout.pos.row = .row_pos(i),
              layout.pos.col = .col_pos(i)))
}

dev.off()

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)
