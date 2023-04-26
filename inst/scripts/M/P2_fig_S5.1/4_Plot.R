###----------------------------------------------------------------###
##  The combined EuXStockMarkets and cGARCH example from P2_fig_S5.1
##  and P2_fig_S5.2

##  This script enables a comparison of the local Gaussian
##  auto-spectra for the marginals of an original data sample (the
##  log-returns of the EuStockMarkets-data) with those generated from
##  a parametric model (cGARCH) fitted to the data.

##  Note that this script in addition to P2_fig_S5.1 also creates
##  P2_fig_S5.2. The default is that P2_fig_S5.1 is created.  In order
##  to create P2_fig_S5.1 change the '.marginal'-argument from "DAX"
##  to "CAC".

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

..main_dir_simulated <- c("~", "LG_DATA_scripts", "P2_fig_11.12")
..main_dir_EuStockMarkets <- c("~", "LG_DATA_scripts", "P2_fig_09")

..main_dir <- c("~", "LG_DATA_scripts")

TS_EuStockMarkets <- "9e59e59f271b88315be95f9e40025f04"

.marginal <- "DAX" ## "CAC"

..Vi <-
    ..Vj <- switch(
        EXPR = .marginal,
        DAX = "Y1",
        SMI = "Y2",
        CAC = "Y3",
        FTSE = "Y4")

.save_file <- switch(
        EXPR = .marginal,
        DAX = "P2_fig_S5.1.pdf",
        SMI = "P2_fig_S5.1_SMI_variant.pdf",
        CAC = "P2_fig_S5.3.pdf",
        FTSE = "P2_fig_S5.1_FTSE_variant.pdf")

###----------------------------------------------------------------###

##  Create two lists: One with the cGARCH-plots, and one with the data
##  for the EuStockMarkets-plots.  These lists will be filled by the
##  help of a double loop over c(0.1, 0.5, 0.9).  Note that this
##  assumes that only those points have been investigated when the
##  data was created!

.plots_simulated <- list()
.plots_EuStockMarkets <- list()

##  Specify the 'curlicues'-data.  This will restrict the attention to
##  the local data only (global data will be ignored), and it will
##  also reduce the size of text and lines.  The latter part is
##  required in order for these plots to look decent when all of them
##  are included in a single grid-based plot.

##  The tweaked line size needs to be inserted several places.

..line.lwd <- 0.2
.title_text.size <- 7

##  It is necessary to save the final grid-plot to file in order to
##  see the final efect of these values.

input_curlicues_simulated = list(
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
            size = ..line.lwd,
            alpha = .8),
        global = list(
            line.include = FALSE,
            ribbon.include = FALSE),
        local = list(
            line.include = TRUE,
            ribbon.include = TRUE,
            line.size = ..line.lwd,
            line.alpha = .8,
            ribbon.alpha = .3)))
rm(.title_text.size)

##  Restrict to only the part having the ordinary setup, since that is
##  what will be extracted later on.  Moreover, include an order to
##  extract the information about 'data' and 'mapping' into a
##  'data_env'-environment attribute of the plots.

input_curlicues_EuStockMarkets = list(
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
        input_simulated <- list(
            TCS_type = "S", 
            window = "Tukey",
            Boot_Approx = NA_character_,
            confidence_interval = "95",
            bw_points = "0.6", 
            cut = .cut,
            frequency_range = c(0, 0.5),
            type = "par_five", 
            levels_Horizontal = v1,
            TS = TS_simulated, 
            S_type = ..S_type,
            point_type = "off_diag", 
            Approx = "Approx__1",
            Vi = ..Vi,
            Vj = ..Vj,
            levels_Vertical = v2, 
            global_local = "local",
            drop_annotation = TRUE)
        ##  Specify the input-list for dmbp.
        input_EuStockMarkets <- list(
            TCS_type = "S",
            window = "Tukey",
            Boot_Approx = "Select a bootstrap approximation",
            confidence_interval = "95",
            bw_points = "0.6",
            cut = .cut,
            frequency_range = c(0, 0.5),
            type = "par_five",
            levels_Horizontal = v1,
            TS = TS_EuStockMarkets,
            S_type = ..S_type,
            point_type = "off_diag",
            Approx = "Approx__1",
            Vi = ..Vi,
            Vj = ..Vj,
            levels_Vertical = v2, 
            global_local = "local")
        ##  Create the aparch-plot.
        .plots_simulated[[.node_name]]  <- LG_plot_helper(
            main_dir = ..main_dir_simulated,
            input = input_simulated,
            input_curlicues = input_curlicues_simulated)
        ##  Create the dmbp-plot.
        .plots_EuStockMarkets[[.node_name]] <- LG_plot_helper(
            main_dir = ..main_dir_EuStockMarkets,
            input = input_EuStockMarkets,
            input_curlicues = input_curlicues_EuStockMarkets)
    }
}
rm(..S_type, ..S_types, .cut, .LGp, .node_name, input_simulated,
   input_curlicues_simulated, input_EuStockMarkets, input_curlicues_EuStockMarkets,
   TS_simulated, TS_EuStockMarkets, v1, v2)

##  The use of 'drop_annotation=TRUE' has removed all the annotated
##  text from the plots in the list '.plots_simulated'.  Extract the data
##  to be used from the attributes, and store those in a separate
##  list.

annotated_text <- lapply(
    X = .plots_simulated,
    FUN = function(x)
        attributes(x)$curlicues$text)

###----------------------------------------------------------------###

##  Create a copy of the aparch-plots, then extract relevant data from
##  the dmbp-plots, and add these to the aparch-plots.

new_plots <- .plots_simulated
.range_list <- list()

for (.name in names(new_plots)) {
    ##  Add a pointer to the 'data_env'-environment that is an
    ##  attribute of the dmbp-plots (exported due to the
    ##  'data_extraction'-node in 'input_curlicues_EuStockMarkets').
    .data_env <- attributes(.plots_EuStockMarkets[[.name]])$data_env
    ##  Update the plots with the dmbp-data.
    new_plots[[.name]] <-
        new_plots[[.name]] +
        geom_line(
            mapping = .data_env$.geom_line_local$mapping, #aes(x = omega, y = orig),
            data = .data_env$.geom_line_local$data , #.orig,
            col = "black",
            lwd = ..line.lwd,
            lty = 1)
    ##  Find the 'ylim' that best fit the selected data.
    .range_list[[.name]] <- range(
        attributes(new_plots[[.name]])$ylim_list$ylim_restricted,
        attributes(.plots_EuStockMarkets[[.name]])$ylim_list$ylim_restricted)
}
rm(.name, .data_env, ..line.lwd)
## rm(.plots_simulated, .plots_EuStockMarkets)

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
        theme(axis.ticks = element_line(linewidth = 0.25),
              axis.ticks.length = unit(.04, "cm"),
              axis.text = element_text(size = 4.5))
}
rm(.name, .scaling_for_annotated_text, annotated_text)

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(..main_dir,
                              collapse = .Platform$file.sep),
                        .save_file)

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
