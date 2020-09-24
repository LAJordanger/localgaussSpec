###----------------------------------------------------------------###
##  The "local trigonometric" example from P1_fig_F4.

##  An investigation of the effect on the estimated pointwise
##  confidence intervals as the block length varies.  This was
##  included in order to check that the dmbp-data in P1_fig_F2 for
##  some reason provided an exceptional case.

##  The three usual points along the diagonal are available, but only
##  the lower tail is considered in this script.  Change the value of
##  '.levels_Diagonal' to '2' (center) or '3' (upper tail) for a
##  similar investigation of those points.

#####------------------------------------------------------------#####

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

#####------------------------------------------------------------#####

##  Specify the packages required for this script.

library(localgaussSpec)
library(ggplot2)
library(grid)


#####------------------------------------------------------------#####

##  Define the directory- and file components needed for the
##  extraction of the data.  The path to the main directory is given
##  as a vector since '.Platform$file.sep' depends on the OS.  Note
##  that these values must correspond to those that are used in the
##  script '2_Data.R', so any modifications there must be mirrored in
##  this script.

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_F4")
..TS <- "ebf2ecaa1791c56724dd4490ad408a9e"
..Approx <- "Approx__1"

#####------------------------------------------------------------#####

##  Define the 'input'-list that specifies the content of the plot.
##  Some of the information in this list is redundant for the present
##  plot, but it is necessary to update the plot-function before those
##  parts can be removed from the list below.

.levels_Diagonal <- 1L

input_common <- list(
    TCS_type = "S",
    window = "Tukey",
    confidence_interval = "95",
    levels_Diagonal = .levels_Diagonal,
    bw_points = "0.5", 
    cut = 10L,
    frequency_range = c(0, 0.5),
    type = "par_five", 
    TS = ..TS,
    S_type = "LS_a",
    levels_Line = 2,
    point_type = "on_diag", 
    Approx = "Approx__1",
    Vi = "Y",
    Vj = "Y",
    global_local = "local",
    L2_distance_vbmL = "L",
    drop_annotation = TRUE)

###----------------------------------------------------------------###

##  Create the list of plots for the four cases to be included.

..plot <- list()

.L_values <- c(10, 25, 50, 69)

..line.size <- 0.2

for (.L in .L_values) {
    ##  Identify details needed for the loading of the correct data.
    .name <- as.character(.L)
    ##  Adjust for the fact that "10" is the first one stored.
    .i <- .L -9
    .Boot_Approx <- sprintf("Boot_Approx_%s%s",
                            ifelse(test = .i <= 9,
                                   yes  = "_",
                                   no   = ""),
                            .i)
    rm(.i)
    input <- c(
        input_common,
        list(
            Boot_Approx = .Boot_Approx))
    ##  Store the ggplot, with adjustments.
    ..plot[[.name]]  <- LG_plot_helper(
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
}
rm(.L, .L_values, .name, .Boot_Approx, input_common, input)

###----------------------------------------------------------------###

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

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

##  This script need two save files.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_F4.pdf")
rm(..main_dir, ..TS)

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(7, 2)))
print(..plot[["10"]],
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 1))
print(..plot[["25"]],
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 2))
print(..plot[["50"]],
      vp = viewport(
          layout.pos.row = 2,
          layout.pos.col = 1))
print(..plot[["69"]],
      vp = viewport(
          layout.pos.row = 2,
          layout.pos.col = 2))

dev.off()

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)
