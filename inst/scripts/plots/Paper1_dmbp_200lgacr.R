## This script contains the code needed in order to recreate the
## Gaussian white noise example in the univariate paper.  In order for
## this script to work, it is necessary that the script that generates
## the data has been used first.  NOTE: This script is based on code
## that picks out the points along the diagonal by using numbers,
## i.e. '1' refers to the lower tail, '2' to the center and '3' to the
## upper tail.  This implies that an unexpected result might occur if
## the data-generating script was modified before it was used.

library(localgaussSpec)
library(ggplot2)
library(grid)

..main_dir <- "~/LG_DATA"
..TS <- "0fb42549ce13fce773c12b77463bdca8"
##  Note: The 'TS' value is based on the 'digest::digest'-function.
##  This is used in order to keep track of the different simulations,
##  and it is used to avoid the re-computation of a script that has
##  already been executed.  It might alas be cases where this value
##  can be influenced by the machine used for the computation, in
##  which case it might be necessary to update the 'TS'-value.

## ## ## data_dir_for_LG_shiny <-
## ## ##     c(ts.dir = ..TS,
## ## ##       approx.dir = "Approx__2")



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
        list(TCS_type = "C",
             window = "Tukey",
             Boot_Approx = "Nothing here to select", 
             TS_key = "other",
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
             Approx = "Approx__2",
             Vi = "Y",
             Vj = "Y",
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

##### TODO: Need to update package so attributes also are added in the
##### lag-case...

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
        theme(axis.ticks = element_line(size = 0.35),
              axis.ticks.length = unit(.08, "cm"),
              axis.text = element_text(size = 6))
}
rm(.name)

################################################################################
##  Add a red dotted line to emphasise the truncation level 'm'.  Note
##  that the line has been shifted '+.5' to avoid overlaying the local
##  Gaussian autocorrelation at that lag.

m <- 10
for (i in seq_along(..plot))
    ..plot[[i]] <- ..plot[[i]] + 
        geom_vline(xintercept = m + .5,
                   linetype = 3,
                   col = "red",
                   alpha = 0.8,
                   lwd = 0.5)
rm(i,m)



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


#####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#####




##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.


## TODO: Adjust save file later on, for now update plot to be used in
## paper.
.save_file <- file.path("~/Dropbox/arXiv/Article_1_JASA/figure", "dmbp_lag-1.pdf")

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(9, 1)))
print(..plot$lower.tail,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 1))
print(..plot$center,
      vp = viewport(
          layout.pos.row = 2,
          layout.pos.col = 1))
print(..plot$upper.tail,
      vp = viewport(
          layout.pos.row = 3,
          layout.pos.col = 1))

dev.off()

##  Crop the result
.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)






################################################################################

##  Create a plot showing the unweighted sum of the local Gaussian
##  autocorrelations.

cumsum_list <- lapply(
    X = ..plot,
    FUN = function(x) {
        cumsum(c(1, x$data$orig))
        ##  Add 1 for the lag zero term.
    })

##  Find the center of the x-range and y-range, in order for
##  plot-stamp to be added there

x_center <- length(cumsum_list$lower.tail) /2
y_center <- local({
    .tmp <- range(cumsum_list)
    .tmp[1] + diff(.tmp) /2
})



##  Create a data-frame to be used by ggplot:
lag_df_cumulative <- data.frame(
    lag = seq_along(cumsum_list$lower.tail) - 1,
    lower = cumsum_list$lower.tail,
    center = cumsum_list$center,
    upper = cumsum_list$upper.tail)

rm(cumsum_list)

##  Adjust to allow a plot of several graphs at once.
lag_df_cumulative <- reshape2::melt(data = lag_df_cumulative,
                                   id.vars = "lag")

####  NB: Some tweaking needed with regard to the legend position.
lag_df_cumulative_plot <- ggplot(
    data = lag_df_cumulative,
    mapping = aes(x = lag,
                  y = value,
                  colour = variable,
                  linetype = variable,
                  line.size = 0.1)) +
    geom_line(lwd = 0.3)  +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme(legend.title = element_blank(),
          legend.key.height = unit(.3, "cm"),
          legend.key.width = unit(.3, "cm"),
          legend.text = element_text(size = 6),
          legend.margin = margin(t = -3, r = 1, b = 1, l = 1, unit = "pt"),
          legend.position = c(0.08,0.7))



## Adjust to get the desired information related to stamp-plots
lag_df_cumulative_plot <- lag_df_cumulative_plot +
    annotate(geom = "text",
             label = "sum(rho[v](i), i==0, h)",
             parse = TRUE,
             x = x_center,
             y = y_center,
             size = annotated_text$lower.tail$annotated_df["plot_stamp", "size"] *
                 .scaling_for_annotated_text,
             alpha = annotated_text$lower.tail$annotated_df["plot_stamp", "alpha"],
             hjust = "center",
             vjust = "center") + 
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
    theme(axis.ticks = element_line(size = 0.35),
          axis.ticks.length = unit(.08, "cm"),
          axis.text = element_text(size = 6))




##  Save the desired file

.save_file <- file.path("~/Dropbox/arXiv/Article_1_JASA/figure", "dmbp_lag_cumsum-1.pdf")

pdf(file = .save_file) 


grid.newpage()
pushViewport(viewport(
    layout = grid.layout(13, 1)))
print(lag_df_cumulative_plot,
      vp = viewport(
          layout.pos.row = 1:2,
          layout.pos.col = 1))

dev.off()

##  Crop the result
.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
