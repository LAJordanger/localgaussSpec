##  Script for the investigation of the effect of selecting the point
##  v along the diagonal.

##  Need to include stuff related to heatmap and to level for the
##  cases of interest.

##  At the moment a bit of a mess, and most likely better split into
##  one script for 'dmt' and one for 'dmbp'.

#####---------------------------------------------------------------------------


##  Code needed for the creation of heatmaps.  I guess two separate
##  scripts might be better to include in the package, since that
##  would imply that it would be easier to tweak the desired details
##  when relevant.  For the time being, use the present setup for the
##  stuff of interest.




.dmbp_or_dmt <- "dmt"  # "dmbp"

.TSC_type <- "S"  # "C"
.spectra_f_or_F <- "f" # "F"
.heatmap_b_or_v = "v" #  "b"


################################################################################


if (.dmbp_or_dmt == "dmbp") {
    ..main_dir <- c("~", "LG_DATA_paper1_revision_dmbp")
    input <- list(TCS_type = .TSC_type,
                  window = "Tukey", Boot_Approx = "Nothing here to select", 
                  TS_key = "other", confidence_interval = "95", levels_Diagonal = 21L, 
                  bw_points = "0.5", cut = 10L, frequency_range = c(0, 0.5), 
                  type = "par_five", levels_Horizontal = 46, TS = "0fb42549ce13fce773c12b77463bdca8", 
                  S_type = "LS_a", levels_Line = 46, point_type = "on_diag", 
                  Approx = "Approx__1", Vi = "Y", Vj = "Y", levels_Vertical = 46, 
                  global_local = "local",
                  heatmap = TRUE,
                  heatmap_b_or_v = .heatmap_b_or_v,
                  spectra_f_or_F = .spectra_f_or_F,
                  drop_annotation = TRUE)
} else {
    ..main_dir <- c("~", "LG_DATA_paper1_revision_dmt")
    input <- list(TCS_type = .TSC_type,
                  window = "Tukey", Boot_Approx = "Nothing here to select", 
                  TS_key = "dmt", confidence_interval = "95", levels_Diagonal = 20L, 
                  bw_points = "0.5", cut = 10L, frequency_range = c(0, 0.5), 
                  type = "par_five", levels_Horizontal = 46, TS = "dmt_df710963a64b8b13ca269e38334d7c3b", 
                  S_type = "LS_a", levels_Line = 46, point_type = "on_diag", 
                  Approx = "Approx__1", Vi = "Y", Vj = "Y", levels_Vertical = 46, 
                  global_local = "local",
                  heatmap = TRUE,
                  heatmap_b_or_v = .heatmap_b_or_v,
                  spectra_f_or_F = .spectra_f_or_F,
                  drop_annotation = TRUE)
}




heatmap_plot <- LG_plot_helper(main_dir = ..main_dir, 
                        input = input,
                        input_curlicues = list(
                            NC_value = list(short_or_long_label = "short"),
                            title = list(
                                label = "Heatmap for the local Gaussian autospectrum",
                                element_text = list(
                                    size = 8))))

annotate_heatmap <- attributes(heatmap_plot)$curlicues$text

####  TODO: Need to add information to these plots too, in order for
####  the 'LG_explain_plot' to have something to work upon..  Not
####  prioritised at the moment.

## explanation <- LG_explain_plot(heatmap_plot_details = plot)


#####-----------------------------------------------------------------

##  And code related to the creation of the L2-part.



##  A script for the investigation of point/level (on the diagonal)
##  versus the L2-norm of the corresponding spectra.

##  This does the work, but it is necessary to get this into some
##  proper functions later on.

################################################################################
##  An attempt at getting a plot of level-vs-norm.  This is based on
##  the adjustments used for the plot of bandwidth-vs-norm, and is
##  thus only a solution for the auto-spectrum and diagonal points.

##  A minor selection in order for this to switch between showing the
##  norms of the spectra, and showing the distance to the norm of the
##  global spectra.


##  Reminder: Should add a line for the norm of the global spectrum
##  too.


..type <- "norms" # "distances"
## ##  Defined above, and must match
## .dmbp_or_dmt <- "dmt" # "dmbp"
.TCS_type <- "S" 

if (.dmbp_or_dmt == "dmbp") {
    ..main_dir <- c("~", "LG_DATA_paper1_revision_dmbp")
    input <- list(TCS_type = .TCS_type,
                  window = "Tukey", Boot_Approx = "Nothing here to select", 
                  TS_key = "other", confidence_interval = "95", levels_Diagonal = 21L, 
                  bw_points = "0.5", cut = 10L, frequency_range = c(0, 0.5), 
                  type = "par_five", levels_Horizontal = 46, TS = "0fb42549ce13fce773c12b77463bdca8", 
                  S_type = "LS_a", levels_Line = 46, point_type = "on_diag", 
                  Approx = "Approx__1", Vi = "Y", Vj = "Y", levels_Vertical = 46, 
                  global_local = "local",
                  L2_distance_normal = TRUE,
                  L2_inspection_vbmL = "v",
                  heatmap = FALSE,
                  heatmap_b_or_v = "v",
                  drop_annotation = TRUE)
} else {
    ..main_dir <- c("~", "LG_DATA_paper1_revision_dmt")
    input <- list(TCS_type = .TCS_type,
                  window = "Tukey", Boot_Approx = "Nothing here to select", 
                  TS_key = "dmt", confidence_interval = "95", levels_Diagonal = 20L, 
                  bw_points = "0.5", cut = 10L, frequency_range = c(0, 0.5), 
                  type = "par_five", levels_Horizontal = 46, TS = "dmt_df710963a64b8b13ca269e38334d7c3b", 
                  S_type = "LS_a", levels_Line = 46, point_type = "on_diag", 
                  Approx = "Approx__1", Vi = "Y", Vj = "Y", levels_Vertical = 46, 
                  global_local = "local",
                  L2_distance_normal = TRUE,
                  L2_inspection_vbmL = "v",
                  heatmap = FALSE,
                  heatmap_b_or_v = "v",
                  drop_annotation = TRUE)
}

.extracted <- LG_plot_helper(main_dir = ..main_dir, 
                             input = input,
                             input_curlicues = list(
                                 x.label_low_high = c(0, 1),
                                 ## v_value = list(include = FALSE),
                                 NC_value = list(short_or_long_label = "short")),
                             .extract_LG_data_only = TRUE)
rm(..main_dir, input)

.env <- .extracted$..env
.env$look_up <- .extracted$look_up
look_up <- .env$look_up
rm(.extracted)

annotate_norm <- look_up$curlicues$text


##  Use the norm-function to compute the different norms and
##  distances.

.L2_norm <- localgaussSpec:::LG_spectrum_norm(
        C1_env = .env,
        W1 = localgaussSpec:::myWindows[[look_up$window]](.cut = look_up$cut))
##  REMINDER: This is not used yet in the script below, it is included
##  here as a placeholder for the time when it is due to adjust this mess.


################################################################################


##  First: Extract the correlations of interest, with a
##  restriction to one of the points of interest.


library(ggplot2)

..lag_values <- names(.env[[look_up$cache$weights_f]][[as.character(look_up$cut)]])
..restrict <- list(variable = "rho",
                   bw_points = look_up$bw_points,
                   lag = ..lag_values)
.data <- restrict_array(
    .arr = .env[["LGC_array_local"]]$on_diag,
    .restrict = ..restrict,
    .drop = TRUE,
    .never_drop = c("lag", "bw_points"))
.global_data <- restrict_array(
    .arr = .env[["LGC_array_global"]],
    .restrict = list(TS = "TS_for_analysis",
                     lag = ..lag_values),
    .drop = TRUE,
    .never_drop = c("lag", "content"))
rm(..lag_values, ..restrict)
## str(.data)
## str(.global_data)

## Next: Depending on the value of '..type', either compute the
## product of the correlations with the weights — or replace the
## '.data' with the values obtained by subtracting the values
## in '.global_data'.

if (..type == "norms") {
    .weighted_data <- multiply_arrays(
        .arr1 = .data,
        .arr2 = .env[[look_up$cache$weights_f]][[as.character(look_up$cut)]])
    .global_weighted_data <- multiply_arrays(
        .arr1 = .global_data,
        .arr2 = .env[[look_up$cache$weights_f]][[as.character(look_up$cut)]])

    ## str(.weighted_data)
    ## str(.global_weighted_data)

    ##  Next: Compute the norms.  In this "on_diagonal"-case for local
    ##  Gaussian autocorrelations, the squared norms are obtained by
    ##  squaring the '.weighted_data', summing over the lags,
    ##  multiplying with 2 (due to folding) and add 1 (to include the
    ##  lag-zero-term).  Take the square root to get the norms.
    .the_norms <- sqrt(
        1 + 2 * my_apply(
                    X = .weighted_data^2,
                    MARGIN = "levels",
                    FUN = sum))
    .the_global_norm <- sqrt(
        1 + 2 * my_apply(
                    X = .global_weighted_data^2,
                    MARGIN = "content",
                    FUN = sum))

    ## str(.the_norms)
    ## str(.the_global_norm)

    ## str(.L2_norm$C1l_vs_C1g)

} else {
    ##  Tweak the dimension of '.global_data' in order for
    ##  'add_arrays' to work.
    .content_dim <- which(names(dimnames(.global_data)) == "content")
    .global_data  <- structure(
        .Data = .global_data,
        .Dim  = dim(.global_data)[-.content_dim],
        .Dimnames = dimnames(.global_data)[-.content_dim])
    ##  Perform the subtraction
    .data <- add_arrays(
        .arr1 = .data,
        .arr2 = -.global_data)
    .weighted_data <- multiply_arrays(
        .arr1 = .data,
        .arr2 = .env[[look_up$cache$weights_f]][[as.character(look_up$cut)]])
    ## The norms computed below is now the distances instead.
    .the_norms <- sqrt(
        1 + 2 * my_apply(
                    X = .weighted_data^2,
                    MARGIN = "levels",
                    FUN = sum))

}




##  Need to adjust the dimension-names for '.the_norms', i.e. we want
##  to have them replaced with the percentages they correspond to.  Recycling som code from 'LG_plot_heatmap'.

.quantile_levels <- vapply(
    X = strsplit(x = dimnames(.the_norms)$levels, 
                 split = "_"), FUN = function(..x) pnorm(as.numeric(..x[1])), 
    FUN.VALUE = numeric(1))
dimnames(.the_norms)$levels <- .quantile_levels
rm(.quantile_levels)


## hist(.the_norms)
## plot(density(.the_norms))
## boxplot(as.vector(.the_norms))

##  Create the data-frame needed for the plot.

.df <- reshape2::melt(data = .the_norms)
##  str(.df)

##  Specify stuff to be included in the plot.
.plot_title <- sprintf("%s for the m=%s local Gaussian autospectrum",
                       ifelse(test = {..type == "norms"},
                              yes  = "Percentiles vs. norm",
                              no   = "Distance local-global"),
                       look_up$m_selected)
.aes_mapping <- aes(x = levels,
                    y = value)
.xlab_expression <- "v = diagonal point/percentage"
.ylab_expression <- "" #expression(group("||", "f","||"))

##  Create the plot.
.plot <- ggplot(data=.df,
                mapping = .aes_mapping) +
    ## ### Skip point when shrinked in grid-setup.
    ## geom_point(size = 0.1) +
    geom_line(size = 0.1,
              colour = "brown") +    
    ##  Adjust legend and labels.
    labs(fill = NULL) +
    xlab(.xlab_expression) +
    ylab(.ylab_expression) +
    ##  Add the title
    ggtitle(label = .plot_title) +
    theme(plot.title = element_text(hjust = 0.5,
                                    vjust = 0,
                                    size = 8,
                                    colour = "brown")) +
    ##  Adjust the x-axis.  The values will in this case always be
    ##  percentiles between 0 and 1
    ggplot2::scale_x_continuous(
                 limits = c(0,1),
                 labels = scales::percent)




###  Add points that shows the 'v'-values used in most of the plots of
###  the local Gaussian spectral densities.

.levels <- c(0.10, 0.50, 0.90)
.plot <-
    .plot +
    annotate(geom = "point",
             x = .levels,
             y = .df$value[which(.df$levels %in% .levels)],
             size = .7,
             shape = 1,
             colour = "blue")
rm(.levels)



##  Add information about the norm of the global spectrum when that is
##  relevant
if (..type == "norms") {
    ##  Add information about the value of the global norm.
    .global_label <- sprintf("D*(f^'%s'*(omega)) == '%s'",
                             look_up$m_selected,
                             .the_global_norm)
    .plot <- .plot +
        ##  Add a line for the norm of the global spectrum.
        geom_hline(yintercept = .the_global_norm,
                   size = 0.1,
                   colour = "red",
                   lty = 1)  +
        ##  Add the label with the value.
        annotate(
            geom = "text",
            x = 0,
            y = .the_global_norm,
            size = 2,
            label = .global_label,
            col = "red",
            vjust = -0.3,
            hjust = "inward",
            parse = TRUE)

} 


##  Tweak the size of the annotated stuff so it looks decent after the
##  grid-plot has been saved.

.scale <- 0.4

annotate_heatmap$annotated$size <- 
    .scale * annotate_heatmap$annotated$size

annotate_norm$annotated$size <- 
    .scale * annotate_norm$annotated$size

heatmap_plot <-
    heatmap_plot +
    eval(annotate_heatmap$annotated)


.plot <-
    .plot +
    eval(annotate_norm$annotated)


##  Adjust details related to the axis ticks/text, and also adjust the
##  way the axis-labels are given.


size_v <- annotate_norm$annotated_df["NC_value", "size"] * .scale
v_just_v <- annotate_norm$annotated_df["NC_value", "vjust"]

.plot <- .plot +
    annotate(geom = "text",
             label = "v",
             parse = TRUE,
             x = Inf,
             y = -Inf,
             size = size_v,
             hjust = "inward",
             vjust = v_just_v) + 
        xlab(label = NULL) +
        theme(axis.ticks = element_line(size = 0.3),
              axis.ticks.length = unit(.06, "cm"),
              axis.text = element_text(size = 6))







#####-----------------------------------------------------------------
##  Adjust the title manually (update code later on), including
##  adjustments of the axes.
heatmap_plot <-
    heatmap_plot +
    ggtitle(label = "Heatmap for the local Gaussian autospectrum") +
    theme(plot.title = element_text(hjust = 0.5,
                                    vjust = 0,
                                    size = 8,
                                    colour = "brown")) +
    annotate(geom = "text",
             label = "omega",
             parse = TRUE,
             x = Inf,
             y = -Inf,
             size = 2,
             hjust = "inward",
             vjust = "inward") +
#####  TODO: Adjust 'size' and 'vjust' based on annotations-data.

    annotate(geom = "text",
             label = "v",
             parse = TRUE,
             x = -Inf,
             y = Inf,
             size = 2,
             hjust = "inward",
             vjust = "inward") +
    xlab(label = NULL) +
    ylab(label = NULL)  +
    theme(axis.ticks = element_line(size = 0.3),
          axis.ticks.length = unit(.06, "cm"),
          axis.text = element_text(size = 6))




.file <- sprintf("~/Dropbox/arXiv/Article_1_JASA/figure/%s_heatmap_and_%s",
                 .dmbp_or_dmt,
                 ifelse(test = {..type == "norms"},
                        yes  = "_levels_vs_norm.pdf",
                        no   = "_distance_levels-norm.pdf"))


## .file <- sprintf("~/LG_DATA_paper1_revision_%s/%s_heatmap_and_%s",
##                  .dmbp_or_dmt,
##                  .dmbp_or_dmt,
##                  ifelse(test = {..type == "norms"},
##                         yes  = "_levels_vs_norm.pdf",
##                         no   = "_distance_levels-norm.pdf"))

##  REMINDER: https://stackoverflow.com/questions/10776139/r-grid-layout-title



##  Some tweaking that governs the ratios between the subplots.
.x <- 25
.y <- 7
.z <- 4


.heatmap.pos.row <- 1:.y
.plot.pos.row <- .y + 1:.z

library(grid)

pdf(.file)

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(.x,3)))
#####  The  pseudo-normalised data, with the bandwidth-bands around the levels
print(heatmap_plot  + 
    theme(legend.key.width = unit(0.15, "cm"),
          legend.text = element_text(size = 4.5)),
      vp = viewport(
          layout.pos.row = .heatmap.pos.row,
          layout.pos.col = 1:2))
print(.plot,
      vp = viewport(
          layout.pos.row = .plot.pos.row,
          layout.pos.col = 1:2))

dev.off()

##  Crop the result
.crop_code <- sprintf("pdfcrop --margins 5 %s %s", .file, .file)
system(.crop_code)

