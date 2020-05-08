#####------------------------------------------------------------#####
##  This script aims at a visualisation of the sensitivity of the
##  resulting spectral densities due to the truncation level 'm', and
##  this is based on the existence of the 200 first lags for the
##  dmbp-data.

##  Two plots are created (and saved): One showing the norms and one
##  showing the distances between successive truncation levels.  Note:
##  It is also possible to consider the percent-wise changes in the
##  norms, but those might be too dependent on the size of the initial
##  autocorrelations, and they have not been included in this script.

##  Warning: This script (in its present incarnation) contains quite a
##  lot of tweaking of code that normally would be hidden in the inner
##  part of the 'localgaussSpec'-package.

library(localgaussSpec)

..main_dir <- c("~", "LG_DATA")
.input_common <- list(
    TCS_type = "S", # "C"
    window = "Tukey",
    Boot_Approx = "Nothing here to select", 
    TS_key = "other",
    confidence_interval = "95",
    bw_points = "0.5",
    cut = 10,
    frequency_range = c(0, 0.5), 
    type = "par_five",
    TS = "0fb42549ce13fce773c12b77463bdca8", 
    S_type = "LS_a",
    point_type = "on_diag", 
    Approx = "Approx__2",
    Vi = "Y",
    Vj = "Y",
    global_local = "local",
    L2_inspection_vbmL = "m")

.names <- c("lower", "center", "upper")
.env_list <- list()

for (.level in 1:3) {
        .name <- .names[.level]
        .env_list[[.name]]  <- LG_plot_helper(
            main_dir = ..main_dir, 
            input = c(.input_common,
                      list(levels_Diagonal = .level,
                           L2_distance_normal = TRUE)),
            input_curlicues = list(
                x.label_low_high = c(0, 200),
                NC_value = list(short_or_long_label = "short")),
            .extract_LG_data_only = TRUE)
}

##  Create a list with the annotated labels, as these will be needed
##  later on when the plots are to be created in this script.
.annoted_labels <- lapply(
    X = .env_list,
    FUN = function(x) {
        x$look_up$curlicues$text
    })
##  Create a tweaked version to be used for the global case.

.annoted_labels$global <- local({
    .tmp <- .annoted_labels[[1]]
    ##  Update the initial label, and set those referring to 'v', 'b'
    ##  and 'NC' to the empty string.
    .main_stamp <- gsub(
        pattern = "\\[v\\]",
        replacement = "",
        x = .tmp$annotated$label[1],
        perl = TRUE)
    .nR_info <- .tmp$annotated$label[length(.tmp$annotated$label)]
    .tmp$annotated$label[] <- ""
    .tmp$annotated$label[1] <- .main_stamp
    .tmp$annotated$label[length(.tmp$label)] <- .nR_info
    .tmp
})



#####-----------------------------------------------------------------
##  Create a lists with all the norms.
.norms_list <- list()

for (.name in names(.env_list)) {
    .tmp <- .env_list[[.name]]
    .tmp$..env$look_up <- .tmp$look_up
    lag_diff <- 1
    .norms_list[[.name]] <- lapply(
    X = 2:(200+1-lag_diff),
    FUN = function(.cut) {
        localgaussSpec:::LG_spectrum_norm(
                             C1_env = .tmp$..env,
                             localgaussSpec:::myWindows$Tukey(.cut=.cut),
                             C2_env = .tmp$..env,
                             localgaussSpec:::myWindows$Tukey(.cut=.cut+lag_diff))
    })
    names(.norms_list[[.name]]) <- {2:(200+1-lag_diff) -1}
}

#####-----------------------------------------------------------------
##  Extract the arrays that can be used to plot the result related to
##  the norms.
.arrays_list <- list()

for (.name in names(.norms_list)) {
    .extract <- "C1l_vs_C2l"
    .arrays_list[[.name]] <-
        leanRcoding::my_abind(
                         lapply(
                             X = .norms_list[[.name]],
                             FUN = function(i)
                                 i[[.extract]]),
                         .list = TRUE,
                         .list_new.dnn = "lag")
}

##  The global case must also be extracted.  This is contained
##  everywhere, and as such the first node can be used for this
##  purpose.

.arrays_list$global <- 
    leanRcoding::my_abind(
                     lapply(
                         X = .norms_list[[1]],
                         FUN = function(i)
                             i[["C1g_vs_C2g"]]),
                     .list = TRUE,
                     .list_new.dnn = "lag")


#####-----------------------------------------------------------------
##  For the present investigation we want to inspect the norms and the
##  distances between the norms.  Extract those from the
##  'arrays_list'.  Note that it is necessary to extract two norms
##  from the last node of the 'arrays_list'.

.L2_norm_list <- list()
for (.name in names(.arrays_list)) {
    .L2_norm_list[[.name]]  <- local({
        .L2_norm_f1 <- leanRcoding::restrict_array(
                                        .arr = .arrays_list[[.name]],
                                        .restrict = list(value = "L2_norm_f1"),
                                        .drop  = TRUE)
        .L2_norm_f2 <- leanRcoding::restrict_array(
                                        .arr = .arrays_list[[.name]],
                                        .restrict = list(value = "L2_norm_f2"),
                                        .drop  = TRUE)
        ##  Adjust '.L2_norm_f1' with the lag 0-value (always 1 for univariate
        ##  time series), and add the lag 200-value from '.L2_norm_f2'
        structure(
            .Data = c(1, .L2_norm_f1, .L2_norm_f2[as.character(200-1)]),
            .Names = c(0, names (.L2_norm_f1), 200))
    })
}

.L2_distances_list <- list()
for (.name in names(.arrays_list)) {
    .L2_distances_list[[.name]]  <- local({
        leanRcoding::restrict_array(
                         .arr = .arrays_list[[.name]],
                         .restrict = list(value = "f1_distance_f2"),
                         .drop  = TRUE)
    })
}

#####-----------------------------------------------------------------
##  Create the lists with the desired plots.

library(ggplot2)

##  Ensure that the same ylim is used for all the cases.
.ylim <- ylim(range(.L2_norm_list))


.norm_plots <- new.env()
for (.name in names(.L2_norm_list)) {
    .norms <- .L2_norm_list[[.name]]
    .annotations <- .annoted_labels[[.name]]$annotated
    ##  Reduce the size to cope with the shrinking of the plot.
    .annotations$size  <- .4 * .annotations$size
    .norm_plots[[.name]]  <-
        ggplot(data = data.frame(x = seq_along(.norms),
                                 y = .norms)) +
        geom_step(aes(x = x,
                      y = y),
                  size = .33,
                  alpha = 0.5) +  
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        .ylim + 
        ##  Add details about content when required.
        eval(.annotations)
}

##  Ensure that the same ylim is used for all the cases.
.ylim <- ylim(range(.L2_distances_list))
.distance_plots <- list()
for (.name in names(.L2_distances_list)) {
    .distances <- .L2_distances_list[[.name]]
    .annotations <- .annoted_labels[[.name]]$annotated
    ##  Need to manually update the plot-stamp-label, since the main
    ##  code does not yet include this variant.
    .annotations$label[1] <- ifelse(
        test = {.name == "global"},
        yes  = "D(f^{m+1}*(omega)-f^m*(omega))",
        no   = "D(f[v]^{m+1}*(omega)-f[v]^m*(omega))")
    ##  Reduce the size to cope with the shrinking of the plot.
    .annotations$size  <- .4 * .annotations$size
    .distance_plots[[.name]]  <-
        ggplot(data = data.frame(x = seq_along(.distances),
                                 y = .distances)) +
        geom_step(aes(x = x,
                      y = y),
                  size = .33,
                  alpha = 0.5) +  
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        .ylim +
        ##  Add details about content when required.
        eval(.annotations)
}


## ## ## ##  Perform some additional adjustments to the size of axis
## ## ## ##  ticks/text, etc.

## $annotated

size_m <- .annoted_labels[[1]]$annotated_df["NC_value", "size"] * 0.4
v_just_m <- .annoted_labels[[1]]$annotated_df["NC_value", "vjust"]


for (.name in names(.norm_plots)) {
    .norm_plots[[.name]] <- .norm_plots[[.name]] +
        annotate(geom = "text",
                 label = "m",
                 parse = TRUE,
                 x = Inf,
                 y = -Inf,
                 size = size_m,
                 hjust = "inward",
                 vjust = v_just_m) + 
        xlab(label = NULL) +
        theme(axis.ticks = element_line(size = 0.3),
              axis.ticks.length = unit(.06, "cm"),
              axis.text = element_text(size = 6))
    .distance_plots[[.name]] <- .distance_plots[[.name]] +
        annotate(geom = "text",
                 label = "m",
                 parse = TRUE,
                 x = Inf,
                 y = -Inf,
                 size = size_m,
                 hjust = "inward",
                 vjust = v_just_m) + 
        xlab(label = NULL) +
        theme(axis.ticks = element_line(size = 0.3),
              axis.ticks.length = unit(.06, "cm"),
              axis.text = element_text(size = 6))

    
}



#####-----------------------------------------------------------------
##  Define file-names relatively the directory that contained the
##  data.  Collect the relevant plots in a grid, with some extra white
##  space that will be cropped away at the end.  (The code for the
##  cropping might be OS-dependent.)

library(grid)
.file <- file.path(
        "~/Dropbox/arXiv/Article_1_JASA/figure/",
    ## paste(..main_dir,
    ##                          collapse = .Platform$file.sep),
    "m_sensitivity_dmbp_norm.pdf")

##  The plots showing the norms.
pdf(file = .file)
grid.newpage()
pushViewport(viewport(
    layout = grid.layout(16, 2)))
print(.norm_plots$global,
      vp = viewport(
          layout.pos.row = 1:3,
          layout.pos.col = 1))
print(.norm_plots$lower,
      vp = viewport(
          layout.pos.row = 1:3,
          layout.pos.col = 2))
print(.norm_plots$center,
      vp = viewport(
          layout.pos.row = 4:6,
          layout.pos.col = 1))
print(.norm_plots$upper,
      vp = viewport(
          layout.pos.row = 4:6,
          layout.pos.col = 2))
dev.off()

##  Crop the result
.crop_code <- sprintf("pdfcrop --margins 5 %s %s", .file, .file)
system(.crop_code)

##  Next up, the percentages-case.

.file <- file.path(
    ## paste(..main_dir,
    ## collapse = .Platform$file.sep),
    "~/Dropbox/arXiv/Article_1_JASA/figure/",
    "m_sensitivity_dmbp_percentages.pdf")

## ##  The plots showing the distances.
## pdf(file = .file)
## grid.newpage()
## pushViewport(viewport(
##     layout = grid.layout(16, 2)))
## print(.distance_plots$global,
##       vp = viewport(
##           layout.pos.row = 1:3,
##           layout.pos.col = 1))
## print(.distance_plots$lower,
##       vp = viewport(
##           layout.pos.row = 1:3,
##           layout.pos.col = 2))
## print(.distance_plots$center,
##       vp = viewport(
##           layout.pos.row = 4:6,
##           layout.pos.col = 1))
## print(.distance_plots$upper,
##       vp = viewport(
##           layout.pos.row = 4:6,
##           layout.pos.col = 2))
## dev.off()

##  Adjusted solution, skipping the global part

##  The plots showing the distances.
pdf(file = .file)



grid.newpage()
pushViewport(viewport(
    layout = grid.layout(32, 1)))
print(.distance_plots$upper,
      vp = viewport(
          layout.pos.row = 1:4,
          layout.pos.col = 1))
print(.distance_plots$center,
      vp = viewport(
          layout.pos.row = 5:8,
          layout.pos.col = 1))
print(.distance_plots$lower,
      vp = viewport(
          layout.pos.row = 9:12,
          layout.pos.col = 1))

dev.off()



##  Crop the result
.crop_code <- sprintf("pdfcrop --margins 5 %s %s", .file, .file)
system(.crop_code)
#####------------------------------------------------------------#####
