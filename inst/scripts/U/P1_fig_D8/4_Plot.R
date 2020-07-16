###----------------------------------------------------------------###
##  The dmbp-example from P1_fig_D8.

##  This script generates a distance-based plot that investigates how
##  the estimated local Gaussian autocorrelations behaves when the
##  truncation level varies.

###----------------------------------------------------------------###

##  In order for this script to work, it is necessary that the script
## '2_Data.R' from P1_fig_04 has been used first.

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

##  WARNING: The distance-part of this script contains solutions based
##  on internal code from the 'localgaussSpec'-package.  This implies
##  that it almost certainly will have to be updated at some point.
##  The plan is to implement a proper function for this task, and the
##  mess in this script will then be replaced with proper code.

#####------------------------------------------------------------#####

##  Specify the packaces required for this script.

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

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_04.D8")
..TS <- "0fb42549ce13fce773c12b77463bdca8"
..Approx <- "Approx__1"

#####------------------------------------------------------------#####

##  Define the 'input'-list that specifies the content of the plot.
##  Some of the information in this list is redundant for the present
##  plot, but it is necessary to update the plot-function before those
##  parts can be removed from the list below.

.input_common <- list(
    TCS_type = "S", # "C"
    window = "Tukey",
    Boot_Approx = "Nothing here to select", 
    confidence_interval = "95",
    bw_points = "0.5",
    cut = 10,
    frequency_range = c(0, 0.5), 
    type = "par_five",
    TS = ..TS,
    S_type = "LS_a",
    point_type = "on_diag", 
    Approx = ..Approx,
    Vi = "Y",
    Vj = "Y",
    global_local = "local",
    L2_inspection_vbmL = "m")

.names <- c("lower", "center", "upper")
.env_list <- list()

for (.level in 1:3) {
        .name <- .names[.level]
        .env_list[[.name]]  <- localgaussSpec:::LG_plot_helper_extract_data_only(
                main_dir = ..main_dir,
                input = c(.input_common,
                          list(levels_Diagonal = .level,
                               L2_distance_normal = TRUE)),
                input_curlicues = list(
                    x.label_low_high = c(0, 200),
                    NC_value = list(short_or_long_label = "short")))
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
                             W1 = localgaussSpec:::myWindows$Tukey(.cut=.cut),
                             C2_env = .tmp$..env,
                             W2 = localgaussSpec:::myWindows$Tukey(.cut=.cut+lag_diff))
    })
    names(.norms_list[[.name]]) <- {2:(200+1-lag_diff) -1}
}

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

##  The global should also be extracted.  This is contained
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

##  For the present investigation we want to inspect the norms and the
##  distances between the norms.  Extract those from the
##  'arrays_list'.  Note that it is necessary to extract two norms
##  from the last node of the 'arrays_list'.


.L2_distances_list <- list()
for (.name in names(.arrays_list)) {
    .L2_distances_list[[.name]]  <- local({
        leanRcoding::restrict_array(
                         .arr = .arrays_list[[.name]],
                         .restrict = list(value = "f1_distance_f2"),
                         .drop  = TRUE)
    })
}

###----------------------------------------------------------------###

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

###----------------------------------------------------------------###

##  Tweak the size of the annotated stuff so it looks decent after the
##  grid-plot has been saved.

size_m <- .annoted_labels[[1]]$annotated_df["NC_value", "size"] * 0.4
v_just_m <- .annoted_labels[[1]]$annotated_df["NC_value", "vjust"]

for (.name in names(.distance_plots)) {
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

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_D8.pdf")
rm(..main_dir, ..TS)

pdf(.save_file)

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

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)
