###----------------------------------------------------------------###
##  The dmbp-example from P1_fig_F2.

##  This creates a few plots related to the sensitivity analysis of
##  the block lenght L in the block bootsrap.  The script requires
##  that the code in "2_Data.R" has been used to create a collection
##  of data that can be used for this investigation.

##  The three usual points along the diagonal are available, but only
##  the lower tail is considered in this script.  Change the value of
##  'levels_Diagonal' to '2' (center) or '3' (upper tail) for a
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

###----------------------------------------------------------------###

##  WARNING: The distance-part of this script contains solutions based
##  on internal code from the 'localgaussSpec'-package.  This implies
##  that it almost certainly will have to be updated at some point.
##  The plan is to implement a proper function for this task, and the
##  mess in this script will then be replaced with proper code.

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

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_F2.F3")
..TS  <- "0fb42549ce13fce773c12b77463bdca8"
..Approx <- "Approx__1"

#####------------------------------------------------------------#####

##  Define the 'input'-list that specifies the content of the plot.
##  Some of the information in this list is redundant for the present
##  plot, but it is necessary to update the plot-function before those
##  parts can be removed from the list below.

.levels_Diagonal <- 1L
.global_local <- "local"  # "global"

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
    Approx = ..Approx,
    Vi = "Y", Vj = "Y",
    global_local = .global_local,
    L2_distance_vbmL = "L",
    drop_annotation = TRUE)


block_length_vec <- 10:69

##  An indicator used for some minor tweaking of the settings.

..type <- "norms"
## ..type <- "changes_in_norms"


## Find suitable positions for the x-component of the labels.

x.label_low_high_percentages  <-
    c(block_length_vec[1],
      rev(block_length_vec)[1])

x.label_low_high_norm <- 
    x.label_low_high_percentages +
    .5 * c(-1, 1) * diff(block_length_vec)[1]

###----------------------------------------------------------------###

##  Create a list that can be used to collect the data of interest.

env_list <- list()

for (i in seq_along(block_length_vec)) {
    ##  Identify details needed for the loading of the correct data.
    block_length  <- block_length_vec[i]
    .Boot_Approx <- sprintf("Boot_Approx_%s%s",
                            ifelse(test = i <= 9,
                                   yes  = "_",
                                   no   = ""),
                            i)
    input <- c(
        input_common,
        list(Boot_Approx = .Boot_Approx,
             L2_distance_plot = TRUE))
    ##  Extract the data, adjust it to the required format.
    .zzz <- localgaussSpec:::LG_plot_helper_extract_data_only(
            main_dir = ..main_dir,
            input = input,
            input_curlicues = list(
                x.label_low_high = x.label_low_high_norm,
                NC_value = list(short_or_long_label = "short")))
    .env <- .zzz$..env
    .env$look_up <- .zzz$look_up
    env_list[[as.character(block_length)]] <- .env
}

##  Extract the annotations for the norms from the list above, and
##  then "repeat" one case in order to get hold of the annotations
##  needed for the plot showing the changes in percentages.

###----------------------------------------------------------------###

input <- c(
    input_common,
    list(
        Boot_Approx = .Boot_Approx,
        L2_distance_percentages = TRUE))
.tmp <- localgaussSpec:::LG_plot_helper_extract_data_only(
        main_dir = ..main_dir,
        input = input,
        input_curlicues = list(
            x.label_low_high = x.label_low_high_percentages,
            NC_value = list(short_or_long_label = "short")))
annotation_for_percentages <- .tmp$look_up$curlicues$text

##  Collect all the relevant details in one list.

.annotations_text <- list(
    norms = env_list[[1]]$look_up$curlicues$text,
    changes_in_norms = annotation_for_percentages)

rm(.Boot_Approx, .env, .tmp, .zzz, block_length,
   block_length_vec, i, input, input_common, x.label_low_high_norm,
   x.label_low_high_percentages, annotation_for_percentages)

###----------------------------------------------------------------###

##  Create a list with the information about the norms of interest.

.block_list <- lapply(
    X = head(seq_along(env_list), n=-1),
    FUN = function(i){
        localgaussSpec:::LG_spectrum_norm(
                             C1_env = env_list[[i]],
                             W1 = localgaussSpec:::myWindows$Tukey(.cut=10+1),
                             C2_env = env_list[[i+1]],
                             W2 = localgaussSpec:::myWindows$Tukey(.cut=10+1))
    })
names(.block_list) <- head(names(env_list), n=-1)

##  Create an array with information about the norms and distances.

.key <- ifelse(
    test = (.global_local == "global"),
    yes  = "C1g_vs_C2g",
    no   = "C1l_vs_C2l")

C1_vs_C2_list <- lapply(
    X = .block_list,
    FUN = function(bl) {
        bl[[.key]]
    })
rm(.key)

.new_dnn <- "block_length"
C1_vs_C2_array <- leanRcoding::my_abind(C1_vs_C2_list,
                                        .list = TRUE,
                                        .list_new.dnn = .new_dnn)

##  Add a helper-function to extract the norms, i.e. taking care of
##  the tweaking required for the last part.

extract_L2_norm <- function(.array, .new_dnn, .new_max) {
    .never_drop <- setdiff(x = names(dimnames(.array)),
                           y = "value")
    .main_part <- leanRcoding::restrict_array(
        .arr = .array,
        .restrict = list(value = c("L2_norm_f1")),
        .drop = TRUE,
        .never_drop = .never_drop)
    .extract_last <- local({
        .tmp <- dimnames(.array)[[.new_dnn]]
        as.character(max(as.numeric(.tmp)))
    })
    .last_part <- leanRcoding::restrict_array(
        .arr = .array,
        .restrict = list(block_length = .extract_last,
                         value = c("L2_norm_f2")),
        .drop = TRUE,
        .never_drop = .never_drop)
    dimnames(.last_part)[[.new_dnn]] <- .new_max
    ##  Collect everything into one array
    .full_array <- leanRcoding::my_abind(.main_part, .last_part)
    ##  Initiate a list to return to the workflow.
    res <- list()
    ##  Split in a part with the "orig"-component an the part
    ##  withouth the orig-component.  Then compute the mean of
    ##  the latter part.
    res$orig_part <- leanRcoding::restrict_array(
        .arr = .full_array,
        .restrict = list(content = "orig"))
    not_orig <- which(dimnames(.full_array)$content != "orig")
    res$block_part <- leanRcoding::restrict_array(
        .arr = .full_array,
        .restrict = list(content = not_orig))
    ##  Compute the changes, in percentage, for the observed
    ##  changes along 'new_dnn'.
    .denominator <- list(head(dimnames(.full_array)[[.new_dnn]], n=-1))
    .numerator <-  list(tail(dimnames(.full_array)[[.new_dnn]], n=-1))
    names(.denominator) <- .new_dnn
    names(.numerator) <- .new_dnn
    res$orig_changes_in_percentages <- local({
        numerator <- leanRcoding::restrict_array(
            .arr = res$orig_part,
            .restrict = .numerator)
        denominator <- leanRcoding::restrict_array(
            .arr = res$orig_part,
            .restrict = .denominator)
        100 * (denominator / numerator - 1)
    })
    res$block_changes_in_percentages <- local({
        numerator <- leanRcoding::restrict_array(
            .arr = res$block_part,
            .restrict = .numerator)
        denominator <- leanRcoding::restrict_array(
            .arr = res$block_part,
            .restrict = .denominator)
        100 * (denominator / numerator - 1)
    })
    ##  Compute the mean of the 'block'-related parts.
    res$mean_of_block_part <- leanRcoding::my_apply(
        X = res$block_part,
        MARGIN = .new_dnn,
        FUN = mean) 
    res$mean_of_block_changes_in_percentages <- leanRcoding::my_apply(
        X = res$block_changes_in_percentages,
        MARGIN = .new_dnn,
        FUN = mean)
    ##  Return the list to the workflow.
    res
}


##  Another similar helper-functions, focusing on the
##  extraction of the distances.

extract_distances <- function(.array, .new_dnn) {
    ##  Extract the distances
    .never_drop <- setdiff(x = names(dimnames(.array)),
                           y = "value")
    .distances <- leanRcoding::restrict_array(
        .arr = .array,
        .restrict = list(value = c("f1_distance_f2")),
        .drop = TRUE,
        .never_drop = .never_drop)
    ##  Initiate a list to return to the workflow.
    res <- list()
    ##  Split in a part with the "orig"-component an the part
    ##  withouth the orig-component.  Then compute the mean of
    ##  the latter part.
    res$orig_part <- leanRcoding::restrict_array(
        .arr = .distances,
        .restrict = list(content = "orig"))
    not_orig <- which(dimnames(.distances)$content != "orig")
    res$block_part <- leanRcoding::restrict_array(
        .arr = .distances,
        .restrict = list(content = not_orig))
    ##  Compute the changes, in percentage, for the observed
    ##  changes along 'new_dnn'.  Reminder: Since the
    ##  distances can be quite small, it will often happen
    ##  that changes in percentage can be large, so it is
    ##  really not that reasonable to use the changes in
    ##  percentage as a proxy in this case.
    .denominator <- list(head(dimnames(.distances)[[.new_dnn]], n=-1))
    .numerator <-  list(tail(dimnames(.distances)[[.new_dnn]], n=-1))
    names(.denominator) <- .new_dnn
    names(.numerator) <- .new_dnn
    res$orig_changes_in_percentages <- local({
        numerator <- leanRcoding::restrict_array(
            .arr = res$orig_part,
            .restrict = .numerator)
        ##  Reminder: NaN-values occur for orig-case, could be
        ##  rewritten to avoid that.
        denominator <- leanRcoding::restrict_array(
            .arr = res$orig_part,
            .restrict = .denominator)
        100 * (denominator / numerator - 1)
    })
    res$block_changes_in_percentages <- local({
        numerator <- leanRcoding::restrict_array(
            .arr = res$block_part,
            .restrict = .numerator)
        denominator <- leanRcoding::restrict_array(
            .arr = res$block_part,
            .restrict = .denominator)
        100 * (denominator / numerator - 1)
    })
    ##  Compute the mean of the 'block'-related parts.
    res$mean_of_block_part <- leanRcoding::my_apply(
        X = res$block_part,
        MARGIN = .new_dnn,
        FUN = mean) 
    res$mean_of_block_changes_in_percentages <- leanRcoding::my_apply(
        X = res$block_changes_in_percentages,
        MARGIN = .new_dnn,
        FUN = mean)
    ##  Return the list to the workflow.
    res
}

##  Get hold of the norms and distances of interest.

.new_max <- as.character(max(as.numeric(names(env_list))))

.L2_norm <- extract_L2_norm(
    .array = C1_vs_C2_array,
    .new_dnn = .new_dnn,
    .new_max = as.character(max(as.numeric(names(env_list)))))

.distances <- extract_distances(
    .array = C1_vs_C2_array,
    .new_dnn = .new_dnn)

rm(extract_distances, extract_L2_norm, C1_vs_C2_list)

##  Create a suitable data-frame for this part.  Use a construction
##  similar to the one used for the plots of the correlations.

look_up <- env_list[[1]]$look_up

###----------------------------------------------------------------###

## Create a list having the two plots of interest

.the_plots_list <- list()

for (..type in c("norms", "changes_in_norms")) {

    .TMP <-
        if (..type == "norms") {
            .L2_norm$block_part      
        } else {
            .L2_norm$block_changes_in_percentages
        }

    ##  Adjust the dimnames when changes in percentages, i.e. ensure
    ##  that the boxplots are positioned between the block lengths
    ##  that are compared.

    if (..type != "norms") {
        ##  Adjusting the x-positions
        .l_values <- as.numeric(dimnames(.L2_norm$block_part)$block_length)
        .l_values <- head(.l_values, n=-1) + diff(.l_values)/2
        dimnames(.TMP)$block_length <- .l_values
    }

    .TMP_df <- local({
        .tmp <- reshape2::melt(data = .TMP)
        ##  Identify if a box-plot is desired, i.e. check if the
        ##  data is from a simulated block or from a bootstrapped
        ##  investigation.
        .boxplot <- any(look_up$is_block,
                        look_up$is_bootstrap )
        attributes(.tmp)$lag_data <- TRUE
        attributes(.tmp)$boxplot <- .boxplot
        .tmp
    })

    ##  Reminder: This seems to be a tweaking of an old version of the
    ##  plot-function...

    ##  Extract the values to be added in the final list.

    .data_list <- list(
        correlation = .TMP_df)

    ##  Specify the values to be used when extracting the aesthetics.
    ##  Reminder: This should be taken care of in 'LG_lookup'

    .aes_xy <- aes(x = block_length,
                   y = value)
    .aes_min_max <- NULL

    ##  Specify the limits to be used.

    .xlim  <- NULL
    .ylim  <- NULL
    .aes_list <- list(xy = .aes_xy,
                      min_max = .aes_min_max)

    ##  Add the desired content

    .plot_list <- list(
        .data_list = .data_list,
        .lag = look_up$details$.selected_lag,
        .percentile = look_up$details$.selected_percentile,
        .xlim = .xlim,
        .ylim = .ylim,
        .aes_list = .aes_list,
        ##  Reminder: This .plot_label' might better be stored in the
        ##  'look_up'-part.
        .plot_label = paste(
            toupper(substr(x = look_up$details$text$plot_type,
                           start = 1,
                           stop = 1)),
            substr(x = look_up$details$text$plot_type,
                   start = 2,
                   stop = nchar(look_up$details$text$plot_type)),
            look_up$details$text$plot_type_YiYj,
            sep = ""),
        .plot_label = look_up$details$.plot_label,
        .annotate_label = look_up$details$text$trust_the_result)


    .data <- .data_list$correlation

    ##  Initiate the framework

    .result <- ggplot(data = .data,
                      mapping = .aes_list$xy) +
        coord_cartesian(xlim = .xlim,
                        ylim = .ylim,
                        default = TRUE) +
        xlab("L = Block length") +
        theme(axis.title.y = element_blank())
    ##  Add stuff based on the 'boxplot'-attribute.
    if (attributes(.data)$boxplot) {
        .result <- .result +
            geom_boxplot(aes(group = cut_width(x = block_length,
                                               width = 0.01,
                                               boundary = 0.1)),
                         alpha = 0.5,
                         outlier.size = 0.1,
                         outlier.alpha = 0.5,
                         size = 0.1)
    }

    ##  Addjust the size of ticks and text for the axis, and add a
    ##  label 'L' in the lower right corner to compactify the resuling
    ##  grid-based plots.

    .result <- .result + 
        xlab(label = NULL) +
        theme(axis.ticks = element_line(linewidth = 0.3),
              axis.ticks.length = unit(.06, "cm"),
              axis.text = element_text(size = 6))

    ##  Add a line corresponding to 'orig' (or 0 in the percentages case).
    .result <- .result +
        geom_hline(
            yintercept = if (..type == "norms") {
                             .L2_norm$orig_part[1]
                         } else
                             0,
            lty = 2,
            lwd = 0.1,
            col = "brown")

    .result <- .result + 
        ##  Add title. 
        ggplot2::ggtitle(label = .plot_list$.plot_label) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    ##  Add the node to the list
    .the_plots_list[[..type]] <- .result
}


##   Want numbers that describes the total number 'q' of blocks and
##   the length 'r' of the last block (length from 1 to L).  Create a
##   helper function for this part, in which the number of complete
##   blocks are given, and the number of observations in the truncated
##   remainder-block also is returned.

.bl_details <- local({
    ## A helper function to find 'q' and 'r'
    .block_numbers <- function(.n, .l) {
        .q <- ceiling(.n / .l)
        data.frame(
            x = .l,
            nr_blocks = .q,
            length_last_block = .n - (.q - 1) * .l )
    }
    ##  The block lengths 'L' for the cases under investigation.
    .L <- as.numeric(dimnames(.L2_norm$block_part)$block_length)
    ##  The numbers.
    .tmp_numbers <- .block_numbers(
        .n = look_up$details$N,
        .l = .L)
    ##  Add information to the plot to make it easier to see the
    ##  connection with the noation used in the paper.
    .tmp_info <- data.frame(
        x = .L[1] - diff(.L[1:2]),
        nr_blocks = "q",
        length_last_block = "r",
        stringsAsFactors = FALSE)
    ## Merge the result into one data-frame.
    rbind(.tmp_info,
          .tmp_numbers)
})

###----------------------------------------------------------------###

##  Scale down the size of the labels before shrinking stuff in the
##  grid-setup, and tweak the position of the plot-stamp

for (..type in c("norms", "changes_in_norms")) {
    .annotations_text[[..type]]$annotated$size <- 
        0.4 * .annotations_text[[..type]]$annotated$size
    .annotations_text[[..type]]$annotated$vjust[1] <-
        2 * .annotations_text[[..type]]$annotated$vjust[1]
}

size_L <- .annotations_text[[1]]$annotated_df["NC_value", "size"] * 0.4
v_just_L <- .annotations_text[[1]]$annotated_df["NC_value", "vjust"]

for (..type in c("norms", "changes_in_norms")) {
    .the_plots_list[[..type]] <-
        .the_plots_list[[..type]] +
        eval(.annotations_text[[..type]]$annotated) +
        annotate(
            geom = "text",
            x = .bl_details$x,
            y = -Inf,
            vjust = -4.5,
            size = 1.5,
            label = .bl_details$nr_blocks,
            col = "brown") +
        annotate(
            geom = "text",
            x = .bl_details$x,
            y = -Inf,
            vjust = -3,
            size = 1.5,
            label = .bl_details$length_last_block,
            col = "blue") +
        ##  Add the 'L' in the lower right corner.
        annotate(geom = "text",
                 label = "L",
                 parse = TRUE,
                 x = Inf,
                 y = -Inf,
                 size = size_L,
                 hjust = "inward",
                 vjust = v_just_L) 
}

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

##  This script need two save files.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_F2.pdf")
rm(..main_dir, ..TS)

pdf(file = .save_file)

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(18, 1)))
print(.the_plots_list$norms + ggtitle(label=NULL),
      vp = viewport(
          layout.pos.row = 1:5,
          layout.pos.col = 1))
print(.the_plots_list$changes_in_norms  + ggtitle(label=NULL),
      vp = viewport(
          layout.pos.row = 6:10,
          layout.pos.col = 1))

dev.off()

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)
