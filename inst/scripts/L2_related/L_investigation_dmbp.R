#####------------------------------------------------------------#####
##  This script creates a few plots related to the sensitivity
##  analysis of the block lenght L in the block bootsrap.  The script
##  requires that the code in <OTHER SCRIPT> has been used to create a
##  collection of data that can be used for this investigation.

##  The three usual points along the diagonal are available, but only
##  the lower tail is considered in this script.  Change the value of
##  'levels_Diagonal' to '2' (center) or '3' (upper tail) for a
##  similar investigation of those points.

library(localgaussSpec)
library(ggplot2)
library(leanRcoding) 


.levels_Diagonal <- 1L

.global_local <- "local"  # "global"

##  The directory containing the data, which for this script also will
##  be used to store the saved plot.
..main_dir <- c("~", "LG_DATA_block_length_investigation_II")

.save_file <- sprintf("%s%s%s__%s",
                      ## paste(..main_dir,
                      ##       collapse = .Platform$file.sep),
                      "~/Dropbox/arXiv/Article_1_JASA/figure/",
                      .Platform$file.sep,
                      .global_local,
                      "block_length_norms_and_changes_in_percentage.pdf")

input_common <- list(
    TCS_type = "S",
    window = "Tukey",
    TS_key = "other", 
    confidence_interval = "95",
    levels_Diagonal = .levels_Diagonal,
    bw_points = "0.5", 
    cut = 10L,
    frequency_range = c(0, 0.5),
    type = "par_five", 
    TS = "0fb42549ce13fce773c12b77463bdca8", 
    S_type = "LS_a",
    levels_Line = 2,
    point_type = "on_diag", 
    Approx = "Approx__1",
    Vi = "Y", Vj = "Y",
    global_local = .global_local,
    L2_inspection_vbmL = "L",
    drop_annotation = TRUE)

##  An indicator used for some minor tweaking of the settings.
..type <- "norms"
## ..type <- "changes_in_norms"

block_length_vec <- 75:134

## block_length_vec <- 75:77 ## Shorter version for testing of script.

## Find suitable positions for the x-component of the labels.
x.label_low_high_percentages  <-
    c(block_length_vec[1],
      rev(block_length_vec)[1])

x.label_low_high_norm <- 
    x.label_low_high_percentages +
    .5 * c(-1, 1) * diff(block_length_vec)[1]



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
        list(
            Boot_Approx = .Boot_Approx,
            L2_distance_normal = TRUE))
    ##  Extract the data, adjust it to the required format.
    .zzz <- LG_plot_helper(main_dir = ..main_dir, 
                           input = input,
                           input_curlicues = list(
                               x.label_low_high = x.label_low_high_norm,
                               NC_value = list(short_or_long_label = "short")),
                           .extract_LG_data_only = TRUE)
    .env <- .zzz$..env
    .env$look_up <- .zzz$look_up
    env_list[[as.character(block_length)]] <- .env
}

##  Extract the annotations for the norms from the list above, and
##  then "repeat" one case in order to get hold of the annotations
##  needed for the plot showing the changes in percentages.


#####---------------
input <- c(
    input_common,
    list(
        Boot_Approx = .Boot_Approx,
        L2_distance_percentages = TRUE))
.tmp <- LG_plot_helper(main_dir = ..main_dir, 
                       input = input,
                       input_curlicues = list(
                           x.label_low_high = x.label_low_high_percentages,
                           NC_value = list(short_or_long_label = "short")),
                       .extract_LG_data_only = TRUE)
annotation_for_percentages <- .tmp$look_up$curlicues$text

##  Put together in one list.
.annotations_text <- list(
    norms = env_list[[1]]$look_up$curlicues$text,
    changes_in_norms = annotation_for_percentages)

#####---------------
rm(..main_dir, .Boot_Approx, .env, .tmp, .zzz, block_length,
   block_length_vec, i, input, input_common, x.label_low_high_norm,
   x.label_low_high_percentages, annotation_for_percentages)


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



##  Create an array with all the information about the norms and
##  distances with.

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
C1_vs_C2_array <- my_abind(C1_vs_C2_list,
                             .list = TRUE,
                             .list_new.dnn = .new_dnn)


##  Add a helper-function to extract the norms, i.e. taking care of
##  the tweaking required for the last part.
extract_L2_norm <- function(.array, .new_dnn, .new_max) {
    .never_drop <- setdiff(x = names(dimnames(.array)),
                           y = "value")
    .main_part <- restrict_array(
        .arr = .array,
        .restrict = list(value = c("L2_norm_f1")),
        .drop = TRUE,
        .never_drop = .never_drop)
    .extract_last <- local({
        .tmp <- dimnames(.array)[[.new_dnn]]
        as.character(max(as.numeric(.tmp)))
    })
    .last_part <- restrict_array(
        .arr = .array,
        .restrict = list(block_length = .extract_last,
                         value = c("L2_norm_f2")),
        .drop = TRUE,
        .never_drop = .never_drop)
    dimnames(.last_part)[[.new_dnn]] <- .new_max
    ##  Collect everything into one array
    .full_array <- my_abind(.main_part, .last_part)
    ##  Initiate a list to return to the workflow.
    res <- list()
    ##  Split in a part with the "orig"-component an the part
    ##  withouth the orig-component.  Then compute the mean of
    ##  the latter part.
    res$orig_part <- restrict_array(
        .arr = .full_array,
        .restrict = list(content = "orig"))
    not_orig <- which(dimnames(.full_array)$content != "orig")
    res$block_part <- restrict_array(
        .arr = .full_array,
        .restrict = list(content = not_orig))
    ##  Compute the changes, in percentage, for the observed
    ##  changes along 'new_dnn'.
    .denominator <- list(head(dimnames(.full_array)[[.new_dnn]], n=-1))
    .numerator <-  list(tail(dimnames(.full_array)[[.new_dnn]], n=-1))
    names(.denominator) <- .new_dnn
    names(.numerator) <- .new_dnn
    res$orig_changes_in_percentages <- local({
        numerator <- restrict_array(
            .arr = res$orig_part,
            .restrict = .numerator)
        denominator <- restrict_array(
            .arr = res$orig_part,
            .restrict = .denominator)
        100 * (denominator / numerator - 1)
    })
    res$block_changes_in_percentages <- local({
        numerator <- restrict_array(
            .arr = res$block_part,
            .restrict = .numerator)
        denominator <- restrict_array(
            .arr = res$block_part,
            .restrict = .denominator)
        100 * (denominator / numerator - 1)
    })
    ##  Compute the mean of the 'block'-related parts.
    res$mean_of_block_part <- my_apply(
        X = res$block_part,
        MARGIN = .new_dnn,
        FUN = mean) 
    res$mean_of_block_changes_in_percentages <- my_apply(
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
    .distances <- restrict_array(
        .arr = .array,
        .restrict = list(value = c("f1_distance_f2")),
        .drop = TRUE,
        .never_drop = .never_drop)
    ##  Initiate a list to return to the workflow.
    res <- list()
    ##  Split in a part with the "orig"-component an the part
    ##  withouth the orig-component.  Then compute the mean of
    ##  the latter part.
    res$orig_part <- restrict_array(
        .arr = .distances,
        .restrict = list(content = "orig"))
    not_orig <- which(dimnames(.distances)$content != "orig")
    res$block_part <- restrict_array(
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
        numerator <- restrict_array(
            .arr = res$orig_part,
            .restrict = .numerator)
        ##  Reminder: NaN-values occur for orig-case, could be
        ##  rewritten to avoid that.
        denominator <- restrict_array(
            .arr = res$orig_part,
            .restrict = .denominator)
        100 * (denominator / numerator - 1)
    })
    res$block_changes_in_percentages <- local({
        numerator <- restrict_array(
            .arr = res$block_part,
            .restrict = .numerator)
        denominator <- restrict_array(
            .arr = res$block_part,
            .restrict = .denominator)
        100 * (denominator / numerator - 1)
    })
    ##  Compute the mean of the 'block'-related parts.
    res$mean_of_block_part <- my_apply(
        X = res$block_part,
        MARGIN = .new_dnn,
        FUN = mean) 
    res$mean_of_block_changes_in_percentages <- my_apply(
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

#####-----------------------------------------------------------------
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
    .xlim  <- NULL  ##..env[[.source]]$.xlim
    .ylim  <- NULL  ##..env[[.source]]$.ylim
    .aes_list <- list(xy = .aes_xy,
                      min_max = .aes_min_max)




    ##################################################
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
        theme(axis.ticks = element_line(size = 0.3),
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
            size = 0.1,
            col = "brown")


    .result <- .result + 
        ##  Add title. 
        ggplot2::ggtitle(label = .plot_list$.plot_label) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    ##  Add the node to the list
    .the_plots_list[[..type]] <- .result
}


##   Want numbers that describes the number of full blocks and the
##   length of the last non-full block (can be zero).  Create a helper
##   function for this part, in which the number of complete blocks
##   are given, and the number of observations in the truncated
##   remainder-block also is returned.


.block_numbers <- function(.n, .l) {
    data.frame(
        x = .l,
        nr_full_blocks = .n %/% .l,
        length_non_full_block = .n %% .l)
}

.bl_details <- .block_numbers(
    .n = look_up$details$N,
    .l = as.numeric(dimnames(.L2_norm$block_part)$block_length))


.copy <- .the_plots_list
.copy_text <- .annotations_text

## .the_plots_list$norms
## .the_plots_list$changes_in_norms

##  Add details about content to the plots

.the_plots_list <- .copy 
.annotations_text <- .copy_text




##  Scale down the size of the labels before shrinking stuff in the
##  grid-setup.

for (..type in c("norms", "changes_in_norms")) {
    .annotations_text[[..type]]$annotated$size <- 
        0.4 * .annotations_text[[..type]]$annotated$size 
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
            label = .bl_details$nr_full_blocks,
            col = "brown") +
        annotate(
            geom = "text",
            x = .bl_details$x,
            y = -Inf,
            vjust = -3,
            size = 1.5,
            label = .bl_details$length_non_full_block,
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


library(grid)

##  Create the plot to use in the paper.
pdf(file = .save_file)
grid.newpage()
pushViewport(viewport(
    layout = grid.layout(16, 1)))
print(.the_plots_list$norms,
      vp = viewport(
          layout.pos.row = 1:5,
          layout.pos.col = 1))
print(.the_plots_list$changes_in_norms,
      vp = viewport(
          layout.pos.row = 6:10,
          layout.pos.col = 1))
dev.off()

##  Crop the result
.crop_code <- sprintf("pdfcrop --margins 5 %s %s", .save_file, .save_file)
system(.crop_code)

#####------------------------------------------------------------#####

## grid.newpage()
## pushViewport(viewport(
##     layout = grid.layout(3, 1)))
## print(.the_plots_list$norms,
##       vp = viewport(
##           layout.pos.row = 1,
##           layout.pos.col = 1))
## print(.the_plots_list$changes_in_norms,
##       vp = viewport(
##           layout.pos.row = 2,
##           layout.pos.col = 1))
