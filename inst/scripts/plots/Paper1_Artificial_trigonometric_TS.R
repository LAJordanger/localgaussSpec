## This script contains the code needed in order to visualise the
## heuristic argument that justifies the expected result for the
## investigation of the artificial trigonometric example.  In
## particular this gives a plot that shows how the 'dmt'-setting
## creates it samples.


library(localgaussSpec)
library(ggplot2)
library(grid)

..main_dir <- "~/LG_DATA"
..TS <- "dmt_3a0010ae2dde5bfcd94c7fee2611292a"
##  Note: The 'TS' value is based on the 'digest::digest'-function.
##  This is used in order to keep track of the different simulations,
##  and it is used to avoid the re-computation of a script that has
##  already been executed.  It might alas be cases where this value
##  can be influenced by the machine used for the computation, in
##  which case it might be necessary to update the 'TS'-value.

## ## ## data_dir_for_LG_shiny <-
## ## ##     c(ts.dir = ..TS,
## ## ##       approx.dir = "Approx__1")



## ## ## shiny::runApp(LG_shiny(
## ## ##            main_dir = ..main_dir,
## ## ##            data_dir = data_dir_for_LG_shiny))

#########################
##  Initiate a list to store the plot-values

..plot <- list()  ##  Initiate list for plots.

..line.size <- 0.1

##  Get hold of a plot of the global spectrum.

.input <-
    list(TCS_type = "S",
         window = "Tukey",
         Boot_Approx = NA_character_,
         TS_key = "WNG",
         confidence_interval = "95",
         levels_Diagonal = 1L,
         bw_points = "0.5",
         cut = 10L,
         frequency_range = c(0, 0.5),
         type = "par_five",
         levels_Horizontal = 2,
         TS = ..TS,
         S_type = "LS_a",
         levels_Line = 2,
         point_type = "on_diag",
         Approx = "Approx__1",
         Vi = "Y", Vj = "Y",
         levels_Vertical = 2,
         global_local = "global",
         drop_annotation = TRUE)
..plot$global_spectrum <- LG_plot_helper(
    main_dir = ..main_dir, 
    input = .input,
    input_curlicues= list(
        spectra_plot = list(
            WN_line  = list(
                size = ..line.size),
            global = list(
                line.size = ..line.size))))




##  Ensure that the y-limit suitable for the global case is used, and
##  adjust some other details too.

..plot$global_spectrum <- ..plot$global_spectrum +
    coord_cartesian(
        ylim = attributes(..plot$global_spectrum)$ylim_list$ylim_restricted)  + 
        xlab(label = NULL) +
        ggtitle(label = NULL) +
        theme(axis.ticks = element_line(size = 0.25),
              axis.ticks.length = unit(.04, "cm"),
              axis.text = element_text(size = 4.5))
 

################################################################################
##  Extract information from the info-file in order to figure out
##  details related to the frequencies.

.info_path <- file.path(
    paste(..main_dir,
          collapse = .Platform$file.sep),
    ..TS,
    localgaussSpec:::LG_default$info_file_name)


##  Load the object into the present workflow, and reduce to the
##  component of interest.

localgaussSpec:::LG_load(.file = .info_path, .name = "info_TS")
info_TS <- info_TS$TS_info$TS_data
##  NB: Need to extrac stuff from the 'fun'-part, as it seems to be
##  the case that the code that should have updated 'args' is a bit
##  off...

## args(info_TS$fun)

alpha <- formals(info_TS$fun)$alpha
A <- formals(info_TS$fun)$A
.delta <- formals(info_TS$fun)$delta
.delta_range <- formals(info_TS$fun)$delta_range

################################################################################

##  The use of 'drop_annotation=TRUE' in the 'input'-argument of
##  'LG_plot_helper' prevented the annotated text to be added to the
##  plots in the list '..plot'.  The information to add them on later
##  on (with an adjusted size-value) can be extracted from the
##  attributes, and can be stored in a separate list.

annotated_text <- attributes(..plot$global_spectrum)$curlicues$text
.scaling_for_annotated_text <- 0.6
##  Adjust the size of the text to be annotated, add more information
##  to the m-value, add the frequency lines to the plot, and add omega
##  in lower right corner.
annotated_text$annotated$size <- annotated_text$annotated$size *
    .scaling_for_annotated_text
annotated_text$annotated$label[2] <- paste(
    annotated_text$annotated$label[2],
    "*' the global spectral density'",
    sep = "")

..plot$global_spectrum <-  ..plot$global_spectrum +
    geom_vline(xintercept = alpha/(2*pi),
               linetype = 3,
               col = "black",
               alpha = 0.8,
               size = 0.3) +
    eval(annotated_text$annotated) + 
    annotate(geom = "text",
             label = "omega",
             parse = TRUE,
             x = Inf,
             y = -Inf,
             size = annotated_text$annotated_df["n_R_L_values", "size"] *
                 .scaling_for_annotated_text,
             hjust = "inward",
             vjust = "inward")



################################################################################


## info_TS$fun(n = 100)


## .seed_for_sample <- 4624342
## .TS_sample <- TS_sample(
##     TS_key = TS_key,
##     N = 1000,
##     nr_samples = 1,
##     A = rbind(c(-2, -1, 0, 1),   ##  Reminder: Not zero mean, but that's not a problem I think.
##               c(1/20, 1/3 - 1/20, 1/3, 1/3)),
##     delta = c(1.0, 0.5, 0.3, 0.5),
##     delta_range = c(0.5, 0.2, 0.2, 0.6),
##     ##    delta_range = NULL,
##     alpha = c(pi/2, pi/8, 4/5 * pi, pi/2) + {
##         set.seed(12)
##         runif(n = 4, min = 0.1, max = 0.2)},
##     theta = NULL,
##     wn = NULL,
##     ##    wn = list(type = "rnorm", args = list(mean = 0, sd = 1), adjust = 0),
##     .seed = NULL)


###  ##  A piece of code from "TS_family_dmt.R":
## A[1, i] + .delta * cos(alpha[i] * t_vec[.n] + theta[i])


t_vec <- 1:100
theta <- {    ##  Phase adjustment
    set.seed(124)
    theta <- runif(n = ncol(A), min = 0, max = 2*pi)
}

set.seed(1423)
.levels <- sample(  ##  Figure out which levels to select.
    x = 1:ncol(A),
    size = length(t_vec),
    replace = TRUE,
    prob = A[2, ])


help_fun <- function(i, t_vec, level, amplitude, frequency, phase){
    level[i] + amplitude[i] * cos(frequency[i] * t_vec + phase[i])
}




test <- lapply(
    X = seq_len(ncol(A)),
    FUN = help_fun,
    level = A[1, ],
    amplitude = .delta,
    frequency = alpha,
    t_vec = t_vec,
    phase = theta)

## str(test)

##  Create the data for the time series to be returned, by simply
##  adjusting the data of the previous data a bit (including zeros),
##  and then take the sums.

ts_data_one <- vapply(
    X = seq_len(length(test)),
    FUN = function(i, test, .levels) {
        test[[i]][.levels != i] <- 0
        test[[i]]
    },
    FUN.VALUE = numeric(length(test[[1]])),
    test = test,
    .levels = .levels)


ts_data <- rowSums(ts_data_one)


##  Convert to data-frame, together with t-vec and ts_data

test2 <- data.frame(t = t_vec, test, ts_data)
## str(test2)

names(test2) <- c("t", "level1", "level2", "level3", "level4", "ts")

plot1 <- ggplot(data =  test2, aes(x = t, y = level1)) +
    ylim(range(test)) +
    theme(axis.title.y = element_blank()) +
    ## ggtitle(label = "Time series based on several cosines")
    annotate(geom = "text",
             x = 0,
             y = Inf,
             size = 3,
             label = "Time series based on several cosines",
             col = "brown",
             alpha = 1,
             vjust = "inward",
             hjust = "inward") +
        theme(axis.ticks = element_line(size = 0.25),
              axis.ticks.length = unit(.04, "cm"),
              axis.text = element_text(size = 4.5))

..plot$dmt_example <- plot1 +
    geom_line(color = "magenta", lty = 3) + 
    geom_line(data = test2, aes(x = t, y = level2), color = "magenta", lty = 3) +
    geom_line(data = test2, aes(x = t, y = level3), color = "magenta", lty = 3) +
    geom_line(data = test2, aes(x = t, y = level4), color = "magenta", lty = 3) +
    geom_line(data = test2, aes(x = t, y = ts), colour = "red4", lty = 1) +
    geom_point(data = test2, aes(x = t, y = ts), colour = "red4", size = .7)


##  Adjust the limit, to make the plot look better when scaled down.
..range <- range(..plot$dmt_example$data[, colnames(..plot$dmt_example$data) != "t"])

..plot$dmt_example$coordinates$limits$y <- ..range * c(1, 1.2)


################################################################################
##  Create reduced data-frames to plot the "restricted local
##  sequence".  Strategy, create several data-frames, one for the
##  cosine, one for the detected points and one for the other points.


i <- 2
selected_cosine <- data.frame(
    t = t_vec,
    level = test[[i]])

detected_points <- data.frame(
    t = t_vec[ts_data_one[, i] != 0],
    detected = ts_data_one[, i][ts_data_one[, i] != 0])

undetected_points <- data.frame(
    t = t_vec[ts_data_one[, i] == 0],
    undetected = test[[i]][ts_data_one[, i] == 0])


.label <- paste(
    "\"Local cosine\" where ",
    nrow(detected_points),
    " points out of ",
    length(t_vec),
    " was detected (red circles)",
    sep = "")

plot2 <- ggplot(data =  selected_cosine, aes(x = t, y = level)) +
    ylim(range(test[[i]])) +
    theme(axis.title.x = element_blank()) + 
    theme(axis.title.y = element_blank()) + 
    annotate(geom = "text",
             x = 0,
             y = Inf,
             size = 3,
             label = .label,
             col = "brown",
             alpha = 1,
             vjust = "inward",
             hjust = "inward") +
        theme(axis.ticks = element_line(size = 0.25),
              axis.ticks.length = unit(.04, "cm"),
              axis.text = element_text(size = 4.5))

..plot$dmt_one_selected_cosine <- plot2 +
    geom_line(color = "magenta", lty = 3) + 
    geom_point(data = detected_points,
               aes(x = t, y = detected),
               colour = "red4",
               shape = 21,
               size = 1) +
    geom_point(data = undetected_points,
               aes(x = t, y = undetected),
               colour = "blue",
               shape = 4,
               size = 1) 

##  Adjust the limit a tiny bit, to make the plot better when scaled down.
..range <- range(..plot$dmt_one_selected_cosine$data[, colnames(..plot$dmt_one_selected_cosine$data) != "t"])

##  NB: This is not a generic approach! Depends on sign and on the
##  widht of the range...  Moreover, keep in mind that this will be
##  rescaled in the grid.
..plot$dmt_one_selected_cosine$coordinates$limits$y <- ..range * c(1, 0.4)

















################################################################################





##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.


## TODO: Adjust save file later on, for now update plot to be used in
## paper.
.save_file <- file.path("~/Dropbox/arXiv/Article_1_JASA/figure", "dmt_example_TS-1.pdf")

pdf(file = .save_file) 


grid.newpage()
pushViewport(viewport(
    layout = grid.layout(10, 1)))
#####  An excerpt of the time series.
print(..plot$dmt_example +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()),
      vp = viewport(
          layout.pos.row = 1:2,
          layout.pos.col = 1))
#####  The global spectrum.
print(..plot$global_spectrum +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()),
       vp = viewport(
           layout.pos.row = 3,
           layout.pos.col = 1))
print(..plot$dmt_one_selected_cosine,
       vp = viewport(
           layout.pos.row = 4,
           layout.pos.col = 1))

dev.off()

##  Crop the result
.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)





