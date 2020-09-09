###----------------------------------------------------------------###
##  The "local trigonometric" example from P1_fig_G1.

##  This script contains the code needed in order to visualise the
##  heuristic argument that justifies the expected result for the
##  investigation of the artificial trigonometric example.

###----------------------------------------------------------------###

##  In order for this script to work, it is necessary to first use the
##  P1_fig_07-script in '2_Data.R'.

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

##  Load the required libraries.

library(localgaussSpec)
library(ggplot2)
library(grid)

###----------------------------------------------------------------###

##  Specify the key arguments that identifies where the data to be
##  investigated can be found.

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_07.G1")
..TS <- "dmt_3a0010ae2dde5bfcd94c7fee2611292a"

###----------------------------------------------------------------###

##  Initiate a list to store the plot-values

..plot <- list()

##  Create a plot of the global spectrum from P1_fig_07.

.line.size <- 0.1
.input <-
    list(TCS_type = "S",
         window = "Tukey",
         Boot_Approx = NA_character_,
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
                size = .line.size),
            global = list(
                line.size = .line.size))))

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
 

###----------------------------------------------------------------###

##  Extract information about the frequencies.

alpha <- attributes(..plot[[1]])$details$fun_formals$alpha
A <- attributes(..plot[[1]])$details$fun_formals$A
.delta <- attributes(..plot[[1]])$details$fun_formals$delta
.delta_range <- attributes(..plot[[1]])$details$fun_formals$delta_range

###----------------------------------------------------------------###

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

###----------------------------------------------------------------###

##  Create a plot of 100 values, from a simplified version with
##  constant amplitudes.

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

##  Create the data for the time series to be returned, by simply
##  adjusting the data of the previous data a bit (insert zeros), and
##  then take the sums.

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
##  width of the range...  Moreover, keep in mind that this will be
##  rescaled in the grid.

..plot$dmt_one_selected_cosine$coordinates$limits$y <- ..range * c(1, 0.4)

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P1_fig_G1.pdf")
rm(..main_dir, ..TS)

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

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)
