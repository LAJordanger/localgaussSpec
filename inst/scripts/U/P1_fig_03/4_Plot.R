###----------------------------------------------------------------###
##  The dmbp-example from P1_fig_03.

##  This script contains the code needed in order to reproduce the
##  plot that shows both the pseduo-normalised version of the
##  'dmbp'-data together with some plots of the lag 1 pairs.

###----------------------------------------------------------------###

##  Load the required libraries.

library(localgaussSpec)
library(ggplot2)
library(grid)

## Access the 'dmbp'-data from the 'localgaussSpec'-pakcage (this is a
## copy of the data in the 'rugarch'-package.)

data(dmbp)

dmbp <- dmbp[, "V1"]

## Create pseudo-normal observations from `dmbp`.

dmbp_pn <- qnorm(p = (rank(dmbp) - 0.5) /length(dmbp))
rm(dmbp)

###----------------------------------------------------------------###

##  Create a plot to present the pseduo-normalised values that the
##  computations are based upon.

dmbp_pn_plot <- ggplot(
    data = data.frame(
        x = seq_along(dmbp_pn),
        y = dmbp_pn),
    mapping = aes(x = x, y = y)) + 
    geom_line(
        mapping = aes(x = x, y = y),
        lwd = 0.1,
        alpha = 0.6) + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    annotate(geom = "text",
             x = -Inf,
             y = Inf,
             size = 3,
             label = "pseduo-normal dmbp",
             col = "brown",
             alpha = 1,
             vjust = 1.3,
             hjust = -0.1) +
    theme(axis.ticks = element_line(linewidth = 0.25),
          axis.ticks.length = unit(.04, "cm"),
          axis.text = element_text(size = 4.5))

##  This plot is shrinked a lot later on, and then the tiny `lwd`
##  gives a decent view.

###----------------------------------------------------------------###

##  Add information related to the selected points in the lower tail,
##  the center and the upper tail.

##  Specify the points needed for the "plots with rectangles"

.lower_tail <- 0.1
.center <- 0.5
.upper_tail <- 0.9

coord_points <- structure(
    .Data = qnorm(c(.lower_tail, .center, .upper_tail)),
    .Names = c("lower", "center", "upper"))

##  Specify the "half-width of the strip"

bw <- 0.5

###  Create a version that contains the bands of interest, i.e. need
###  to have a data-frame

df_details <- data.frame(
    x =  seq_along(dmbp_pn),
    y = dmbp_pn,
    lower_min  = unname(coord_points["lower"]  - bw),
    lower_max  = unname(coord_points["lower"]  + bw),
    center_min = unname(coord_points["center"] - bw),
    center_max = unname(coord_points["center"] + bw),
    upper_min  = unname(coord_points["upper"]  - bw),
    upper_max  = unname(coord_points["upper"]  + bw))

##  Specify details to be used when adding strips to the plot of the
##  pseduo-normalised time series.

.border_colour <- "blue"
.fill <- "magenta"
.border_size <- 0.1

dmbp_pn_plot_extra <- dmbp_pn_plot +
    geom_hline(yintercept = coord_points,
               linetype = 2,
               size = .3) +
    geom_rect(data = data.frame(
                  xmin = -Inf,
                  xmax = Inf,
                  ymin = coord_points["lower"] - bw,
                  ymax = coord_points["lower"] + bw),
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color = .border_colour,
              alpha = 0.2,
              size = .border_size,
              fill = .fill,
              inherit.aes = FALSE) +
    geom_rect(data = data.frame(
                  xmin = -Inf,
                  xmax = Inf,
                  ymin = coord_points["center"] - bw,
                  ymax = coord_points["center"] + bw),
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color = .border_colour,
              alpha = 0.2,
              size = .border_size,
              fill = .fill,
              inherit.aes = FALSE) +
    geom_rect(data = data.frame(
                  xmin = -Inf,
                  xmax = Inf,
                  ymin = coord_points["upper"] - bw,
                  ymax = coord_points["upper"] + bw),
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color = .border_colour,
              alpha = 0.2,
              size = .border_size,
              fill = .fill,
              inherit.aes = FALSE) +
    annotate(
        geom = "text",
        x = Inf,
        y = coord_points,
        size = 1.5,
        label = c("10%", "50%", "90%"),
        col = "brown",
        vjust = -0.5,
        hjust = 1) +
    annotate(
        geom = "text",
        x = -Inf,
        y = coord_points,
        size = 1.5,
        label = c("10%", "50%", "90%"),
        col = "brown",
        vjust = -0.5,
        hjust = 0) 

###----------------------------------------------------------------###

##  The next part is to create the plots that shows the lag-h pairs,
##  with a rectangle added that highligths the differences in the
##  points for the three points of interest.

##  Count the number of observations captured in these strips, by
##  investigating some logical vectors.

count_lower <- {dmbp_pn <= coord_points["lower"] + bw} -
    {dmbp_pn < coord_points["lower"] - bw}

count_center <- {dmbp_pn <= coord_points["center"] + bw} -
    {dmbp_pn < coord_points["center"] - bw}

count_upper <- {dmbp_pn <= coord_points["upper"] + bw} -
    {dmbp_pn < coord_points["upper"] - bw}

nr_in_lower_strip <- sum(count_lower)

nr_in_center_strip <- sum(count_center)

nr_in_upper_strip <- sum(count_upper)

##  And while at it, find the number of points within the
##  corresponding squares for the desired lag.

h <- 1
x_lower <- tail(count_lower, -h)
y_lower <- head(count_lower, -h)
nr_in_lower_square <- sum(x_lower * y_lower)
##
x_center <- tail(count_center, -h)
y_center <- head(count_center, -h)
nr_in_center_square <- sum(x_center * y_center)
##
x_upper <- tail(count_upper, -h)
y_upper <- head(count_upper, -h)
nr_in_upper_square <- sum(x_upper * y_upper)

##  Also count the combination of "lower" and "upper", to be used when
##  discussing an off-diagonal example

nr_in_lower__upper_square <- sum(x_lower * y_upper)

##  Do the same for the highest number of lags included.

h.max <- 200
x_lower_h.max <- tail(count_lower, -h.max)
y_lower_h.max <- head(count_lower, -h.max)
nr_in_lower_square_h.max <- sum(x_lower_h.max * y_lower_h.max)
##
x_center_h.max <- tail(count_center, -h.max)
y_center_h.max <- head(count_center, -h.max)
nr_in_center_square_h.max <- sum(x_center_h.max * y_center_h.max)
##
x_upper_h.max <- tail(count_upper, -h.max)
y_upper_h.max <- head(count_upper, -h.max)
nr_in_upper_square_h.max <- sum(x_upper_h.max * y_upper_h.max)

##  Also count the combination of "lower" and "upper", to be used when
##  discussing an off-diagonal example

nr_in_lower__upper_square_h.max <- sum(x_lower_h.max * y_upper_h.max)

###----------------------------------------------------------------###

##  Create a plot that shows the lagged pairs for the lag = 1 case of
##  the pseudo-normalised dmbp data.

h <- 1
x_dmbp_pn <- tail(dmbp_pn, -h)
y_dmbp_pn <- head(dmbp_pn, -h)

label_dmbp_pn <- paste(
    "Lag",
    h,
    "pairs") #" for pseduo-normal dmbp")

dmbp_pn_lag1_data <- data.frame(
           x = x_dmbp_pn,
           y = y_dmbp_pn)

.alpha <- 2/10
.size <- 4/10


dmbp_pn_lag1 <-
    ggplot(data = dmbp_pn_lag1_data, aes(x=x, y=y)) + 
    geom_point(shape = 19, size = .size, alpha= .alpha) +
    geom_abline(colour = "magenta", linetype = 2, size = .3) +
    annotate(geom = "text",
             x = -Inf,
             y = Inf,
             size = 3,
             label = label_dmbp_pn,
             col = "brown",
             alpha = 1,
             vjust = 1.3,
             hjust = -0.1) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme(axis.ticks = element_line(linewidth = 0.25),
          axis.ticks.length = unit(.04, "cm"),
          axis.text = element_text(size = 4.5))

##  Add points that shows where the local Gaussian correlations will
##  be computed, and a corresponding bandwidht-square.  Information
##  about the number of points within these squares might also be of
##  interest.

dmbp_pn_lag1_lower <-
    dmbp_pn_lag1 +
    geom_rect(data = data.frame(
                  xmin = coord_points["lower"] - bw,
                  xmax = coord_points["lower"] + bw,
                  ymin = coord_points["lower"] - bw,
                  ymax = coord_points["lower"] + bw),
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color = "white",
              alpha = 0.3,
              inherit.aes = FALSE) +
    annotate(geom = "text",
             x = -2.5,
             y = -Inf,
             size = 3,
             label = paste("box =",
                           nr_in_lower_square,
                           "observations"),
             col = "brown",
             alpha = 1,
             vjust = -0.5,
             hjust = 0) +
    annotate(
        geom = "text",
        x = Inf,
        y = Inf,
        size = 3,
        label = "10% :: 10%",
        col = "brown",
        vjust = 1.2,
        hjust = 1.2) +
    geom_point(data = data.frame(
                   x = coord_points["lower"],
                   y = coord_points["lower"]),
               shape = 20,
               size = 4 * .size,
               color = "white")

dmbp_pn_lag1_center <-
    dmbp_pn_lag1 +
    geom_rect(data = data.frame(
                  xmin = coord_points["center"] - bw,
                  xmax = coord_points["center"] + bw,
                  ymin = coord_points["center"] - bw,
                  ymax = coord_points["center"] + bw),
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color = "white",
              alpha = 0.3,
              inherit.aes = FALSE) +
    annotate(geom = "text",
             x = -2.5,
             y = -Inf,
             size = 3,
             label = paste("box =",
                           nr_in_center_square,
                           "observations"),
             col = "brown",
             alpha = 1,
             vjust = -0.5,
             hjust = 0) +
    annotate(
        geom = "text",
        x = Inf,
        y = Inf,
        size = 3,
        label = "50% :: 50%",
        col = "brown",
        vjust = 1.2,
        hjust = 1.2) +
        geom_point(data = data.frame(
                   x = coord_points["center"],
                   y = coord_points["center"]),
               shape = 20,
               size = 4 * .size,
               color = "white")

dmbp_pn_lag1_upper <-
    dmbp_pn_lag1 +
    geom_rect(data = data.frame(
                  xmin = coord_points["upper"] - bw,
                  xmax = coord_points["upper"] + bw,
                  ymin = coord_points["upper"] - bw,
                  ymax = coord_points["upper"] + bw),
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color = "white",
              alpha = 0.3,
              inherit.aes = FALSE) +
    annotate(geom = "text",
             x = -2.5,
             y = -Inf,
             size = 3,
             label = paste("box =",
                           nr_in_upper_square,
                           "observations"),
             col = "brown",
             alpha = 1,
             vjust = -0.5,
             hjust = 0) +
    annotate(
        geom = "text",
        x = Inf,
        y = Inf,
        size = 3,
        label = "90% :: 90%",
        col = "brown",
        vjust = 1.2,
        hjust = 1.2) +
    geom_point(data = data.frame(
                   x = coord_points["upper"],
                   y = coord_points["upper"]),
               shape = 20,
               size = 4 * .size,
               color = "white")

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

##  WARNING: For this particular example the file will be saved in the
##  active working directory of R.

.save_file <- "P1_fig_03.pdf"

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(6,3)))
print(dmbp_pn_plot_extra +
     theme(legend.position = "none"),
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 1:3))
##### The trhee points for the lagged pairs case
print(dmbp_pn_lag1_lower,
       vp = viewport(
           layout.pos.row = 2,
           layout.pos.col = 1))
print(dmbp_pn_lag1_center,
       vp = viewport(
           layout.pos.row = 2,
           layout.pos.col = 2))
print(dmbp_pn_lag1_upper,
       vp = viewport(
           layout.pos.row = 2,
           layout.pos.col = 3))

dev.off()

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)
