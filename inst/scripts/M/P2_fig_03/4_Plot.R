###----------------------------------------------------------------###
##  The "bivariate cosine" example from P2_fig_03.

##  This script investigates a complex valued presentation of the data
##  from P2_fig_02.

###----------------------------------------------------------------###

##  In order for this script to work, it is necessary that the script
## '2_Data.R' from P2_fig_02 has been used first.

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

##  WARNING: This script contains solutions based on internal code
##  from the 'localgaussSpec'-package.  This implies that it almost
##  certainly will have to be updated at some point.  The plan is to
##  implement a proper function for this task, and the mess in this
##  script will then be replaced with proper code.

###----------------------------------------------------------------###

##  Load the required libraries.

library(localgaussSpec)
library(ggplot2)
library(grid)

#####------------------------------------------------------------#####

##  Specify the key arguments that identifies where the data to be
##  investigated can be found.

..main_dir <- c("~", "LG_DATA_scripts", "P2_fig_02.03")
..TS <- "dmt_bivariate_e65da95d163c4f4df65813aee851ec99"
..Approx <- "Approx__1"

###----------------------------------------------------------------###

##  Extract information about the frequency at wich the peak are
##  expected.

.info_path <- file.path(
    paste(..main_dir,
          collapse = .Platform$file.sep),
    ..TS,
    localgaussSpec:::LG_default$info_file_name)

##  Load the object into the present workflow, and reduce to the
##  component of interest.

localgaussSpec:::LG_load(.file = .info_path, .name = "info_TS")
rm(.info_path)

##  Extract information about the frequency of interest.

.frequency <- formals(info_TS$TS_info$TS_data$fun)$first$alpha

##  Convert to the desired scale.
.omega <- .frequency / (2*pi)

##  Tweak the input list so the first frequency in 'frequency_range'
##  becomes '.omega'. 

input <- list(TCS_type = "S", 
              window = "Tukey",
              Boot_Approx = NA_character_,
              confidence_interval = "95",
              levels_Diagonal = 1L,
              bw_points = "0.6", 
              cut = 10L,
              frequency_range = c(.omega, .omega+0.1),
              type = "par_five", 
              levels_Horizontal = 2,
              TS = "dmt_bivariate_e65da95d163c4f4df65813aee851ec99", 
              S_type = "LS_c_Co",
              levels_Line = 2,
              point_type = "on_diag", 
              Approx = "Approx__1",
              Vi = "Y1",
              Vj = "Y2",
              levels_Vertical = 2, 
              global_local = "local")

##  Do not create the plot, but extract the underlying data instead. 

.LG_data_only <- LG_plot_helper(
    main_dir = ..main_dir, 
    input = input,
    input_curlicues= list(
        NC_value = list(
            short_or_long_label = "short")),
    .extract_LG_data_only=TRUE)

##  Extract from this internal object the required 'bookmarks'

..env <- .LG_data_only$..env
..look_up <- .LG_data_only$look_up
..cache <- ..look_up$cache
..bm <- ..look_up$.bm_CI_local_spectra
..restrct_list <-
    c(list(omega = 1),
      ..look_up$restrict$NC_check$CS$pos_lags[c("levels", "pairs", "bw_points")])

##  Extract the desired chunk of data, restricted to the subset of
##  interest for this plot.

..arr <- leanRcoding::restrict_array(
                          .arr = ..env[[..cache$spectra_local]][[..bm]],
                          .restrict = ..restrct_list,
                          .drop = TRUE,
                          .never_drop = c("S_type", "content"),
                          .keep_attributes = FALSE)

##  Create a data frame to plot the points in the (complex) plane,
##  keep in mind the need for a sign change for the Quad-part.

points_df <- data.frame(
    x = ..arr["Co", ],
    y = - ..arr["Quad",])

##  Want limits that gives a rectangular plot, since a circle is to be
##  added later on.

..limits <- c(-1, 1) * max(..arr["amplitude", ])

##  Help function to get imaginary units on the second axis.

..imaginary <- function(x)
    paste(x, "i", sep = "")

##  Create a basic rectangular plot, without any titles on the axes,
##  and with the coordinat-axes emphasised.

.canvas <- ggplot(
    data = points_df,
    mapping = aes(x = x,
                  y = y)) +
    geom_hline(
        yintercept = 0,
        col = "black",
        alpha = 0.25,
        size = .5) +
    geom_vline(
        xintercept = 0,
        col = "black",
        alpha = 0.25,
        size = .5) +
    geom_point(
        size = .3,
        alpha = 0.25,
        na.rm = TRUE) +
    scale_x_continuous(
        limits = ..limits) +
    scale_y_continuous(
        limits = ..limits,
        labels = ..imaginary) +
    coord_fixed() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())

##  Add lines based on the median/quantiles of the observations.  Need
##  to first identify the specified value for the pointwise confidence
##  intervals.

.CI_percentage <-..look_up$details$CI_percentage  / 100
.lower <- (1-.CI_percentage)/2
.upper <- .CI_percentage + .lower
.probs <- c(.lower, 0.5, .upper)
rm(.CI_percentage, .lower, .upper)

..x_lines <- quantile(x = ..arr["Co", ],
                      probs = .probs)

..y_lines <- quantile(x = - ..arr["Quad", ],
                      probs = .probs)

..radii <- quantile(x = ..arr["amplitude", ],
                    probs = .probs)

..phases <- quantile(x = ..arr["phase", ],
                     probs = .probs)
rm(.probs)

##  Add lines to the plots
.line_type <- c(3, 2, 3)
.line_size <- 0.3
.line_col <- c("blue", "red", "blue")
.line_alpha <- 0.5

.canvas_cartesian <- .canvas +
    geom_vline(
        xintercept = ..x_lines,
        linetype = .line_type,
        col = .line_col,
        size = .line_size,
        alpha = .line_alpha) +
    geom_hline(
        yintercept = ..y_lines,
        linetype = .line_type,
        col = .line_col,
        size = .line_size,
        alpha = .line_alpha)

###----------------------------------------------------------------###

##  Credit: Circles and radii added by a variations of a solution
##  presented by 'Luis',
##  http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2#6863490

## #g is your plot
## #r, xc, yc are the radius and center coordinates

## g<-g+annotate("path",
##    x=xc+r*cos(seq(0,2*pi,length.out=100)),
##    y=yc+r*sin(seq(0,2*pi,length.out=100)))

##  Full circle...

..frequencies <-
    seq(from = 0,
        to = 2 * pi,
        length.out = 100)

..amplitude <-
    seq(from = 0,
        to = 2 * max(..arr["amplitude", ]),
        length.out = 100)

.canvas_polar <- .canvas

for (.i in seq_along(..radii)) 
    .canvas_polar <- .canvas_polar +
        annotate(
            geom = "path",
            x = ..radii[.i] * cos(..frequencies),
            y = ..radii[.i] * sin(..frequencies),
            linetype = .line_type[.i],
            col = .line_col[.i],
            size = .line_size,
            alpha = .line_alpha,
            na.rm = TRUE) +
        annotate(
            geom = "path",
            x = ..amplitude * cos(..phases[.i]),
            y = ..amplitude * sin(..phases[.i]),
            linetype = .line_type[.i],
            col = .line_col[.i],
            size = .line_size,
            alpha = .line_alpha,
            na.rm = TRUE)

##  Need to add some information that makes it easier for the reader
##  to digest the content of the two plots.

..text_for_curlicues <- ..look_up$curlicues$text

##  The messed up strategy for the plot implies that the initial
##  values given in the original input-list is of no interest, and
##  these must be replaced.

.annotated <- ..text_for_curlicues$annotated

.annotated$size <- .annotated$size * .5
.annotated$x <- c(0, ..limits[1], ..limits[2], ..limits[2], ..limits[1], ..limits[2])

##  Add information about frequency, after m-value.
.annotated$label[2]  <- sprintf("list(%s, omega == '%.3f')",
                         .annotated$label[2],
                         .omega)

##  Create a new plot stamp, and adjust its position.

.annotated$label[1] <- sprintf("f[v]^'%s'*(omega)",
                        ..look_up$m_selected)
.annotated$vjust[1]  <- 2 * .annotated$vjust[1]

##  Need to add information one line below the m-value
##  information. Fix this by copy and replace strategy.

.new_information <- local({
    .tmp <- ..text_for_curlicues$
        annotated_df[c("m_value", "m_value"), ]
    rownames(.tmp) <- c("cartesian", "polar")
    .tmp$label <- c(
        cartesian = sprintf("'Cartesian, '*z == x + i*y"),
        polar = sprintf("'Polar, '*z == re^{i*theta}"))
    .tmp$vjust <- .tmp$vjust * c(2, 1.5)
    ##  Reminder: The exponent in the polar label implied that 'vjust'
    ##  had to be adjusted in order for the two plots to have the
    ##  approximate same baseline.
    .tmp$parse <- c(TRUE, TRUE)
    .tmp$x <- .annotated$x[2]
    .tmp$size <- .annotated$size[2]
    .tmp
})

##  Create a list with the cartesian and polar presentations.

..plot <- list(
    polar = .canvas_polar +
        do.call(what = annotate, args = .new_information["polar", ]) +
        eval(.annotated),
    cartesian = .canvas_cartesian +
        do.call(what = annotate, args = .new_information["cartesian", ]) +
        eval(.annotated))

###----------------------------------------------------------------###

##  Include a plot with only the points too, in order to get 3 plots
##  in a line, which gives a more aesthetically pleasing plot in the
##  paper.  This last plot is a zoomed in version of the points, and
##  it might be of interest to add more details to this plot later on.
##  One option of interest would be to add an ellipsis and related
##  that to the complex-valued normal distribution that these values
##  should have when the sample size increases.

##  Want limits that gives a rectangular plot here to, to ensure that
##  this plots look decent compared to the other ones.

##  Strategy, find the largest range, and use that to create limits
##  for the two plots that gives the same range on both axes.

..range <- max(diff(range(..arr["Co", ])),
               diff(range(- ..arr["Quad", ])))

..x_limit <- mean(..arr["Co", ]) +
    c(-1, 1) * 0.5 * ..range
..y_limit <- mean(- ..arr["Quad", ]) +
    c(-1, 1) * 0.5 * ..range

##  A revision of the basic annotated details is once more needed.

.annotated_2 <- .annotated
.annotated_2$x <- c(mean(..arr["Co", ]), ..x_limit[1], ..x_limit[2], ..x_limit[2], ..x_limit[1], ..x_limit[2])

..plot$points_only <- ggplot(
    data = points_df,
    mapping = aes(x = x,
                  y = y)) +
    geom_point(
        size = .3,
        alpha = 0.25,
        na.rm = TRUE) +
    scale_x_continuous(
        limits = ..x_limit) +
    scale_y_continuous(
        limits = ..y_limit,
        labels = ..imaginary) +
    coord_fixed() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    eval(.annotated_2)
rm(points_df, ..x_limit, ..y_limit, ..imaginary)


###  Credit: The following answer contained information that got me
###  past a hurdle in the code.
###  http://stackoverflow.com/questions/27303019/ggplot-annotate-with-greek-symbol-and-1-apostrophe-or-2-in-between-text#27307160

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

.save_file <- file.path(paste(c(..main_dir, ..TS),
                              collapse = .Platform$file.sep),
                        "P2_fig_03.pdf")
rm(..main_dir, ..TS)

pdf(file = .save_file) 
grid.newpage()

pushViewport(viewport(
    layout = grid.layout(1, 3)))
print(..plot$polar,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 1))
print(..plot$cartesian,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 2))
print(..plot$points_only,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 3))

dev.off()

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)

###----------------------------------------------------------------###
