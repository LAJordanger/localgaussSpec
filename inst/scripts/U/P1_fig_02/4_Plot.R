###----------------------------------------------------------------###
##  The dmbp-example from P1_fig_02.

##  This script contains the code needed in order to reproduce the
##  plot that shows both the original and the pseduo-normalised
##  version of the 'dmbp'-data.

###----------------------------------------------------------------###

##  Load the required libraries.

library(localgaussSpec)
library(ggplot2)
library(grid)

###----------------------------------------------------------------###

## Access the 'dmbp'-data from the 'localgaussSpec'-pakcage (this is a
## copy of the data in the 'rugarch'-package.)

data(dmbp)

dmbp <- dmbp[, "V1"]

## Create pseudo-normal observations from `dmbp`.

dmbp_pn <- qnorm(p = (rank(dmbp) - 0.5) /length(dmbp))

###----------------------------------------------------------------###

##  Create a plot to present the original values.

dmbp_original_plot <- ggplot(
    data = data.frame(
        x = seq_along(dmbp),
        y = dmbp),
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
             label = "original dmbp",
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

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

##  WARNING: For this particular example the file will be saved in the
##  active working directory of R.

.save_file <- "P1_fig_02.pdf"

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(8, 1)))
print(dmbp_original_plot,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 1))
print(dmbp_pn_plot,
      vp = viewport(
          layout.pos.row = 2,
          layout.pos.col = 1))

dev.off()

rm(dmbp, dmbp_pn, dmbp_pn_plot, dmbp_original_plot)

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)
