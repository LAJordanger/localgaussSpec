###----------------------------------------------------------------###
##  The EuStockMarkets-example from P2_fig_07.

##  This script contains the code needed in order to reproduce the
##  plot that shows the pseduo-normalised version of the log-returns
##  of the 'EuStockMarkets'-data, with focus on the variables DAX and
##  CAC.  It does not depend on any local Gaussian correlations, which
##  is the reason there is no "2_Data.R"-file in this folder.

###----------------------------------------------------------------###

##  Load the required libraries.

library(ggplot2)
library(grid)

###----------------------------------------------------------------###

## Access the 'EuStockMarkets'-data from the 'datasets'-pakcage.

data(EuStockMarkets)

##  Compute the daily log-returns, since they are the ones used in the
##  calculations.

.first <- head(EuStockMarkets, n = -1)
.second <- tail(EuStockMarkets, n = -1)
.TS <- log(.second/.first)
rm(.first, .second, EuStockMarkets)

##  Extract the desired comonents, and then replace them with their
##  pseudo-normalised observations.
Y1_DAX <-  local({
    .tmp <- .TS[, "DAX"]
    qnorm(p = (rank(.tmp) - 0.5) /length(.tmp))
})
Y3_CAC <-  local({
    .tmp <- .TS[, "CAC"]
    qnorm(p = (rank(.tmp) - 0.5) /length(.tmp))
})
rm(.TS)


###----------------------------------------------------------------###

##  Create a plot to present the DAX-values.

DAX_plot <- ggplot(
    data = data.frame(
        x = seq_along(Y1_DAX),
        y = Y1_DAX),
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
             label = "EuStockMarkets Y1 = DAX",
             col = "brown",
             alpha = 1,
             vjust = 1.3,
             hjust = -0.1) +
    theme(axis.ticks = element_line(linewidth = 0.25),
          axis.ticks.length = unit(.04, "cm"),
          axis.text = element_text(size = 4.5))  +
    ##  Add information about the levels of interest later on,
    ##  i.e. the 10%, 50% and 90% percentiles of the standard normal
    ##  distribution.
    geom_hline(yintercept = qnorm(p = c(0.1, 0.5, 0.9)),
               linetype = 2,
               col = "brown",
               alpha = 0.8,
               lwd = 0.1)
    
##  Create a plot to present the CAC-values.

CAC_plot <- ggplot(
    data = data.frame(
        x = seq_along(Y3_CAC),
        y = Y3_CAC),
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
             label = "EuStockMarkets Y3 = CAC",
             col = "brown",
             alpha = 1,
             vjust = 1.3,
             hjust = -0.1) +
    theme(axis.ticks = element_line(linewidth = 0.25),
          axis.ticks.length = unit(.04, "cm"),
          axis.text = element_text(size = 4.5))  +
    ##  Add information about the levels of interest later on,
    ##  i.e. the 10%, 50% and 90% percentiles of the standard normal
    ##  distribution.
    geom_hline(yintercept = qnorm(p = c(0.1, 0.5, 0.9)),
               linetype = 2,
               col = "brown",
               alpha = 0.8,
               lwd = 0.1)
    
##  This plot is shrinked a lot later on, and then the tiny `lwd`
##  gives a decent view.

###----------------------------------------------------------------###

##  Create the desired grid of plots, and save this grid to disk.
##  Note: It is only after having saved the result to a file, that the
##  effect of the size-arguments for the text can be properly
##  investigated.

##  WARNING: For this particular example the file will be saved in the
##  active working directory of R.

.save_file <- "P2_fig_07.pdf"

pdf(file = .save_file) 

grid.newpage()
pushViewport(viewport(
    layout = grid.layout(8, 1)))
print(DAX_plot,
      vp = viewport(
          layout.pos.row = 1,
          layout.pos.col = 1))
print(CAC_plot,
      vp = viewport(
          layout.pos.row = 2,
          layout.pos.col = 1))

dev.off()

##  Crop the result.  This approach requires that 'pdfcrop' is
##  available on the system.

.crop_code <- sprintf("pdfcrop --margins 0 %s %s", .save_file, .save_file)
system(.crop_code)
rm(.crop_code, .save_file)
