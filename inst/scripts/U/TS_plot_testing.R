##  TESTING FOR TS-plotting...

###----------------------------------------------------------------###
##  The cosine with some noise example from P1_fig_06.

##  This scripts investigates the local Gaussian auto-spectra for a
##  cosine with some noise.  See also P1_fig_G4.

###----------------------------------------------------------------###

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

##  Load the required libraries.

library(localgaussSpec)
library(ggplot2)
library(grid)

###----------------------------------------------------------------###

##  Specify the key arguments that identifies where the data to be
##  investigated can be found.

..main_dir <- c("~", "LG_DATA_scripts", "P1_fig_06")
..TS <- "dmt_48189c8a2232981eaa19be8c457777ae"
## ..Approx <- "Approx__1"

###----------------------------------------------------------------###

##  Specify input values for the selected point.
.input <-
    list(TCS_type = "T",
         TS = ..TS,
         TS_type_or.pn = "pseudo-normalised",
         TS_restrict = list(
             content = 1))

##         ,
## #             observations = 10:194,
##              variables = "Y"))

..plot <- LG_plot_helper(
    main_dir = ..main_dir, 
    input = .input,
    input_curlicues= list(
        TS_plot = list(
            description = list(
                label = "cosine and i.i.d. Gaussian noise"),
            hline = list(
                yintercept = qnorm(p = c(0.1, 0.5, 0.9))))))

rm(.input)



        ## theme(axis.ticks = element_line(size = 0.25),
        ##       axis.ticks.length = unit(.04, "cm"),
        ##       axis.text = element_text(size = 4.5))


###----------------------------------------------------------------###

##  Reminder: The formals are available in this setting too.

alpha <- attributes(..plot)$details$fun_formals$alpha
