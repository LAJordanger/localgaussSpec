################################################################################
#####  2014-09-10
##  It dawned on me that it would be a bad idea to include the
##  specification of the window-functions in a file that might be
##  deleted if the new version for the computation of the local
##  Gaussian spectra works out as intended.

###-------------------------------------------------------------------
##  Introduce a list of window-functions, i.e. "Tukey", "Parzen" and
##  "Bartlett".

##  I guess I should do something with regard to how this is stored
##  and presented. It would be natural to have some information about
##  it in the package.

##  WARNING: The setup here is with regard to window-functions that
##  gives zero-weight to every lag from `cut' and out.  I suppose this
##  should not pose a problem, but nevertheless let's keep a reminder
##  here in case I once want to add a window-function where the value
##  at `cut' is non-zero.

#####  2014-10-06.  I realised to my chagrin that different parts of
#####  the code used different versions of the original code, and
#####  although I suspect that I in the end will be happy to use only
#####  one version I probably should make a version that accepts both
#####  posibilites. The approach is rather crude, but whatever works
#####  is good enough.



myWindows <- list(
    ##  Due to symmetry, only necessary to create non-negative part,
    ##  and since the first value (the weight for lag zero) always are
    ##  1 that one will also be dropped from this investigation.
    ##  Moreover, the case 'cut' equal to '1' will always be treated
    ##  in a separate manner later on in the code, so I do not worry
    ##  about the fact that that value creates an error.
###-------------------------------------------------------------------
    Tukey = function(.cut) {
        ##  Use the 'Tukey'-window for the computation.
        .all <- pi/.cut * 1:(.cut-1)
        structure(
            .Data = as.array(0.5 * (1 + cos(.all))),
            .Dimnames = list(lag = 1:(.cut-1)))        
    },
###-------------------------------------------------------------------
    Parzen = function(.cut) {
        ##  Use the 'Parzen'-window for the computation.
        .all <- 1/.cut * 1:(.cut-1)
        .first <- .all[seq_len(floor(.cut/2))]
        .second <- setdiff(x = .all,
                           y = .first)
        structure(
            .Data = as.array(c(1 - 6 * (.first)^2 + 6 * (.first)^3,
                               2 * (1-.second)^2)),
            .Dimnames = list(lag = 1:(.cut-1)))
    },
###-------------------------------------------------------------------
    Bartlett = function(.cut) {
        ##  Use the 'Bartlett'-window for the computation.
        .all <- 1/.cut * 1:(.cut-1)
        structure(
            .Data = as.array(1 - .all),
            .Dimnames = list(lag = 1:(.cut-1)))
    }
)


##  2018-10-31: Some adjustments to get out an array instead of the
##  vector that originaly where produced.  The original code is
##  present below.


## ## ## myWindows <- list(
## ## ##     ##  Due to symmetry, only necessary to create non-negative part.
## ## ## ###-------------------------------------------------------------------
## ## ##     Tukey = function(cut, .length){
## ## ##         if (missing(.length)) {
## ## ##             res <- c(1, rep(0, cut - 1))
## ## ##             ##  Note that res[1] always will be 1.
## ## ##             if (cut == 1)
## ## ##                 return(res)
## ## ##             res[2:cut] <- 0.5 * (1 + cos (pi/cut * 1:(cut-1)) )
## ## ##             names(res) <- paste("lag", (1:cut - 1), sep="")
## ## ##             return(res)
## ## ##         } else {
## ## ##             res <- c(1, rep(0, .length))
## ## ##             ##  Note that res[1] always will be 1.
## ## ##             res[2:cut] <- 0.5 * (1 + cos (pi/cut * 1:(cut-1)) )
## ## ##             ##  Note that res[cut+1] always will be 0.
## ## ##             names(res) <- paste("lag", 0:.length, sep="")
## ## ##             return(res)
## ## ##         }
## ## ##     },
## ## ## ###-------------------------------------------------------------------
## ## ##     Parzen = function(cut, .length){
## ## ##         if (missing(.length)) {
## ## ##             res <- c(1, rep(0, cut - 1))
## ## ##             ##  Note that res[1] always will be 1.
## ## ##             if (cut == 1)
## ## ##                 return(res)
## ## ##             if (cut == 2) {
## ## ##                 res[2] <- 0.25
## ## ##             } else {
## ## ##                 cut2 <- floor(cut/2)
## ## ##                 res[2:(cut2 + 1)] <-
## ## ##                     1 - 6 * (1:cut2/cut)^2 + 6 * (1:cut2/cut)^3
## ## ##                 res[(cut2+2):cut] <-
## ## ##                     2 * (1 - (cut2+1):(cut-1)/cut)^3
## ## ##             }
## ## ##             names(res) <- paste("lag", (1:cut - 1), sep="")
## ## ##             return(res)
## ## ##         } else {
## ## ##             res <- c(1, rep(0, .length))
## ## ##             ##  Note that res[1] always will be 1, so do nothing in
## ## ##             ##  the case when 'cut==1'.
## ## ##             if (! cut == 1) {
## ## ##                 cut2 <- floor(cut/2)
## ## ##                 res[2:(cut2+1)] <-
## ## ##                     1 - 6 * (1:cut2/cut)^2 + 6 * (1:cut2/cut)^3
## ## ##                 if (cut > 2)
## ## ##                     res[(cut2+2):cut] <-
## ## ##                     2 * (1 - (cut2+1):(cut-1)/cut)^3 
## ## ##                 ##  Note that res[cut+1] always will be 0.
## ## ##             }
## ## ##             names(res) <- paste("lag", 0:.length, sep="")
## ## ##             return(res)
## ## ##         }
## ## ##     },
## ## ## ###-------------------------------------------------------------------
## ## ##     Bartlett = function(cut, .length){
## ## ##         if (missing(.length)) {
## ## ##             res <- c(1, rep(0, cut - 1))
## ## ##             ##  Note that res[1] always will be 1.
## ## ##             if (cut == 1)
## ## ##                 return(res)
## ## ##             res[2:cut] <- 1 - 1:(cut-1)/cut
## ## ##             names(res) <- paste("lag", (1:cut - 1), sep="")
## ## ##             return(res)
## ## ##         } else {
## ## ##             res <- c(1, rep(0, .length))
## ## ##             ##  Note that res[1] always will be 1.
## ## ##             res[2:cut] <- 1 - 1:(cut-1)/cut
## ## ##             ##  Note that res[cut+1] always will be 0.
## ## ##             names(res) <- paste("lag", 0:.length, sep="")
## ## ##             return(res)
## ## ##         }
## ## ##     }
## ## ## ###-------------------------------------------------------------------
## ## ## )
