################################################################################
#####  2016-01-19

#' Time Series Sample-Generator
#'
#' @details This function will create one or more time series based on
#'     the "keys" stored in \code{TS_families}, with emphasis on also
#'     storing the required arguments needed in order to re-create it
#'     later on.
#'
#' @param TS_key A key, i.e. a \code{character}, corresponding to an
#'     element in \code{TS_families}.  Default value \code{WNG},
#'     i.e. "White Noise Gaussian".
#' 
#' @param N The desired length of the time series.  Default value
#'     \code{500}.
#' 
#' @param nr_samples The desired number of independent samples to be
#'     produced.  The default value \code{1} is the one to use when
#'     testing for the effect of different settings for bootstrapping,
#'     whereas (much) higher values is intented to be used to get an
#'     idea about what the true behaviour should be when investigating
#'     the Local Gaussian Spectral Density.  
#'
#' @param ... \code{dotsMethods}-strategy for feeding parameters to
#'     the function (identified by \code{TS_key} that generates the
#'     time series.
#' 
#' @param .seed Use this to enable reproducible results.  Default value
#'     \code{NULL} (it will be generated and recorded in the code).
#'
#' @param .kind_vstr_list This can be used to create a list with the
#'     values for \code{kind}, \code{normal.kind} and \code{vstr}.
#'     (See the help-page of \code{Random} for details about these
#'     three arguments.)  Note that the default value \code{NULL} will
#'     imply that the function \code{set_seed} will be used to create
#'     the required list based on the present settings.
#' 
#' @return This function returns a list with four parts \code{TS},
#'     \code{TS_data}, \code{spy_report} and \code{seed_vec}.
#'     \code{TS} contains an array with the time series generated
#'     according to the specified arguments, whereas \code{TS_data}
#'     includes some additional stuff that will be used when
#'     \code{TS_LG_object} takes care of the saving of the data. The
#'     arguments needed in order to redo the computation later on is
#'     stored in \code{spy_report}.  The part \code{seed_vec} reflects
#'     that the internal workings of the code creates a vector of
#'     seeds (based on \code{.seed} that then can be used to create an
#'     individual series later on (this is of course only of interest
#'     when \code{nr_samples} is larger than one, and we for some
#'     reason later on would like to do an in depth analysis of one of
#'     the resulting time series.
#' 
#' @export


TS_sample <- function (
    TS_key = "WNG",
    N = 500,
    nr_samples = 1,
    ...,
    .seed = NULL,
    .kind_vstr_list = NULL) {
    ##  Use 'spy' to capture information needed for reproducibility.
    spy_report <- spy()
    ##  Use 'set_seed' to update '.kind_vstr_list'.
    spy_report$envir$.kind_vstr_list <- eval(
        create_call(.cc_fun = set_seed,
                    c(.kind_vstr_list,
                      list(create_kind_vstr_list = TRUE)),
                    .cc_list = TRUE))
    kill(.kind_vstr_list)
    ##  If necessary: Create '.seed' and update 'spy_report'
    if (is.null(.seed))
        .seed <- eval(create_call(
            .cc_fun = leanRcoding::seed_sample,
            spy_report$envir$.kind_vstr_list,
            .cc_list = TRUE))


    eval(create_call(
        .cc_fun = leanRcoding::set_seed,
        c(list(seed = .seed),
          spy_report$envir$.kind_vstr_list),
        .cc_list = TRUE))

    
#####  TASK: I guess there's more that should be done here.  My
#####  present setup will require that stuff are computed within the
#####  'expr'-argument, and it would be nice to have an argument that
#####  opens up for the adjustment of the global RNG-settings too.

    
    seed_vec <- if (nr_samples == 1) {
        .seed
    } else
        vapply(X = 1:nr_samples,
               FUN = function(x) {
                   as.integer(paste(
                       sample(0:9, size = 8),
                       collapse = ""))
               },
               FUN.VALUE = integer(1))

    eval(create_call(
        .cc_fun = leanRcoding::set_seed,
        c(list(seed = .seed),
          spy_report$envir$.kind_vstr_list),
        .cc_list = TRUE))

    ##  Create the values for dimensions "content" and "observations".
    .content <- if (nr_samples > 1) {
        paste(LG_default$sample.prefix, 1:nr_samples, sep = "")
    } else
        LG_default$sample.prefix
    .observations <- paste("t", 1:N, sep = "")

    ##   Generate the information required to generate the data.
    TS_data <- myTS(TS_key = TS_key,  ...)
    ##  Update the formals of the function with the value 'N'
    formals(TS_data$fun)[[TS_data$fun_data$size_name]] <- N
    ##  Generate the required time series sample(s).
    ## TS <- if (TS_key %in% c("rmgarch", "sample_dccgarch")) {
    TS <- if (TS_key == "rmgarch") {
              ##  Generate the desired sample in a reproducible way:
              ##  Adjust formals to include nr_samples - and - update
              ##  'rseed' if it is 'NULL' (otherwise this will not be
              ##  reproducible when a parallel backend is used).
              formals(TS_data$fun)[["m.sim"]] <- nr_samples
              TS_data$fun_data$m.sim <- nr_samples
              if (is.null(formals(TS_data$fun)[["rseed"]])) {
                  formals(TS_data$fun)[["rseed"]] <- seed_vec
                  TS_data$fun_data$rseed <- seed_vec
              }
              TS_data$fun()
          } else if (TS_key == "rugarch") {
              ##  Generate the desired sample in a reproducible way:
              ##  Adjust formals to include nr_samples - and - update
              ##  'rseed' if it is 'NA' (otherwise this will not be
              ##  reproducible when a parallel backend is used).
              formals(TS_data$fun)[["m.sim"]] <- nr_samples
              TS_data$fun_data$m.sim <- nr_samples
              if (identical(formals(TS_data$fun)[["rseed"]], NA)) {
                  formals(TS_data$fun)[["rseed"]] <- seed_vec
                  TS_data$fun_data$rseed <- seed_vec
              }
              ##  Transpose to get "content" as the first dimension, in
              ##  order to have unified solution with the one below.
              TS <- TS_data$fun()@path$seriesSim
              t(TS)
          } else
              ##  Solution to deal with univariate and multivariate time
              ##  series at the same time.
              aaply(.data = array(
                        data = 1:nr_samples,
                        dim = c(1, nr_samples),
                        dimnames = list(
                            observations = NULL,
                            content = .content)),
                    .margins = 2,
                    .fun = function(x) {
                        set.seed(seed_vec[x])
                        .tmp <- TS_data$fun()
                        ##  Get rid of attributes not related to
                        ##  dimensions, as these made 'aaply' unhappy.
                        attributes(.tmp) <- list(
                            dim = attributes(.tmp)$dim,
                            dimnames = attributes(.tmp)$dimnames)
                        ##  Reminder: Ugly solution that was needed in
                        ##  order to avoid an error that crept in
                        ##  after some update of 'aaply'...
                        if (! is.numeric(.tmp))
                            .tmp[] <- as.numeric(.tmp)
                        .tmp
                    },
                    .drop = FALSE)
    
    ##  'TS' will have two dimensions for the univariate cases and
    ##  three for the multivariate cases, and this will be used for
    ##  the tweaking of dimensions and dimension names.
    if (length(dim(TS)) == 2) {
        ##  Add extra dimension, and update the dimension-names.
        TS <- structure(
            .Data = TS,
            .Dim = c(dim(TS), 1),
            .Dimnames = list(
                content = .content,
                observations = .observations,
                variables = "Y"),
            .multivariate_TS = FALSE,  
            class = c(
                LG_default$class$array,
                if  (nr_samples > 1)
                    LG_default$class$block))
    } else {
        ##  Adjust the dimension-names for the multivariate cases, and
        ##  set the required attributes.
        TS <- structure(
            .Data = TS,
            .Dim = dim(TS),
            .Dimnames = list(
                content = .content,
                observations = .observations,
                variables = paste("Y",
                                  1:(dim(TS)[3]),
                                  sep = "")),
            .multivariate_TS = TRUE,  
            class = c(
                LG_default$class$array,
                if  (nr_samples > 1)
                    LG_default$class$block))
    }
    ##  Update the environment part of 'spy_report' with stuff from
    ##  'TS_data' in order for "hidden defaults" to be recorded too.
    for (new in  setdiff(names(TS_data$args.i),
                         ls(spy_report$envir, all.names = TRUE)))
        spy_report$envir[[new]] <- TS_data$args.i[[new]]
    ##  Update 'call_eval', using the same code as in 'spy', check
    ##  there for details.
    names_formals <- names(formals(spy_report$fun))
    dots_pos <- which(names_formals == "...")
    L <- length(names_formals)
    names_formals <-
        c(names_formals[0:(dots_pos - 1)],
          names(TS_data$args.i),
          if (dots_pos < L) {
              names_formals[(dots_pos + 1):L]
          } else {
              character(0)
          })
    update_these <- seq_along(names_formals) + 1
    spy_report$call_eval[update_these] <- 
        lapply(X = names_formals, FUN = as.name)
    names(spy_report$call_eval)[update_these] <- 
        names_formals
    ##  Return the result as a list-object, with an added class-
    ##  attribute to enable the 'TS_LG_object'-function to distinguish
    ##  it from time-series that we do not know the origin of.
    structure(
        .Data = list(
            TS = TS,
            TS_data = TS_data,
            spy_report = spy_report,
            seed_vec = seed_vec),
        class = c("list", LG_default$class$TS))
}
