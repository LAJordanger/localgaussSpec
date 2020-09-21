#' Load files to be used during the interactive inspection of the plots
#'
#' @description This internal function takes care of some details
#'     related to the loading of files needed for the creation of a
#'     plot, and it then stores the result in the environment given to
#'     the \code{.env}-argument.  It does this both for the
#'     interactive solution in \code{LG_shiny} and for the
#'     non-reactive solution used when plots are created for the use
#'     in papers/presentations.
#'
#' @param look_up A list created by \code{LG_lookup} (in the function
#'     \code{LG_plot_helper}), where the key details have been
#'     extracted (or constructed) from a (non-reactive copy) of the
#'     values defined by the \code{shiny}-interface of
#'     \code{LG_shiny}.
#'
#' @param .env The environment in which the loaded data will be
#'     stored.
#'
#' @param .extract_LG_data_only Logical argument, default value
#'     \code{FALSE}.  The value \code{FALSE} will ensure that a plot
#'     is created, whereas the value \code{TRUE} is used when the
#'     desired outcome is an object containing the estimated local
#'     Gaussian values.
#' 
#' @return The required data will be loaded from files into
#'     \code{.env}, but only if it hasn't been done before.  The
#'     resulting array will then be assigned to the environment of the
#'     calling function under the name given by \code{.result}.
#'
#' @keywords internal

LG_plot_load <- function(look_up,
                         .env,
                         .extract_LG_data_only) {
    ##  Some shortcuts to make the code slightly easier to read.
    cache <- look_up$cache
    env_name <- cache$env_name
    global_name <- look_up$global_name
    local_name <- look_up$local_name
    ##  Check if existing data has been loaded already.  If necessary
    ##  load the data from files.  Reminder: The strategy is based on
    ##  the existence of an environment inside of '.env'.  If that
    ##  environment does not exist, then it is necessary to create it.
    if (! exists(x = env_name, envir = .env)) {
        ##  Create the environment
        .env[[env_name]] <- new.env()
        ##  Load global data into the new environment, with an
        ##  adjustment for the cases where bootstrap is present.
        LG_load( 
            .file = look_up$.global_file,
            .env = .env[[env_name]],
            .name = global_name)
        if (look_up$is_bootstrap) {
            ##  Add details from the original time-series into the
            ##  details obtained from the bootstrapped values.
            LG_load( 
                .file = look_up$.orig_files$.global_file,
                .name = ".temp")
            ##  Restrict the 'TS'-dimension of the new data in order
            ##  for the two arrays to be merged
            .ts_part <-  dimnames(.env[[env_name]][[global_name]])["TS"]
            .temp <- 
                leanRcoding::restrict_array(
                                 .arr = .temp,
                                 .restrict = .ts_part)
            ##  Add the new data to the bootstrap-data, which implies
            ##  that the 'content'-dimension now also will have a "orig"-component.
            .env[[env_name]][[global_name]] <- my_abind(
                .temp,
                .env[[env_name]][[global_name]])
            ##  Remove ".temp" from our desired environment.
            kill(.temp, .ts_part)
        }
        ##  Load local data into the new environment, with an
        ##  adjustment for the cases where bootstrap is present.
        LG_load(.file = look_up$approx_file,
                .env = .env[[env_name]],
                .name = local_name)
        if (look_up$is_bootstrap) {
            ##  Add details from the original time-series into the
            ##  details obtained from the bootstrapped values.
            LG_load( 
                .file = look_up$.orig_files$approx_file,
                .name = ".temp")
            ##  Restrict the dimensions of the new data in order for
            ##  the two arrays to be merged.  Reminder: This is
            ##  necessary since the function that computes the
            ##  bootstrap-part is allowed to restrict the attention to
            ##  a subset of those used in the computation of original.
            for (.part in names(.temp)) {
                if (is.null(.temp[[.part]]))
                    next
                .the_dimnames <- dimnames(.env[[env_name]][[local_name]][[.part]])
                ##  Remove "content"-part
                .the_dimnames <- .the_dimnames[! names(.the_dimnames) %in% "content"]
                ##  Perform the update for this part.
                .temp[[.part]] <-
                    leanRcoding::restrict_array(
                                     .arr = .temp[[.part]],
                                     .restrict = .the_dimnames)
                ##  Add the new data to the bootstrap-data, which implies
                ##  that the 'content'-dimension now also will have a "orig"-component.
                .env[[env_name]][[local_name]][[.part]] <- my_abind(
                    .temp[[.part]],
                    .env[[env_name]][[local_name]][[.part]])
            }
            kill(.temp, .the_dimnames, .part)
        }
    }
    ##  To make the code later on more compact, create a pointer to
    ##  the environment that we want to update.
    ..env <- .env[[env_name]]
    kill(.env, env_name)
    ##  Call different functions depending on the type of plot that
    ##  should be created.
    if (look_up$TCS_type == "S") {
        ##  The extractions and computations required for the
        ##  inspection of the local Gaussian spectra are taken care of
        ##  here, and the results are stored in '..env'.
        LG_plot_df_spectra(look_up = look_up, ..env = ..env)
    }
    if (look_up$TCS_type == "C") {
        ##  The extractions and computations required for the
        ##  inspection of the local Gaussian correlations are taken
        ##  care of here, and the results are stored in '..env'.
        LG_plot_df_correlation(look_up = look_up, ..env = ..env)
    }
    if (!any(look_up$heatmap,
             look_up$distance_plot)) {
        ##  Add an environment 'plot_data' to '..env', with the
        ##  data-frames needed for the desired plot of correlations
        ##  and spectra.
        LG_create_plot_df(look_up = look_up,
                          ..env = ..env)
    }
    ##  Return the underlying data when required.
    if (.extract_LG_data_only) {
        return(list(..env = ..env,
                    look_up = look_up))
    } 
    ##  Create the plot
    ..env$.lag_plot<- LG_plot(look_up = look_up,
                              ..env = ..env)
    ##   Return the plot to the workflow.
    ..env$.lag_plot
    
}
