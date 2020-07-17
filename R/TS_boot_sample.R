#' Create bootstrap-replicates of a time series.
#'
#' @description This internal function will, based on the specified
#'     bootstrapping-algorithm and the given time series, create and
#'     save a matrix of bootstrapped time series.
#'
#' @param TS The original time series that we want to create
#'     bootstrap-replicates of.
#'
#' @template main_dir_arg
#' @template save_dir_arg
#' @template nb_boot
#' @template boot_type_boot
#' @template block_length_boot
#' @template boot_seed_boot
#'
#' @details The bootstrapped replicates are stored in a matrix with
#'     \code{nb} rows and \code{length(TS)} columns, i.e.  each row is
#'     a bootstrapped replicate of \code{TS}.
#'
#' @return A list with the following three components is returned to
#'     the internal workflow.
#'
#' \describe{
#'
#' \item{main_dir}{The path (to the hierarchy) given by the
#'     \code{main_dir}-argument.}
#'
#' \item{TS}{The internal (in-hierarchy) path to the saved data.}
#'
#' \item{save_dir}{The (in-hierarchy) name of the directory that the
#'     files are saved into.}
#'
#' }
#'
#' @keywords internal

TS_boot_sample <- function(
    TS,
    main_dir,
    save_dir,
    nb = 5,
    boot_type = c("cibbb_tuples", "block"),
    block_length = 20,
    boot_seed = NULL) {
    ##  Restrict 'boot_type' to its first argument if it is longer
    ##  than one.
    if (length(boot_type) > 1) 
        boot_type <- boot_type[1]
    ##  Update 'TS' with the object from file
    load(file = paste(c(main_dir, TS),
                      collapse = .Platform$file.sep))
    ##  If 'TS' originates from 'TS_LG_object', it should have an
    ##  attribute 'TS_for_analysis' that should be used instead of TS.
    if (! identical(x = attributes(TS)$TS_for_analysis,
                    y = NULL))
        TS <- attributes(TS)$TS_for_analysis
    ##  Check that 'TS' has the correct properties.
    .OK_TS <- nested_if(
        if_list = list(
            is.array(TS),
            length(dim(TS)) == 3,
            all(names(dimnames(TS)) %in% c("observations", "variables", "content")),
            length(dimnames(TS)$content) == 1),
        expr_not_all_TRUE = FALSE)
    if (! .OK_TS)
        error(.argument = "TS",
              c("The argument",
                sQuote("TS"),
                "must be an array of dimension three,",
                "having the dimension names",
                sQuote("observations"),
                ",",
                sQuote("variables"),
                "and",
                sQuote("content"),
                "(in any order).",
                "Moreover, the length of the",
                sQuote("content"),
                "dimension must be one."))
    kill(.OK_TS)
    ##  Set the seed, if it is given
    if (! is.null(boot_seed))
        set.seed(seed = boot_seed)
    kill(boot_seed)
    ##  Extract the attributes to be reinserted later on.
    TS_attributes_to_keep <-
        attributes(TS)[!names(attributes(TS))%in%c("dim", "dimnames")]
    ##  Compute the bootstrap replicates, including an update of the
    ##  dimension-names and attributes to tell other parts of the code
    ##  what to do when this object is encountered.  First: Drop the
    ##  content dimension
    TS <- adrop(x = TS, drop = which(names(dimnames(TS))=="content"))
    ##  Adjust the array so the observations are at the second
    ##  dimension.  (The subsetting used in the construction will due
    ##  to this be contiguous blocks, which might improve the speed.)
    TS <- restrict_array(
        .arr = TS,
        .restrict = dimnames(TS)[c("variables", "observations")],
        .permute = TRUE)
    ##  Create the collection of indices, i.e. a vector that enables
    ##  subsetting of vectors to be used in the construction of the
    ##  bootstrapped sample.
    .boot_indices <- local({
        ##  Identify the properties of the sample.
        .d <- length(dimnames(TS)$variables)
        .n <- length(dimnames(TS)$observations)
        ##  Creat a help function to be used when selecting the
        ##  indices needed for the subsetting of TS (when taking into
        ##  account the way that the elements of the array 'TS' are
        ##  indexed as a vector).  Note: Only the second alternative
        ##  will be used due to the formating of 'TS' made above, but
        ##  the code is left here as a reminder.
        .help_fun <- if (names(dimnames(TS))[1] == "observations") {
            function(x)
                if (.d == 1) {
                    x
                } else
                    as.vector(vapply(
                        X = 0:(.d-1),
                        FUN = function(..d)
                            x + ..d * .n,
                        FUN.VALUE = numeric(.n)))
        } else {
            function(x) {
                if (any(.d == 1, boot_type == "cibbb_tuples")) {
                    x
                } else
                    as.vector(vapply(
                        X = x,
                        FUN = function(.x)
                        (.x-1) * .d + 1:.d,
                        FUN.VALUE = numeric(.d)))
            }
        }
        ##  Create the indices to be used for the subsetting.
        as.vector(t(tsbootstrap(
            x = seq(dimnames(TS)$observations),
            nb = nb,
            statistic = .help_fun,
            b = block_length,
            type = "block")$statistic))
    })
    ##  Create the desired 'TS_boot', by exploiting the format that
    ##  'TS' has been converted to.  There are (for the time being)
    ##  two options here, one based on the block-bootstrap, and one
    ##  based on the circular index-based block bootstrap for tuples.
    ##  Reminder: In the latter case it is the "starting-indices" that
    ##  are stored, and the extraction is taken care of in other
    ##  functions later on.
    TS_boot <- if (boot_type == "cibbb_tuples") {
                   structure(
                       .Data = .boot_indices,
                       .Dim = c(1, dim(TS)[2], nb),
                       .Dimnames = c(
                           list(variables = "cibbb_tuples"),
                           dimnames(TS)[2],
                           list(content = paste(LG_default$boot.prefix, 1:nb, sep = ""))))
               } else {
                   structure(
                       .Data = TS[.boot_indices],
                       .Dim = c(dim(TS), nb),
                       .Dimnames = c(
                           dimnames(TS),
                           list(content = paste(LG_default$boot.prefix, 1:nb, sep = ""))))
               }
    attributes(TS_boot) <- c(
        attributes(TS_boot),
        list(bootstrap = TRUE,
             orig_TS = TS,
             boot_type = boot_type),
        TS_attributes_to_keep)
    kill(TS, nb, block_length, TS_attributes_to_keep, .boot_indices)
    ##  Save the result to file.
    save_file.Rda <- LG_default$global["TS_boot"]
    LG_save(data = TS_boot,
            save_file.Rda = save_file.Rda,
            save_dir = save_dir)
    ##  Adjust save_dir to enable portability.
    save_dir <- strsplit(
        x = gsub(
            pattern = paste(main_dir,
                            .Platform$file.sep,
                            sep = ""),
            replacement = "",
            x = save_dir),
        split = .Platform$file.sep)[[1]]
    ##  Return data needed for the next function in line, and for the
    ##  update of the 'info'-object in the calling function.
    list(main_dir = main_dir,
         TS = c(save_dir, save_file.Rda),
         save_dir = save_dir)
}
