################################################################################

#' Select points for the local Gaussian inspections.
#'
#' The purpose of this function is to create the
#' "points to inspect"-argument needed for the local Gaussian
#' investigation.  
#'
#' @param .P1 A bivariate vector, with values in the range (0,1), that
#'     gives the first endpoint.  A single number will be accepted, in
#'     which case it will be registered as a diagonal point.  The
#'     values will be converted by \code{qnorm} to get points in the
#'     plane.
#'
#' @param .P2 A bivariate vector, with values in the range (0,1), that
#'     gives the second endpoint.  A single number will be accepted,
#'     in which case it will be registered as a diagonal point.  The
#'     values will be converted by \code{qnorm} to get points in the
#'     plane.  Note that \code{.P2} can coincide with \code{.P1} in
#'     which case the values given to \code{.shape} will be ignored.
#'
#' @param .shape A bivariate vector of integers, that describes the
#'     shape of the resulting "grid".  Note that it is required that
#'     the first component of \code{.shape} must be a positive integer
#'     and the second a non-negative integer.  This argument will,
#'     when \code{.P1} and \code{.P2} are different, either specify a
#'     rectangular/quadratic with the points \code{.P1} and \code{.P2}
#'     as opposite corners, or it will specify points upon a line with
#'     \code{.P1} and \code{.P2} as endpoints if the second component
#'     is zero.  A single positive integer will be accepted, and
#'     it will be interpreted as if the missing number should be zero.
#'     This argument will be ignored if \code{.P1} and \code{.P2}
#'     coincide completely.  If \code{.P1} and \code{.P2} coincide at
#'     one of the components, and a bivariate vector with two positive
#'     integers is given to \code{.shape}, then shape will be adjusted
#'     to produce a line instead in order to avoid redundant
#'     computations.  The values given in \code{.P1} and \code{.P2}
#'     must be contained in the grid, so the value \code{1} can only
#'     be used in \code{.shape} when \code{.P1} and \code{.P2} have a
#'     common component.  If the components differ, the value will be
#'     corrected to \code{2}, which is the smallest integer that gives
#'     a reasonable result.
#'
#' 
#'
#' @return An array containing points computed according to the rules
#'     specified under the description of the shape-argument.  The
#'     three arguments \code{.P1}, \code{.P2} and \code{.shape} will
#'     be stored as attributes, together with three additional
#'     attributes \code{Shape}, \code{Horizontal} and \code{Vertical},
#'     of which the last two will specify the points at the corner of
#'     the grid, whereas \code{Shape} will contain the value "point"
#'     if \code{.P1} and \code{.P2} are equal, the value "rectangle"
#'     if \code{.shape} contains two positive integers, or else it
#'     will contain the value "line.".  The result will in addition
#'     have the class-attribute "LG_points"
#'
#' @export

LG_select_points <- function(.P1, .P2, .shape) {
    ##  Sanity check '.P1' and '.P2'.
    .valid_points <- local({
        .range <- range(.P1, .P2)
        .range_OK <- min(.range) > 0 & max(.range) < 1
        if (.range_OK) {
            max(length(.P1), length(.P2)) <= 2
        } else
            FALSE
    })
    if (! .valid_points)
        error(.argument = c(".P1", ".P2"),
              c("Invalid values, insert univariate/bivariate values in",
              "the open interval (0,1)."))
    kill(.valid_points)
    ##  Adjust the points to bivariate if necessary.
    if (length(.P1) == 1)
        .P1[2] <- .P1[1]
    if (length(.P2) == 1)
        .P2[2] <- .P2[1]
    ##  Sanity check '.shape', when required.
    if (! identical(x = .P1, .P2)) {
        .valid_shape <- local({
            .length <- length(.shape)
            .length_OK <- .length <= 2 & .length > 0
            if (.length_OK) {
                ##  Check that the content satifies the requirements.
                if (all(as.integer(.shape) == .shape)) {
                    all(.shape[1] > 0,
                        if (.length > 1)
                            .shape[2] >= 0)
                } else
                    FALSE
            } else FALSE
        })
        if (! .valid_shape) 
            error(.argument = ".shape",
                  c("This must either be a bivariate vector whose",
                    "first component is a univiariate positive integer",
                    "and whose second component is a nonegative integer.",
                    "A single positive integer will also be accepted."))
        kill(.valid_shape)
        ##  Adjust '.shape' to bivariate if necessary.
        if (length(.shape) == 1)
            .shape[2] <- 0
        ##  Adjust '.shape' (when necessary) to produce a line if
        ##  there is an overlap on one of the components (overlap on
        ##  both is excluded in this branch of the code)
        if (.shape[1] > 0 & .shape[2] > 0) {
            if (.P1[1] == .P2[1])
                .shape[2] <- 0
            if (.P1[2] == .P2[2]) {
                .shape[1] <- .shape[2]
                .shape[2] <- 0
            }
        }
        ##  Check that the value '1' is not used when the points have
        ##  different components, and if so update it to '2'.
        for (i in 1:2) 
            if (.shape[i] ==1)
                if (.P1[i] != .P2[i])
                    .shape[i] <- 2
    } else
        .shape <- c(1, 0)
###-------------------------------------------------------------------
    ##  Create the collection of points to return, in two steps: Find
    ##  arrays containing all the points of interest first, then ues
    ##  'qnorm' to find the points in the plane.
    .array_of_points <-
        if  (identical(x = .P1, .P2)) {
            .key1 <- .P1[1]
            .key2 <- .P1[2]
            ##  Return the point with dimensional attributes.
            structure(
                .Data = .P1,
                .Dim = c(1, 2))
        } else {
            ##  A line or a rectangle, "starting" at the lower right,
            ##  introdcue '.p1' and '.p2' to achieve this.
            if (.P1[1] < .P2[1]) {
                .p1 <- .P1
                .p2 <- .P2
            } else {
                if (.P1[1] == .P2[1]) {
                    if (.P1[2] < .P2[2]){
                        .p1 <- .P1
                        .p2 <- .P2
                    } else {
                        .p1 <- .P2
                        .p2 <- .P1
                    }
                } else {
                    .p1 <- .P2
                    .p2 <- .P1
                }
            }
            ##  Find the key-points of interest
            .key1 <- seq(
                from = .p1[1],
                to = .p2[1],
                length.out = .shape[1])
            .key2 <- seq(
                from = .p1[2],
                to = .p2[2],
                length.out =
                    if (.shape[2] == 0) {
                        .shape[1]
                    } else
                        .shape[2])
            ##  Create the array based on shape.
            if (.shape[2] == 0) {
                cbind(.key1, .key2)
            } else
                local({
                    .tmp <- expand.grid(first = .key1, second = .key2)
                    cbind(.tmp$first, .tmp$second)
                })
        }
    ##  Update the array by the help of 'qnorm', and add the required
    ##  attributes
    structure(
        .Data = qnorm(.array_of_points),
        .Dim = dim(.array_of_points),
        .Dimnames = list(
            level = apply(X = .array_of_points,
                          MARGIN = 1,
                          FUN = paste,
                          collapse = "_"),
            comp = c("v1", "v2")),
        class = c(LG_default$class$points,
                  LG_default$class$array),
        .P1 = .P1,
        .P2 = .P2,
        .shape = .shape,
        Horizontal = qnorm(.key1),
        Vertical = qnorm(.key2),
        Shape =
            if (identical(x = .P1, .P2)) {
                "points"
            } else {
                if (.shape[2] == 0) {
                    "line"
                } else
                    "rectangle"
            }
    )
}
