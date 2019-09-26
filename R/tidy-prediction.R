# helper functions for tidying prediction results -------------------------

#' @title Helper function for tidying `keras`' prediction results
#'
#' @param generator A reference data generator generated from
#'  `kerasgenerator` package.
#' @param prediction A `matrix` or an `array` containing the prediction results
#' @param ... Further argument for specific generators. See details.
#'
#' @details
#'
#'  soon updated
#'
#' @return A `tible` containing tidied prediction results
#'
#' @export

tidy_prediction <- function(generator, prediction, ...) UseMethod("tidy_prediction")

# default routers
tidy_prediction.function <- function(generator, prediction, ...) NextMethod()

tidy_prediction.default <- function(generator, prediction, ...) {

  stop("tidy_prediction() only supports generators from kerasgenerator package")

}
