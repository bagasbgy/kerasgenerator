#' @title Time series data generator for training and testing
#'
#' @description A data generator function for training and testing
#'  a supervised time series Keras model
#'
#' @family time series generators
#'
#' @param data A data frame or matrix containing the time series data.
#' @param x Numeric or character vectors that identify
#'  which column(s) in the \code{data} is/are the feature(s).
#' @param y Numeric or character vectors that identify
#'  which column(s) in the \code{data} is/are the target(s).
#' @param lookback A numeric vector which identify
#'  the number of lookback period.
#' @param timesteps A numeric vector which identify
#'  the number of timesteps length.
#' @param start_index A numeric vector which identify
#'  the first row index of the the \code{data}
#'  that would be included as a sample.
#' @param end_index A numeric vector which identify
#'  the first row index of the the \code{data}
#'  that would be included as a sample.
#' @param batch_size A numeric vector which identify
#'  how many observation is considered as a batch.
#' @param return_target A boolean indicating if the generator.
#'  should return the target or not.
#'
#' @return If \code{return_target} is set to \code{TRUE},
#'  the generator will returning and object of \code{list} containing
#'  a batch of \code{x} and \code{y arrays}.
#'
#' @export

# time series generator
series_generator <- function(

  data, x, y, lookback, timesteps,
  start_index, end_index, batch_size,
  return_target = TRUE

  ) {
  
  # stop if data is not a proper object
  if (!inherits(data, c("data.frame", "matrix")))
    stop("'data' must be an object of 'data.frame' or 'matrix'")
  
  # convert if data.frame
  if ("data.frame" %in% class(data)) data <- data.matrix(data)
  
  # check start & end index
  if (is.null(start_index)) start_index <- 1
  if (is.null(end_index)) end_index <- nrow(data)
  
  # set some initial params
  n_col_x <- length(x)
  n_col_y <- length(y)
  
  # start iterator
  i <- start_index

  # return an iterator
  function() {
    
    # reset iterator if already seen all data
    if ((i + batch_size - 1) > end_index) i <<- start_index

    # current batch params
    y_rows <- c(i:min(i + batch_size - 1, end_index))
    n_sample <- length(y_rows)
    
    # update to next iteration
    i <<- i + batch_size

    # create container arrays
    x_array <- array(0, dim = c(n_sample, timesteps, n_col_x))
    if (return_target) y_array <- array(0, dim = c(n_sample, 1, n_col_y))

    # fill the container
    for (j in 1:n_sample) {

      # adjust x row by lookback
      x_row <- y_rows[j] - lookback

      # select x indices according to timesteps
      x_indices <- seq(
        from = x_row - timesteps + 1,
        to = x_row
      )

      # fill the arrays
      x_array[j, , ] <- data[x_indices, x]
      if (return_target) y_array[j, , ] <- data[y_rows[j], y]

    }

    # return the batch
    if (!return_target) list(x_array)
    else list(x_array, y_array)

  }

}

#' @title Time series data generator for forecasting
#'
#' @description A data generator function for forecasting
#'  a supervised time series Keras model
#'
#' @family time series generators
#'
#' @inheritParams series_generator
#'
#' @return The generator will returning and object of \code{list} containing
#'  a batch of \code{x} arrays.
#'
#' @export

# time series forecast generator
forecast_generator <- function(

  data, x, timesteps,
  start_index, end_index,
  batch_size

  ) {
    
  # stop if data is not a proper object
  if (!inherits(data, c("data.frame", "matrix")))
    stop("'data' must be an object of 'data.frame' or 'matrix'")
  
  # convert if data.frame
  if ("data.frame" %in% class(data)) data <- data.matrix(data)

  # check start & end index
  if (is.null(start_index)) start_index <- 1
  if (is.null(end_index)) end_index <- nrow(data)

  # set some initial params
  n_col_x <- length(x)

  # start iterator
  i <- start_index

  # return an iterator
  function() {

    # reset iterator if already seen all data
    if ((i + batch_size - 1) > end_index) i <<- start_index

    # current batch params
    x_rows <- c(i:min(i + batch_size - 1, end_index))
    n_sample <- length(x_rows)

    # update to next iteration
    i <<- i + batch_size

    # # create container array
    x_array <- array(0, dim = c(n_sample, timesteps, n_col_x))

    # fill the container
    for (j in 1:length(x_rows)) {

      # x row
      x_row <- x_rows[j]

      # select x indices according to timesteps
      x_indices <- seq(
        from = x_row - timesteps + 1,
        to = x_row
      )

      # fill the x array
      x_array[j, , ] <- data[x_indices, x]

    }

    # return the batch
    list(x_array)

  }

}
