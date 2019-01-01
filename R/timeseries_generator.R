#' @title Time series data generator for training and testing
#'
#' @description A data generator function for training and testing
#'  a supervised time series Keras model
#'
#' @family time series generators
#'
#' @param data A data frame containing the time series data.
#' @param y Numeric or character vectors that identify
#'  which column(s) in the \code{data} is/are the target(s).
#' @param x Numeric or character vectors that identify
#'  which column(s) in the \code{data} is/are the feature(s).
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

  data, y, x, lookback, timesteps,
  start_index = NULL, end_index = NULL,
  batch_size, return_target = TRUE

  ) {

  # check start & end index
  if (is.null(start_index)) start_index <- 1
  if (is.null(end_index)) end_index <- nrow(data)

  # start iterator
  i <- start_index

  function() {

    # reset iterator if already seen all data
    if ((i + batch_size - 1) > end_index) i <<- start_index

    # y rows
    y_rows <- c(i:min(i + batch_size - 1, end_index))

    # update to next iteration
    i <<- i + batch_size

    # arrays dim
    x_dim <- c(length(y_rows), timesteps, length(x))
    if (return_target == FALSE) y_dim <- c(length(y_rows), 1, length(y))

    # array containers
    x_array <- array(0, dim = x_dim)
    if (return_target == FALSE) y_array <- array(0, dim = y_dim)

    # fill the container
    for (j in 1:length(y_rows)) {

      # adjust x row by lookback
      x_row <- y_rows[j] - lookback

      # select x indices according to timesteps
      x_indices <- seq(
        from = x_row - timesteps + 1,
        to = x_row
      )

      # fill the arrays
      x_array[j, , ] <- data[x_indices, x]
      if (return_target == FALSE) y[j, , ] <- data[y_rows[j], y]

    }

    # return the batch
    if (return_target == TRUE) list(x)
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

  data, x, timesteps, start_index = NULL, end_index = NULL, batch_size

  ) {

  # check start & end index
  if (is.null(start_index)) start_index <- 1
  if (is.null(end_index)) end_index <- nrow(data)

  # start iterator
  i <- start_index

  function() {

    # reset iterator if already seen all data
    if ((i + batch_size - 1) > end_index) i <<- start_index

    # sample rows
    sample_rows <- c(i:min(i + batch_size - 1, end_index))

    # update to next iteration
    i <<- i + batch_size

    # samples dims
    samples_dim <- c(length(sample_rows), timesteps, length(x))

    # array container for sample
    samples <- array(0, dim = samples_dim)

    # fill the container
    for (j in 1:length(sample_rows)) {

      # sample row
      sample_row <- sample_rows[j]

      # select sample indices according to timesteps
      sample_indices <- seq(
        from = sample_row - timesteps + 1,
        to = sample_row
      )

      # fill the sample array
      samples[j, , ] <- data[sample_indices, x]

    }

    # return the batch
    list(samples)

  }

}
