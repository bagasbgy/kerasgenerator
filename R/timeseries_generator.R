# series_generator
#-------------------------------------------------------------------------------

#' @title Data generator for training or testing a time series Keras model
#'
#' @description A data generator function for training or testing
#'  a supervised time series Keras model
#'
#' @family time series generators
#'
#' @param data A \code{data.frame} or \code{matrix} containing
#'  the time series data.
#' @param x \code{numeric} or \code{character} vectors that identify
#'  which column(s) in the \code{data} is/are the feature(s).
#' @param y \code{numeric} or \code{character} vectors that identify
#'  which column(s) in the \code{data} is/are the target(s).
#' @param lookback A \code{numeric} vector which identify
#'  the number of lookback period.
#' @param timesteps A \code{numeric} vector which identify
#'  the number of timesteps length.
#' @param start_index A \code{numeric} vector which identify
#'  the first row index of the the \code{data}
#'  that would be included as a sample.
#' @param end_index A \code{numeric} vector which identify
#'  the first row index of the the \code{data}
#'  that would be included as a sample.
#' @param batch_size A \code{numeric} vector which identify
#'  how many observation is considered as a batch.
#' @param return_target A \code{boolean} indicating if the generator.
#'  should return the target or not.
#' @param prep_funs A custom function for preprocess the \code{data}
#'  on the fly, optional.
#'
#' @return If \code{return_target} is set to \code{TRUE},
#'  the generator will returning an object of \code{list} containing
#'  a batch of \code{x_array} and \code{y_array}; otherwise,
#'  it only returns an object of \code{list} containing
#'  a batch of \code{x_array}
#'
#' @export

# time series generator
series_generator <- function(

  data, x, y, lookback = 1, timesteps = 1,
  start_index = NULL, end_index = NULL,
  batch_size = 32, return_target = TRUE,
  prep_funs = NULL

  ) {
  
  # stop if data is not a proper object
  if (!inherits(data, c("data.frame", "matrix")))
    stop("'data' must be an object of 'data.frame' or 'matrix'")
  
  # check start & end index
  if (is.null(start_index)) start_index <- 1
  if (is.null(end_index)) end_index <- nrow(data)
  
  # set some global params
  n_col_x <- length(x)
  n_col_y <- length(y)
  
  # start iterator
  i <- start_index

  # return an iterator
  function() {
    
    # reset iterator if already seen all data
    if ((i + batch_size - 1) > end_index) i <<- start_index

    # iterate current batch's target rows
    y_rows <- c(i:min(i + batch_size - 1, end_index))
    
    # update to next iteration
    i <<- i + batch_size

    # current batch's number of sample
    n_sample <- length(y_rows)
    
    # current batch's row index
    batch_start <- y_rows[1] - lookback - timesteps + 1
    batch_end <- y_rows[n_sample]
    
    # subset the data into current batch
    batch <- data[batch_start:batch_end, ]
    
    # adjust y rows to current batch's index
    y_rows <- c((nrow(batch) - n_sample + 1):nrow(batch))

    # preprocess the batch
    if (!is.null(prep_funs)) batch <- prep_funs(batch)
    if (inherits(batch, "data.frame")) batch <- data.matrix(batch)

    # create container arrays
    x_array <- array(0, dim = c(n_sample, timesteps, n_col_x))
    if (return_target) y_array <- array(0, dim = c(n_sample, n_col_y))
    
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
      x_array[j, , ] <- batch[x_indices, x]
      if (return_target) y_array[j, ] <- batch[y_rows[j], y]

    }

    # return the batch
    if (!return_target) list(x_array)
    else list(x_array, y_array)

  }

}

# forecast_generator
#-------------------------------------------------------------------------------

#' @title Data generator for forecasting using time series Keras model
#'
#' @description A data generator function for forecasting using
#'  a supervised time series Keras model
#'
#' @family time series generators
#'
#' @inheritParams series_generator
#'
#' @return The generator will returning an object of \code{list} containing
#'  a batch of \code{x_array}.
#'
#' @export

# time series forecast generator
forecast_generator <- function(

  data, x, lookback = 1, timesteps = 1,
  last_index = NULL, horizon = 1,
  batch_size = 1, prep_funs = NULL

  ) {
    
  # stop if data is not a proper object
  if (!inherits(data, c("data.frame", "matrix")))
    stop("'data' must be an object of 'data.frame' or 'matrix'")
  
  # sample index
  if (is.null(last_index)) last_index <- nrow(data)
  end_index <- last_index + horizon
  start_index <- end_index - horizon + 1

  # set some global params
  n_col_x <- length(x)

  # start iterator
  i <- start_index

  # return an iterator
  function() {

    # stop iterator if already seen all data
    if ((i + batch_size - 1) > end_index) {
      
      # give warning message
      message("The generator has seen all data, resetting to first batch.")
      
      # reset the iterator
      i <<- start_index
      
    }

    # iterate current batch's x rows
    y_rows <- c(i:min(i + batch_size - 1, end_index))
    x_rows <- y_rows - lookback
    
    # update to next iteration
    i <<- i + batch_size

    # current batch's number of sample
    n_sample <- length(x_rows)
    
    # current batch's row index
    batch_start <- x_rows[1] - timesteps + 1
    batch_end <- x_rows[n_sample]
    
    # subset the data into current batch
    batch <- data[batch_start:batch_end, ]
    
    # adjust x rows to current batch's index
    x_rows <- c((nrow(batch) - n_sample + 1):nrow(batch))

    # preprocess the batch
    if (!is.null(prep_funs)) batch <- prep_funs(batch)
    if (inherits(batch, "data.frame")) batch <- data.matrix(batch)

    # create container array
    x_array <- array(0, dim = c(n_sample, timesteps, n_col_x))

    # fill the container
    for (j in 1:length(x_rows)) {

      # select x indices according to timesteps
      x_indices <- seq(
        from = x_rows[j] - timesteps + 1,
        to = x_rows[j]
      )
      
      # fill the arrays
      x_array[j, , ] <- batch[x_indices, x]

    }

    # return the batch
    list(x_array)

  }

}
