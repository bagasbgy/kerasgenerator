# series data generator ---------------------------------------------------

#' @title Setup a time series data generator specification
#'
#' @param data A `tibble` or `data.frame`.
#' @param x A `character` vectors containing the column names for `x` variables
#' @param y A `character` vectors containing the column names for `y` variables
#' @param length_out An `integer` specifying length out size.
#' @param stride An `integer` specifying stride size.
#' @param lookback An `integer` specifying lookback period.
#' @param timesteps An `integer` specifying timesteps length.
#' @param batch_size An `integer` specifying batch size.
#' @param mode A `character` vector of length one which specify
#'  the generator behaviour: either `"training"` or `"prediction"`.
#'  See details for further information.
#'
#' @details
#'
#'  soon updated
#'
#' @return A `function` which could generate batches of x and/or y arrays
#'
#' @examples
#'
#' # import library
#' library(kerasgenerator)
#'
#' # create example data
#' sunspots_df <- as.data.frame(sunspots)
#'
#' # setup a generator
#' data_gen <- flow_series_from_dataframe(
#'   data = sunspots_df,
#'   x = "x",
#'   y = "x",
#'   length_out = 1,
#'   stride = 1,
#'   lookback = 1,
#'   timesteps = 12,
#'   batch_size = 32,
#'   mode = "training"
#' )
#'
#' # quick check
#' arrays <- data_gen()
#'
#' str(arrays[[1]])
#' str(arrays[[2]])
#'
#' @export

flow_series_from_dataframe <- function(data, x, y, length_out, stride,
                                       lookback, timesteps, batch_size = 32,
                                       mode = "training") {

  # force to tibble
  if (!is(data, "tbl")) {

    data <- as_tibble(data)

  }

  # resolve supervised sequence specification
  max_sample_first_pos <- nrow(data) - lookback - timesteps + 1
  max_sample_first_pos <- length_out * floor(max_sample_first_pos / length_out)
  max_sample_first_pos <- stride * ceiling(max_sample_first_pos / stride)

  n_sample <- max_sample_first_pos / stride

  steps_to_all <- ceiling(n_sample / batch_size)

  y_start <- timesteps + lookback

  i <- seq(y_start, by = batch_size * stride, length.out = steps_to_all)

  j <- c(i[-1] - stride, max_sample_first_pos + lookback + timesteps - 1) + length_out - 1

  j[length(j)] <- min(max(j), max_sample_first_pos + lookback + timesteps - 1)

  # initial setup
  partition <- 1

  # build generator
  results <- function() {

    # get current partition indices
    ij <- c(i[partition]:j[partition])

    y_rows <- list()
    x_rows <- list()

    # define current batch size
    n <- 0

    for (size in 1:batch_size) {

      if (length_out + stride * (size - 1) <= length(ij)) {

        n <- n + 1

      }

    }

    for (index in 1:n) {

      y_rows[[index]] <- ij[c(1:length_out) + stride * (index - 1)]

      x_rows[[index]] <- (min(y_rows[[index]]) - timesteps + 1):min(y_rows[[index]])

      x_rows[[index]] <- x_rows[[index]] - lookback

    }

    # resolve x array
    if (mode %in% c("training", "prediction")) {

      x_array <- array(0, c(n, timesteps, length(x)))

      for (k in c(1:length(x_rows))) {

        x_array[k, , ] <- data.matrix(data[x_rows[[k]], x])

      }

    }

    # resolve y array
    if (mode == "training") {

      y_array <- array(0, c(n, length_out, length(y)))

      for (k in c(1:length(y_rows))) {

        y_array[k, , ] <- data.matrix(data[y_rows[[k]], y])

      }

    }

    # update to next partition
    if (partition + 1 > steps_to_all) {

      partition <<- 1

    } else {

      partition <<- partition + 1

    }

    # return the array
    if (mode == "prediction") {

      list(x_array)

    } else {

      list(x_array, y_array)

    }

  }

  # return the generator
  results

}
