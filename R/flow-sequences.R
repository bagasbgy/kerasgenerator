# sequence data generator -------------------------------------------------

#' @title Setup a sequence data generator specification
#'
#' @param data A `tibble` or `data.frame`.
#' @param x_names A `character` vectors containing
#'  the column names for x variables
#' @param y_names A `character` vectors containing
#'  the column names for y variables
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
#' data_gen <- flow_sequences_from_dataframe(
#'   data = sunspots_df,
#'   x_names = "x",
#'   y_names = "x",
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

flow_sequences_from_dataframe <- function(data, x_names, y_names,
                                          lookback, timesteps, batch_size = 32,
                                          mode = "training") {

  # force to tibble
  if (!is(data, "tbl")) {

    data <- as_tibble(data)

  }

  # resolve supervised sequence specification
  steps_to_all <- ceiling((nrow(data) - lookback - timesteps + 1) / batch_size)

  y_start <- timesteps + lookback

  i <- seq(y_start, by = batch_size, length.out = steps_to_all)

  j <- c(i[-length(i)] + batch_size - 1, nrow(data))

  # initial setup
  partition <- 1

  # build generator
  results <- function() {

    # get current partition data
    y_rows <- c(i[partition]:j[partition])

    x_rows <- y_rows - lookback

    rows <- c((min(x_rows) - timesteps + 1):max(y_rows))

    n <- length(y_rows)

    batch <- data[rows, ]

    y_rows <- c((nrow(batch) - n + 1):nrow(batch))

    x_rows <- y_rows - lookback

    # resolve x array
    if (mode %in% c("training", "prediction")) {

      x_batch <- batch[, x_names]

      x_array <- array(0, c(n, timesteps, length(x_names)))

      for (k in c(1:length(x_rows))) {

        x_indices <- seq(x_rows[k] - timesteps + 1, x_rows[k])

        x_array[k, , ] <- data.matrix(x_batch[x_indices, ])

      }

    }

    # resolve y array
    if (mode == "training") {

      y_batch <- batch[, y_names]

      y_array <- array(0, c(n, length(y_names)))

      y_array[, ] <- data.matrix(y_batch[y_rows, ])

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
