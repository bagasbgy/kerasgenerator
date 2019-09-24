# get generator metadata --------------------------------------------------

#' @title Get a specific metadata from a generator
#'
#' @param x A generator generated using `kerasgenerator`.
#' @param meta A `character` vectors of length one to specify which
#'  metadata to get. **Optionally**, the function also support `"all"` value
#'  to return all metadata. See details.
#'
#' @details
#'
#'  soon updated
#'
#' @return An object with class according to specified metadata
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
#' # check generator mode
#' generator_meta(data_gen, "mode")
#'
#' # get all metadata as list
#' meta <- generator_meta(data_gen, "all")
#'
#' meta[["batch_size"]]
#'
#' @export

generator_meta <- function(x, meta) {

  # get the environment
  env <- as.list(environment(x))

  # get the specified metadata
  if (meta == "all") {

    results <- env

  } else {

    results <- env[[meta]]

  }

  # return the results
  results

}

# set generator mode ------------------------------------------------------

#' @title Set/modify a generator's mode
#'
#' @inheritParams generator_meta
#'
#' @param new_mode new mode that would be assigned to the generator.
#'
#' @details
#'
#'  soon updated
#'
#' @return An modified data generator (copied as new environment)
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
#' # check generator mode
#' generator_meta(data_gen, "mode")
#'
#' # modify generator mode
#' data_gen_modified <- generator_mode(data_gen, "prediction")
#'
#' # compare old and new generators
#' generator_meta(data_gen, "mode")
#' generator_meta(data_gen_modified, "mode")
#'
#' @export

generator_mode <- function(x, new_mode) {

  # make a copy
  results <- copy_as_new(x)

  # get all specified meta
  assign("mode", new_mode, envir = environment(results))

  # return modified generator
  results

}

copy_as_new <- function(x) {

  # make a copy
  results <- x

  # readjust copied environment
  environment(results) <- new.env()

  for(n in ls(environment(x), all.names = TRUE)) {

    assign(n, get(n, environment(x)), envir = environment(results))

  }

  # return the results
  results

}
