# create example data
sunspots_df <- as.data.frame(sunspots)

# example generator: sequences to one
data_gen <- flow_series_from_dataframe(
  data = sunspots_df,
  x = "x",
  y = "x",
  length_out = 1,
  stride = 1,
  lookback = 1,
  timesteps = 12,
  batch_size = 32,
  mode = "training"
)

# quick check
arrays <- data_gen()

# test result dimension
test_that("flow_series_from_dataframe return correct dimension", {

  expect_equal(dim(arrays[[1]]), c(32, 12, 1))
  expect_equal(dim(arrays[[2]]), c(32, 1, 1))

})

# get x and y example
x <- arrays[[1]]
y <- arrays[[2]]

# test result values
test_that("flow_series_from_dataframe return correct values for length_out = 1 and stride = 1", {

  expect_equal(as.numeric(x[1, 1:12, 1]), sunspots_df[1:12, 1])
  expect_equal(as.numeric(y[1, 1, 1]), sunspots_df[13, 1])

})
