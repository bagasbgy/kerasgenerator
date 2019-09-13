# create example data
sunspots_df <- as.data.frame(sunspots)

# example generator: sequences to one
data_gen <- flow_sequences_from_dataframe(
  data = sunspots_df,
  x_names = "x",
  y_names = "x",
  lookback = 1,
  timesteps = 12,
  batch_size = 32,
  mode = "training"
)

# quick check
arrays <- data_gen()

# test result dimension
test_that("flow_sequences_from_dataframe return correct dimension", {

  expect_equal(dim(arrays[[1]]), c(32, 12, 1))
  expect_equal(dim(arrays[[2]]), c(32, 1))

})

# get x and y example
x <- arrays[[1]]
y <- arrays[[2]]

# test result values
test_that("flow_sequences_from_dataframe return correct values", {

  expect_equal(as.numeric(x[1, 1:12, 1]), sunspots_df[1:12, 1])
  expect_equal(as.numeric(y[1, 1]), sunspots_df[13, 1])

})
