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

# create modified generator using `generator_mode()`
data_gen_modified <- generator_mode(data_gen, "prediction")

# gather old and new results
old_env <- environment(data_gen)
new_env <- environment(data_gen_modified)

old_mode <- generator_meta(data_gen, "mode")
new_mode <- generator_meta(data_gen_modified, "mode")

# test set mode
test_that("set mode correctly handle environment copy and modify", {

  expect_false(identical(old_env, new_env))
  expect_false(identical(old_mode, new_mode))

})
