# test settings
#--------------------

# test context
context("Time Series Generator Output")

# example data
data <- data.frame(
  x = runif(80),
  y = runif(80),
  z = runif(80)
)

# variables
x <- c("x", "y")
y <- 2:3

# supervise settings
lookback <- 10
timesteps <- 10

# data settings
start_index <- 20
end_index <- 80
batch_size <- 10

# sample generator
data_gen <- series_generator(
  data = data,
  y = y,
  x = x,
  lookback = lookback,
  timesteps = timesteps,
  start_index = start_index,
  end_index = end_index,
  batch_size = batch_size,
  return_target = TRUE
)

# test: equal
#--------------------

# get the first iteration
data_list <- data_gen()

# get the x and y arrays
x_sample <- data_list[[1]]
y_sample <- data_list[[2]]

# test the output dimension
test_that("Generated output's dimension is equal to specified parameter", {
  
  # test x and y dimension
  expect_equal(dim(x_sample), c(batch_size, timesteps, length(x)))
  expect_equal(dim(y_sample), c(batch_size, 1, length(y)))
  
})

# test if identic
test_that("Generated output's dimension is identical to original data", {
  
  # test if x array identical to original data
  expect_equal(x_sample[1, , 1], data[1:10, "x"])
  expect_equal(x_sample[2, , 1], data[2:11, "x"])
  expect_equal(x_sample[1, , 2], data[1:10, "y"])
  expect_equal(x_sample[2, , 2], data[2:11, "y"])
  
  # test if y array identical to original data
  expect_equal(y_sample[1, , 1], data[20, "y"])
  expect_equal(y_sample[2, , 1], data[21, "y"])
  expect_equal(y_sample[1, , 2], data[20, "z"])
  expect_equal(y_sample[2, , 2], data[21, "z"])
  
})
