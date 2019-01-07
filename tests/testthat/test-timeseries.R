# test settings
#--------------------

# test context
context("Time Series Generator Output")

# example data
data <- data.frame(
  x = runif(100),
  y = runif(100),
  z = runif(100)
)

# variables
x <- c("x", "y")
y <- 2:3

# supervised parameter
lookback <- 10
timesteps <- 10

# dataset settings
train_size <- 40
val_size <- 20

# forecast settings

# train-val row indices
val_end <- nrow(data)
val_start <- val_end - val_size + 1

train_end <- val_start - 1
train_start <- train_end - train_size + 1

# number of steps to see full data
batch_size <- 20

train_steps <- ceiling(train_size / batch_size)
val_steps <- ceiling(val_size / batch_size)

# test: equal
#--------------------

# sample generator
train_gen <- series_generator(
  data = data,
  x = x,
  y = y,
  lookback = lookback,
  timesteps = timesteps,
  start_index = train_start,
  end_index = train_end,
  batch_size = batch_size,
  return_target = TRUE
)

# get the first iteration
sample_list <- train_gen()

# get the x and y arrays
x_sample <- sample_list[[1]]
y_sample <- sample_list[[2]]

# test the output dimension
test_that("output's dimension is equal to specified parameter", {
  
  # test x and y dimension
  expect_equal(dim(x_sample), c(batch_size, timesteps, length(x)))
  expect_equal(dim(y_sample), c(batch_size, length(y)))
  
})

# test if identic
test_that("output is identical to original data", {
  
  # test if x array identical to original data
  expect_equal(x_sample[1, , 1], data[22:31, "x"])
  expect_equal(x_sample[2, , 1], data[23:32, "x"])
  expect_equal(x_sample[1, , 2], data[22:31, "y"])
  expect_equal(x_sample[2, , 2], data[23:32, "y"])
  
  # test if y array identical to original data
  expect_equal(y_sample[1, 1], data[41, "y"])
  expect_equal(y_sample[2, 1], data[42, "y"])
  expect_equal(y_sample[1, 2], data[41, "z"])
  expect_equal(y_sample[2, 2], data[42, "z"])
  
})

# test: passing to keras
#--------------------

# import libs
library(keras)

# sample generators
train_gen <- series_generator(
  data = data,
  x = x,
  y = y,
  lookback = lookback,
  timesteps = timesteps,
  start_index = train_start,
  end_index = train_end,
  batch_size = batch_size,
  return_target = TRUE
)

val_gen <- series_generator(
  data = data,
  x = x,
  y = y,
  lookback = lookback,
  timesteps = timesteps,
  start_index = val_start,
  end_index = val_end,
  batch_size = batch_size,
  return_target = TRUE
)

val_pred_gen <- series_generator(
  data = data,
  x = x,
  y = y,
  lookback = lookback,
  timesteps = timesteps,
  start_index = val_start,
  end_index = val_end,
  batch_size = batch_size,
  return_target = FALSE
)


# sample model
model <- keras_model_sequential() %>%
  layer_lstm(units = 8, input_shape = list(timesteps, length(x))) %>%
  layer_dense(units = length(y), activation = "linear")

model %>% compile(
  optimizer = "rmsprop",
  loss = "mse"
)

# test if could be used
test_that("generators could be used for fit, evaluate, and predict", {

    # test on fit
    expect_silent(
      model %>% fit_generator(
        generator = train_gen,
        steps_per_epoch = train_steps,
        validation_data = val_gen,
        validation_steps = val_steps,
        epochs = 5
      )
    )
  
    # test on evaluate
    expect_silent(
      model %>% evaluate_generator(
        generator = val_gen,
        steps = val_steps
      )
    )
  
    # test on predict
    expect_silent(
      model %>% predict_generator(
        generator = val_pred_gen,
        steps = val_steps
      )
    )
    
  }
  
)

# test: forecast generators
#--------------------

# forecast settings
horizon <- val_size
fcast_steps <- ceiling(horizon / batch_size)

# forecast to validation sample
fcast_gen <- forecast_generator(
  data = data,
  x = x,
  lookback = lookback,
  timesteps = timesteps,
  last_index = train_end,
  horizon = horizon,
  batch_size = batch_size
)

fcast <- model %>% predict_generator(
  generator = fcast_gen,
  steps = fcast_steps
)

# predict on validation
val_pred_gen <- series_generator(
  data = data,
  x = x,
  y = y,
  lookback = lookback,
  timesteps = timesteps,
  start_index = val_start,
  end_index = val_end,
  batch_size = batch_size,
  return_target = FALSE
)

val_pred <- model %>% predict_generator(
  generator = val_pred_gen,
  steps = val_steps
)

# test forecast output
test_that("output from forecast equivalent to prediction", {

    # test if output is identical to reference
    expect_equal(dim(fcast), dim(val_pred))
    expect_equal(fcast, val_pred)
    
  }
  
)
