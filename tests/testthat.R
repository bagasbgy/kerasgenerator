# prepare environment
#--------------------

# clear environment
rm(list = ls())

# load libs
library(testthat)
library(keras)
library(kerasgenerator)

# perform test
#--------------------

# launch all test
test_check("kerasgenerator")
