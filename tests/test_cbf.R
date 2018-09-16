library(testthat)

source("cbf/simple_cbf.R")

# User Profiles
test_that("User1 profile is right", {
  user1 = c(3,-2,-1,0,0,2,-1,-1,1,0)
  expect_identical(get_user_profile(1), user1)
})

test_that("User2 profile is right", {
  user2 = c(-2,2,2,3,-1,-2,0,3,0,-1)
  expect_identical(get_user_profile(2), user2)
})

test_that("Best pick for User1 is 16", {
  expect_equal(which.max(predict_scores(1)), 16)
})

test_that("Highest predicted score for User1 is 6", {
  expect_equal(max(predict_scores(1)), 6)
})

test_that("The model predicts 4 negative scores for User2", {
  expect_equal(length(which(predict_scores(2) < 0)), 4)
})
