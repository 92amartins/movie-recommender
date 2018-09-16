library(testthat)

source("non_personalized/non_personalized.R")

test_that("Forrest Gump's mean rating is 2.7", {
  expect_identical(movie_mean_rating(356), 2.7)
})

test_that("Forrest Gump's rating count is 10", {
  expect_identical(movie_rating_count(356), as.integer(10))
})

test_that("Forrest Gump's % of 4+ is 0.3", {
  expect_identical(movie_above_four_percentage(356), 0.3)
})

test_that("Forrest Gump's association with Toy Story equals to 0.412", {
  expect_identical(movies_association(1, 356), 0.412)
})

test_that("Forrest Gump's ratings's correlation with Toy Story equals to 0.523", {
  expect_identical(movies_correlation(356, 1), 0.523)
})

test_that("Forrest Gump's female mean rating is 3", {
  expect_identical(mean_rating_by_gender(356, 1), 3)
})

test_that("Forrest Gump's male mean rating is 2.25", {
  expect_identical(mean_rating_by_gender(356, 0), 2.25)
})

test_that("Forrest Gump's mean rating difference between genders is 0.75", {
  expect_identical(abs(mean_rating_by_gender(356, 0) - mean_rating_by_gender(356, 1)), 0.75)
})

test_that("percentage of female that rate Forrest Gump equal or above 4 is 0.5", {
  expect_identical(percentage_above_four_by_gender(356, 1), 0.5)
})

test_that("percentage of male that rate Forrest Gump equal or above 4 is 0", {
  expect_identical(percentage_above_four_by_gender(356, 0), 0)
})
