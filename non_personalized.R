library(broom)
library(dplyr)

ratings <- read.csv("data/HW1-data.csv")

movie_mean_rating <- function(movieId){
  movieId <- paste0("X", movieId)
  movie_ratings <- Filter(function(x) !is.na(x), ratings[, movieId])
  mean(movie_ratings, na.rm = TRUE)
}

movie_rating_count <- function(movieId){
  movieId <- paste0("X", movieId)
  movie_ratings <- Filter(function(x) !is.na(x), ratings[, movieId])
  length(movie_ratings)
}

movie_above_four_percentage <- function(movieId){
  movieId <- paste0("X", movieId)
  movie_ratings <- Filter(function(x) !is.na(x), ratings[, movieId])
  length(which(movie_ratings >= 4)) / length(movie_ratings)
}

movies_correlation <- function(m1, m2){
  m1 <- paste0("X", m1)
  m2 <- paste0("X", m2)
  m1_ratings <- ratings[, m1]
  m2_ratings <- ratings[, m2]
  intersection <- intersect(which(!is.na(m1_ratings)), which(!is.na(m2_ratings)))
  round(cor(m1_ratings[intersection], m2_ratings[intersection]), 3)
}
