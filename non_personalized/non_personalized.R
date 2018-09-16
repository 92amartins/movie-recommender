library(broom)
library(dplyr)

ratings <- read.csv("data/HW1-data.csv")

movie_mean_rating <- function(x){
  movieId <- paste0("X", x)
  movie_ratings <- pull(ratings, !!movieId)
  mean(movie_ratings, na.rm = TRUE)
}

movie_rating_count <- function(x){
  movieId <- paste0("X", x)
  movie_ratings <- Filter(function(x) !is.na(x), ratings[, movieId])
  length(movie_ratings)
}

movie_above_four_percentage <- function(x){
  movieId <- paste0("X", x)
  movie_ratings <- Filter(function(x) !is.na(x), ratings[, movieId])
  length(which(movie_ratings >= 4)) / length(movie_ratings)
}

movies_association <- function(x, y){
  x <- paste0("X", x)
  y <- paste0("X", y)
  x_ratings <- ratings[, x]
  y_ratings <- ratings[, y]
  round(length(which(x_ratings & y_ratings) == TRUE) / length(which(!is.na(x_ratings))), 3)
}

movies_correlation <- function(x, y){
  x <- paste0("X", x)
  y <- paste0("X", y)
  m1_ratings <- ratings[, x]
  m2_ratings <- ratings[, y]
  intersection <- intersect(which(!is.na(m1_ratings)), which(!is.na(m2_ratings)))
  round(cor(m1_ratings[intersection], m2_ratings[intersection]), 3)
}

mean_rating_by_gender <- function(x, gender){
  movieId <- paste0("X", x)
  gender_ratings <- filter(ratings, Gender..1..F..0.M. == gender) %>% pull(!!movieId)
  mean(gender_ratings, na.rm = T)
}

percentage_above_four_by_gender <- function(x, gender){
  movieId <- paste0("X", x)
  gender_ratings <- filter(ratings, Gender..1..F..0.M. == gender) %>% pull(!!movieId)
  gender_ratings <- gender_ratings[!is.na(gender_ratings)]
  length(which(gender_ratings >= 4)) / length(gender_ratings)
}
