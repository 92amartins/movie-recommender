library(broom)
library(dplyr)

ratings <- read.csv("data/HW1-data.csv")
tidy_ratings <- tidy(ratings)

movie_mean_rating <- function(movieId){
  (tidy_ratings %>%
    filter(grepl(movieId, column)))$mean
}
