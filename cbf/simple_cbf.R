bow <- read.csv("data/assignment2.csv")
user1 <- c(1,-1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,-1,0)
user2 <- c(-1,1,0,1,0,0,0,0,0,0,0,-1,0,0,0,0,1,0,0,0)
users <- rbind(user1, user2)

get_user_profile <- function(userId){
  as.vector(users[userId,] %*% data.matrix(bow)[,2:11])
}

predict_scores <- function(userId){
  user_profile <- get_user_profile(userId)
  data.matrix(bow)[,2:11] %*% matrix(user_profile, 10, 1)
}
