# 2048
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
# Recently, on a plane ride to a friend's wedding, I re-discovered
# the game 2048 on the in-flight entertainment. It's a pretty simple
# game; the goal is just to sum tiles to 2048 by shifting them in
# the cardinal directions, with one new value added randomly every
# turn. If you run out of free tiles, you lose!
#
# I figured I could code this pretty quickly to run in the console.
# So here's that! I'm not sure if it follows the exact rules of the
# original 2048, but I think it's close enough for an afternoon
# project. Runs on base R 4.6.1.
#
# To play, just run this file.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Build a function to add identical values in vectors
add_id <- function(vec1, vec2) {
    for (i in 1:4) {
      if (vec1[i] == vec2[i]) {
        vec1[i] <- (vec1[i] + vec2[i])
        vec2[i] <- 0
      } else if (vec1[i] == 0) {
        vec1[i] <- (vec1[i] + vec2[i])
        vec2[i] <- 0
      } else {
        vec1[i] <- vec1[i]
        vec2[i] <- vec2[i]
      }
    }
    return(list(vec1 = vec1, vec2 = vec2))
  }  

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Shift tiles, agnostic of direction
shift_board <- function(vec_list) {
  for (i in 1:3) {
    h <- add_id(vec_list[[i]], vec_list[[i+1]])
    vec_list[[i]] <- h$vec1
    vec_list[[i+1]] <- h$vec2
  }
  return(vec_list)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Accept user input to determine the direction
# in which the board shifts.
user_direction <- function(board, direction) {
  if (direction == "up") {
    vec1 <- board[1,]
    vec2 <- board[2,]
    vec3 <- board[3,]
    vec4 <- board[4,]
  }
  if (direction == "down") {
    vec1 <- board[4,]
    vec2 <- board[3,]
    vec3 <- board[2,]
    vec4 <- board[1,]
  }
  if (direction == "left") {
    vec1 <- board[,1]
    vec2 <- board[,2]
    vec3 <- board[,3]
    vec4 <- board[,4]
  }
  if (direction == "right") {
    vec1 <- board[,4]
    vec2 <- board[,3]
    vec3 <- board[,2]
    vec4 <- board[,1]
  }
  vec_list <- list(vec1, vec2, vec3, vec4)
  return(vec_list)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

reassemble_board <- function(vec_list, direction) {
  col1 <- c(0, 0, 0, 0)
  col2 <- c(0, 0, 0, 0)
  col3 <- c(0, 0, 0, 0)
  col4 <- c(0, 0, 0, 0)
  board <- data.frame(col1, col2, col3, col4)
  if (direction == "up") {
    board[1,] <- vec_list[[1]]
    board[2,] <- vec_list[[2]]
    board[3,] <- vec_list[[3]]
    board[4,] <- vec_list[[4]]
  }
  if (direction == "down") {
    board[4,] <- vec_list[[1]]
    board[3,] <- vec_list[[2]]
    board[2,] <- vec_list[[3]]
    board[1,] <- vec_list[[4]]
  }
  if (direction == "left") {
    board[,1] <- vec_list[[1]]
    board[,2] <- vec_list[[2]]
    board[,3] <- vec_list[[3]]
    board[,4] <- vec_list[[4]]
  }
  if (direction == "right") {
    board[,4] <- vec_list[[1]]
    board[,3] <- vec_list[[2]]
    board[,2] <- vec_list[[3]]
    board[,1] <- vec_list[[4]]
  }
  return(board)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Add a random 2 or 4 to the board.
add_rand <- function(board, prob_four = 0.1) {
  zeros <- which(as.matrix(board) == 0, arr.ind = TRUE)
  if (nrow(zeros) == 0L) return(board)
  pick <- zeros[sample.int(nrow(zeros), 1L), ]
  board[pick[["row"]], pick[["col"]]] <- if (runif(1) < prob_four) 4 else 2
  board
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Check for loss conditions.
has_adjacent_pair <- function(board) {
  m <- as.matrix(board)
  any(m[, -1] == m[, -ncol(m)]) || any(m[-1, ] == m[-nrow(m), ])
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# String these all together into a function that runs
# the game in the console.
play_game <- function() {
  
  # First, initialize the board with two random digits.
  col1 <- c(0, 0, 0, 0)
  col2 <- c(0, 0, 0, 0)
  col3 <- c(0, 0, 0, 0)
  col4 <- c(0, 0, 0, 0)
  
  board <- data.frame(col1, col2, col3, col4)
  
  board <- add_rand(board)
  board <- add_rand(board)
  
  direction <- "right"
  
  while (has_adjacent_pair(board) || min(as.matrix(board)) == 0) {
    
    if (max(board) < 2048) {
    
      print(unname(board), row.names = FALSE)
      direction <- readline(prompt = "Pick a direction in which to shift tiles: ")
      
      if (direction %notin% c("left", "right", "up", "down")) {
        print("Error: please provide a valid input from: left, right, up, down.")
        direction <- readline(prompt = "Pick a direction in which to shift tiles: ")
      }
      
      g <- user_direction(board, direction)
      h <- shift_board(g)
      j <- reassemble_board(h, direction)
      k <- add_rand(j)
      
      board <- k
      
      cat("\014")
      
    } else {
      print(board)
      print("You win!")
    }
  }
  print("You lose :(")
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

play_game()
