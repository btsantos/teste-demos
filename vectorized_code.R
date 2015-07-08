########## not vectorized; abs_loop uses a for loop to manipulate each element
abs_loop <- function(vec){
  for (i in 1:length(vec)) {
    if (vec[i] < 0) {
      vec[i] <- -vec[i]
    }
  }
  vec
}

##########  abs_set, is a vectorized version of abs_loop
abs_sets <- function(vec){
  negs <- vec < 0
  vec[negs] <- vec[negs] * -1
  vec
}

## To compare abs_loop and abs_set, first make a long vector of positive and negative
## numbers. long will contain 10 million values:
long <- rep(c(-1, 1), 5000000)

## use system.time to measure how much time takes each function to evaluate
## Don’t confuse system.time with Sys.time, which returns the current time.
system.time(abs_loop(long))
system.time(abs_sets(long))

#####other example ##################################
# not vectorized
change_symbols <- function(vec){
  for (i in 1:length(vec)){
    if (vec[i] == "DD") {
      vec[i] <- "joker"
    } else if (vec[i] == "C") {
      vec[i] <- "ace"
    } else if (vec[i] == "7") {
      vec[i] <- "king"
    }else if (vec[i] == "B") {
      vec[i] <- "queen"
    } else if (vec[i] == "BB") {
      vec[i] <- "jack"
    } else if (vec[i] == "BBB") {
      vec[i] <- "ten"
    } else {
      vec[i] <- "nine"
    }
  }
  vec
}
#vectorized
change_vec <- function (vec) {
  vec[vec == "DD"] <- "joker"
  vec[vec == "C"] <- "ace"
  vec[vec == "7"] <- "king"
  vec[vec == "B"] <- "queen"
  vec[vec == "BB"] <- "jack"
  vec[vec == "BBB"] <- "ten"
  vec[vec == "0"] <- "nine"
  vec
}
#even better, use a lookup table
change_vec2 <- function(vec){
  tb <- c("DD" = "joker", "C" = "ace", "7" = "king", "B" = "queen",
          "BB" = "jack", "BBB" = "ten", "0" = "nine")
  unname(tb[vec])
}

vec <- c("DD", "C", "7", "B", "BB", "BBB", "0")
change_vec2(vec)

many <- rep(vec, 1000000)
system.time(change_symbols(many))
system.time(change_vec(many))
system.time(change_vec2(many))
