install.packages("microbenchmark")

# s listami a vlastny operator
prva <- function(t, scale) {
  t$differ <- c()
  for (j in 1:nrow(t$train_set)) { #nrow(t$train_set)
    t$differ[j] <- (
      90 * (abs(t$chosen_one['gho'] - t$train_set[j,'gho']) %p% scale$scale[1]) 
      +
        10 * (abs(t$chosen_one['teplota'] - t$train_set[j,'teplota']) %p% scale$scale[2])
      +
        (abs(t$chosen_one['vietor'] - t$train_set[j,'vietor']) %p% scale$scale[3])
    )
  }
}

# list bez operatora
druha <- function(t, scale) {
  t$differ <- c()
  for (j in 1:nrow(t$train_set)) { #nrow(t$train_set)
    t$differ[j] <- (
      90 * (abs(t$chosen_one['gho'] - t$train_set[j,'gho']) %p% scale$scale[1]) 
      +
        10 * (abs(t$chosen_one['teplota'] - t$train_set[j,'teplota']) %p% scale$scale[2])
      +
        (abs(t$chosen_one['vietor'] - t$train_set[j,'vietor']) %p% scale$scale[3])
    )
  }
}


t$differ <- c()
for (j in 1:nrow(t$train_set)) { #nrow(t$train_set)
  t$differ[j] <- (
    90 * (abs(t$chosen_one['gho'] - t$train_set[j,'gho']) %p% scale$scale[1]) 
    +
      10 * (abs(t$chosen_one['teplota'] - t$train_set[j,'teplota']) %p% scale$scale[2])
    +
      (abs(t$chosen_one['vietor'] - t$train_set[j,'vietor']) %p% scale$scale[3])
  )
}