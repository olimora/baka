#install.packages("microbenchmark")
library(microbenchmark)
library(compiler)

act <- 3
pred <- c(3,2)

prva <- function(x,y) {
  compute_weigths_num(x, y)
}

druha <- function(x,y) {
  cmp_weigths_num(x, y)
}

compare <- microbenchmark(prva(act,pred), druha(act,pred), times = 1000)
library(ggplot2)
autoplot(compare)