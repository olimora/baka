install.packages("foreach")
library(microbenchmark)
library(compiler)
library(ggplot2)

act <- 3
pred <- c(3,2)
z <- c(1:1000000)

fff <- function(d) {
  d <- 548 * 3655 / 9413 + 84205 * 88522 / 822
}

prva <- function(x) {
  lapply(x, f)
}

druha <- function(tr) {

}

z <- c(1:1000)
system.time(lapply(z, fff))
cl<-makeCluster(4,type="SOCK")
system.time(clusterApply(cl, z, fff))
stopCluster(cl)

system.time(prva(z))
system.time(druha(z))

compare <- microbenchmark(lapply(z, fff), times = 1000)
cl<-makeCluster(4,type="SOCK")
compare2 <- microbenchmark(clusterApply(cl, z, fff), times = 1000)
stopCluster(cl)

autoplot(compare)

library(snow)

# process in parallel
library(doParallel) 
library(foreach)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

# turn parallel processing off and run sequentially again:
registerDoSEQ()
