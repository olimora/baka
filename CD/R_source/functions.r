library(compiler)

# vrati spojenie s databazou
getConnection <- function(drv) {
  con <- dbConnect(drv, dbname = "test_db", host = "localhost", 
                   port = 5432, user = "postgres", password = "password")
  return(con)
}
getConnection <- cmpfun(getConnection)

# vypocet statistiky presnosti 
library(Metrics)
library(sirad)
all_statistics <- function(actual, predicted) {
  mdval <- modeval(predicted, actual, stat=c("N","MBE","RMBE","RMSE","RRMSE","MAE","RMAE","MPE","SD"),minlength=2)
  mdval$MAXAE <- max(ae(actual, predicted))
  return(mdval)
}

# formatovanie casu
format.time <- function(x) UseMethod("format.time")
format.time.difftime <- function(x) {
  units(x) <- "secs"
  x <- unclass(x)
  NextMethod()
}
format.time.default <- function(x) {
  y <- abs(x)
  sprintf("%s%02dh:%02dm:%02ds", 
          ifelse(x < 0, "-", ""), # sign
          y %% 86400 %/% 3600,  # hours 
          y %% 3600 %/% 60,  # minutes
          y %% 60 %/% 1) # seconds
}

# vlastny operator na spajanie retazcov
`%s%` <- function(s1, s2) paste0(s1, s2)
`%s%` <- cmpfun(`%s%`)
