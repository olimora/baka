library(compiler)
library(Metrics)
library(sirad)


# spajanie stringov
`%s%` <- function(s1, s2) paste0(s1, s2)
`%s%` <- cmpfun(`%s%`)

#to compute numbers of weights
compute_weigths_num <- function(input, layers) {
  ret <- 0
  mul <- input
  for (i in 1:length(layers)) {
    ret <- ret + mul*layers[i] + layers[i]
    mul <- layers[i]
  }
  ret <- ret + layers[length(layers)] + 1
  return(ret)
}
compute_weigths_num <- cmpfun(compute_weigths_num)

#build insert stats
build_insert_stats <- function(e, stats, ttime, fve) {
  for (name in names(stats)) {
    if (is.infinite(stats[[name]]) | !is.numeric(stats[[name]]) | is.nan(stats[[name]])) stats[[name]] <- 999.999
  }
  ret <- sprintf("INSERT INTO t_experiment (cas_behu, metoda, param1, param2, param3, param4, param5,
                 N, MBE, RMBE, RMSE, RRMSE, MAE, RMAE, MPE, MAXAE, SD,
                 tm_velkost, tm_opis, tm_select, fve, den_hod,
                 pod_gho, pod_oblacnost, pod_teplota, pod_vietor, pod_vlhkost, pod_tlak, pod_dlzkadna, pod_azim, pod_elev,
                 in_gho, in_oblacnost, in_teplota, in_vietor, in_vlhkost, in_tlak, in_dlzkadna, in_azim, in_elev)
                 VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s',
                 %d, %f, %f, %f, %f, %f, %f, %f, %f, %f,
                 %d, '%s', '%s', '%s', '%s',
                 %f, %f, %f, %f, %f, %f, %f, %f, %f,
                 %s, %s, %s, %s, %s, %s, %s, %s, %s);",
                 ttime, e$metoda, e$param1, e$param2, e$param3, e$param4, e$param5,
                 stats$N, stats$MBE, stats$RMBE, stats$RMSE, stats$RRMSE, stats$MAE, stats$RMAE, stats$MPE, stats$MAXAE, stats$SD,
                 e$tm_velkost, e$tm_opis, e$tm_select, e$fve[fve], e$den_hod,
                 e$pod_gho, e$pod_obl, e$pod_tep, e$pod_vie, e$pod_vlh, e$pod_tla, e$pod_dlz, e$pod_azi, e$pod_ele,
                 e$in_gho, e$in_obl, e$in_tep, e$in_vie, e$in_vlh, e$in_tla, e$in_dlz, e$in_azi, e$in_ele)
  return(ret)
}
build_insert_stats <- cmpfun(build_insert_stats)



# format casu
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

# vrati connection na databazu
getConnection <- function(drv) {
  con <- dbConnect(drv, dbname = "test_db", host = "localhost", 
                   port = 5432, user = "postgres", password = "password")
  return(con)
}
getConnection <- cmpfun(getConnection)

getConnection2 <- function(drv) {
  con <- dbConnect(drv, dbname = "test2_db", host = "localhost", 
                   port = 5432, user = "postgres", password = "password")
  return(con)
}
getConnection2 <- cmpfun(getConnection)2


# all statistics 
all_statistics <- function(actual, predicted) {
  mdval <- modeval(predicted, actual, stat=c("N","MBE","RMBE","RMSE","RRMSE","MAE","RMAE","MPE","SD"),minlength=2)
  mdval$MAXAE <- max(ae(actual, predicted))
  return(mdval)
}

# executes command stored in string
unq <- function(com) {
  return(eval(parse(text=com)))
}
unq <- cmpfun(unq)

# add item into vector
vector.add <- function(vec, item) {
  vec[length(vec)+1] <- item
  return(vec)
}
vector.add <- cmpfun(vector.add)
