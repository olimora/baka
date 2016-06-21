library("Metrics")

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

#build insert neur stats
build_insert_stats.neur <- function(e, stats.neur, time) {
  ret <- sprintf("INSERT INTO t_experiment (cas_behu, in_gho, in_teplota, in_vietor, in_oblacnost,
                    in_vlhkost, in_tlak, in_azim, in_zen, in_elev, in_dlzkadna, den_hod, fve, 
                    tren_mnoz, tren_mnoz_velkost, tren_mnoz_select, tren_mnoz_opis, 
                    neural, neural_layers, neural_threshold, neural_algorithm, neural_startweights,
                    N, MBE, RMBE, RMSE, RRMSE, MAE, RMAE, MPE, MAXAE, SD)
                    VALUES ('%s', %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, '%s', '%s', 
                           '%s', %d, '%s', '%s', %s, '%s', %f, '%s', '%s',
                           %d, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');",
                           time, e$gho, e$teplota, e$vietor, e$oblacnost, e$vlhkost, e$tlak,
                           e$azim, e$zen, e$elev, e$dlzkadna, e$den_hod, e$fve[fve],
                           e$tren_mnoz, e$tren_mnoz_velkost, gsub("'", '"', e$tren_mnoz_select), e$tren_mnoz_opis,
                           e$neural, e$neural_layers, e$neural_threshold, e$neural_algorithm, 
                           as.character(e$neural_startweights),
                           stats.neur$N, stats.neur$MBE, stats.neur$RMBE, stats.neur$RMSE, stats.neur$RRMSE,
                           stats.neur$MAE, stats.neur$RMAE, stats.neur$MPE, stats.neur$MAXAE, stats.neur$SD)
  return(ret)
}

build_insert_stats.forest <- function(e, stats.forest, time) {
  ret <- sprintf("INSERT INTO t_experiment (cas_behu, in_gho, in_teplota, in_vietor, in_oblacnost,
                    in_vlhkost, in_tlak, in_azim, in_zen, in_elev, in_dlzkadna, den_hod, fve, 
                           tren_mnoz, tren_mnoz_velkost, tren_mnoz_select, tren_mnoz_opis, 
                           forest, forest_ntree, forest_mtry,
                           N, MBE, RMBE, RMSE, RRMSE, MAE, RMAE, MPE, MAXAE, SD)
                           VALUES ('%s', %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, '%s', '%s', 
                           '%s', %d, '%s', '%s', %s, %d, %d,
                           %d, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');",
                           time, e$gho, e$teplota, e$vietor, e$oblacnost, e$vlhkost, e$tlak,
                           e$azim, e$zen, e$elev, e$dlzkadna, e$den_hod, e$fve[fve],
                           e$tren_mnoz, e$tren_mnoz_velkost, gsub("'", '"', e$tren_mnoz_select), e$tren_mnoz_opis,
                           e$forest, e$forest_mtry, e$forest_ntree, 
                           stats.forest$N, stats.forest$MBE, stats.forest$RMBE, stats.forest$RMSE, stats.forest$RRMSE,
                           stats.forest$MAE, stats.forest$RMAE, stats.forest$MPE, stats.forest$MAXAE, stats.forest$SD)
  return(ret)
}

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

library("Metrics")
library("sirad")
# all statistics 
all_statistics <- function(actual, predicted) {
  mdval <- modeval(predicted, actual, stat=c("N","MBE","RMBE","RMSE","RRMSE","MAE","RMAE","MPE","SD"),minlength=4)
  mdval$MAXAE <- max(ae(actual, predicted))
  return(mdval)
}

# operator to compute percentage of part in base
`%p%` <- function(part, base) {
  return(part * 100 / base)
}

# executes command stored in string
unq <- function(com) {
  return(eval(parse(text=com)))
}

# add item into vector
vector.add <- function(vec, item) {
  vec[length(vec)+1] <- item
  return(vec)
}

to.vector <- function (frame, cols) {
  if (cols == 'all') {
    return(frame[1,1:ncol(frame)])
  }
  return(frame[1,cols])
}

frame.getRow <- function(frame, row) {
  return(frame[row,])
}