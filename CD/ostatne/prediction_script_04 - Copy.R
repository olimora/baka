# script na naplnanie databazy
#vymaze vsetko okrem funkcii
#rm(list = setdiff(ls(), lsf.str()))
options(digits = 6)
library(RPostgreSQL)
library(neuralnet)
library(sirad)
library(plyr)
library(randomForest)
library(nnet)
source('~/GitHub/baka/R source/project/functions.R')

# nastavenia a premenne
{
  e <- list()
  e$gho <- TRUE
  e$teplota <- TRUE
  e$vietor <- TRUE
  e$oblacnost <- TRUE
  e$vlhkost <- TRUE
  e$tlak <- F
  e$azim <- F
  e$zen <- F
  e$elev <- TRUE
  e$dlzkadna <- TRUE
  e$den_hod <- c("den", "hod")[1]
  e$fve <- c("***", "***", "***")
  e.fve <- e$fve
  e$tren_mnoz <- "najpodobnejsich 60"
  e$tren_mnoz_velkost <- 60
  e.tren_mnoz_velkost <- e$tren_mnoz_velkost
  e$tren_mnoz_select <- "select datum, sum(praca) praca, sum(gho) gho, sum(teplota) teplota, sum(vietor) vietor,
  sum(oblacnost) oblacnost, sum(vlhkost) vlhkost, max(dlzkadna) dlzkadna, max(elev) elev
  from v_data where fve_nazov = '%s'group by datum order by datum"
  e.tren_mnoz_select <- e$tren_mnoz_select
  e$tren_mnoz_opis <- "vyber %d najpodobnejsich dni podla scitaneho gho, teploty, vetra, oblacnosti, vlhkosti, 
  maximalnej dlzky dna a elevacie.
  normalizovane na rozsah(skalu) medzi min a max za vsetky data elektrarne.
  gho * %d, teplota * %d, vietor *%d, oblacnost * %d, vlhkost %d, dlzkadna * %d, elev * %d"
  e$podobnost <- "%d*gho, %d*teplota, %d*vietor, %d*oblacnost, %d*vlhkost, %d*dlzkadna, %d*elev"
  formula <- unq("praca~gho+teplota+vietor+oblacnost+vlhkost+dlzkadna+elev")
  e$neural <- TRUE
  e$neural_layers <- "c(7,5,3)"
  e.neural_layers <- unq(e$neural_layers)
  e$neural_threshold <- 0.01  #0.01 default
  e.neural_threshold <- e$neural_threshold
  e$neural_algorithm <- c("backprop", "rprop+", "rprop-", "sag", "slr") # default je rprop+
  e.neural_algorithm <- e$neural_algorithm
  e$neural_startweights <- "rep(1, n.pocet_vah)"
  e.neural_startweights <- unq(e$neural_startweights)
  e$forest <- TRUE
  e$forest_ntree <- 500 # 0 ak default
  e.forest_ntree <- e$forest_ntree
  e$forest_mtry <- 7 # 0 ak default
  e.forest_mtry <- e$forest_mtry
  
  n.input_v <- c(e$gho, e$teplota, e$vietor, e$oblacnost, e$vlhkost, e$tlak, e$azim, e$zen, e$elev, e$dlzkadna)
  n.pocet_vah <- compute_weigths_num(length(subset(n.input_v, n.input_v == TRUE)), e.neural_layers)
  n.net <- 0
  n.output <- 0
  n.stats <- 0
  
  f.forest <- 0
  f.output <- 0
  f.stats <- 0
  
  t.all_days <- 0
  t.all_days_count <- 0
  t.chosen_one <- 0
  t.not_i <- 0
  t.differ <- 0
  t.train_set <- 0
  t.ordered <- 0
  t.actual <- 0
  
  prog.printed <- -10
  prog.print_perc <- 0
  prog.print_perc_all <- 0
  prog.baseAll <- 0
  prog.basePart <- 0
  prog.i <- 0
  prog.actual_time <- 0
  prog.estimated_time <- 0
  prog.diff <- 1
  
  scale.maxims <- 0
  scale.minims <- 0
  scale.scale <- 0
  
  time.start <- 0
  time.end <- 0
  
  db.drv <- 0
  db.result <- 0
  

  
}



db.drv <- dbDriver("PostgreSQL")
if (exists("db.con")) dbDisconnect(db.con)
# db.con <- getConnection(db.drv)
# prog.baseAll <- dbGetQuery(db.con, "SELECT count(*) FROM
#                            (SELECT DISTINCT datum, fve FROM t_produkcia WHERE fve = 3) s")
# prog.baseAll <- prog.baseAll$count
# dbDisconnect(db.con)

# ll <- list()
# ll$a <- "c(7)"
# ll$b <- "c(14)"
# ll$d <- "c(7,3)"
# ll$e <- "c(7,5)"
# ll$f <- "c(7,7)"
# ll$j <- "c(14,7)"
# ll$m <- "c(21,7)"
# ll$p <- "c(7,5,3)"
# ll$r <- "c(7,7,7)"

diff_gho <- 90
diff_tep <- 10
diff_viet <- 1
diff_obl <- 0
diff_vlh <- 0
diff_dlz <- 10
diff_elev <- 0
vzorka <- 100


e$fve <- e$fve[2]
e.fve <- e$fve
#e$tren_mnoz <- sprintf(e$tren_mnoz, )
e$tren_mnoz_opis <- sprintf(e$tren_mnoz_opis, e.tren_mnoz_velkost, diff_gho, diff_tep, diff_viet, diff_obl, diff_vlh, diff_dlz, diff_elev)
e$podobnost <- sprintf(e$podobnost, diff_gho, diff_tep, diff_viet, diff_obl, diff_vlh, diff_dlz, diff_elev)


sizee <- c("3", "5", "7", "10", "14", "18", "21")
tren_velk_v <- c(30, 60, 90, 120)
# th_v <- c(0.01)
#e.neural_algorithm <- e.neural_algorithm[2:5]

prog.baseAll <- length(sizee) * length(tren_velk_v) * vzorka

#na meranie casu
time.start <- Sys.time()

for (layers_i in 1:length(sizee)) {
  e$neural_layers <- sizee[layers_i]
  e.neural_layers <- unq(e$neural_layers)
  
  # n.input_v <- c(e$gho, e$teplota, e$vietor, e$oblacnost, e$vlhkost, e$tlak, e$azim, e$zen, e$elev, e$dlzkadna)
  # n.pocet_vah <- compute_weigths_num(length(subset(n.input_v, n.input_v == TRUE)), e.neural_layers)
  
  for (tren_velk_i in tren_velk_v) {
    e$tren_mnoz <- sprintf("najpodobnejsich %d", tren_velk_i)
    e$tren_mnoz_velkost <- tren_velk_i
    e.tren_mnoz_velkost <- tren_velk_i
      
    for (fve in 1:length(e.fve)) { #length(e.fve)
      if (exists("db.con")) dbDisconnect(db.con)
      db.con <- getConnection(db.drv)
      t.all_days <- dbGetQuery(db.con, sprintf(e.tren_mnoz_select, e.fve[fve]))
      dbDisconnect(db.con)
      
      scale.maxims <- c(max(t.all_days[,3]), max(t.all_days[,4]), max(t.all_days[,5]), 
                        max(t.all_days[,6]), max(t.all_days[,7]), max(t.all_days[,8]), max(t.all_days[,9]))
      scale.minims <- c(min(t.all_days[,3]), min(t.all_days[,4]), min(t.all_days[,5]), 
                        min(t.all_days[,6]), min(t.all_days[,7]), min(t.all_days[,8]), min(t.all_days[,9]))
      scale.scale <- abs(scale.maxims - scale.minims)
      
      all_days_zaloha <- t.all_days
      
      rand_vec <- c()
      while (length(rand_vec) < vzorka) {
        rand <- round(runif(1, 1, nrow(t.all_days)), 0)
        if (!rand %in% rand_vec) {
          rand_vec <- vector.add(rand_vec, rand)
        }
      }
      t.all_days <- t.all_days[rand_vec,]
      
      t.all_days_count <- nrow(t.all_days)
      prog.basePart <- t.all_days_count
      
      t.actual <- c()
      n.output <- c()
      f.output <- c()
      
      for (i in 1:t.all_days_count) { #t.all_days_count
        # pre kazdy den vybrat trenovaciu mnozinu
        # vsetky okrem toho pre ktory idem predikovat
        #i = 1
        t.chosen_one <- t.all_days[i,]
        t.not_i <- c(t.chosen_one[1,'datum'] != all_days_zaloha[,'datum'])
        t.train_set <- all_days_zaloha[t.not_i,] 
        
        # vypocitat rozdielnost
        t.differ <- c()
        for (j in 1:nrow(t.train_set)) { #nrow(t.train_set)
          t.differ[j] <- (
            (abs(t.chosen_one['gho'] - t.train_set[j,'gho']) * 100 / scale.scale[1]) * diff_gho
            +
              (abs(t.chosen_one['teplota'] - t.train_set[j,'teplota']) * 100 / scale.scale[2]) * diff_tep
            +
              (abs(t.chosen_one['vietor'] - t.train_set[j,'vietor']) * 100 / scale.scale[3]) *diff_viet
            # +
            #   (abs(t.chosen_one['oblacnost'] - t.train_set[j,'oblacnost']) * 100 / scale.scale[4]) * diff_obl
            # +
            #   (abs(t.chosen_one['vlhkost'] - t.train_set[j,'vlhkost']) * 100 / scale.scale[5]) * diff_vlh
            +
              (abs(t.chosen_one['dlzkadna'] - t.train_set[j,'dlzkadna']) * 100 / scale.scale[6]) * diff_dlz
            # +
            #   (abs(t.chosen_one['elev'] - t.train_set[j,'elev']) * 100 / scale.scale[7]) * diff_elev
          )
        }
        
        t.train_set['diff'] <- unlist(t.differ)
        #t.train_set <- t.train_set[c(t.train_set[,6] < 3),]
        
        t.ordered <- arrange(t.train_set, t.train_set[,'diff'])
        t.train_set <- t.ordered[1:e.tren_mnoz_velkost,]
        t.actual <- vector.add(t.actual, t.chosen_one[1,'praca'])
        
        capture.output(file = 'NUL', n.net <- nnet(formula, t.train_set, size= e.neural_layers, linout = T))
        n.output <- vector.add(n.output, predict(n.net, t.chosen_one[1,3:ncol(t.chosen_one)], type = "raw"))
        
        # # do db tabulky pocet hidden layer a trueshold
        # n.net <- neuralnet(formula, t.train_set, startweights = e.neural_startweights,
        #                    hidden=e.neural_layers, threshold=e.neural_threshold, algorithm=neur_alg_i)
        # n.output <- vector.add(n.output, compute(n.net, t.chosen_one[1,3:ncol(t.chosen_one)])$net.result)
        # #plot(n.net)
        
        prog.i <- prog.i + 1
        #prog.print_perc <- (i * 100 / prog.basePart)
        prog.print_perc_all <- (prog.i * 100 / prog.baseAll)
        if (prog.print_perc_all >= prog.printed + prog.diff) {
          prog.actual_time <- as.numeric(difftime(Sys.time(), time.start, units = "sec"))
          prog.estimated_time <- prog.actual_time * 100 / prog.print_perc_all
          print(sprintf("Neural: %6.2f%s all, p: %5.d/%d, Estimated time: %s, Actual: %s",
                        prog.print_perc_all, "%", prog.i, prog.baseAll,
                        format.time(prog.estimated_time),
                        format.time(prog.actual_time)),
                quote=F)
          prog.printed <- prog.print_perc_all
        }
      }
      
      # statistika presnosti
      if (exists("db.con")) dbDisconnect(db.con)
      db.con <- getConnection(db.drv)
      n.stats <- all_statistics(t.actual, n.output)
      db.result <- dbGetQuery(db.con, build_insert_stats.neur(e, n.stats, time.start, fve))
      dbDisconnect(db.con)
    }
  }
}


time.end <- Sys.time()
print(paste("Celkovy cas =", format.time(difftime(time.end, time.start, units = "sec"))))

dbDisconnect(db.con)
dbUnloadDriver(db.drv)
#rm(list = setdiff(ls(), lsf.str()))