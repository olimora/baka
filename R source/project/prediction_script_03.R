# script na naplnanie databazy
#vymaze vsetko okrem funkcii
#rm(list = setdiff(ls(), lsf.str()))
options(digits = 6)
library(RPostgreSQL)
library(neuralnet)
library(sirad)
library(plyr)
library(randomForest)
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
  e$fve <- c("FVE Dubravy 1", "FVE Dubravy 2", "FVE Plesivec")[3]
  e.fve <- e$fve
  e$tren_mnoz <- "najpodobnejsich 60"
  e$tren_mnoz_velkost <- 60
  e.tren_mnoz_velkost <- e$tren_mnoz_velkost
  e$tren_mnoz_select <- "select datum, sum(praca) praca, sum(gho) gho, sum(teplota) teplota, sum(vietor) vietor,
  sum(oblacnost) oblacnost, sum(vlhkost) vlhkost, max(dlzkadna) dlzkadna, max(elev) elev
  from v_data where fve_nazov = '%s'group by datum order by datum"
  e.tren_mnoz_select <- e$tren_mnoz_select
  e$tren_mnoz_opis <- "vyber 60 najpodobnejsich dni podla scitaneho gho, teploty, vetra, oblacnosti, vlhkosti, 
  maximalnej dlzky dna a elevacie.
  normalizovane na rozsah(skalu) medzi min a max za vsetky data elektrarne.
  gho * 1, teplota * 1, oblacnost * 1, dlzkadna * 1"
  e$podobnost <- "1*gho, 1*teplota, 1*oblacnost, 1*dlzkadna"
  formula <- unq("praca~gho+teplota+vietor+oblacnost+vlhkost+dlzkadna+elev")
  e$neural <- TRUE
  e$neural_layers <- "c(7,5,3)"
  e.neural_layers <- unq(e$neural_layers)
  e$neural_threshold <- 0.01  #0.01 default
  e.neural_threshold <- e$neural_threshold
  e$neural_algorithm <- c("backprop", "rprop+", "rprop-", "sag", "slr")[2] # default je rprop+
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
  prog.diff <- 0
  
  scale.maxims <- 0
  scale.minims <- 0
  scale.scale <- 0
  
  time.start <- 0
  time.end <- 0
  
  db.drv <- 0
  db.result <- 0
  
  vzorka <- 100
  
}

#na meranie casu
time.start <- Sys.time()

db.drv <- dbDriver("PostgreSQL")
if (exists("db.con")) dbDisconnect(db.con)
# db.con <- getConnection(db.drv)
# prog.baseAll <- dbGetQuery(db.con, "SELECT count(*) FROM
#                            (SELECT DISTINCT datum, fve FROM t_produkcia WHERE fve = 3) s")
# prog.baseAll <- prog.baseAll$count
# dbDisconnect(db.con)

ntree_v <- c(100, 300, 500)
mtry_v <- c(2:7)
tren_velk_v <- c(30, 60, 90, 120)

prog.baseAll <- length(ntree_v) * length(mtry_v) * length(tren_velk_v) * vzorka

for (ntree_i in ntree_v) {
  e$forest_ntree <- ntree_i 
  e.forest_ntree <- ntree_i
  
  for (mtry_i in mtry_v) {
    e$forest_mtry <- mtry_i 
    e.forest_mtry <- mtry_i
    
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
        prog.printed <- -10
        
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
          
          # vypocitat podobnost
          t.differ <- c()
          for (j in 1:nrow(t.train_set)) { #nrow(t.train_set)
            t.differ[j] <- (
              (abs(t.chosen_one['gho'] - t.train_set[j,'gho']) * 100 / scale.scale[1]) 
              +
                (abs(t.chosen_one['teplota'] - t.train_set[j,'teplota']) * 100 / scale.scale[2])
              +
                (abs(t.chosen_one['oblacnost'] - t.train_set[j,'oblacnost']) * 100 / scale.scale[4]) 
              +
                (abs(t.chosen_one['dlzkadna'] - t.train_set[j,'dlzkadna']) * 100 / scale.scale[6])
            )
          }
          
          t.train_set['diff'] <- unlist(t.differ)
          #t.train_set <- t.train_set[c(t.train_set[,6] < 3),]
          
          t.ordered <- arrange(t.train_set, t.train_set[,'diff'])
          t.train_set <- t.ordered[1:e.tren_mnoz_velkost,]
          t.actual <- vector.add(t.actual, t.chosen_one[1,'praca'])
          
          f.forest <- randomForest(formula, data=t.train_set, ntree = e.forest_ntree, mtry = e.forest_mtry,
                                   importance=TRUE, proximity=TRUE)
          f.output <-  vector.add(f.output, predict(f.forest, t.chosen_one[1,3:ncol(t.chosen_one)], type="response", norm.votes=TRUE))
          #plot(f.forest, type="l")
          
          prog.i <- prog.i + 1
          #prog.print_perc <- (i * 100 / prog.basePart)
          prog.print_perc_all <- (prog.i * 100 / prog.baseAll)
          if (prog.print_perc_all >= prog.printed + prog.diff) {
            prog.actual_time <- as.numeric(difftime(Sys.time(), time.start, units = "sec"))
            prog.estimated_time <- prog.actual_time * 100 / prog.print_perc_all
            print(sprintf("Forest: %03.2f%s all, p: %00000.d/%00000.d, Estimated time: %s, Actual: %s",
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
        f.stats <- all_statistics(t.actual, f.output)
        db.result <- dbGetQuery(db.con, build_insert_stats.forest(e, f.stats, time.start, fve))
        dbDisconnect(db.con)
      }
      
    }
  }
}




time.end <- Sys.time()
print(paste("Celkovy cas =", format.time(difftime(time.end, time.start, units = "sec"))))

dbDisconnect(db.con)
dbUnloadDriver(db.drv)
#rm(list = setdiff(ls(), lsf.str()))