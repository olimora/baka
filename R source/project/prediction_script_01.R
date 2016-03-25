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
  e$fve <- c("FVE Dubravy 1", "FVE Dubravy 2", "FVE Plesivec")
  e.fve <- e$fve
  e$tren_mnoz <- "najpodobnejsich 30"
  e$tren_mnoz_velkost <- 30
  e.tren_mnoz_velkost <- e$tren_mnoz_velkost
  e$tren_mnoz_select <- "select datum, sum(praca) praca, sum(gho) gho, sum(teplota) teplota, sum(vietor) vietor,
		                            sum(oblacnost) oblacnost, sum(vlhkost) vlhkost, max(dlzkadna) dlzkadna, max(elev) elev
                          from v_data where fve_nazov = '%s'group by datum order by datum"
  e.tren_mnoz_select <- e$tren_mnoz_select
  e$tren_mnoz_opis <- "vyber 30 najpodobnejsich dni podla scitaneho gho, teploty, vetra, oblacnosti, vlhkosti, 
                        maximalnej dlzky dna a elevacie.
                        normalizovane na rozsah(skalu) medzi min a max za vsetky data elektrarne.
                        gho * 90, teplota * 10, vietor * 1, oblacnost * 100, vlhkost * 0.5, dlzkadna * 10, elev * 10"
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
  e$forest_ntree <- 0 # 0 ak default
  e.forest_ntree <- e$forest_ntree
  e$forest_mtry <- 0 # 0 ak default
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
  prog.diff <- 5
  
  scale.maxims <- 0
  scale.minims <- 0
  scale.scale <- 0
  
  time.start <- 0
  time.end <- 0
  
  db.drv <- 0
  db.con <- 0
  db.result <- 0
  
}

#na meranie casu
time.start <- Sys.time()

db.drv <- dbDriver("PostgreSQL")
if (exists("db.con") & db.con != 0) dbDisconnect(db.con)
db.con <- getConnection(db.drv)
prog.baseAll <- dbGetQuery(db.con, "SELECT count(*) FROM
                           (SELECT DISTINCT datum, fve FROM t_produkcia) s")
dbDisconnect(db.con)

stats_all <- data.frame()
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
  
  t.actual <- c()
  n.output <- c()
  f.output <- c()
  
  t.all_days_count <- nrow(t.all_days)
  prog.basePart <- t.all_days_count
  prog.printed <- -10
  
  for (i in 1:t.all_days_count) { #t.all_days_count
    # pre kazdy den vybrat trenovaciu mnozinu
    # vsetky okrem toho pre ktory idem predikovat
    #i = 1
    t.chosen_one <- t.all_days[i,]
    t.not_i <- c(i != c(1:t.all_days_count))
    t.train_set <- t.all_days[t.not_i,]
    
    # vypocitat podobnost
    t.differ <- c()
    for (j in 1:nrow(t.train_set)) { #nrow(t.train_set)
      t.differ[j] <- (
        90 * (abs(t.chosen_one['gho'] - t.train_set[j,'gho']) * 100 / scale.scale[1]) 
        +
          10 * (abs(t.chosen_one['teplota'] - t.train_set[j,'teplota']) * 100 / scale.scale[2])
        +
          (abs(t.chosen_one['vietor'] - t.train_set[j,'vietor']) * 100 / scale.scale[3])
        +
          100 * (abs(t.chosen_one['oblacnost'] - t.train_set[j,'oblacnost']) * 100 / scale.scale[4]) 
        +
          0.5 * (abs(t.chosen_one['vlhkost'] - t.train_set[j,'vlhkost']) * 100 / scale.scale[5])
        +
          10 * (abs(t.chosen_one['dlzkadna'] - t.train_set[j,'dlzkadna']) * 100 / scale.scale[6])
        +
          10 * (abs(t.chosen_one['elev'] - t.train_set[j,'elev']) * 100 / scale.scale[7])
      )
    }
    
    t.train_set['diff'] <- unlist(t.differ)
    #t.train_set <- t.train_set[c(t.train_set[,6] < 3),]
    #print(nrow(t.train_set))
    
    t.ordered <- arrange(t.train_set, t.train_set[,'diff'])
    t.train_set <- t.ordered[1:e.tren_mnoz_velkost,]
    t.actual <- vector.add(t.actual, t.chosen_one[1,'praca'])
    
    # do db tabulky pocet hidden layer a trueshold
    n.net <- neuralnet(formula, t.train_set, startweights = e.neural_startweights,
                       hidden=e.neural_layers, threshold=e.neural_threshold)
    n.output <- vector.add(n.output, compute(n.net, t.chosen_one[1,3:ncol(t.chosen_one)])$net.result)
    #plot(n.net)
    
    f.forest <- randomForest(formula, data=t.train_set, importance=TRUE, proximity=TRUE)
    f.output <-  vector.add(f.output, predict(f.forest, t.chosen_one[1,3:ncol(t.chosen_one)], type="response", norm.votes=TRUE))
    #plot(f.forest, type="l")
    
    prog.i <- prog.i + 1
    prog.print_perc <- (i * 100 / prog.basePart)
    if (prog.print_perc >= prog.printed + prog.diff) {
      prog.print_perc_all <- (prog.i * 100 / prog.baseAll)
      prog.actual_time <- as.numeric(difftime(Sys.time(), time.start, units = "sec"))
      prog.estimated_time <- prog.actual_time * 100 / prog.print_perc_all$count
      print(sprintf("Progress: %03.f%s all, FVE(%d): %03.f%s, Estimated time: %s, Actual: %s",
                    prog.print_perc_all, "%", fve, prog.print_perc, "%",
                    format.time(prog.estimated_time),
                    format.time(prog.actual_time)),
            quote=F)
      prog.printed <- prog.print_perc
    }
  }
  
  # statistika presnosti
  if (exists("db.con")) dbDisconnect(db.con)
  db.con <- getConnection(db.drv)
  
  n.stats <- all_statistics(t.actual, n.output)
  db.result <- dbGetQuery(db.con, build_insert_stats.neur(e, n.stats, time.start, fve))
  
  f.stats <- all_statistics(t.actual, f.output)
  db.result <- dbGetQuery(db.con, build_insert_stats.forest(e, f.stats, time.start, fve))
  
  dbDisconnect(db.con)
}

time.end <- Sys.time()
print(paste("Celkovy cas =", format.time(difftime(time.end, time.start, units = "sec"))))

dbDisconnect(db.con)
dbUnloadDriver(db.drv)
#rm(list = setdiff(ls(), lsf.str()))