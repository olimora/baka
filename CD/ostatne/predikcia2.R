# script na naplnanie databazy
#vymaze vsetko okrem funkcii
#rm(list = setdiff(ls(), lsf.str()))
library("RPostgreSQL")
library("neuralnet")
library("sirad")
library('plyr')
library("randomForest")
source('~/GitHub/baka/R source/funkcie.R')

{
  e <- list()
  e$gho <- TRUE
  e$teplota <- TRUE
  e$vietor <- TRUE
  e$oblacnost <- F
  e$vlhkost <- F
  e$tlak <- F
  e$azim <- F
  e$zen <- F
  e$elev <- F
  e$dlzkadna <- F
  e$den_hod <- c("den", "hod")[1]
  e$fve <- c("***", "***", "***")
  e$tren_mnoz <- "najpodobnejsich 20"
  e$tren_mnoz_velkost <- 20
  e$tren_mnoz_select <- "select datum, sum(gho) gho, sum(teplota) teplota,
  sum(vietor) vietor, sum(praca) praca
  from v_data
  where fve_nazov = '%s'
  group by datum
  order by datum"
  e$tren_mnoz_opis <- "vyber 20 najpodobnejsich dni podla scitaneho gho, teploty, vetra.
  normalizovane na rozsah(skalu) medzi min a max za vsetky data elektrarne.
  gho * 90, teplota * 10, vietor * 1."
  e$neural <- TRUE
  e$neural_layers <- "c(3,2)"
  e$neural_threshold <- 0.01  #0.01 default
  e$neural_algorithm <- c("backprop", "rprop+", "rprop-", "sag", "slr")[2] # default je rprop+
  e$neural_startweights <- "rep(1, pocet_vah) # vektor jednotiek"
  e$forest <- TRUE
  e$forest_ntree <- 0 # 0 ak default
  e$forest_mtry <- 0 # 0 ak default
}
input_v <- c(e$gho, e$teplota, e$vietor, e$oblacnost, e$vlhkost, 
             e$tlak, e$azim, e$zen, e$elev, e$dlzkadna)
pocet_vah <- compute_weigths_num(length(subset(input_v, input_v == TRUE)), 
                                 unq(e$neural_layers))
{
  progress <- list()
  progress$printed <- -10
  progress$print <- 0
  progress$baseAll <- 0
  progress$basePart <- 0
  progress$i <- 0
}

#na meranie casu
start_time_all <- Sys.time()

drv <- dbDriver("PostgreSQL")
if (exists("con")) dbDisconnect(con)
con <- getConnection(drv)

progress$baseAll <- dbGetQuery(con, "SELECT count(*) FROM
                               (SELECT DISTINCT datum, fve FROM t_produkcia) s")

stats_all <- data.frame()
for (fve in 1:length(e$fve)) {
  all_days <- dbGetQuery(con, sprintf(e$tren_mnoz_select, e$fve[fve]))
  maxims <- c(max(all_days[,2]), max(all_days[,3]), max(all_days[,4]))
  minims <- c(min(all_days[,2]), min(all_days[,3]), min(all_days[,4]))
  scale <- abs(maxims - minims)
  
  namerane <- c()
  predikovane <- c()
  predikovane2 <- c()
  
  progress$printed <- -10
  progress$basePart <- nrow(all_days)
  
  for (i in 1:nrow(all_days)) { #nrow(all_days)
    # pre kazdy den vybrat trenovaciu mnozinu
    # vsetky okrem toho pre ktory idem predikovat
    chosen_one <- all_days[i,]
    not_i <- c(i != c(1:nrow(all_days)))
    train_set <- all_days[not_i,]
    
    # vypocitat podobnost
    differ <- c()
    for (j in 1:nrow(train_set)) { #nrow(train_set)
      differ[j] <- (
        90*(abs(chosen_one['gho']-train_set[j,'gho']) %p% scale[1]) 
        +
          10*(abs(chosen_one['teplota']-train_set[j,'teplota']) %p% scale[2])
        +
          (abs(chosen_one['vietor']-train_set[j,'vietor']) %p% scale[3])
      )
    }
    train_set['diff'] <- unlist(differ)
    #train_set <- train_set[c(train_set[,6] < 3),]
    #print(nrow(train_set))
    
    ordered <- arrange(train_set, train_set[,6])
    train_set <- ordered[1:e$tren_mnoz_velkost,]
    namerane <- vector.add(namerane, chosen_one[1,'praca'])
    
    # do db tabulky pocet hidden layer a trueshold
    neur_net <- neuralnet(praca~gho+teplota+vietor, train_set, startweights = unq(e$neural_startweights),
                          hidden=unq(e$neural_layers), threshold=e$neural_threshold)
    predikovane <- vector.add(predikovane, compute(neur_net, chosen_one[1,2:4])$net.result)
    #plot(neur_net)
    
    # forest <- randomForest(praca~gho+teplota+vietor, data=train_set, importance=TRUE, proximity=TRUE)
    # predikovane2 <-  vector.add(predikovane2, predict(forest, chosen_one[1,2:4], type="response", norm.votes=TRUE))
    # #plot(forest, type="l")
    
    progress$i <- progress$i + 1
    progress$print <- (i %p% progress$basePart)
    if (progress$print >= progress$printed + 10) {
      print(sprintf("Celkovo: %.2f%s, FVE(%d): %.2f%s, %s", 
                    progress$i %p% progress$baseAll, "%", fve, progress$print, "%",
                    format.time(difftime(Sys.time(), start_time_all, units = "sec"))),
            quote=F)
      progress$printed <- progress$print
    }
  }
  
  #nove <- cbind(namerane, predikovane, predikovane2)
  # statistika presnosti
  stats.neur <- all_statistics(namerane, predikovane)
  #stats.forest <- all_statistics(namerane, predikovane2)
  
  result <- dbGetQuery(con, build_insert_stats.neur(e, stats.neur, start_time_all))
  #result <- dbGetQuery(con, build_insert_stats.forest(e, stats.forest, start_time_all))
}

end_time_all <- Sys.time()
print(paste("Celkovy cas =", format.time(difftime(end_time_all, start_time_all, units = "sec"))))

dbDisconnect(con)
dbUnloadDriver(drv)
#rm(list = setdiff(ls(), lsf.str()))