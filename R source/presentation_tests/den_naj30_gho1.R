#predikcia po dni, setky FVE, 30 predchadzajucich, oblacnost normalne
{
  library(RPostgreSQL)
  library(plyr)
  library(randomForest)
  library(snow)
  
  source('~/GitHub/baka/R source/presentation_tests/functions.R')
}

# cl <- makeCluster(4, type='SOCK')



{
  write_results <- T
  fve <- c(1, 2, 3)
  tm_velkost <- 30
  f.ntree <- 700
  f.mtry <- 2
  
  pod_gho <- 1
  
  prog.diff <- 10
  prog.printed_all <- -10000
  prog.printed_part <- -10000
  prog.print_perc_part <- 0
  prog.print_perc_all <- 0
  prog.baseAll <- 0
  prog.basePart <- 0
  prog.i <- 0
  
  db.drv <- dbDriver("PostgreSQL")
  if (exists("db.con")) dbDisconnect(db.con)
  db.con <- getConnection(db.drv)
}

{
  #pocet vsetkyc dni - pocitam percenta
  prog.baseAll <- dbGetQuery(db.con, "select count(*) as ccc from (select distinct * from (select datum, fve from v_data) s1) s2")
  prog.baseAll <- prog.baseAll$ccc
}

time.start <- Sys.time()
actual <- vector(mode = "numeric", length = prog.baseAll)
output <- vector(mode = "numeric", length = prog.baseAll)
i.vec <- 0

select <- " SELECT datum, sum(praca) praca, sum(gho) gho, sum(oblacnost) oblacnost, 
sum(teplota) teplota, sum(vietor) vietor, max(dlzkadna) dlzkadna 
FROM v_data WHERE fve = %d GROUP BY datum ORDER BY datum"

for (i.fve in fve) {
  all_days <- dbGetQuery(db.con, sprintf(select, i.fve))
  ad_ncol <- ncol(all_days)
  maxims <- sapply(all_days[,3:ad_ncol], max)
  minims <- sapply(all_days[,3:ad_ncol], min)
  scale <- abs(maxims - minims)
  all_days <- data.matrix(all_days)
  
  chosen_days <- all_days
  
  for (i.day in 1:nrow(chosen_days)) {
    
    dayd <- chosen_days[i.day,]
    potencial <- all_days[all_days[,'datum'] != dayd[['datum']],]
    
    diff <- vector(mode = "numeric", length = nrow(potencial))
    
    diff <- sapply(1:length(diff), function(x) {
      return(diff[x] + abs(dayd[['gho']] - potencial[[x,'gho']]) * 100 / scale[['gho']] * pod_gho)
    })
    
    
    # diff <- sapply(1:length(diff), function(x) {
    #   abs(dayd[['gho']] - potencial[[x,'gho']]) * 100 / scale[['gho']] * pod_gho
    # })
    
    train_set <- arrange(as.data.frame(potencial), diff)[1:30,]
    
    forest <- randomForest(praca~gho+oblacnost+teplota+vietor+dlzkadna, 
                           data=train_set, ntree = f.ntree, mtry = f.mtry, importance=TRUE, proximity=TRUE)
    predic <-predict(forest, data.frame(gho = dayd[['gho']], oblacnost = dayd[['oblacnost']],
                                        teplota = dayd[['teplota']], vietor = dayd[['vietor']], 
                                        dlzkadna = dayd[['dlzkadna']]), type="response", norm.votes=TRUE)
    # varImpPlot(forest)
    
    i.vec <- i.vec + 1
    actual[i.vec] <- dayd[['praca']] 
    output[i.vec] <- predic
    
    {
      prog.i <- prog.i + 1
      # prog.print_perc_part <- (i * 100 / prog.basePart)
      prog.print_perc_all <- (prog.i * 100 / prog.baseAll)
      if (prog.print_perc_all >= prog.printed_all + prog.diff) {
        prog.actual_time <- as.numeric(difftime(Sys.time(), time.start, units = "sec"))
        prog.estimated_time <- prog.actual_time * 100 / prog.print_perc_all
        print(sprintf("Forest perc: %6.2f%s, num: %7.d/%d, Estimated time: %s, Actual: %s",
                      prog.print_perc_all, "%", prog.i, prog.baseAll,
                      format.time(prog.estimated_time),
                      format.time(prog.actual_time)),
              quote=F)
        prog.printed_all <- prog.print_perc_all
      }
    }
  }
}

# statistika presnosti
stats <- all_statistics(actual, output)
if (write_results) {
  for (name in names(stats)) {
    if (is.infinite(stats[[name]]) | !is.numeric(stats[[name]]) | is.nan(stats[[name]])) stats[[name]] <- 999.999
  }
  insert <- sprintf("INSERT INTO t_experiment (cas_behu, metoda, param1, param2, param3, param4, param5,
                    N, MBE, RMBE, RMSE, RRMSE, MAE, RMAE, MPE, MAXAE, SD,
                    tm_velkost, tm_opis, tm_select, fve, den_hod,
                    in_gho, in_oblacnost, in_teplota, in_vietor, in_vlhkost, in_tlak, in_dlzkadna, in_azim, in_elev)
                    VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s',
                    %d, %f, %f, %f, %f, %f, %f, %f, %f, %f,
                    %d, '%s', '%s', '%s', '%s',
                    %s, %s, %s, %s, %s, %s, %s, %s, %s);",
                    time.start, "random forest", "tm 30", "ntree 700", "mtry 2", "", "",
                    stats$N, stats$MBE, stats$RMBE, stats$RMSE, stats$RRMSE, stats$MAE, stats$RMAE, stats$MPE, stats$MAXAE, stats$SD,
                    30, "30 najpodob dni podla gho", select, "vsetky", "den",
                    TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  
  db.result <- dbGetQuery(db.con, insert)
}

time.end <- Sys.time()
print(sprintf("Start: %s, End: %s, Duration: %s",
              time.start, time.end, 
              format.time(difftime(time.end, time.start, units = "sec"))),
      quote = F)

if (exists("db.con")) dbDisconnect(db.con)
dbUnloadDriver(db.drv)

#stopCluster(cl)