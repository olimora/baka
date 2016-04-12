#predikcia po dni, setky FVE, 30 predchadzajucich, oblacnost normalne
{
  library(RPostgreSQL)
  library(plyr)
  library(randomForest)
  library(snow)
  source('~/GitHub/baka/R source/presentation_tests/functions.R')
}

cl <- makeCluster(4, type='SOCK')

clusterEvalQ(cl, format.time <- function(x) UseMethod("format.time"))
clusterEvalQ(cl, { library(plyr); library(randomForest) })

#stopCluster(cl)

{
  write_results <- F
  fve <- c(1, 2, 3)
  tm.velkost <- c(130, 110, 90, 70, 50, 30) [2]
  f.ntree <- c(1000, 700, 500, 300, 100) [2]
  f.mtry <- c(2)
  
  pod_gho <- c(240, 220, 200, 0)[2]
  pod_obl <- c(90, 80, 70, 0)[2]
  pod_tep <- c(35, 30, 25, 0)[2]
  pod_vie <- c(6, 5, 4, 0)[2]
  pod_dlz <- c(55, 50, 45, 0)[2]
  
  prog.diff <- 0
  prog.printed_all <- -10000
  prog.printed_ops <- -10000
  prog.print_perc_all <- 0
  prog.baseAll <- 0
  prog.opsAll <- 0
  prog.i <- 0
  prog.op <- 0
  
  db.drv <- dbDriver("PostgreSQL")
  if (exists("db.con")) dbDisconnect(db.con)
  db.con <- getConnection(db.drv)
}

{
  #pocet vsetkyc dni - pocitam percenta
  prog.baseAll <- dbGetQuery(db.con, "select count(*) as ccc from (select distinct * from (select datum, fve from v_data) s1) s2")
  prog.baseAll <- prog.baseAll$ccc
  prog.opsAll <- length(tm.velkost) * length(f.ntree) * length(f.mtry) * 
    length(pod_gho) * length(pod_obl) * length(pod_tep) * length(pod_vie) * length(pod_dlz)
  prog.baseAll <- prog.baseAll * prog.opsAll
}

time.start <- Sys.time()
# actual <- vector(mode = "numeric", length = prog.baseAll)
# output <- vector(mode = "numeric", length = prog.baseAll)



select <- " SELECT datum, sum(praca) praca, sum(gho) gho, sum(oblacnost) oblacnost, 
sum(teplota) teplota, sum(vietor) vietor, max(dlzkadna) dlzkadna 
FROM v_data WHERE fve = %d GROUP BY datum ORDER BY datum"

days_done <- 0
ops_done <- 0


for (i.velkost in tm.velkost) {

  for (i.ntree in f.ntree) {

    for (i.mtry in f.mtry) {

      for (i.pod_gho in pod_gho) {

        for (i.pod_obl in pod_obl) {

          for (i.pod_tep in pod_tep) {

            for (i.pod_vie in pod_vie) {

              for (i.pod_dlz in pod_dlz) {

                # tu uz ide predpoved pre 3 elektrarne spolu
                ops_done <- ops_done + 1

                actual <- c()
                output <- c()

                for (i.fve in fve) {

                  all_days <- dbGetQuery(db.con, sprintf(select, i.fve))

                  ad_ncol <- ncol(all_days)
                  maxims <- sapply(all_days[,3:ad_ncol], max)
                  minims <- sapply(all_days[,3:ad_ncol], min)
                  scale <- abs(maxims - minims)
                  all_days <- data.matrix(all_days)

                  #chosen_days <- all_days

                  fve_actual <- all_days[,'praca']

                  clusterExport(cl, list("all_days", "scale",
                                         "i.pod_gho", "i.pod_obl", "i.pod_tep", "i.pod_vie", "i.pod_dlz",
                                         "i.ntree", "i.mtry", "i.velkost"))
                  fve_output <- parSapply(cl, 1:nrow(all_days), function(y) {
                    dayd <- all_days[y,]
                    potencial <- all_days[all_days[,'datum'] != dayd[['datum']],]
                    diff <- vector(mode = "numeric", length = nrow(potencial))

                    diff <- sapply(1:length(diff), function(x) {
                      ret <- abs(dayd[['gho']] - potencial[[x,'gho']]) * 100 / scale[['gho']] * i.pod_gho
                      ret <- ret + abs(dayd[['oblacnost']] - potencial[[x,'oblacnost']]) * 100 / scale[['oblacnost']] * i.pod_obl
                      ret <- ret + abs(dayd[['teplota']] - potencial[[x,'teplota']]) * 100 / scale[['teplota']] * i.pod_tep
                      ret <- ret + abs(dayd[['vietor']] - potencial[[x,'vietor']]) * 100 / scale[['vietor']] * i.pod_vie
                      ret <- ret + abs(dayd[['dlzkadna']] - potencial[[x,'dlzkadna']]) * 100 / scale[['dlzkadna']] * i.pod_dlz
                      return(ret)
                    })

                    train_set <- arrange(as.data.frame(potencial), diff)[1:i.velkost,]

                    forest <- randomForest(praca~gho+oblacnost+teplota+vietor+dlzkadna,
                                           data=train_set, ntree = i.ntree, mtry = i.mtry)
                    predic <-predict(forest, data.frame(gho = dayd[['gho']], oblacnost = dayd[['oblacnost']],
                                                        teplota = dayd[['teplota']], vietor = dayd[['vietor']],
                                                        dlzkadna = dayd[['dlzkadna']]), type="response", norm.votes=TRUE)
                    # varImpPlot(forest)

                    return(predic)
                  })

                  actual <- append(actual, fve_actual)
                  output <- append(output, fve_output)

                  days_done <- days_done + nrow(all_days)
                  prog.i <- days_done
                  prog.print_perc_all <- (prog.i * 100 / prog.baseAll)
                  
                  prog.op <- ops_done
                  prog.print_perc_ops <- (prog.op * 100 / prog.opsAll)
                  if (prog.print_perc_all > prog.printed_all + prog.diff) {
                    prog.actual_time <- as.numeric(difftime(Sys.time(), time.start, units = "sec"))
                    prog.estimated_time <- prog.actual_time * 100 / prog.print_perc_all
                    print(sprintf("Forest perc: %6.2f%s, ops: %7.d/%d, day: %9.d/%d, Estimated time: %s, Actual: %s", #
                                  prog.print_perc_all, "%", prog.op, prog.opsAll, prog.i, prog.baseAll, #
                                  format.time(prog.estimated_time),
                                  format.time(prog.actual_time)),
                          quote=F)
                    prog.printed_all <- prog.print_perc_all
                    #prog.printed_ops <- prog.print_perc_ops
                  }


                } # fve

                # statistika presnosti
                stats <- all_statistics(actual, output)
                if (write_results) {
                  for (name in names(stats)) {
                    if (is.infinite(stats[[name]]) | !is.numeric(stats[[name]]) | is.nan(stats[[name]])) stats[[name]] <- 999.999
                  }
                  insert <- sprintf("INSERT INTO t_experiment (cas_behu, metoda, param1, param2, param3, param4, param5,
                    N, MBE, RMBE, RMSE, RRMSE, MAE, RMAE, MPE, MAXAE, SD,
                    tm_velkost, tm_opis, tm_select, fve, den_hod,
                    pod_gho, pod_oblacnost, pod_teplota, pod_vietor, pod_vlhkost, pod_tlak, pod_dlzkadna, pod_azim, pod_elev,
                    in_gho, in_oblacnost, in_teplota, in_vietor, in_vlhkost, in_tlak, in_dlzkadna, in_azim, in_elev)
                    VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s',
                    %d, %f, %f, %f, %f, %f, %f, %f, %f, %f,
                    %d, '%s', '%s', '%s', '%s',
                    %f, %f, %f, %f, %f, %f, %f, %f, %f,
                    %s, %s, %s, %s, %s, %s, %s, %s, %s);",
                                    time.start, "random forest", "tm " %s% as.character(i.velkost), "ntree " %s% as.character(i.ntree), "mtry " %s% "as.character(i.mtry)", "", "",
                                    stats$N, stats$MBE, stats$RMBE, stats$RMSE, stats$RRMSE, stats$MAE, stats$RMAE, stats$MPE, stats$MAXAE, stats$SD,
                                    i.velkost, "najpodob dni ", select, "vsetky", "den",
                                    i.pod_gho, i.pod_obl, i.pod_tep, i.pod_vie, 0, 0, i.pod_dlz, 0, 0,
                                    TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)

                  db.result <- dbGetQuery(db.con, insert)
                }
                
                

              } # pod_dlz

            } # pod_vie

          } # pod_tep

        } # pod_obl

      } # pod_gho

    } # mtry

  } # ntree

} # tm.velkost


time.end <- Sys.time()
print(sprintf("Start: %s, End: %s, Duration: %s",
              time.start, time.end, 
              format.time(difftime(time.end, time.start, units = "sec"))),
      quote = F)

if (exists("db.con")) dbDisconnect(db.con)
dbUnloadDriver(db.drv)

stopCluster(cl)