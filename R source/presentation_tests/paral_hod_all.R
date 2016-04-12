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


{
  write_results <- F
  fve <- c(1, 2, 3)
  tm.velkost <- c(130, 110, 90, 70, 50, 30) [6]
  f.ntree <- c(1000, 700, 500, 300, 100) [3]
  f.mtry <- c(2)
  
  pod_gho <- 1 #90 #c(240, 220, 200, 0)[2]
  pod_obl <- 1 #c(90, 80, 70, 0)[2]
  pod_tep <- 1 #c(35, 30, 25, 0)[2]
  pod_vie <- 1 #c(6, 5, 4, 0)[2]
  pod_vlh <- 1 #c(0)
  pod_dlz <- 1 #c(55, 50, 45, 0)[2]
  pod_ele <- 1 #c(0)
  
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
  prog.baseAll <- dbGetQuery(db.con, "select count(*) as ccc from (select distinct * from (select cas, fve from v_data) s1) s2")
  prog.baseAll <- prog.baseAll$ccc
  prog.opsAll <- length(tm.velkost) * length(f.ntree) * length(f.mtry) * 
    length(pod_gho) * length(pod_obl) * length(pod_tep) * length(pod_vie) * 
    length(pod_vlh) * length(pod_dlz) * length(pod_ele)
  prog.baseAll <- prog.baseAll * prog.opsAll
}

time.start <- Sys.time()
# actual <- vector(mode = "numeric", length = prog.baseAll)
# output <- vector(mode = "numeric", length = prog.baseAll)



select <- " SELECT datum, cas, praca, gho, oblacnost, 
teplota, vietor, vlhkost, dlzkadna, elev
FROM v_data WHERE fve = %d ORDER BY cas"

hours_done <- 0
ops_done <- 0

i.pod_gho <- pod_gho
i.pod_obl <- pod_obl
i.pod_tep <- pod_tep
i.pod_vie <- pod_vie
i.pod_vlh <- pod_vlh
i.pod_dlz <- pod_dlz
i.pod_ele <- pod_ele
i.velkost <- tm.velkost
i.ntree <- f.ntree
i.mtry <- f.mtry


ops_done <- ops_done + 1

actual <- c()
output <- c()

for (i.fve in fve) {
  
  all_hours <- dbGetQuery(db.con, sprintf(select, i.fve))
  
  ad_ncol <- ncol(all_hours)
  maxims <- apply(all_hours[,4:ad_ncol], 2, max)
  minims <- apply(all_hours[,4:ad_ncol], 2, min)
  scale <- abs(maxims - minims)
  all_hours <- data.matrix(all_hours)
  
  chosen_hours <- all_hours
  
  fve_actual <- chosen_hours[,'praca']
  
  clusterExport(cl, list("chosen_hours", "all_hours", "scale",
                         "i.pod_gho", "i.pod_obl", "i.pod_tep", "i.pod_vie", "i.pod_dlz",
                         "i.pod_vlh", "i.pod_ele",
                         "i.ntree", "i.mtry", "i.velkost"))
  fve_output <- parSapply(cl, 1:nrow(chosen_hours), function(y) {
    hourh <- chosen_hours[y,]
    potencial <- all_hours[all_hours[,'datum'] != hourh[['datum']],]
    diff <- vector(mode = "numeric", length = nrow(potencial))
    
    diff <- sapply(1:length(diff), function(x) {
      ret <- abs(hourh[['gho']] - potencial[[x,'gho']]) * 100 / scale[['gho']] * i.pod_gho
      ret <- ret + abs(hourh[['oblacnost']] - potencial[[x,'oblacnost']]) * 100 / scale[['oblacnost']] * i.pod_obl
      ret <- ret + abs(hourh[['teplota']] - potencial[[x,'teplota']]) * 100 / scale[['teplota']] * i.pod_tep
      ret <- ret + abs(hourh[['vietor']] - potencial[[x,'vietor']]) * 100 / scale[['vietor']] * i.pod_vie
      ret <- ret + abs(hourh[['vlhkost']] - potencial[[x,'vlhkost']]) * 100 / scale[['vlhkost']] * i.pod_vlh
      ret <- ret + abs(hourh[['dlzkadna']] - potencial[[x,'dlzkadna']]) * 100 / scale[['dlzkadna']] * i.pod_dlz
      ret <- ret + abs(hourh[['elev']] - potencial[[x,'elev']]) * 100 / scale[['elev']] * i.pod_ele
      return(ret)
    })
    
    train_set <- arrange(as.data.frame(potencial), diff)[1:i.velkost,]
    
    forest <- randomForest(praca~gho+oblacnost+teplota+vietor+vlhkost+dlzkadna+elev,
                           data=train_set, ntree = i.ntree, mtry = i.mtry)
    predic <-predict(forest, data.frame(gho = hourh[['gho']], oblacnost = hourh[['oblacnost']],
                                        teplota = hourh[['teplota']], vietor = hourh[['vietor']],
                                        vlhkost = hourh[['vlhkost']], dlzkadna = hourh[['dlzkadna']],
                                        elev = hourh[['elev']]), type="response", norm.votes=TRUE)
    #varImpPlot(forest)
    
    return(predic)
  })
  
  actual <- append(actual, fve_actual)
  output <- append(output, fve_output)
  
  hours_done <- hours_done + nrow(all_hours)
  prog.i <- hours_done
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

# if (write_results) {
#   for (name in names(stats)) {
#     if (is.infinite(stats[[name]]) | !is.numeric(stats[[name]]) | is.nan(stats[[name]])) stats[[name]] <- 999.999
#   }
#   insert <- sprintf("INSERT INTO t_experiment (cas_behu, metoda, param1, param2, param3, param4, param5,
#     N, MBE, RMBE, RMSE, RRMSE, MAE, RMAE, MPE, MAXAE, SD,
#     tm_velkost, tm_opis, tm_select, fve, den_hod,
#     in_gho, in_oblacnost, in_teplota, in_vietor, in_vlhkost, in_tlak, in_dlzkadna, in_azim, in_elev)
#     VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s',
#     %d, %f, %f, %f, %f, %f, %f, %f, %f, %f,
#     %d, '%s', '%s', '%s', '%s',
#     %s, %s, %s, %s, %s, %s, %s, %s, %s);",
#                     time.start, "random forest", "tm 30", "ntree 700", "mtry 2", "", "",
#                     stats$N, stats$MBE, stats$RMBE, stats$RMSE, stats$RRMSE, stats$MAE, stats$RMAE, stats$MPE, stats$MAXAE, stats$SD,
#                     30, "30 najpodob dni podla gho90, tep10, vie1", select, "vsetky", "den",
#                     TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
#
#   db.result <- dbGetQuery(db.con, insert)
# }



time.end <- Sys.time()
print(sprintf("Start: %s, End: %s, Duration: %s",
              time.start, time.end, 
              format.time(difftime(time.end, time.start, units = "sec"))),
      quote = F)

# if (exists("db.con")) dbDisconnect(db.con)
# dbUnloadDriver(db.drv)

#stopCluster(cl)

# db.drv <- dbDriver("PostgreSQL")
# if (exists("db.con")) dbDisconnect(db.con)
db.con <- getConnection(db.drv)
all_data <-  dbGetQuery(db.con, "SELECT fve, datum, cas, gho, oblacnost,
                      teplota, vietor, vlhkost, dlzkadna, elev, praca
                      FROM v_data ORDER BY fve, cas")
all_data <- cbind(all_data, output)
to_see <- cbind(all_data, dif = abs(output - actual) * 100 / actual)
to_see <- arrange(to_see, to_see$dif)

groupped <- ddply(all_data,~datum+fve,summarise, gho=sum(gho), oblacnost=sum(oblacnost),
                  teplota=sum(teplota), vietor=sum(vietor), vlhkost=sum(vlhkost),
                  dlzkadna=max(dlzkadna), praca=sum(praca), output=sum(output))

stats_day <- all_statistics(groupped$praca, groupped$output)

if (exists("db.con")) dbDisconnect(db.con)
stopCluster(cl)
