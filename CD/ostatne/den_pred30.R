#predikcia po dni, setky FVE, 30 predchadzajucich, oblacnost normalne
{
library(RPostgreSQL)
library(plyr)
library(randomForest)
source('~/GitHub/baka/R source/presentation_tests/functions.R')
}

{
write_results <- T
fve <- c(1, 2, 3)
tm_velkost <- 30
f.ntree <- 700
f.mtry <- 2

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
prog.baseAll <- dbGetQuery(db.con, "SELECT cc1 + cc2 + cc3 AS ccc FROM 
	  (SELECT count (*) AS CC1 FROM 
                           (SELECT DISTINCT datum, row_number() OVER (ORDER BY datum) AS rw FROM v_data WHERE fve = 1 GROUP BY datum ORDER BY datum) s1
                           WHERE s1.rw > 30) ss1,
                           (SELECT count (*) AS CC2 FROM 
                           (SELECT DISTINCT datum, row_number() OVER (ORDER BY datum) AS rw FROM v_data WHERE fve = 2 GROUP BY datum ORDER BY datum) s2
                           WHERE s2.rw > 30) ss2,
                           (SELECT count (*) AS CC3 FROM 
                           (SELECT DISTINCT datum, row_number() OVER (ORDER BY datum) AS rw FROM v_data WHERE fve = 3 GROUP BY datum ORDER BY datum) s3
                           WHERE s3.rw > 30) ss3")
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

  chosen_days <- all_days[31:nrow(all_days),]

  for (i.day in 1:nrow(chosen_days)) {
    
    dayd <- chosen_days[i.day,]
    train_set <- all_days[all_days[['datum']] < dayd[['datum']] & all_days[['datum']] >= dayd[['datum']] - 30,]
    
    forest <- randomForest(praca~gho+oblacnost+teplota+vietor+dlzkadna, 
                           data=train_set, ntree = f.ntree, mtry = f.mtry, importance=TRUE, proximity=TRUE)
    predic <-predict(forest, dayd[1,3:7], type="response", norm.votes=TRUE)
    
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
                 time.start, "random forest", "tm 30 predch", "ntree 700", "mtry 2", "", "",
                 stats$N, stats$MBE, stats$RMBE, stats$RMSE, stats$RRMSE, stats$MAE, stats$RMAE, stats$MPE, stats$MAXAE, stats$SD,
                 30, "30 predchadzajucich dni", select, "vsetky", "den",
                 TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  
  db.result <- dbGetQuery(db.con, insert)
}