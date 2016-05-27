library(RPostgreSQL)
library(plyr)
library(randomForest)
library(doParallel)
library(foreach)
library(insol)
# cl <- makeCluster(detectCores(), type='SOCK')
# registerDoParallel(cl)

# registerDoSEQ()


source('~/GitHub/baka/R source/project/functions.R')

write_results <- T
vzorka <- 0
e.fve <- c(1, 2, 3)[1]
e.tm_velkost <- c(30)
e.ntree <- 500
e.mtry <- c(2)
e.pod_gho <- c(220)
e.pod_obl <- c(85)
e.pod_tep <- c(30)
e.pod_vie <- c(5)
e.pod_dlz <- c(50)
e.pod_ele <- c(100)

prog.diff <- 1

{
  
  e.metoda <- "randomForest"
  
  pod_gho <- 0
  pod_obl <- 0
  pod_tep <- 0
  pod_vie <- 0
  pod_vlh <- 0
  pod_tla <- 0
  pod_dlz <- 0
  pod_azi <- 0
  pod_ele <- 0
  
  e.den_hod <- "hod"
  formula <- unq("praca~gho+oblacnost+teplota+vietor+vlhkost+dlzkadna+elev") 
  in_gho <- TRUE
  in_obl <- TRUE
  in_tep <- TRUE
  in_vie <- TRUE
  in_vlh <- F
  in_tla <- F
  in_dlz <- TRUE
  in_azi <- F
  in_ele <- TRUE
  
  {
    e <- list()
    e$metoda <- e.metoda
    e$param1 <- ""
    e$param2 <- ""
    e$param3 <- ""
    e$param4 <- ""
    e$param5 <- ""
    
    e.tm_select_datum <- " SELECT DISTINCT datum FROM v_data 
    WHERE fve = %d and datum >= to_date('01.01.2015', 'DD.MM.YYYY') 
    ORDER BY datum "
    
    e.tm_select_hod <- " SELECT cas, praca, gho, (100 - oblacnost) oblacnost, (100 + teplota) teplota, vietor, (100-vlhkost) vlhkost, dlzkadna, elev 
    FROM v_data WHERE fve = %d AND datum = '%s' ORDER BY cas "
    
    e.tm_select <- " SELECT datum, cas, praca, gho, (100-oblacnost) oblacnost, 
    (teplota + 100) teplota, vietor, (100 - vlhkost) vlhkost, dlzkadna, elev
    FROM v_data WHERE fve = %d ORDER BY datum"
    
    e.tm_opis <- " %d najpodobnejsich hodin podla" %s% 
      ifelse(pod_gho > 0, " dho * " %s% pod_gho %s% ",", "") %s% 
      ifelse(pod_obl > 0, " opacnej oblacnosti * " %s% pod_obl %s% ",", "") %s% 
      ifelse(pod_tep > 0, " teploty + 100 * " %s% pod_tep %s% ",", "") %s% 
      ifelse(pod_vie > 0, " vetra * " %s% pod_vie %s% ",", "") %s% 
      ifelse(pod_tla > 0, " tlaku * " %s% pod_tla %s% ",", "") %s% 
      ifelse(pod_dlz > 0, " dlzky dna * " %s% pod_dlz %s% ",", "") %s% 
      ifelse(pod_azi > 0, " azimutu * " %s% pod_azi %s% ",", "") %s% 
      ifelse(pod_ele > 0, " elevacie * " %s% pod_ele %s% ",", "") %s% 
      "  predikcia metodou " %s% e.metoda %s% " na vzorke " %s% 
      ifelse(!is.null(vzorka) && vzorka > 0, vzorka %s% " nahodnych", " vsetkych") %s% 
      " dni pre elektraren %s."
    
    # e$tm_velkost <- e.tm_velkost
    # e$tm_opis <- e.tm_opis
    # e$tm_select <- e.tm_select
    e$fve <- c("FVE Dubravy 1", "FVE Dubravy 2", "FVE Plesivec")
    e$den_hod <- e.den_hod 
    
    e$pod_gho <- pod_gho
    e$pod_obl <- pod_obl
    e$pod_tep <- pod_tep
    e$pod_vie <- pod_vie
    e$pod_vlh <- pod_vlh
    e$pod_tla <- pod_tla
    e$pod_dlz <- pod_dlz
    e$pod_azi <- pod_azi
    e$pod_ele <- pod_ele
    
    e$in_gho <- in_gho
    e$in_obl <- in_obl
    e$in_tep <- in_tep
    e$in_vie <- in_vie
    e$in_vlh <- in_vlh
    e$in_tla <- in_tla
    e$in_dlz <- in_dlz
    e$in_azi <- in_azi
    e$in_ele <- in_ele
  }
  
  prog.printed_all <- -10000
  prog.printed_part <- -10000
  prog.print_perc_part <- 0
  prog.print_perc_all <- 0
  prog.baseAll <- 0
  prog.basePart <- 0
  prog.i <- 0
  
  db.drv <- dbDriver("PostgreSQL")
  
}

if (exists("db.con")) dbDisconnect(db.con)
db.con <- getConnection(db.drv)

if (!is.null(vzorka) && vzorka > 0) { # ak predikujemlen pre vzorku nahodnych
  prog.baseAll <- vzorka
} else {                              # ak predikujemlen pre vsetky dni
  prog.baseAll <- dbGetQuery(db.con, "SELECT count(*) FROM " %s%
                               "(SELECT DISTINCT datum FROM t_produkcia WHERE fve = " %s% e.fve[1] %s% 
                               ifelse(length(e.fve) > 1, " OR fve = " %s% e.fve[2], "") %s%
                               ifelse(length(e.fve) > 2, " OR fve = " %s% e.fve[3], "") %s% " 
                             and datum >= to_date('01.01.2015', 'DD.MM.YYYY')) s")
  prog.baseAll <- prog.baseAll$count
}
prog.baseAll <- prog.baseAll * length(e.fve) * length(e.tm_velkost) * length(e.ntree) * length(e.mtry) * 
  length(e.pod_gho) * length(e.pod_obl) * length(e.pod_tep) * length(e.pod_vie) * length(e.pod_dlz) * length(e.pod_ele)

time.start <- Sys.time()

for (i.pod_gho in e.pod_gho) {
  pod_gho <- i.pod_gho
  e$pod_gho <- i.pod_gho
  
  for (i.pod_obl in e.pod_obl) {
    pod_obl <- i.pod_obl
    e$pod_obl <- i.pod_obl
    
    for (i.pod_tep in e.pod_tep) {
      pod_tep <- i.pod_tep
      e$pod_tep <- i.pod_tep
      
      for (i.pod_vie in e.pod_vie) {
        pod_vie <- i.pod_vie
        e$pod_vie <- i.pod_vie
        
        for (i.pod_dlz in e.pod_dlz) {
          pod_dlz <- i.pod_dlz
          e$pod_dlz <- i.pod_dlz
          
          for (i.pod_ele in e.pod_ele) {
            pod_ele <- i.pod_ele
            e$pod_ele <- i.pod_ele
            
            for (i.fve in e.fve) {
              e$tm_select <- sprintf(e.tm_select_datum, i.fve)
              i.tm_select <- sprintf(e.tm_select_datum, i.fve)
              
              t.all_hours <- dbGetQuery(db.con, sprintf(e.tm_select, i.fve))
              t.ncol <- ncol(t.all_hours)
              scale.maxims <- sapply(t.all_hours[,4:t.ncol], max)
              scale.minims <- sapply(t.all_hours[,4:t.ncol], min)
              scale.scale <- abs(scale.maxims - scale.minims)
              
              for (i.tn_velkost in e.tm_velkost) {
                e$tm_velkost <- i.tn_velkost
                e$tm_opis <- sprintf(e.tm_opis, i.tn_velkost, e$fve[i.fve])
                e$param1 <- "tm_velk " %s% i.tn_velkost
                
                for (i.ntree in e.ntree) { 
                  e$param2 <- "ntree " %s% i.ntree
                  
                  for (i.mtry in e.mtry) {
                    e$param3 <- "mtry " %s% i.mtry
                    
                    {
                      actual <- c()
                      output <- c()
                      # vytiahnem vsetky dni
                      t.all_days = dbGetQuery(db.con, sprintf(e.tm_select_datum, i.fve))
                      
                      for (day in 1:nrow(t.all_days)) {
                        day.datum <- t.all_days[[day, 'datum']]
                        # vytiahnem hodiny
                        t.hours = dbGetQuery(db.con, sprintf(e.tm_select_hod, i.fve, day.datum))
                        
                        actual <- append(actual, sum(t.hours[['praca']]))
                        output_hod <- c()
                        
                        tm.potencial <- t.all_hours[c(day.datum > t.all_hours[['datum']]),]
                        
                          
                          tm.train_set = tm.potencial[(nrow(tm.potencial)-30):nrow(tm.potencial),]
                          
                          forest <- randomForest(as.factor(praca)~gho+oblacnost+teplota+vietor+vlhkost+dlzkadna+elev, 
                                                 data=tm.train_set, ntree = i.ntree, mtry = i.mtry, importance=TRUE, proximity=TRUE)
                          output_hod <- append(output_hod, predict(forest, t.hours[,3:9], type="response", norm.votes=TRUE))
                          
                        output <- append(output, sum(output_hod))
                        
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
                      
                      # statistika presnosti
                      stats <- all_statistics(actual, output)
                      if (write_results) {
                        db.result <- dbGetQuery(db.con, build_insert_stats(e, stats, time.start, i.fve))
                      }
                    }
                    
                  } # for (i.mtry in e.mtry)
                  
                } # for (i.ntree in e.ntree)
                
              } # for (i.tn_velkost in e.tm_velkost)
              
            } #for (i.fve in e.fve)
            
          } # for (i.pod_ele in e.pod_ele)
          
        } # for (i.pod_dlz in e.pod_dlz)
        
      } # for (i.pod_vie in e.pod_vie)
      
    } # for (i.pod_tep in e.pod_tep)
    
  } # for (i.pod_obl in e.pod_obl)
  
} # for (i.pod_gho in e.pod_gho)


time.end <- Sys.time()
print(sprintf("Start: %s, End: %s, Duration: %s",
              time.start, time.end, 
              format.time(difftime(time.end, time.start, units = "sec"))),
      quote = F)

registerDoSEQ()

if (exists("db.con")) dbDisconnect(db.con)
dbUnloadDriver(db.drv)

