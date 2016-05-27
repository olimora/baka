library(RPostgreSQL)
library(plyr)
library(randomForest)
library(doParallel)
library(foreach)
# cl <- makeCluster(detectCores(), type='PSOCK')
# registerDoParallel(cl)
# registerDoSEQ()


source('~/GitHub/baka/R source/project/functions.R')

write_results <- T
vzorka <- 0
e.fve <- c(1, 2, 3)[3]
e.tm_velkost <- 30
e.ntree <- c(700)
e.mtry <- c(2)
e.pod_gho <- 0 #c(220)
e.pod_obl <- 0 #c(85)
e.pod_tep <- 0 #c(30)
e.pod_vie <- 0 #c(5)
e.pod_dlz <- 0 #c(50)
obl_napk <- T

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
  
  e.den_hod <- "den"
  formula <- unq("praca~gho+oblacnost+teplota+vietor+dlzkadna") 
  in_gho <- TRUE
  in_obl <- TRUE
  in_tep <- TRUE
  in_vie <- TRUE
  in_vlh <- F
  in_tla <- F
  in_dlz <- TRUE
  in_azi <- F
  in_ele <- F
  
  {
    e <- list()
    e$metoda <- e.metoda
    e$param1 <- ""
    e$param2 <- ""
    e$param3 <- ""
    e$param4 <- ""
    e$param5 <- ""
    
    e.tm_select <- " SELECT datum, sum(praca) praca, sum(gho) gho, sum(oblacnost) oblacnost, 
    sum(teplota) teplota, sum(vietor) vietor, max(dlzkadna) dlzkadna
    FROM v_data WHERE fve = %d GROUP BY datum ORDER BY datum"
    # e.tm_select2 <- " SELECT datum, sum(praca) praca, sum(gho) gho, sum(100-oblacnost) oblacnost, 
    # sum(teplota) teplota, sum(vietor) vietor, max(dlzkadna) dlzkadna
    # FROM v_data WHERE fve = %d GROUP BY datum ORDER BY datum"
    e.tm_opis <- " %d poslednych dni podla" %s% 
      ifelse(pod_gho > 0, " scitaneho dho * " %s% pod_gho %s% ",", "") %s% 
      ifelse(pod_obl > 0, " scitanej opacnej oblacnosti * " %s% pod_obl %s% ",", "") %s% 
      ifelse(pod_tep > 0, " scitanej teploty * " %s% pod_tep %s% ",", "") %s% 
      ifelse(pod_vie > 0, " scitaneho vetra * " %s% pod_vie %s% ",", "") %s% 
      ifelse(pod_tla > 0, " priemerneho tlaku * " %s% pod_tla %s% ",", "") %s% 
      ifelse(pod_dlz > 0, " dlzky dna * " %s% pod_dlz %s% ",", "") %s% 
      ifelse(pod_azi > 0, " azimutu * " %s% pod_azi %s% ",", "") %s% 
      ifelse(pod_ele > 0, " maximalnej elevacie * " %s% pod_ele %s% ",", "") %s% 
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

if (!is.null(vzorka) && vzorka > 0) { # ak predikujemlen pre vzorku nahodnych
  prog.baseAll <- vzorka
} else {                              # ak predikujemlen pre vsetky dni
  if (exists("db.con")) dbDisconnect(db.con)
  db.con <- getConnection(db.drv)
  prog.baseAll <- dbGetQuery(db.con, "SELECT count(*) FROM " %s%
                               "(SELECT DISTINCT datum, fve FROM t_produkcia WHERE fve = " %s% e.fve[1] %s% 
                               ifelse(length(e.fve) > 1, " OR fve = " %s% e.fve[2], "") %s%
                               ifelse(length(e.fve) > 2, " OR fve = " %s% e.fve[3], "") %s% "
                             and datum >= to_date('01.01.2015', 'DD.MM.YYYY')) s")
  prog.baseAll <- prog.baseAll$count
  dbDisconnect(db.con)
}
prog.baseAll <- prog.baseAll * length(obl_napk) * length(e.fve) * length(e.tm_velkost) * length(e.ntree) * length(e.mtry) * 
  length(e.pod_gho) * length(e.pod_obl) * length(e.pod_tep) * length(e.pod_vie) * length(e.pod_dlz)

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
          
          for (i.fve in e.fve) {
            e$tm_select <- sprintf(e.tm_select, i.fve)
            i.tm_select <- sprintf(e.tm_select, i.fve)
            
            for (i.tn_velkost in e.tm_velkost) {
              e$tm_velkost <- i.tn_velkost
              e$tm_opis <- sprintf(e.tm_opis, i.tn_velkost, e$fve[i.fve])
              e$param1 <- "tm_velk " %s% i.tn_velkost
              
              for (i.ntree in e.ntree) { 
                e$param2 <- "ntree " %s% i.ntree
                
                for (i.mtry in e.mtry) {
                  e$param3 <- "mtry " %s% i.mtry
                  
                  {
                    # vybrat vsetky dni elektratne / vzorku
                    if (exists("db.con")) dbDisconnect(db.con)
                    db.con <- getConnection(db.drv)
                    t.all_days <- dbGetQuery(db.con, sprintf(i.tm_select, i.fve))
                    dbDisconnect(db.con)
                    t.all_days[['oblacnost']]
                    
                    t.ncol <- ncol(t.all_days)
                    scale.maxims <- sapply(t.all_days[,3:t.ncol], max)
                    scale.minims <- sapply(t.all_days[,3:t.ncol], min)
                    scale.scale <- abs(scale.maxims - scale.minims)
                    
                    t.chosen_days <- 0
                    if (!is.null(vzorka) && vzorka > 0) { # ak len vzorka
                      rand_vec <- c()
                      while (length(rand_vec) < vzorka) {
                        rand <- round(runif(1, 1, nrow(t.all_days)), 0)
                        if (!rand %in% rand_vec) {
                          rand_vec <- vector.add(rand_vec, rand)
                        }
                      }
                      t.chosen_days <- t.all_days[rand_vec,]
                      rm(rand_vec, rand)
                    } else {                              # ak pre vsetky predikujem
                      t.chosen_days <- t.all_days[31:nrow(t.all_days),]
                    }
                    
                    t.chosen_days_c <- nrow(t.chosen_days)
                    prog.basePart <- t.chosen_days_c
                    actual <- c()
                    output <- c()
                  }
                  
                  for (i in 1:t.chosen_days_c) { #t.chosen_days_c
                    # pre kazdy den vybrat trenovaciu mnozinu
                    t.chosen_one <- t.chosen_days[i,]
                    #t.not_i <- c(t.chosen_one[1,'datum'] != t.all_days[,'datum'])
                    t.train_set <- t.all_days[c(t.all_days[['datum']] < t.chosen_one[1,'datum'] & t.all_days[['datum']] >= t.chosen_one[1,'datum']-30),] 
                    
                    # # vypocitat rozdielnost
                    # t.differ <- c()
                    # for (j in 1:nrow(t.train_set)) { #nrow(t.train_set)
                    #   t.differ[j] <- ( 
                    #     ifelse(in_gho, (abs(t.chosen_one[['gho']] - t.train_set[[j,'gho']]) * 100 / scale.scale[['gho']]) * pod_gho, 0)
                    #     +
                    #       ifelse(in_obl, (abs(t.chosen_one[['oblacnost']] - t.train_set[[j,'oblacnost']]) * 100 / scale.scale[['oblacnost']]) * pod_obl, 0)
                    #     +
                    #       ifelse(in_tep, (abs(t.chosen_one[['teplota']] - t.train_set[[j,'teplota']]) * 100 / scale.scale[['teplota']]) * pod_tep, 0)
                    #     +
                    #       ifelse(in_vie, (abs(t.chosen_one[['vietor']] - t.train_set[[j,'vietor']]) * 100 / scale.scale[['vietor']]) * pod_vie, 0)
                    #     # + 
                    #     #   ifelse(in_vlh, (abs(t.chosen_one[['vlhkost']] - t.train_set[[j,'vlhkost']]) * 100 / scale.scale[['vlhkost']]) * pod_vlh, 0)
                    #     # +
                    #     #   ifelse(in_tla, (abs(t.chosen_one[['tlak']] - t.train_set[[j,'tlak']]) * 100 / scale.scale[['tlak']]) * pod_tla, 0)
                    #     +
                    #       ifelse(in_dlz, (abs(t.chosen_one[['dlzkadna']] - t.train_set[[j,'dlzkadna']]) * 100 / scale.scale[['dlzkadna']]) * pod_dlz, 0)
                    #     # +
                    #     #   ifelse(in_azi, (abs(t.chosen_one[['azim']] - t.train_set[[j,'azim']]) * 100 / scale.scale[['azim']]) * pod_azi, 0)
                    #     # +
                    #     #   ifelse(in_ele, (abs(t.chosen_one[['elev']] - t.train_set[[j,'elev']]) * 100 / scale.scale[['elev']]) * pod_ele, 0)
                    #   ) 
                    # }
                    # 
                    # t.train_set['diff'] <- t.differ
                    # t.ordered <- arrange(t.train_set, t.train_set[,'diff'])
                    # t.train_set <- t.ordered[1:i.tn_velkost,]
                    

                    actual <- vector.add(actual, t.chosen_one[1,'praca'])
                    
                    forest <- randomForest(formula, data=t.train_set, ntree = i.ntree, mtry = i.mtry, importance=TRUE, proximity=TRUE)
                    output <-  vector.add(output, predict(forest, t.chosen_one[1,3:ncol(t.chosen_one)], type="response", norm.votes=TRUE))
                    #plot(f.forest, type="l")
                    
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
                    if (exists("db.con")) dbDisconnect(db.con)
                    db.con <- getConnection(db.drv)
                    db.result <- dbGetQuery(db.con, build_insert_stats(e, stats, time.start, i.fve))
                    dbDisconnect(db.con)
                    #print("insert")
                  }
                  
                  
                } # for (i.mtry in e.mtry)
                
              } # for (i.ntree in e.ntree)
              
            } # for (i.tn_velkost in e.tm_velkost)
            
          } #for (i.fve in e.fve)
          
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

