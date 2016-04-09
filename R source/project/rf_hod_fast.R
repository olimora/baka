library(RPostgreSQL)
library(plyr)
library(randomForest)
library(snow)
#c1 <- makeCluster(detectCores(), type='SOCK')
# test cluster nodes to see name and machine
#clusterCall(c1, function() Sys.info()[c("nodename","machine")])

# registerDoSEQ()



source('~/GitHub/baka/R source/project/functions.R')

write_results <- F
vzorka <- 0
e.fve <- c(1, 2, 3)[2]
e.tm_velkost <- c(30)
e.ntree <- 500
e.mtry <- c(2)
e.pod_gho <- c(220)
e.pod_obl <- c(85)
e.pod_tep <- c(30)
e.pod_vie <- c(5)
e.pod_dlz <- c(50)
e.pod_ele <- c(100)

prog.diff <- 10

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
    
    e.tm_select_datum <- " SELECT DISTINCT datum FROM v_data WHERE fve = %d ORDER BY datum "
    
    e.tm_select_hod <- " SELECT praca, gho, (100 - oblacnost) oblacnost, (50 + teplota) teplota, vietor, (100-vlhkost) vlhkost, dlzkadna, elev 
    FROM v_data WHERE fve = %d AND datum = '%s' ORDER BY cas "
    
    e.tm_select_all <- " SELECT datum, cas, praca, gho, (100-oblacnost) oblacnost, 
    (teplota + 50) teplota, vietor, (100 - vlhkost) vlhkost, dlzkadna, elev
    FROM v_data WHERE fve = %d ORDER BY datum, cas"
    
    e.tm_select_scale <- " SELECT gho, (100-oblacnost) oblacnost, 
    (teplota + 50) teplota, vietor, (100 - vlhkost) vlhkost, dlzkadna, elev
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
  vec.length <- 0
  
}

if (exists("db.con")) dbDisconnect(db.con)
db.con <- getConnection(db.drv)

if (!is.null(vzorka) && vzorka > 0) { # ak predikujemlen pre vzorku nahodnych
  vec.length <- vzorka
} else {                              # ak predikujemlen pre vsetky dni
  vec.length <- dbGetQuery(db.con, "SELECT count(*) FROM " %s%
                               "(SELECT DISTINCT datum FROM t_produkcia WHERE fve = " %s% e.fve[1] %s% 
                               ifelse(length(e.fve) > 1, " OR fve = " %s% e.fve[2], "") %s%
                               ifelse(length(e.fve) > 2, " OR fve = " %s% e.fve[3], "") %s% ") s")
  vec.length <- vec.length$count
}
prog.baseAll <- vec.length * length(e.fve) * length(e.tm_velkost) * length(e.ntree) * length(e.mtry) * 
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
              
              # vytiahnem vsetky dni
              t.all_hours <- dbGetQuery(db.con, sprintf(e.tm_select_all, i.fve))
              t.ncol <- ncol(t.all_hours)
              scale.maxims <- sapply(t.all_hours[,4:t.ncol], max)
              scale.minims <- sapply(t.all_hours[,4:t.ncol], min)
              scale.scale <- abs(scale.maxims - scale.minims)
              t.all_hours <- data.matrix(t.all_hours)
              t.all_days <- unique(t.all_hours[,'datum'])

              for (i.tn_velkost in e.tm_velkost) {
                e$tm_velkost <- i.tn_velkost
                e$tm_opis <- sprintf(e.tm_opis, i.tn_velkost, e$fve[i.fve])
                e$param1 <- "tm_velk " %s% i.tn_velkost
                
                for (i.ntree in e.ntree) { 
                  e$param2 <- "ntree " %s% i.ntree
                  
                  for (i.mtry in e.mtry) {
                    e$param3 <- "mtry " %s% i.mtry
                    
                    {
                      # actual <- c()
                      # output <- c()
                      actual <- vector(mode = "numeric", length = vec.length)
                      output <- vector(mode = "numeric", length = vec.length)
                      vec.i <- 1
                      
                      for (day in t.all_days) {
                        
                        t.chosen_hours <- t.all_hours[t.all_hours[,'datum'] == day,]
                        t.chosen_hours_df <- as.data.frame(t.chosen_hours)
                        t.train_set <- t.all_hours[t.all_hours[,'datum'] != day,]
                        output_hod <- vector(mode = "numeric", length = nrow(t.chosen_hours))

                        
                        for (hour in 1:nrow(t.chosen_hours)) {
                          t.chosen_one <- t.chosen_hours[hour,]
                          
                          # vypocitat rozdielnost
                          t.differ <- vector(mode = "numeric", length = nrow(t.train_set))
                          for (j in 1:nrow(t.train_set)) { #nrow(t.train_set)
                            t.differ[j] <- (
                              ifelse(in_gho, (abs(t.chosen_one[['gho']] - t.train_set[[j,'gho']]) * 100 / scale.scale[['gho']]) * pod_gho, 0)
                              +
                                ifelse(in_obl, (abs(t.chosen_one[['oblacnost']] - t.train_set[[j,'oblacnost']]) * 100 / scale.scale[['oblacnost']]) * pod_obl, 0)
                              +
                                ifelse(in_tep, (abs(t.chosen_one[['teplota']] - t.train_set[[j,'teplota']]) * 100 / scale.scale[['teplota']]) * pod_tep, 0)
                              +
                                ifelse(in_vie, (abs(t.chosen_one[['vietor']] - t.train_set[[j,'vietor']]) * 100 / scale.scale[['vietor']]) * pod_vie, 0)
                              # +
                              #   ifelse(in_vlh, (abs(t.chosen_one[['vlhkost']] - t.train_set[[j,'vlhkost']]) * 100 / scale.scale[['vlhkost']]) * pod_vlh, 0)
                              # +
                              #   ifelse(in_tla, (abs(t.chosen_one[['tlak']] - t.train_set[[j,'tlak']]) * 100 / scale.scale[['tlak']]) * pod_tla, 0)
                              +
                                ifelse(in_dlz, (abs(t.chosen_one[['dlzkadna']] - t.train_set[[j,'dlzkadna']]) * 100 / scale.scale[['dlzkadna']]) * pod_dlz, 0)
                              # +
                              #   ifelse(in_azi, (abs(t.chosen_one[['azim']] - t.train_set[[j,'azim']]) * 100 / scale.scale[['azim']]) * pod_azi, 0)
                              +
                                ifelse(in_ele, (abs(t.chosen_one[['elev']] - t.train_set[[j,'elev']]) * 100 / scale.scale[['elev']]) * pod_ele, 0)
                            )
                          }
                          
                          ordered <- t.train_set[order(t.differ),]
                          t.train_set <- ordered[1:i.tn_velkost,]
                          t.train_set_df <- as.data.frame(t.train_set[,3:10])

                          forest <- randomForest(as.factor(praca)~gho+oblacnost+teplota+vietor+vlhkost+dlzkadna+elev, 
                                                 data=t.train_set, ntree = i.ntree, mtry = i.mtry, importance=F, proximity=F)
                          output_hod[hour] <- predict(forest, t.chosen_hours_df[hour,4:10], type="response", norm.votes=TRUE)
                          
                        }
                        actual[vec.i] <- sum(t.chosen_hours[,'praca'])
                        output[vec.i] <- sum(output_hod)
                        vec.i <- vec.i + 1
                        
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


if (exists("db.con")) dbDisconnect(db.con)
dbUnloadDriver(db.drv)

