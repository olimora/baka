library(RPostgreSQL)
library(plyr)
library(randomForest)
source('~/GitHub/baka/R source/project/functions.R')

id <- 961

vzorka <- 100
e.fve <- c(1, 2, 3)[2]
e.tm_velkost <- c(110) 
e.ntree <- c(700)
e.mtry <- c(2)
e.pod_gho <- c(0, 30, 60, 90)
e.pod_obl <- c(0, 30, 60, 90)
e.pod_tep <- c(0, 10, 20, 30)
e.pod_vie <- c(0, 1, 3, 5)
e.pod_dlz <- c(0, 10, 20, 30)

prog.diff <- 1

{
    prog.printed_all <- -10000
  prog.printed_part <- -10000
  prog.print_perc_part <- 0
  prog.print_perc_all <- 0
  prog.baseAll <- 0
  prog.basePart <- 0
  prog.i <- 0
  
  db.drv <- dbDriver("PostgreSQL")
  
}



prog.baseAll <- length(e.pod_gho) * length(e.pod_obl) * length(e.pod_tep) * length(e.pod_vie) * length(e.pod_dlz)

if (exists("db.con")) dbDisconnect(db.con)
  db.con <- getConnection(db.drv)

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
          
          qquery <- sprintf(" UPDATE t_experiment SET pod_gho = %d, pod_oblacnost = %d, pod_teplota = %d,
                            pod_vietor = %d, pod_dlzkadna = %d WHERE id = %d;",
                            i.pod_gho, i.pod_obl, i.pod_tep, i.pod_vie, i.pod_dlz, id)
          #print(gsub("\\n", "", qquery))
          updateq <- dbGetQuery(db.con, qquery)
          id <- id+1
          
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
db.con <- getConnection(db.drv)
dbUnloadDriver(db.drv)

