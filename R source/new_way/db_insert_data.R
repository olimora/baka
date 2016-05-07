# script na naplnanie databazy
#vymaze vsetko okrem funkcii
rm(list = setdiff(ls(), lsf.str()))
#na meranie casu
start_time_all <- Sys.time()
# nainstalovanie baliku - potrebne len raz
#install.packages("RPostgreSQL")
library("RPostgreSQL")

# nacitanie ovladaca, vytvorenie spojenia s databazou, 
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "test2_db", host = "localhost", 
                 port = 5432, user = "postgres", password = "password")

# pripravit tabulky
start_time <- Sys.time()
df_postgres <- dbGetQuery(con, " SELECT pripravit_tabulky_data();");
end_time <- Sys.time()
print("con a pripravit tabulky")
print(difftime(end_time, start_time, units="sec"))

# naplnenie databazy predpovedami modelu Aladin
start_time <- Sys.time()
path <- 'C:/000/skola/baka/data/Aladin/gho'
files <- list.files(path, recursive = T, full.names = T)
for(i in 1:length(files)) {
  # df_postgres <- dbGetQuery(con, paste0(" SELECT import_aladin_csv('", files[i], "');"))
  df_postgres <- dbGetQuery(con, sprintf(" SELECT import_aladin_csv('%s');", files[i]))
}
end_time <- Sys.time()
print("import aladin")
print(difftime(end_time, start_time, units="sec"))

start_time <- Sys.time()
path <- 'C:/000/skola/baka/data/fve'
files <- list.files(path, recursive = T, full.names = T)
for(i in 1:length(files)) {
  content_of_file <- read.csv2(files[i], row.names=NULL)
  # vybranie iba riadkov s datami a stlpcov Cas, vykon a praca
  to_insert <- content_of_file[c(3:nrow(content_of_file)),c(1,9)]
  to_insert[,2] <- as.numeric(as.character(to_insert[,2]))
  colnames(to_insert) <- c('time', 'work')
  # vytiahnutie elektrarne
  elektraren <- sub("/Ktab.*", "", sub(".*fve/", "", files[i]))
  # vyskladanie insertu
  insert_query <- " INSERT INTO t_produkcia_import 
  (in_cas, in_praca, in_fve) VALUES "
  prev_work <- to_insert[1,'work']
  for(j in 1:nrow(to_insert)) {
    insert_query <- paste0(insert_query,
                           sprintf("('%s', %d, '%s'),",
                                   to_insert[j, 'time'],
                                   to_insert[j, 'work'] - prev_work,
                                   elektraren))
    prev_work <- to_insert[j,'work']
  }
  df_postgres <- dbGetQuery(con, gsub(",$", ";", insert_query))
}
df_postgres <- dbGetQuery(con, " SELECT import_fve_csv();")

end_time <- Sys.time()
print("import fve")
print(difftime(end_time, start_time, units="sec"))

# vymazanie nepotrebnych a chybnych dat
start_time <- Sys.time()
df_postgres <- dbGetQuery(con, " SELECT delete_abundant_data();")
end_time <- Sys.time()
print("delete data")
print(difftime(end_time, start_time, units="sec"))

end_time_all <- Sys.time()
print("all")
print(difftime(end_time_all, start_time_all, units="sec"))
print(difftime(end_time_all, start_time_all, units="mins"))

dbDisconnect(con)
dbUnloadDriver(drv)
rm(list = setdiff(ls(), lsf.str()))