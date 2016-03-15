# script na naplnanie databazy
start_time_all <- Sys.time()
start_time <- Sys.time()
# nainstalovanie baliku - potrebne len raz
#install.packages("RPostgreSQL")
#require("RPostgreSQL")

# nacitanie ovladaca, vytvorenie spojenia s databazou, 
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "test_db", host = "localhost", 
                 port = 5432, user = "postgres", password = "password")

# pripravit tabulky
df_postgres <- dbGetQuery(con, "SELECT pripravit_tabulky_data();");

end_time <- Sys.time()
print("conn a pripravit tabulky")
print(difftime(end_time, start_time, units="sec"))

start_time <- Sys.time()
# naplnenie databazy predpovedami modelu Aladin
path <- 'C:/000/skola/baka/data/Aladin/gho'
files <- list.files(path, recursive = T, full.names = T)
for(i in 1:length(files)) {
  select_import_func <- paste0("SELECT import_aladin_csv('", files[i], "');")
  #print(select_import_func)
  df_postgres <- dbGetQuery(con, select_import_func)
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
  only_wanted <- content_of_file[c(3:nrow(content_of_file)),c(1,3,9)]
  only_wanted[,3] <- as.numeric(as.character(only_wanted[,3]))
  # vytiahnutie elektraren
  elektraren <- sub(".*fve/", "", files[i])
  elektraren <- sub("/Ktab.*", "", elektraren)
  # vyskladanie insertu
  giant_insert <- " INSERT INTO t_produkcia_import 
                      (in_cas, in_vykon, in_praca, praca, in_fve) VALUES "
  giant_insert <- paste(giant_insert, "('", only_wanted[1,1], "','", 
                        gsub(",", ".", only_wanted[1,2]), "',", 
                        only_wanted[1,3], ",",
                        0, ",'",
                        elektraren, "'),", sep="")
  for(j in 2:nrow(only_wanted)) {
    giant_insert <- paste(giant_insert, "('", only_wanted[j,1], "','", 
                          gsub(",", ".", only_wanted[j,2]), "',", 
                          only_wanted[j,3], ",",
                          only_wanted[j,3] - only_wanted[j-1,3], ",'",
                          elektraren, "'),", sep="")
  }
  #nahradit poslendnu ciarku bodkociarkou a poslat do databzy
  df_postgres <- dbGetQuery(con, gsub(",$", ";", giant_insert))
}
df_postgres <- dbGetQuery(con, " SELECT import_fve_csv();")

end_time <- Sys.time()
print("import fve")
print(difftime(end_time, start_time, units="sec"))
print(difftime(end_time, start_time, units="mins"))

# vymazanie nepotrebnych a chybnych dat
start_time <- Sys.time()

df_postgres <- dbGetQuery(con, " SELECT delete_abundant_data();")

end_time <- Sys.time()
print("delete data")
print(difftime(end_time, start_time, units="sec"))
print(difftime(end_time, start_time, units="mins"))

end_time_all <- Sys.time()
print("all")
print(difftime(end_time_all, start_time_all, units="sec"))
print(difftime(end_time_all, start_time_all, units="mins"))

# zatvorenie spojenia a uvolnenie ovladacu
#dbDisconnect(con)
#dbUnloadDriver(drv)
