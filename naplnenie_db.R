# script na naplnanie databazy
start_time_all <- Sys.time()
start_time <- Sys.time()
# nainstalovanie baliku - potrebne len raz
#install.packages("RPostgreSQL")
require("RPostgreSQL")

# nacitanie ovladaca, vytvorenie spojenia s databazou, 
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "test_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "password")

# pripravit tabulky
df_postgres <- dbGetQuery(con, "SELECT pripravit_tabulky();");

end_time <- Sys.time()
print("conn a pripravit tabulky")
print(difftime(end_time, start_time, units="sec"))
print(difftime(end_time, start_time, units="mins"))

start_time <- Sys.time()
# naplnenie databazy predpovedami modelu Aladin
path <- 'C:\\000\\skola\\baka\\data\\Aladin\\gho'
directories<-list.files(path)

for(i in 1:length(directories)) {
  dir_path <- paste(path, directories[i], sep='\\')
  files<-list.files(dir_path)
  for(j in 1:length(files)) {
    file_path <- paste(dir_path, files[j], sep='\\')
    select_import_func <- paste(" SELECT import_aladin_csv('", file_path, "');", sep="")
    df_postgres <- dbGetQuery(con, select_import_func)
  }
}

end_time <- Sys.time()
print("import aladin")
print(difftime(end_time, start_time, units="sec"))
print(difftime(end_time, start_time, units="mins"))

# priklad vysledneho volanie funkcie na importovanie udajov zo suboru:
# "SELECT import_aladin_csv('C:\\000\\skola\\baka\\data\\Aladin\\gho\\2015-10\\ALADIN-DUB_48.587_19.369-2015-10.csv');"

start_time <- Sys.time()

path <- 'C:\\000\\skola\\baka\\data\\fve'
directories<-list.files(path)

for(i in 1:length(directories)) { #length(directories)
  dir_path <- paste(path, directories[i], sep='\\')
  files<-list.files(dir_path)
  for(j in 1:length(files)) { #length(files)
    file_path <- paste(dir_path, files[j], sep='\\')
    content_of_file <- read.csv2(file_path, row.names=NULL)
    # vybranie iba riadkov s datami a stlpcov Cas a Vykon
    only_wanted <- content_of_file[c(3:nrow(content_of_file)),c(1,3)]
    giant_insert <- " INSERT INTO t_produkcia_import (in_cas, in_vykon, in_fve) VALUES "
    for(k in 1:nrow(only_wanted)) {
      giant_insert <- paste(giant_insert, "('",only_wanted[k,1], "','", 
                            gsub(",", ".", only_wanted[k,2]), "','", directories[i], "'),", sep="")
    }
    #nahradit poslendnu ciarku bodkociarkou
    giant_insert <- gsub(",$", ";", giant_insert) 
    df_postgres <- dbGetQuery(con, giant_insert)
  }
}

df_postgres <- dbGetQuery(con, " SELECT import_fve_csv();")

end_time <- Sys.time()
print("import fve")
print(difftime(end_time, start_time, units="sec"))
print(difftime(end_time, start_time, units="mins"))

end_time_all <- Sys.time()
print("all")
print(difftime(end_time_all, start_time_all, units="sec"))
print(difftime(end_time_all, start_time_all, units="mins"))


# zatvorenie spojenia a uvolnenie ovladacu

dbDisconnect(con)
#dbUnloadDriver(drv)
