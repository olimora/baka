# script na naplnanie databazy

# nainstalovanie baliku - potrebne len raz
#install.packages("RPostgreSQL")
#require("RPostgreSQL")

# nacitanie ovladaca
drv <- dbDriver("PostgreSQL")
# vytvorenie spojenia s databazou
con <- dbConnect(drv, dbname = "test_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "password")

# overenie spojenia
dbExistsTable(con, "cartable")

# naplnenie databazy predpovedami modelu Aladin

df_postgres <- dbGetQuery(con, "DELETE FROM t_predpoved_import;")
# ulozenie absolutnej cesty do premennej
path <- 'C:\\000\\skola\\baka\\data\\Aladin\\gho'

# ulozenie zoznamu podpriecinkov
directories<-list.files(path)

# pre kazdy podpriecinok
for(i in 1:length(directories)) {
  # ulozenie cesty k podpriecinku
  dir_path <- paste(path, directories[i], sep='\\')
  # ulozenie zoznamu suborov v podpriecinku
  files<-list.files(dir_path)
  # pre kazdy subor v podpriecinku
  for(j in 1:length(files)) {
    # ulozenie adresy suboru
    file_path <- paste(dir_path, files[j], sep='\\')
    # ulozenie volania databazovej funkcie pre konkretny subor
    select_import_func <- paste("SELECT import_aladin_csv('", file_path, "');", sep="")
    # zavolanie funkcie na databaze
    df_postgres <- dbGetQuery(con, select_import_func)
  }
}

# priklad vysledneho volanie funkcie na importovanie udajov zo suboru:
# "SELECT import_aladin_csv('C:\\000\\skola\\baka\\data\\Aladin\\gho\\2015-10\\ALADIN-DUB_48.587_19.369-2015-10.csv');"

df_postgres <- dbGetQuery(con, "DELETE FROM t_produkcia_import;")

start_time <- Sys.time()

# cesta a priecinky
path <- 'C:\\000\\skola\\baka\\data\\fve'
directories<-list.files(path)

# pre kazdy priecinok
for(i in 1:length(directories)) {
  dir_path <- paste(path, directories[i], sep='\\')
  files<-list.files(dir_path)
  # pre kazdy subor v priecinku
  for(j in 1:length(files)) {
    file_path <- paste(dir_path, files[j], sep='\\')
    # nacitanie csvsuboru
    content_of_file <- read.csv2(file_path, row.names=NULL)
    # vybranie iba riadkov s datami a stlpcov Cas a Vykon
    only_wanted <- content_of_file[c(3:nrow(content_of_file)),c(1,3)]
    
    # priprava INSERT-u pre databazu
    giant_insert <- "INSERT INTO t_produkcia_import (in_cas, in_vykon, in_fve) VALUES "
    m <- nrow(only_wanted) - 1 
    for(k in 1:m) {
      giant_insert <- paste(giant_insert, "('",only_wanted[k,1], "','", 
                            gsub(",", ".", only_wanted[k,2]), "','", directories[i], "'),", sep="")
    }
    m <- m + 1
    giant_insert <- paste(giant_insert, "('",only_wanted[m,1], "','", 
                          gsub(",", ".", only_wanted[m,2]), "','", directories[i], "');", sep="")
    # zavolanie INSERT-u
    df_postgres <- dbGetQuery(con, giant_insert)
  }
}

end_time <- Sys.time()
print(difftime(end_time, start_time, units="mins"))

df_postgres <- dbGetQuery(con, "SELECT import_fve_csv();")



# zatvorenie spojenia a uvolnenie ovladacu
dbDisconnect(con)
dbUnloadDriver(drv)
