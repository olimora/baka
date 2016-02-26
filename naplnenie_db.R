# script na naplnanie databazy

# nainstalovanie baliku - potrebne len raz
install.packages("RPostgreSQL")
require("RPostgreSQL")

# nacitanie ovladaca
drv <- dbDriver("PostgreSQL")
# vytvorenie spojenia s databazou
con <- dbConnect(drv, dbname = "test_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "password")

# overenie spojenia
dbExistsTable(con, "cartable")

# naplnenie databazy predpovedami modelu Aladin
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

# zatvorenie spojenia a uvolnenie ovladacu
dbDisconnect(con)
dbUnloadDriver(drv)
