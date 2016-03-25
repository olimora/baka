#vymaze vsetko okrem funkcii
rm(list = setdiff(ls(), lsf.str()))
#na meranie casu
start_time_all <- Sys.time()
# nainstalovanie baliku - potrebne len raz
#install.packages("RPostgreSQL")
#install.packages("insol")
library("RPostgreSQL")
library("insol")

# nacitanie ovladaca, vytvorenie spojenia s databazou, 
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "test_db", host = "localhost", 
                 port = 5432, user = "postgres", password = "password")

# pripravit tabulky
start_time <- Sys.time()
df_postgres <- dbGetQuery(con, "SELECT pripravit_tabulky_zaklad();");
end_time <- Sys.time()
print("pripravit tabulky")
print(difftime(end_time, start_time, units="sec"))

#naplnenie t_solar_hod
start_time <- Sys.time()
lat <- 48.593646
long <- 19.374655
tmz <- 1

all_hours <- seq(ISOdate(2014,7,1,00),ISOdate(2015,11,1,00),by='hour')
sun_pos <- sunpos(sunvector(JD(all_hours), lat, long, tmz))
elev <- 90 - sun_pos[,2]
to_insert <- data.frame(sun_pos, elev)
to_insert <- round(to_insert, 6)
to_insert <- data.frame(as.character(all_hours), to_insert)

insert_query <- " INSERT INTO t_solar_hod
(cas, azim, zen, elev) VALUES "
for (i in 1:nrow(to_insert)) {
  insert_query <- paste0(insert_query,
                         "('", to_insert[i,1], "',",
                         to_insert[i,2], ",",
                         to_insert[i,3], ",",
                         to_insert[i,4], "),")
  if ((i %% 100) == 0) {
    df_postgres <- dbGetQuery(con, gsub(",$", ";", insert_query))
    insert_query <- " INSERT INTO t_solar_hod
    (cas, azim, zen, elev) VALUES "
  }
  }
df_postgres <- dbGetQuery(con, gsub(",$", ";", insert_query))

end_time <- Sys.time()
print("naplnit solar_hod tabulky")
print(difftime(end_time, start_time, units="sec"))

#naplnenie t_solar_den
start_time <- Sys.time()
all_days <- seq(ISOdate(2014,7,1),ISOdate(2015,11,1),by='day')
day_length <- daylength(lat, long, JD(all_days), tmz)[,3]
to_insert <- data.frame(as.character(as.Date(all_days)), day_length)

insert_query <- " INSERT INTO t_solar_den 
(datum, dlzkadna) VALUES "
for (i in 1:nrow(to_insert)) {
  insert_query <- paste0(insert_query, 
                         "('", to_insert[i,1], "',", 
                         to_insert[i,2], "),")
}
df_postgres <- dbGetQuery(con, gsub(",$", ";", insert_query))

end_time <- Sys.time()
print("naplnit solar_den tabulky")
print(difftime(end_time, start_time, units="sec"))

# pripravit tabulky pre experimenty
start_time <- Sys.time()
df_postgres <- dbGetQuery(con, "SELECT pripravit_tabulky_experimenty();");
end_time <- Sys.time()
print("pripravit tabulky pre experimenty")
print(difftime(end_time, start_time, units="sec"))

end_time_all <- Sys.time()
print("all")
print(difftime(end_time_all, start_time_all, units="sec"))

dbDisconnect(con)
dbUnloadDriver(drv)
rm(list = setdiff(ls(), lsf.str()))