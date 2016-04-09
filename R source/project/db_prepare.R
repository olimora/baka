library(insol)
library(RPostgreSQL)

# nacitanie ovladaca, vytvorenie spojenia s databazou, 
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "test_db", host = "localhost", 
                 port = 5432, user = "postgres", password = "password")

# pripravit tabulky
df_postgres <- dbGetQuery(con, "SELECT pripravit_tabulky_zaklad();");

#naplnenie t_solar_hod
lat <- 48.593646
long <- 19.374655
tmz <- 0

all_hours <- seq(ISOdate(2014,7,1,00), ISOdate(2015,11,1,00),by='hour')
sun_pos <- sunpos(sunvector(JD(all_hours), lat, long, tmz))
sun_pos <- cbind(sun_pos, elev = 90 - sun_pos[,'zenith'])
sun_pos <- round(sun_pos, 6)
sun_pos <- data.frame(sun_pos, time = as.character(all_hours))

insert_query <- " INSERT INTO t_solar_hod (cas, azim, elev) VALUES "
for (i in 1:nrow(sun_pos)) {
  insert_query <- paste0(insert_query, 
                         sprintf("('%s', %f, %f),",
                                 sun_pos[i,'time'], 
                                 sun_pos[i,'azimuth'],
                                 sun_pos[i,'elev']))
}
df_postgres <- dbGetQuery(con, gsub(",$", ";", insert_query))

#naplnenie t_solar_den
all_days <- seq(ISOdate(2014,7,1),ISOdate(2015,11,1),by='day')
day_length <- daylength(lat, long, JD(all_days), tmz)[,3]
to_insert <- data.frame(date = as.character(as.Date(all_days)), day_length)

insert_query <- " INSERT INTO t_solar_den (datum, dlzkadna) VALUES "
for (i in 1:nrow(to_insert)) {
  insert_query <- paste0(insert_query, 
                         sprintf("('%s', %f),",
                                 to_insert[i,'date'],
                                 to_insert[i,'day_length']))
}
df_postgres <- dbGetQuery(con, gsub(",$", ";", insert_query))

# pripravit tabulky pre experimenty
df_postgres <- dbGetQuery(con, "SELECT pripravit_tabulky_experimenty();");

dbDisconnect(con)
dbUnloadDriver(drv)