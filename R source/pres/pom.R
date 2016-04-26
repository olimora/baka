
db.drv <- dbDriver("PostgreSQL")
if (exists("db.con")) dbDisconnect(db.con)
db.con <- getConnection(db.drv)

obsah <- dbGetQuery(db.con, "SELECT * FROM v_data_all WHERE fve = 1 
and datum >= to_date('01-07-2015', 'DD-MM-YYYY')
and datum <= to_date('31-07-2015', 'DD-MM-YYYY')")

write.csv2(obsah, 'C:/Users/Oliver/Documents/GitHub/baka/dub1_jul15.csv')