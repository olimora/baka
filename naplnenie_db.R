# script na naplnanie databazy

install.packages("RPostgreSQL")
require("RPostgreSQL")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "password"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "test_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password

# check for the cartable
dbExistsTable(con, "cartable")
# TRUE

df_postgres <- dbGetQuery(con, "SELECT * from t_lokalita")

dbWriteTable(con, "t_predpoved_hodina", pripravene, append=TRUE, row.names=FALSE)

# close the connection
dbDisconnect(con)
dbUnloadDriver(drv)
