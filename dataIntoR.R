# get the forest data into R, through a MonetDB database
library(MonetDBLite)
library(DBI)

dbfolder <- "~/Data/forest/MonetDB"
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )

dataloc <- "~/Data/forest/covtype.csv"
tablename <- "forest"

# read in data types
headers <- read.csv(dataloc, nrows = 500)
cl <- sapply( headers , class )
names( headers ) <- tolower( names( headers ) )
colTypes <- ifelse( cl == 'integer' , 'INT' , 'VARCHAR(255)' )
colDecl <- paste( names( headers ) , colTypes )
sql <-
  sprintf(
    paste(
      "CREATE TABLE" ,
      tablename ,
      "(%s)"
    ) ,
    paste(
      colDecl ,
      collapse = ", "
    )
  )
dbSendQuery( db , sql )

# read in data
dbSendQuery( 
  db , 
  paste0( 
    "copy offset 2 into " , 
    tablename , 
    " from '" , 
    normalizePath( dataloc ) , 
    "' using delimiters ',','\\n','\"'  NULL AS ''" 
  ) 
) 

gc()
dbDisconnect( db , shutdown = TRUE )

