library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user=rstudioapi::askForPassword("Database username"),
                  password=rstudioapi::askForPassword("Database password"))

res <- dbSendQuery(wrds, "select distinct table_schema
                   from information_schema.tables
                   where table_type ='VIEW'
                   order by table_schema")
data_all <- dbFetch(res, n=-1)
dbClearResult(res)
data_all

res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='ibes'
                   order by table_name")
ibes <- dbFetch(res, n=-1)
dbClearResult(res)
ibes

res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='ibes'
                   and table_name='recddet'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

res <- dbSendQuery(wrds, "SELECT * FROM ibes.det_epsus")
data <- dbFetch(res, n=100)
dbClearResult(res)
library(tidyverse)
data <- as.tbl(data)
