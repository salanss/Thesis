library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require')

res <- dbSendQuery(wrds, "SELECT * FROM dataset")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data