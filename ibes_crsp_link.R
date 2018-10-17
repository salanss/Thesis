library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user=rstudioapi::askForPassword("Database username"),
                  password=rstudioapi::askForPassword("Database password"))


