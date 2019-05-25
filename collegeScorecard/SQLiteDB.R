# Jason Dean
# March 14, 2017
# This script downloads the college scorecard data and creates a SQLite database
# The data, entitled 'MERGED2015_16_PP.csv' (too big for github) can be downloaded here:
# https://collegescorecard.ed.gov/data/
# by selecting:  'Most recent data'

# Create College Scorecard Database in RAM
college.db <- dbConnect(SQLite(), dbname = ":memory:")
dskdb <- dbConnect(SQLite(), dbname = "collegeScorecard.sqlite")
sqliteCopyDatabase(dskdb, college.db)
dbDisconnect(dskdb)

# Add data tables to the DB
college.data <- read.csv('rawData/MERGED2015_16_PP.csv', header=TRUE)
college.data <- college.data[,-c(442:1350)]
dbWriteTable(college.db, "collegeData", college.data)
dbListTables(college.db)
