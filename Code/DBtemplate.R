#DB processing code template
library(RSQLite)
#any additional libraries needed for analysis

#Connect the DB
setwd("~/Documents/School/GradSchool/Sap Flow")
db <- dbConnect(SQLite(), dbname="SapFlow")
initExtension(db)

#Pull data with - change brakets!
data <- dbGetQuery("SELECT [columns] FROM [table] [additional requirements]")

#Processing etc goes here, use dbSendQuery to change data in the db

#When done:
dbDisconnect(db)
