#Sap Flow Code Draft

#DB processing code template
library(RSQLite)

#Connect the DB
setwd("~/Documents/School/GradSchool/Sap Flow")
db <- dbConnect(SQLite(), dbname="SapFlow")
initExtension(db)






#When done:
dbDisconnect(db)
