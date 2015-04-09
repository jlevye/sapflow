#Water potential calculations
library(RSQLite)

#FUNCTIONS

#Error handling:
#Omit any flagged results
#Date compensation: If date(sapflow) doesn't match date(WP), check +/- 2 (more?) - use try/catch?
#days

#Identify sap flow steady state - look for moving average with lowest SE
#Requires columns TreeID, Sapflow, Time, (Year? JulDay)
steadyStateFlow <- function(df) {
  #Intialize output vectors
  flow <- numeric(nrows(unique(df)))
  start <- numeric(nrows(unique(df)))
  end <- numeric(nrows(unique(df)))
  i <- 1

  #Handle 2002 vs 2003; multiple days?
  #Runs through each tree
  for (tree in unique(df$TreeID)){
    #Set up data
    sub <- df[df$TreeID==tree]
    #Measurements either every 10 min or every 30 min; handles 2 cases to get 1hr intervals
    #TODO: 2003 is slightly different: most trees have 36 measurements; some missing first; some have
    #23 or 24 recorded - need to actually check times if nrows isn't 12 or 36
    if (nrows(sub)==36) {
      n = 36; s = 5
    } else{
      n=12; s = 1
    }

    #Initialize
    j = 1
    min <- 10^6 #Setting as very large number so default value will never be min
    f <- 0
    s <- 0
    f <- 0
    #Runs through 1-hr increments, choses the hour with lowest standard error
    while (j+s<=n){
      if (sd(sub$SapFlow[j:j+s]/sqrt(n))<min) {
        min <- sd(sub$SapFlow[j:j+s]/sqrt(n))
        f <- mean(sub$SapFlow[j:j+s])
        s <- sub$Time[j]
        e <- sub$Time[j+s]
      }
      j <- j + 1
    }
    flow[i] <- f
    start[i] <- s
    end[i] <- e
    i <- i + 1
  }
  #TODO: Add column names
  return(data.frame(tree,flow,start,end))
}


#MAIN METHOD

#Import Data

#Merge/clean data

#Average any waterpotential values
md <- aggregate(PDY, list(TreeID, Location, Year, JulDay), mean, data=md)
pd <- aggregate(PDY, list(TreeID, Location, Year, JulDay), mean, data=pd)

#Calculate steady state
sapflow <- steadystateFlow(data)

#Calculate conductivity

#Compile data table

#Export to database
