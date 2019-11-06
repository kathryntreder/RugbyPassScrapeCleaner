#install.packages("stringr")
library(stringr)

library(RColorBrewer)


# read in all of the scraped data

blu <- read.csv("blues_Scrape10162019.csv")
bru <- read.csv("brumbies_Scrape10162019.csv")
bul <- read.csv("bulls_Scrape10162019.csv")
chi <- read.csv("chiefs_Scrape10162019.csv")
cru <- read.csv("crusaders_Scrape10172019.csv")
hil <- read.csv("highlanders_Scrape10172019.csv")
hur <- read.csv("hurricanes_Scrape10172019.csv")
jag <- read.csv("jaguares_Scrape10172019.csv")
lio <- read.csv("lions_Scrape10172019.csv")
reb <- read.csv("rebels_Scrape10172019.csv")
red <- read.csv("reds_Scrape10172019.csv")
sha <- read.csv("sharks_Scrape10172019.csv")
sto <- read.csv("stormers_Scrape10172019.csv")
sun <- read.csv("sunwolves_Scrape10172019.csv")
war <- read.csv("waratahs_Scrape10182019.csv")


# merge stats
stats <- rbind(blu, bru, bul, chi, cru, hil, hur, jag, lio, reb, red, sha, sto, sun, war)

# upload minutes

bluMin <- read.csv("blues_Minutes_Games_Played_Scrape10252019.csv")
bruMin <- read.csv("brumbies_Minutes_Games_Played_Scrape10252019.csv")
bulMin <- read.csv("bulls_Minutes_Games_Played_Scrape10252019.csv")
chiMin <- read.csv("chiefs_Minutes_Games_Played_Scrape10252019.csv")
cruMin <- read.csv("crusaders_Minutes_Games_Played_Scrape10262019.csv")
hilMin <- read.csv("highlanders_Minutes_Games_Played_Scrape10262019.csv")
hurMin <- read.csv("hurricanes_Minutes_Games_Played_Scrape10262019.csv")
jagMin <- read.csv("jaguares_Minutes_Games_Played_Scrape10282019.csv")
lioMin <- read.csv("lions_Minutes_Games_Played_Scrape10282019.csv")
rebMin <- read.csv("rebels_Minutes_Games_Played_Scrape10282019.csv")
redMin <- read.csv("reds_Minutes_Games_Played_Scrape10282019.csv")
shaMin <- read.csv("sharks_Minutes_Games_Played_Scrape10282019.csv")
stoMin <- read.csv("stormers_Minutes_Games_Played_Scrape10282019.csv")
sunMin <- read.csv("sunwolves_Minutes_Games_Played_Scrape10282019.csv")
warMin <- read.csv("waratahs_Minutes_Games_Played_Scrape10282019.csv")

# merge minutes data

minutes <- rbind(bluMin, bruMin, bulMin, chiMin, cruMin, hilMin, hurMin, jagMin, lioMin, rebMin, redMin, shaMin, stoMin, sunMin, warMin)


# merge stats and minutes data
data <- merge(stats, minutes, by = c("Name", "Year", "Team"))


# create a numberic attribute for each player position
data$PosNumber <- NA

for(i in 1:nrow(data)){
  
  if(data$Position[i] == "Loose Head Prop"){
    data$PosNumber[i] <- 1
  } else if(data$Position[i] == "Hooker"){
    data$PosNumber[i] <- 2
  } else if(data$Position[i] == "Tight Head Prop"){
    data$PosNumber[i] <- 3
  } else if(data$Position[i] == "Lock 4"){
    data$PosNumber[i] <- 4
  } else if(data$Position[i] == "Lock 5"){
    data$PosNumber[i] <- 5
  } else if(data$Position[i] == "Blindside Flanker"){
    data$PosNumber[i] <- 6
  } else if(data$Position[i] == "Openside Flanker"){
    data$PosNumber[i] <- 7
  } else if(data$Position[i] == "Number 8"){
    data$PosNumber[i] <- 8
  } else if(data$Position[i] == "Scrum Half"){
    data$PosNumber[i] <- 9
  } else if(data$Position[i] == "Fly Half"){
    data$PosNumber[i] <- 10
  } else if(data$Position[i] == "Left Wing"){
    data$PosNumber[i] <- 11
  } else if(data$Position[i] == "Inside Centre"){
    data$PosNumber[i] <- 12
  } else if(data$Position[i] == "Outside Centre"){
    data$PosNumber[i] <- 13
  } else if(data$Position[i] == "Right Wing"){
    data$PosNumber[i] <- 14
  } else {
    data$PosNumber[i] <- 15
  }
}  


# take letters off of weight and change to integer
data$Weight <- str_remove_all(data$Weight, "[kg]")
data$Weight <- as.integer(data$Weight)

# take letters off of height and change to integer
data$Height <- str_remove_all(data$Height, "[cm]")
data$Height <- as.integer(data$Height)

# calculate and create value for meters per run 
data$MeteresPerRun <- data$Metres / data$Runs


# correct the ages for each year
for(i in 1:nrow(data)){
  yearDiff <- 2019 - data$Year[i]
  data$CorrectAge[i] <- data$Age[i] - yearDiff
}


# split the teams played for into 3 separate columns
for(i in 1:nrow(data)){
  data$Team1[i] <- strsplit(as.character(data$Teams[i]), split="          ")[[1]][1]
  data$Team2[i] <- strsplit(as.character(data$Teams[i]), split="          ")[[1]][2]
  data$Team3[i] <- strsplit(as.character(data$Teams[i]), split="          ")[[1]][3]
}



write.csv(data, "CLEAN_SuperRugbyComplete.csv")