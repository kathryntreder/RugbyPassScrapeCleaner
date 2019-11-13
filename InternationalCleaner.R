# The purpose of this file is to clean the men's international data that was scraped from the Rugby Pass website

#install.packages("stringr")
library(stringr)


# read in all of the scraped data
arg <- read.csv("argentina_Scrape10182019.csv")
aus <- read.csv("australia_Scrape10182019.csv")
can <- read.csv("canada_Scrape10182019.csv")
eng <- read.csv("england_Scrape10182019.csv")
fiji <- read.csv("fiji_Scrape10242019.csv")
fra<- read.csv("france_Scrape10182019.csv")
ire <- read.csv("ireland_Scrape10182019.csv")
ita <- read.csv("italy_Scrape10242019.csv")
jap <- read.csv("japan_Scrape10182019.csv")
nzl <- read.csv("new-zealand_Scrape10182019.csv")
sco <- read.csv("scotland_Scrape10182019.csv")
sam <- read.csv("samoa_Scrape10182019.csv")
saf <- read.csv("south-africa_Scrape10182019.csv")
ton <- read.csv("tonga_Scrape10182019.csv")
usa <- read.csv("usa_Scrape10182019.csv")
wal <- read.csv("wales_Scrape10182019.csv")

# merge stats
stats <- rbind(arg, aus, can, eng, fiji, fra, ire, ita, jap, nzl, saf, sam, sco, ton, usa, wal)

# upload minutes data
argMin <- read.csv("argentina_Minutes_Games_Played_Scrape10252019.csv")
ausMin <- read.csv("australia_Minutes_Games_Played_Scrape10252019.csv")
canMin <- read.csv("canada_Minutes_Games_Played_Scrape10252019.csv")
engMin <- read.csv("england_Minutes_Games_Played_Scrape10252019.csv")
fijiMin <- read.csv("fiji_Minutes_Games_Played_Scrape10252019.csv")
fraMin <- read.csv("france_Minutes_Games_Played_Scrape10252019.csv")
ireMin <- read.csv("ireland_Minutes_Games_Played_Scrape10252019.csv")
itaMin <- read.csv("italy_Minutes_Games_Played_Scrape10252019.csv")
japMin <- read.csv("japan_Minutes_Games_Played_Scrape10252019.csv")
nzlMin <- read.csv("new-zealand_Minutes_Games_Played_Scrape10252019.csv")
scoMin <- read.csv("scotland_Minutes_Games_Played_Scrape10252019.csv")
samMin <- read.csv("samoa_Minutes_Games_Played_Scrape10252019.csv")
safMin <- read.csv("south-africa_Minutes_Games_Played_Scrape10252019.csv")
tonMin <- read.csv("tonga_Minutes_Games_Played_Scrape10252019.csv")
usaMin <- read.csv("usa_Minutes_Games_Played_Scrape10252019.csv")
walMin <- read.csv("wales_Minutes_Games_Played_Scrape10252019.csv")

# merge minutes data
minutes <- rbind(argMin, ausMin, canMin, engMin, fijiMin, fraMin, ireMin, itaMin, japMin, nzlMin, safMin, samMin, scoMin, tonMin, usaMin, walMin)

# read in the dataset that contains minute and game data on kickers
kickMinutes <- read.csv("MensInternationals_KickerMinutesGames.csv")

# replace the NAs in the minutes data for kickers with the minutes data just uploaded
for(i in 1:nrow(minutes)){
  row <- which(minutes$Name == as.character(kickMinutes$Name[i]) & minutes$Year == kickMinutes$Year[i] & minutes$Team == kickMinutes$Team[i])
  minutes$Minutes.Played[row] <- kickMinutes$Minutes.Played[i]
  minutes$Games[row] <- kickMinutes$Games[i]
}

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


# Export the clean data
write.csv(data, "superData.csv")    
 
