
#install.packages("stringr")
library(stringr)

library(RColorBrewer)


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

# upload minutes

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


# I confine the dataset to just those instances that have height and weight data
# clustering will otherwise not work
Complete <- data[complete.cases(data[,6:7]),] # 69 cases where data is missing

# I run k-means clustering with six clusters
Cluster <- kmeans(Complete[,6:7], 6)

# I add their cluster assignments to the dataframe
Complete$Cluster <- Cluster$cluster

# I create a vector that assigns colors based on the cluster assignment
colors <- rep("red", nrow(Complete))
colors[Complete$Cluster == 2] <- "orange"
colors[Complete$Cluster == 3] <- "darkgreen"
colors[Complete$Cluster == 4] <- "blue"
colors[Complete$Cluster == 5] <- "purple"
colors[Complete$Cluster == 6] <- "pink"
colors[Complete$Cluster == 7] <- "brown"
colors[Complete$Cluster == 8] <- "black"
colors[Complete$Cluster == 9] <- "gold"


plot(Complete$Height, Complete$Weight, cex = 0.5, 
     pch = "", col = colors, 
     main = "Height x Weight 6 Clusters, International Rugby", xlab = "Weight (kg)", ylab = "Height (cm)")
text(Complete$Height, Complete$Weight, cex = 0.5, 
     labels = as.character(Complete$PosNumber), col = colors)




########### backs #############
backs <- Complete[Complete$PosNumber == 9 | Complete$PosNumber == 10 | Complete$PosNumber == 11 | Complete$PosNumber == 12 | Complete$PosNumber == 13 | Complete$PosNumber == 14 | Complete$PosNumber == 15, ]

backCluster <- kmeans(backs[,6:7], 3)

backs$Cluster <- backCluster$cluster

backColors <- rep("red", nrow(backs))
backColors[backs$Cluster == 2] <- "orange"
backColors[backs$Cluster == 3] <- "darkgreen"
backColors[backs$Cluster == 4] <- "blue"
backColors[backs$Cluster == 5] <- "purple"
backColors[backs$Cluster == 6] <- "pink"
backColors[backs$Cluster == 7] <- "brown"



plot(backs$Height, backs$Weight, cex = 0.5, 
     pch = "", col = backColors, 
     main = "Height x Weight 6 Clusters, International Rugby BACKS", xlab = "Weight (kg)", ylab = "Height (cm)")
text(backs$Height, backs$Weight, cex = 0.5, 
     labels = as.character(backs$PosNumber), col = backColors)

table(backs$PosNumber, backs$Cluster)
backSequential <- brewer.pal(7, "Spectral")
barplot(table(backs$Cluster, backs$PosNumber), col = backSequential, 
        main = "Cluster Distribution by Position, 4 Clusters")







table(Complete$PosNumber, Complete$Cluster)

sequential <- brewer.pal(7, "Spectral")
barplot(table(Complete$Cluster, Complete$PosNumber), col = sequential, 
        main = "Cluster Distribution by Position, 7 Clusters")







plot(data$Metres, data$Runs, cex = 0.5, 
     pch = "", col = colors, 
     main = "Super Rugby", xlab = "Metres", ylab = "Runs")
text(data$Metres, data$Runs, labels = as.character(Complete$PosNumber), cex = 0.5)
abline(a = 0, b = (1/3.149), col = "red")




write.csv(data, "superData.csv")




superTacklesbyPos <- NA

for(i in 1:15){
  superTacklesbyPos[i] <- mean(data$Tackles[data$PosNumber == i], na.rm = TRUE)
}

barplot(superTacklesbyPos, names.arg = c(1:15), col = "brown1", main = "Tackles Made by Position, Super Rugby")

boxplot(data$Tackles ~ data$PosNumber, xlab = "Position", ylab = "Tackles", 
        main = "Distribution of Tackles Made by Position, Super Rugby")
