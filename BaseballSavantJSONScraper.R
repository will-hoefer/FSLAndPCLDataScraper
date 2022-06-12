library(tidyverse)
library(baseballr)
library(purrr)
library(jsonlite)
library(dplyr)

Yesterday <- Sys.Date()-1
#-----------------------------------------------------------------------------------------------------------
#Here is where we set the dates that we want games scraped from. 
#By default, I use the Yesterday variable declared above, since I am up to date and just want to scrape the previous day's games.
#NOTE: I would advise checking the daily scoreboard for each date you wish to scrape, since a lack of games to scrape throws an error that stops the loop. 
#Mondays are typically the off day for both the FSL and PCL (with a few exceptions). Scraping day to day is the most advisable way to use this script at present.
FSLDatesInput <-seq(as.Date(Yesterday), as.Date(Yesterday), by="days")
FSL <- c("Palm Beach Cardinals","Fort Myers Mighty Mussels", "Jupiter Hammerheads", "Bradenton Marauders", 
         "Lakeland Flying Tigers", "Dunedin Blue Jays", "St. Lucie Mets", "Tampa Tarpons",
         "Clearwater Threshers")

#Iterating through get_game_pks with date(s) to get a list of gameIds
for (i in seq_along(FSLDatesInput)) {
  date <- FSLDatesInput[i]
  payload <- get_game_pks_mlb(date, level_ids = c(14))
  FSLpayload <- filter(payload, teams.home.team.name==FSL[1]|teams.home.team.name==FSL[2]|teams.home.team.name==FSL[3]|
                         teams.home.team.name==FSL[4]|teams.home.team.name==FSL[5]|teams.home.team.name==FSL[6]|
                         teams.home.team.name==FSL[7]|teams.home.team.name==FSL[8]|teams.home.team.name==FSL[9]) %>%
    select(game_pk)
  ## FSLPayload will contain the game_pk that we can iterate through and assign to gameId, creating a Statcast database of data 
  ## every pitch and batted ball in the FSL.
  
  ##Looping through our gameIds to pull JSON files of each game and gameInfo variable for file exporting
  for (i in seq_along(FSLpayload$game_pk)) {
    gameId <- FSLpayload$game_pk[i]
    date<- gsub("-", "", mlb_game_info(gameId)$game_date)
    homeTeam <- gsub(" ", "", mlb_game_linescore(gameId)$home_team_name[1])
    awayTeam <- gsub(" ", "", mlb_game_linescore(gameId)$away_team_name[1])
    gameInfo <- paste(date,gameId,homeTeam,awayTeam, sep= '')
    gameFile <- paste("Pitches" ,gameInfo,".csv", sep = '')
    
    
    ##PITCH DATA SCRAPE USING JSONLITE
    json_file <- paste("https://baseballsavant.mlb.com/gf?game_pk=",gameId, sep = '')
    data <- fromJSON(json_file)
    teamHome <- data[["team_home"]]
    teamAway <- data[["team_away"]]
    FSL_full_game <- bind_rows(teamHome, teamAway) %>%
      arrange(game_total_pitches)
    
    #Converting to dataframe and exporting as csv
    gamefeed_data <- as.data.frame(FSL_full_game)
    #Alter your path variable to your directory of choice
    path <- 'insertyourFSLScrapedGamesfilepathhere'
    write_csv(gamefeed_data, file.path(path, gameFile))
    print(paste(gameId, "finished"))
  }
  print(paste(date, "finished"))
}
print("FSL Game Loop Exited")
#-------------------------------------------------------------------------------------------------------------------
#Again, remember to set the dates for the PCL games you wish to scrape--and that going day to day is recommended at this stage.

PCLDatesInput <-seq(as.Date(Yesterday), as.Date(Yesterday), by="days")
PCL <- c("Albuquerque Isotopes","El Paso Chihuahuas", "Oklahoma City Dodgers", "Round Rock Express", 
         "Sugar Land Space Cowboys", "Las Vegas Aviators", "Reno Aces", "Sacramento River Cats",
         "Salt Lake Bees", "Tacoma Rainiers")
#Aside from unique PCL variable declarations and team names, this is the same as the FSL scraper above.
for (i in seq_along(PCLDatesInput)) {
  date <- PCLDatesInput[i]
  payload <- get_game_pks_mlb(date, level_ids = c(11))
  PCLpayload <- filter(payload,teams.home.team.name==PCL[1]|teams.home.team.name==PCL[2]|
                         teams.home.team.name==PCL[3]|teams.home.team.name==PCL[4]|
                         teams.home.team.name==PCL[5]|teams.home.team.name==PCL[6]|
                         teams.home.team.name==PCL[7]|teams.home.team.name==PCL[8]|
                         teams.home.team.name==PCL[9]|teams.home.team.name==PCL[10]) %>%
    select(game_pk)

for (i in seq_along(PCLpayload$game_pk)) {
    gameId <- PCLpayload$game_pk[i]
    date<- gsub("-", "", mlb_game_info(gameId)$game_date)
    homeTeam <- gsub(" ", "", mlb_game_linescore(gameId)$home_team_name[1])
    awayTeam <- gsub(" ", "", mlb_game_linescore(gameId)$away_team_name[1])
    gameInfo <- paste(date,gameId,homeTeam,awayTeam, sep= '')
    gameFile <- paste("Pitches" ,gameInfo,".csv", sep = '')
    
    
    ##PITCH DATA SCRAPE
    json_file <- paste("https://baseballsavant.mlb.com/gf?game_pk=",gameId, sep = '')
    data <- fromJSON(json_file)
    teamHome <- data[["team_home"]]
    teamAway <- data[["team_away"]]
    teamHome$pfxXWithGravity <- as.character(teamHome$pfxXWithGravity)
    PCL_full_game <-
      bind_rows(teamHome, teamAway) %>%
      arrange(game_total_pitches)
    
    #Converting to dataframe and exporting as csv
    gamefeed_data <- as.data.frame(PCL_full_game)
    path <- 'insertyourPCLScrapedGamesfilepathhere'
    write_csv(gamefeed_data, file.path(path, gameFile))
    print(paste(gameId, "finished"))
    
  }
  print(paste(date, "finished"))
}
print("PCL Game Loop Exited")
#----------------------------------------------------------------------------------------------------------------------
#Listing all of our scraped gamefiles from the path we sent them to in the FSL scraper above.
#We then map them to FSL_df, creating our pitch by pitch data frame.
FSL_df <- list.files(path = 'referencetheFSLscrapedgamesfilepathhere',
                     pattern="*.csv", 
                     full.names = T) %>% 
  map_df(~read_csv(.))

#We can also write our FSL_df in csv format to a filepath
gameFile <- paste("FSLAllPitches.csv", sep = '')

#Alter your path variable to your directory of choice
path <- 'insertFSLDatasetfilepathhere'
write_csv(FSL_df, file.path(path, gameFile))
#----------------------------------------------------------------------------------------------------------------------
#We'll do the same below for PCL_df
PCL_df <- list.files(path = 'referencethePCLscrapedgamesfilepathhere',
                     pattern="*.csv", 
                     full.names = T) %>% 
  map_df(~read_csv(.))

gameFile <- paste("PCLAllPitches.csv", sep = '')

#Alter your path variable to your directory of choice
path <- 'insertPCLDatasetfilepathhere'
write_csv(PCL_df, file.path(path, gameFile))
#----------------------------------------------------------------------------------------------------------------------
#IMPORTANT: Remember to alter the dates and file paths to accomodate the games you wish to scrape.
#Now, with all the avaiable games scraped, we have up to date FSL and PCL pitch by pitch dataframes!
