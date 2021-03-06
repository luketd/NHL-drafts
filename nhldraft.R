library(nhlapi)
library(dplyr) 
library(jsonlite)
library(stringr)
library(lubridate)
library(tidyverse)

list2 <- list()

#Getting relevant information from the draft API for the drafts 2000-2015
#Get Draft Year, Teamname,PlayerName, Round,OV, prospectID
for (x in 2000:2015) {
  draftlink <- nhlapi:::nhl_url_drafts(x)
  drafts <- nhlapi:::nhl_from_json(draftlink)
  
  list1 <- list()
  
  for (i in 1:7){
    
    
    draftsCleaned <- data.frame( Draftyear = drafts[["drafts"]][["draftYear"]], 
                                 TeamName = drafts[["drafts"]][["rounds"]][[1]][["picks"]][[i]][["team.name"]], 
                                 PlayerName = drafts[["drafts"]][["rounds"]][[1]][["picks"]][[i]][["prospect.fullName"]],
                                 Round = drafts[["drafts"]][["rounds"]][[1]][["picks"]][[i]][["round"]],
                                 OV = drafts[["drafts"]][["rounds"]][[1]][["picks"]][[i]][["pickOverall"]],
                                 ProspectID = drafts[["drafts"]][["rounds"]][[1]][["picks"]][[i]][["prospect.id"]])
    list1[[i]] <- draftsCleaned
  }
  
  NHLDrafts <- do.call(rbind, list1)
  list2[[x]] <- NHLDrafts
  
}

AllNHLDrafts <- do.call(rbind, list2)

#get playerID and Position from the prospectID
AllNHLDrafts$PlayerID <- ""
AllNHLDrafts$Posistion <- ""
for (y in 1: length(AllNHLDrafts$Draftyear)) {
  #print(y)
  player <- AllNHLDrafts[y,6]
  
  #some fixes because NHLAPI is dumb and doesn't have prospectID for some players
  #And I am dumb for fixing them like this
  if (AllNHLDrafts[y,3] == "Brian Lee") {
    test <- 8471683
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Erik Johnson"){
    test <- 8473446
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Kevin Koopman"){
    test <- 8473570
    AllNHLDrafts$PlayerID[y] <- test
  }else if (AllNHLDrafts[y,3] == "Justin Taylor"){
    test <- 8471525
    AllNHLDrafts$PlayerID[y] <- test
  }else if (AllNHLDrafts[y,3] == "Matt  Martin"){
    test <- 8474709
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Sean Collins"){
    test <- 8473913
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Joey Martin"){
    test <- 8468673
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Craig Anderson"){
    test <- 8467950
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Neil Komadoski"){
    test <- 8448518
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Anthony Aquino"){
    test <- 8469541
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Chris Higgins"){
    test <- 8470274
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Michael Erickson"){
    test <- 8470051
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Ryan Jones"){
    test <- 8471323
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Jason Smith"){
    test <- 8470789
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Mike Brown"){
    test <- 8471371
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Joe Cooper"){
    test <- 8471431
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "David Macdonald"){
    test <- 8462245
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Matt Martin") {
    test <- 8474709
    AllNHLDrafts$PlayerID[y] <- test
  } else if (AllNHLDrafts[y,3] == "Cory Stillman") {
    test <- 8469550
    AllNHLDrafts$PlayerID[y] <- test
  }
  else {
    #see if prospectID is blank, if so then get the playerID and position from their name.
    if (is.na(player) == TRUE) {
      #try catch, if the player name is not found, then assume they never played in the NHL, so they don't matter
      catch1 <- tryCatch(
        {
          playerName <- AllNHLDrafts[y,3]
          playerID_No_ProspectID <- nhl_players(playerName) %>%
            select(id)
          position_No_ProspectID <- nhl_players(playerName) %>%
            select(primaryPosition.code)
          
          
        },
        error=function(cond) {
          
          test <- 0
          position <- "NA"
        },
        finally={
          #Change LW, RW, and Center to just Forward and then log them in
          if (position_No_ProspectID == "L" || position_No_ProspectID == "R" || position_No_ProspectID == "C"){
            position_No_ProspectID <- "F"
          }
          AllNHLDrafts$PlayerID[y] <- playerID_No_ProspectID
          AllNHLDrafts$Posistion[y] <- position_No_ProspectID
        }
        
      )
      #Else: we have their prospectID, so we can get the position and ID from their page
    }else {
      player <- paste("https://statsapi.web.nhl.com/api/v1/draft/prospects/",player, sep = "")
      player <- nhlapi:::nhl_from_json(player)
      position <- player[["prospects"]][["primaryPosition.code"]]
      id <- player[["prospects"]][["nhlPlayerId"]]
      

      if (is.null(id) == TRUE)
      {
        playerName <- AllNHLDrafts[y,3]
        id <- nhl_players(playerName) %>%
          select(id)
        position <- nhl_players(playerName) %>%
          select(primaryPosition.code)
      }
      ########Sometimes ProspectID doesnt return player ID because ?????
      
      #Change LW, RW, and Center to just Forward and then log them in
      if (position == "R" || position == "L" || position == "C"){
        position <- "F"
      }
      
      AllNHLDrafts$PlayerID[y] <- id
      AllNHLDrafts$Posistion[y] <- position
    }
  }
}



#######################################################
#Getting NHL Career Stats for the players
#######################################################
AllNHLDrafts$GP <- ""
AllNHLDrafts$Goals <- ""
AllNHLDrafts$Assists <- ""
AllNHLDrafts$Wins <- ""
for (z in 1:length(AllNHLDrafts$Draftyear)) {
  getGames <- tryCatch(
    {
      #get their stats from the nhlAPI
      getPlayerStats <- nhlapi:::nhl_players_allseasons( playerIds = AllNHLDrafts[z,7]) %>%
        filter(league.name == "National Hockey League")
      games <- sum(getPlayerStats$stat.games)
      
      #if the position is Forward or Defense, pull the data away
      if(AllNHLDrafts$Posistion[z] == "F" || AllNHLDrafts$Posistion[z] == "D" ) {
        #print(sum( getPlayerStats$stat.goals ))
        goals <- sum( getPlayerStats$stat.goals )
        assists <- sum(getPlayerStats$stat.assists)
        wins <- 0
        
        #else they are goalies
      } else {
        wins <- sum(getPlayerStats$stat.wins)
        goals <- 0
        assists <- 0
      }
    },
    #or there was an error, so record no stats(meaning they never played in the NHL)
    error=function(cond) {
      games <- 0
      goals <- 0
      assists <- 0
      wins <- 0
    },
    finally={
      
      AllNHLDrafts$GP[z] <- games
      AllNHLDrafts$Goals[z] <- goals
      AllNHLDrafts$Assists[z] <- assists
      AllNHLDrafts$Wins[z] <- wins
    }
  )
}

#############
#Change Pheonix to Arizona, and Atlanta Thrashers to Winnipeg Jets
AllNHLDrafts$TeamName[AllNHLDrafts$TeamName == "Atlanta Thrashers"] <- "Winnipeg Jets"
AllNHLDrafts$TeamName[AllNHLDrafts$TeamName == "Phoenix Coyotes"] <- "Arizona Coyotes"





dfwrite <- apply(AllNHLDrafts,2, as.character)
write.csv(dfwrite,"C:\\Users\\Luke\\Desktop\\NHLDraft.csv", row.names = FALSE)



############################################################################
#Using the data collected to modify the data to see who "made the SHL"
#made is b=y using https://dobberprospects.com/2020/05/16/nhl-draft-pick-probabilities/amp/
#which puts people who made it to a regular NHL players is someone who played over 100 GP


AllNHLDraftsFinal$GP <- as.numeric(AllNHLDraftsFinal$GP)
AllNHLDraftsFinal$Goals <- as.numeric(AllNHLDraftsFinal$Goals)
AllNHLDraftsFinal$Assists <- as.numeric(AllNHLDraftsFinal$Assists)
AllNHLDraftsFinal$Wins <- as.numeric(AllNHLDraftsFinal$Wins)

GPThreshold <- 100

Round1 <- sum(AllNHLDraftsFinal$GP >= GPThreshold & AllNHLDraftsFinal$Round == 1 )/
  sum(AllNHLDraftsFinal$Round == 1)

Round2 <- sum(AllNHLDraftsFinal$GP >= GPThreshold & AllNHLDraftsFinal$Round == 2 )/
  sum(AllNHLDraftsFinal$Round == 2)

Round3 <- sum(AllNHLDraftsFinal$GP >= GPThreshold & AllNHLDraftsFinal$Round == 3 )/
  sum(AllNHLDraftsFinal$Round == 3)

Round4 <- sum(AllNHLDraftsFinal$GP >= GPThreshold & AllNHLDraftsFinal$Round == 4 )/
  sum(AllNHLDraftsFinal$Round == 4)

Round5 <- sum(AllNHLDraftsFinal$GP >= GPThreshold & AllNHLDraftsFinal$Round == 5 )/
  sum(AllNHLDraftsFinal$Round == 5)


Round6 <- sum(AllNHLDraftsFinal$GP >= GPThreshold & AllNHLDraftsFinal$Round == 6 )/
  sum(AllNHLDraftsFinal$Round == 6)

Round7 <- sum(AllNHLDraftsFinal$GP >= GPThreshold & AllNHLDraftsFinal$Round == 7 )/
  sum(AllNHLDraftsFinal$Round == 7)



###############################################################################
#All NHL Team Odds
###############################################################################
round <- as.character(3)

Flyers <- sum(AllNHLDraftsFinal$GP >= GPThreshold &  AllNHLDraftsFinal$Round == round & AllNHLDraftsFinal$TeamName == "Philadelphia Flyers") / 
  sum(AllNHLDraftsFinal$Round == round & AllNHLDraftsFinal$TeamName == "Philadelphia Flyers")
Flyers

test <- AllNHLDraftsFinal %>%
  group_by(TeamName, Round) %>%
  count( (GP[GP>=100]) )

IndTeams <- AllNHLDraftsFinal %>%
  count(AllNHLDraftsFinal$TeamName,AllNHLDraftsFinal$Round)

#Can be done easier in a mysql query than in R

