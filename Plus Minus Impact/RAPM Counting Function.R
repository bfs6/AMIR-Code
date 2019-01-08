####RAPM Counting Function####
rapm_counting_function <- function(year){
  print(paste0("YEAR: ", year))
  
  ####Read in Libraries####
  library(RMySQL)
  library(DBI)
  library(dplyr)
  
  ####Read in Data####
  ##Read in all Games from 2016 Season
  mydb_pbp <- dbConnect(RMySQL::MySQL(), user = "root", password = "B@zm@x123!", dbname = "nba_pbp_data_scrape", host = "35.199.55.188")
  all_games_list <- dbListTables(mydb_pbp)
  all_games_list <- all_games_list[which(grepl(paste0("play_by_play_game_id_002", substr(year, 3, 4)), all_games_list) == T)]
  game_list <- list()
  print("Loading Game Data")
  for(i in seq_along(all_games_list)){
    game_list[[i]] <- dbReadTable(mydb_pbp, all_games_list[i])
  }
  print("Data Done Loading")
  names(game_list) <- all_games_list
  dbDisconnect(mydb_pbp)
  
  
  ##List of Players from 2016 Season
  mydb_bbref <- dbConnect(RMySQL::MySQL(), user = "root", password = "B@zm@x123!", dbname = "basketball_ref_scrape", host = "35.199.55.188")
  per_game_stats <- dbReadTable(mydb_bbref, "per_game_stats")
  player_df <- data.frame(player = per_game_stats$player[which(per_game_stats$season == year + 1)],
                          id = per_game_stats$link[which(per_game_stats$season == year + 1)],
                          games_played = per_game_stats$g[which(per_game_stats$season == year + 1)],
                          minutes_per_game = per_game_stats$mp[which(per_game_stats$season == year + 1)])
  player_df$minutes_played <- player_df$games_played * player_df$minutes_per_game
  player_df <- subset(player_df, select = -c(games_played, minutes_per_game))
  player_df <- player_df %>% group_by(player, id) %>% dplyr::summarize(minutes_played = sum(minutes_played))
  rm(per_game_stats)
  dbDisconnect(mydb_bbref)
  
  
  ##List of Players from 
  nba_stats_player_list <- list()
  for(game_id in seq_along(game_list)){
    game <- game_list[[game_id]]
    df <- data.frame(Player = as.character(do.call(c, game[, c(paste0("Home_Player_", c(1:5)), paste0("Away_Player_", c(1:5)))])),
                     ID = do.call(c, game[, c(paste0("Home_Player_IDs_", c(1:5)), paste0("Away_Player_IDs_", c(1:5)))]))
    df <- unique(df)
    row.names(df) <- NULL
    nba_stats_player_list[[game_id]] <- df
  }
  nba_stats_player_df <- do.call(rbind, nba_stats_player_list)
  nba_stats_player_df <- unique(nba_stats_player_df)
  nba_stats_player_df <- dplyr::arrange(nba_stats_player_df, ID)
  
  
  ####Initialize Stint Matrix and Response Vectors####
  stints <- as.data.frame(matrix(NA,0,length(unique(nba_stats_player_df$Player))))
  names(stints) <- unique(nba_stats_player_df$Player)
  stintCount = 1
  
  ##Partition Actions that Terminate Possessions
  stint_list <- list()
  print("Stint Counting")
  for(game_id in seq_along(game_list)){
    print(game_id)
    game <- game_list[[game_id]]
    game$home_score <- sapply(seq_along(game$SCORE), function(x) as.numeric(strsplit(game$SCORE[x], " - ")[[1]][2]))
    game$home_score[1] = 0
    for(x in seq_along(game$home_score)){
      if(is.na(game$home_score[x]) == T){
        game$home_score[x] = game$home_score[x-1]
      }else{
        game$home_score[x] = game$home_score[x]
      }
    }
    game$away_score <- sapply(seq_along(game$SCORE), function(x) as.numeric(strsplit(game$SCORE[x], " - ")[[1]][1]))
    game$away_score[1] = 0
    for(x in seq_along(game$away_score)){
      if(is.na(game$away_score[x]) == T){
        game$away_score[x] = game$away_score[x-1]
      }else{
        game$away_score[x] = game$away_score[x]
      }
    }
    off_rebound <- sapply(seq_along(game$GAME_ID)[-1],
                          function(x){
                            if(game$EVENTMSGTYPE[x] == 4 & game$EVENTMSGTYPE[x-1] == 2 & is.na(game$HOMEDESCRIPTION[x]) == T & is.na(game$HOMEDESCRIPTION[x-1]) == T){
                              1
                            }else if(game$EVENTMSGTYPE[x] == 4 & game$EVENTMSGTYPE[x-1] == 2 & is.na(game$HOMEDESCRIPTION[x]) == F & is.na(game$HOMEDESCRIPTION[x-1]) == F){
                              1
                            }else{
                              0
                            }
                          })
    game$off_rebound <- c(0, off_rebound)
    game$def_rebound <- rep(0, nrow(game))
    game$def_rebound[which(game$EVENTMSGTYPE == 4 & game$off_rebound == 0)] = 1
    game$points <- rep(0, nrow(game))
    game$points[which(game$EVENTMSGTYPE == 1)] = 2
    game$points[which(game$EVENTMSGTYPE == 1 & grepl("3PT", game$HOMEDESCRIPTION) == T)] = 3
    game$points[which(game$EVENTMSGTYPE == 1 & grepl("3PT", game$VISITORDESCRIPTION) == T)] = 3
    game$points[which(game$EVENTMSGACTIONTYPE %in% c(10:16) & grepl("MISS", game$HOMEDESCRIPTION) == F & is.na(game$HOMEDESCRIPTION) == F)] = 1
    game$points[which(game$EVENTMSGACTIONTYPE %in% c(10:16) & grepl("MISS", game$VISITORDESCRIPTION) == F & is.na(game$VISITORDESCRIPTION) == F)] = 1
    shots <- game[sort(c(which(game$EVENTMSGTYPE %in% c(1:3, 5)), which(game$EVENTMSGTYPE == 4 & game$def_rebound == 1))),]
    row.names(shots) = NULL
    
    ##Home and Away Starting Lineups
    awayplayersStart <- sort(unique(unlist(shots[1,paste0("Away_Player_", c(1:5))])))
    homeplayersStart <- sort(unique(unlist(shots[1,paste0("Home_Player_", c(1:5))])))
    
    
    ##Walk Through Shot Situations and Check when Lineups Change
    homeScore <- 0
    awayScore <- 0
    homePoints <- 0
    awayPoints <- 0
    possessions <- 0
    for(i in seq_along(shots$GAME_ID)){
      awayplayers <- sort(unique(unlist(shots[i,paste0("Away_Player_", c(1:5))])))
      homeplayers <- sort(unique(unlist(shots[i,paste0("Home_Player_", c(1:5))])))
      bothHome <- homeplayersStart %in% homeplayers
      bothAway <- awayplayersStart %in% awayplayers
      if(all(bothHome) == TRUE & all(bothAway) == TRUE){
        if(shots[i, c("EVENTMSGTYPE")] == 1){
          possessions = possessions + 1
          if(awayScore == shots[i, "away_score"]){
            homeScore = homeScore + shots[i, "points"]
            homePoints = homePoints + shots[i, "points"]
          }else{
            awayScore = awayScore + shots[i, "points"]
            awayPoints = awayPoints + shots[i, "points"]
          }
        }else if(shots[i, c("EVENTMSGTYPE")] == 2){
          possessions = possessions + 1
        }else if(shots[i, c("def_rebound")] == 1){
          possessions = possessions + 1
        }else if(shots[i, c("EVENTMSGTYPE")] == 5){
          possessions = possessions + 1
        }else if(shots[i, c("EVENTMSGTYPE")] == 3){
          if(shots[i, c("EVENTMSGACTIONTYPE")] == 10){
            currentShoot = 1
            totalToShoot = 1
            midFreeThrow = shots[i, "PCTIMESTRING"]
          }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 11){
            currentShoot = 1
            totalToShoot = 2
            midFreeThrow = shots[i, "PCTIMESTRING"]
          }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 12){
            currentShoot = 2
            totalToShoot = 2
            midFreeThrow = shots[i, "PCTIMESTRING"]
          }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 13){
            currentShoot = 1
            totalToShoot = 3
            midFreeThrow = shots[i, "PCTIMESTRING"]
          }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 14){
            currentShoot = 2
            totalToShoot = 3
            midFreeThrow = shots[i, "PCTIMESTRING"]
          }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 15){
            currentShoot = 3
            totalToShoot = 3
            midFreeThrow = shots[i, "PCTIMESTRING"]
          }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 16){
            currentShoot = 1
            totalToShoot = 1
            midFreeThrow = shots[i, "PCTIMESTRING"]
          }
          if(totalToShoot > currentShoot){
            if(awayScore == shots[i, "away_score"]){
              homeScore = homeScore + shots[i, "points"]
              homePoints = homePoints + shots[i, "points"]
            }else if(homeScore == shots[i, "home_score"]){
              awayScore = awayScore + shots[i, "points"]
              awayPoints = awayPoints + shots[i, "points"]
            }
          }
          else{
            if(shots[i, "points"] > 0){
              if(awayScore == shots[i, "away_score"]){
                homeScore = homeScore + shots[i, "points"]
                homePoints = homePoints + shots[i, "points"]
                possessions = possessions + 1
              }else if(homeScore == shots[i, "home_score"]){
                awayScore = awayScore + shots[i, "points"]
                awayPoints = awayPoints + shots[i, "points"]
                possessions = possessions + 1
              }
            }
          }
        }
      }
      else{
        if(midFreeThrow == shots[i, "PCTIMESTRING"]){
          if(shots[i, c("EVENTMSGTYPE")] == 3){
            if(shots[i, c("EVENTMSGACTIONTYPE")] == 10){
              currentShoot = 1
              totalToShoot = 1
            }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 11){
              currentShoot = 1
              totalToShoot = 2
            }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 12){
              currentShoot = 2
              totalToShoot = 2
            }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 13){
              currentShoot = 1
              totalToShoot = 3
            }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 14){
              currentShoot = 2
              totalToShoot = 3
            }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 15){
              currentShoot = 3
              totalToShoot = 3
            }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 16){
              currentShoot = 1
              totalToShoot = 1
            }
            if(totalToShoot > currentShoot){
              if(awayScore == shots[i, "away_score"]){
                homeScore = homeScore + shots[i, "points"]
                homePoints = homePoints + shots[i, "points"]
              }else if(homeScore == shots[i, "home_score"]){
                awayScore = awayScore + shots[i, "points"]
                awayPoints = awayPoints + shots[i, "points"]
              }
            }
            else{
              if(shots[i, "points"] > 0){
                if(awayScore == shots[i, "away_score"]){
                  homeScore = homeScore + shots[i, "points"]
                  homePoints = homePoints + shots[i, "points"]
                  possessions = possessions + 1
                }else if(homeScore == shots[i, "home_score"]){
                  awayScore = awayScore + shots[i, "points"]
                  awayPoints = awayPoints + shots[i, "points"]
                  possessions = possessions + 1
                }
              }
            }
          }
        }
        if(possessions > 0){
          for(p in nba_stats_player_df$Player){
            if(p %in% awayplayersStart){
              stints[stintCount, p] = -1
            }else if(p %in% homeplayersStart){
              stints[stintCount, p] = 1
            }else{
              stints[stintCount, p] = 0
            }
          }
          stints[stintCount, "Margin"] = 100*(homePoints - awayPoints)/possessions
          stintCount = stintCount + 1
        }
        homeplayersStart = homeplayers
        awayplayersStart = awayplayers
        homePoints = 0
        awayPoints = 0
        possessions = 0
        if(shots[i, c("EVENTMSGTYPE")] == 1){
          possessions = possessions + 1
          if(awayScore == shots[i, "away_score"]){
            homeScore = homeScore + shots[i, "points"]
            homePoints = homePoints + shots[i, "points"]
          }else{
            awayScore = awayScore + shots[i, "points"]
            awayPoints = awayPoints + shots[i, "points"]
          }
        }else if(shots[i, c("EVENTMSGTYPE")] == 2){
          possessions = possessions + 1
        }else if(shots[i, c("def_rebound")] == 1){
          possessions = possessions + 1
        }else if(shots[i, c("EVENTMSGTYPE")] == 5){
          possessions = possessions + 1
        }
      }
    }
  }
  return(stints)
}
