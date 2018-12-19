####Read in Libraries####
library(RMySQL)
library(DBI)



####Read in Data####
##Read in all Games from 2016 Season
mydb_pbp <- dbConnect(RMySQL::MySQL(), user = "root", password = "B@zm@x123!", dbname = "nba_pbp_data_scrape", host = "35.199.55.188")
all_games_list <- dbListTables(mydb_pbp)
all_games_2k16 <- all_games_list[which(grepl("play_by_play_game_id_00216", all_games_list) == T)]
game_list <- list()
for(i in seq_along(all_games_2k16)){
  print(i)
  game_list[[i]] <- dbReadTable(mydb_pbp, all_games_2k16[i])
}
dbDisconnect(mydb_pbp)


##List of Players from 2016 Season
mydb_bbref <- dbConnect(RMySQL::MySQL(), user = "root", password = "B@zm@x123!", dbname = "basketball_ref_scrape", host = "35.199.55.188")
per_game_stats <- dbReadTable(mydb_bbref, "per_game_stats")
player_df <- data.frame(player = per_game_stats$player[which(per_game_stats$season == 2017)],
                        id = per_game_stats$link[which(per_game_stats$season == 2017)])
rm(per_game_stats)
dbDisconnect(mydb_bbref)


####Initialize Stint Matrix and Response Vectors####
stints <- as.data.frame(matrix(NA,0,length(player_df$player)))
names(stints) <- player_df$player
stintCount = 1

##Partition Actions that Terminate Possessions
game <- game_list[[1]]
game$home_score <- sapply(seq_along(game$SCORE), function(x) as.numeric(strsplit(game$SCORE[x], " - ")[[1]][2]))
game$away_score <- sapply(seq_along(game$SCORE), function(x) as.numeric(strsplit(game$SCORE[x], " - ")[[1]][1]))
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
shots <- game[sort(c(which(game$EVENTMSGTYPE %in% c(1:3, 5)), which(game$EVENTMSGTYPE == 4 & game$def_rebound == 1))),]

##Home and Away Starting Lineups
awayplayersStart <- sort(unique(unlist(shots[1,paste0("Away_Player_", c(1:5))])))
homeplayersStart <- sort(unique(unlist(shots[1,paste0("Home_Player_", c(1:5))])))

##Walk Through Shot Situations and Check when Lineups Change
for(i in seq_along(shots$GAME_ID)){
  awayplayers <- sort(unique(unlist(shots[i,paste0("Away_Player_", c(1:5))])))
  homeplayers <- sort(unique(unlist(shots[i,paste0("Home_Player_", c(1:5))])))
  bothHome <- homeplayersStart %in% homeplayers
  bothAway <- awayplayersStart %in% awayplayers
  if(all(bothHome) == T & all(bothAway) == T){
    if(shots[i, c("EVENTMSGTYPE")] == 1){
      possessions = possessions + 1
      
      
      ######GOTTA ADD POINTS OF STINTS######
      
      
      
      
    }else if(shots[i, c("EVENTMSGTYPE")] == 2){
      possessions = possessions + 1
    }else if(shots[i, c("def_rebound")] == 1){
      possessions = possessions + 1
    }else if(shots[i, c("EVENTMSGTYPE")] == 5){
      possessions = possessions + 1
    }else if(shots[i, c("EVENTMSGTYPE")] == 3){
      # midFreeThrow = shots[i, ""]
      if(shots[i, c("EVENTMSGACTIONTYPE")] == 10){
        totalToShoot = 1
        currentShoot = 1
      }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 11){
        totalToShoot = 1
        currentShoot = 2
      }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 12){
        totalToShoot = 2
        currentShoot = 2
      }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 13){
        totalToShoot = 1
        currentShoot = 3
      }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 14){
        totalToShoot = 2
        currentShoot = 3
      }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 15){
        totalToShoot = 3
        currentShoot = 3
      }else if(shots[i, c("EVENTMSGACTIONTYPE")] == 16){
        totalToShoot = 1
        currentShoot = 1
      }
      if(totalToShoot > currentShoot){
        if(awayScore == )
      }
    }
  }
}








