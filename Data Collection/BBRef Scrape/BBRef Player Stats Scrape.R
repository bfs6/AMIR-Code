####Read in Libraries####
library(magrittr)
library(dplyr)
library(janitor)
library(RMySQL)
library(DBI)
library(data.table)
library(knitr)
library(rvest)
library(curl)
library(jsonlite)
library(plyr)
library(pacman)


####Per Game Stats Scraping####
per_game_list <- list()
for(season in c(1950:2019)){
  print(season)
  NBA_api_base <- "https://www.basketball-reference.com"
  page_type <- "per_game"
  nba_url <- paste0(NBA_api_base, "/leagues/NBA_", season, "_", page_type, ".html")
  pg <- xml2::read_html(nba_url)
  nba_stats <- dplyr::tbl_df(rvest::html_table(pg, fill = T)[[1]])
  if(utils::packageVersion("janitor") > "0.3.1"){
    nba_stats <- nba_stats %>%
      janitor::clean_names(case = "old_janitor") %>%
      dplyr::filter(.data$player != "Player")
  }else{
    nba_stats <- nba_stats %>%
      janitor::clean_names() %>%
      janitor::remove_empty_cols() %>%
      dplyr::filter(.data$player != "Player")
  }
  links <- pg %>%
    rvest::html_nodes("tr.full_table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  link_names <- pg %>%
    rvest::html_nodes("tr.full_table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  
  links_df <- dplyr::data_frame(player = as.character(link_names), link = as.character(links))
  links_df[] <- lapply(links_df, as.character)
  nba_stats <- dplyr::left_join(nba_stats, links_df, by = "player")
  nba_stats <- dplyr::mutate_at(nba_stats, dplyr::vars(-.data$player, -.data$pos, -.data$tm, -.data$link), dplyr::funs(as.numeric))
  nba_stats <- subset(nba_stats, select = -c(rk))
  nba_stats$season <- rep(season, nrow(nba_stats))
  per_game_list[[season - 1949]] <- nba_stats
}
per_game_stats <- do.call(rbind, per_game_list)
per_game_stats <- dplyr::arrange(per_game_stats, -season)
per_game_stats <- as.data.frame(per_game_stats)

####Per 36 Minutes Stats Scraping####
per_36_min_list <- list()
for(season in c(1950:2019)){
  print(season)
  NBA_api_base <- "https://www.basketball-reference.com"
  page_type <- "per_minute"
  nba_url <- paste0(NBA_api_base, "/leagues/NBA_", season, "_", page_type, ".html")
  pg <- xml2::read_html(nba_url)
  nba_stats <- dplyr::tbl_df(rvest::html_table(pg, fill = T)[[1]])
  if(utils::packageVersion("janitor") > "0.3.1"){
    nba_stats <- nba_stats %>%
      janitor::clean_names(case = "old_janitor") %>%
      dplyr::filter(.data$player != "Player")
  }else{
    nba_stats <- nba_stats %>%
      janitor::clean_names() %>%
      janitor::remove_empty_cols() %>%
      dplyr::filter(.data$player != "Player")
  }
  links <- pg %>%
    rvest::html_nodes("tr.full_table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  link_names <- pg %>%
    rvest::html_nodes("tr.full_table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  
  links_df <- dplyr::data_frame(player = as.character(link_names), link = as.character(links))
  links_df[] <- lapply(links_df, as.character)
  nba_stats <- dplyr::left_join(nba_stats, links_df, by = "player")
  nba_stats <- dplyr::mutate_at(nba_stats, dplyr::vars(-.data$player, -.data$pos, -.data$tm, -.data$link), dplyr::funs(as.numeric))
  nba_stats <- subset(nba_stats, select = -c(rk))
  nba_stats$season <- rep(season, nrow(nba_stats))
  per_36_min_list[[season - 1949]] <- nba_stats
}
per_36_min_stats <- do.call(rbind, per_36_min_list)
per_36_min_stats <- dplyr::arrange(per_36_min_stats, -season)
per_36_min_stats <- as.data.frame(per_36_min_stats)

####Per 100 Possessions Stats Scraping####
per_100_poss_list <- list()
for(season in c(1974:2019)){
  print(season)
  NBA_api_base <- "https://www.basketball-reference.com"
  page_type <- "per_poss"
  nba_url <- paste0(NBA_api_base, "/leagues/NBA_", season, "_", page_type, ".html")
  pg <- xml2::read_html(nba_url)
  nba_stats <- dplyr::tbl_df(rvest::html_table(pg, fill = T)[[1]][,-30])
  if(utils::packageVersion("janitor") > "0.3.1"){
    nba_stats <- nba_stats %>%
      janitor::clean_names(case = "old_janitor") %>%
      dplyr::filter(.data$player != "Player")
  }else{
    nba_stats <- nba_stats %>%
      janitor::clean_names() %>%
      janitor::remove_empty_cols() %>%
      dplyr::filter(.data$player != "Player")
  }
  links <- pg %>%
    rvest::html_nodes("tr.full_table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  link_names <- pg %>%
    rvest::html_nodes("tr.full_table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  
  links_df <- dplyr::data_frame(player = as.character(link_names), link = as.character(links))
  links_df[] <- lapply(links_df, as.character)
  nba_stats <- dplyr::left_join(nba_stats, links_df, by = "player")
  nba_stats <- dplyr::mutate_at(nba_stats, dplyr::vars(-.data$player, -.data$pos, -.data$tm, -.data$link), dplyr::funs(as.numeric))
  nba_stats <- subset(nba_stats, select = -c(rk))
  nba_stats$season <- rep(season, nrow(nba_stats))
  per_100_poss_list[[season - 1949]] <- nba_stats
}
per_100_poss_stats <- do.call(rbind, per_100_poss_list)
per_100_poss_stats <- dplyr::arrange(per_100_poss_stats, -season)
per_100_poss_stats <- as.data.frame(per_100_poss_stats)

####Per Game Advanced Stats Scraping####
advanced_list <- list()
for(season in c(1950:2019)){
  print(season)
  NBA_api_base <- "https://www.basketball-reference.com"
  page_type <- "advanced"
  nba_url <- paste0(NBA_api_base, "/leagues/NBA_", season, "_", page_type, ".html")
  pg <- xml2::read_html(nba_url)
  nba_stats <- dplyr::tbl_df(rvest::html_table(pg, fill = T)[[1]][-c(20,25)])
  if(utils::packageVersion("janitor") > "0.3.1"){
    nba_stats <- nba_stats %>%
      janitor::clean_names(case = "old_janitor") %>%
      dplyr::filter(.data$player != "Player")
  }else{
    nba_stats <- nba_stats %>%
      janitor::clean_names() %>%
      janitor::remove_empty_cols() %>%
      dplyr::filter(.data$player != "Player")
  }
  links <- pg %>%
    rvest::html_nodes("tr.full_table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  link_names <- pg %>%
    rvest::html_nodes("tr.full_table") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  
  links_df <- dplyr::data_frame(player = as.character(link_names), link = as.character(links))
  links_df[] <- lapply(links_df, as.character)
  nba_stats <- dplyr::left_join(nba_stats, links_df, by = "player")
  nba_stats <- dplyr::mutate_at(nba_stats, dplyr::vars(-.data$player, -.data$pos, -.data$tm, -.data$link), dplyr::funs(as.numeric))
  nba_stats <- subset(nba_stats, select = -c(rk))
  nba_stats$season <- rep(season, nrow(nba_stats))
  advanced_list[[season - 1949]] <- nba_stats
}
advanced_stats <- do.call(rbind, advanced_list)
advanced_stats <- dplyr::arrange(advanced_stats, -season)
advanced_stats <- as.data.frame(advanced_stats)


####Scrape Position, Age, etc. from NBA Stats####
player_bios_list <- list()
for(season in c(1997:2019)){
  print(season)
  NBA_api_base <- "http://stats.nba.com/stats/"
  page_type <- "leaguedashplayerbiostats"
  per_mode <- "PerGame"
  season_specific <- paste(season-1, substr(season, 3, 4), sep = "-")
  season_type <- "Regular+Season"
  league_id <- "00"
  nba_url <- paste0(NBA_api_base, page_type, "/?", "PerMode=", per_mode, "&", "Season=",
                    season_specific, "&", "SeasonType=", season_type, "&", "LeagueID=", league_id)
  raw_nba_data <- readLines(nba_url, warn = "F")
  nba_json <- as.data.frame(t(sapply(raw_nba_data, fromJSON)))
  nba_json_headers <- nba_json$resultSets[[1]]$headers[[1]]
  nba_json <- as.data.frame(nba_json$resultSets[[1]]$rowSet[[1]])
  names(nba_json) <- nba_json_headers
  nba_json[nba_json_headers] <- lapply(nba_json[nba_json_headers], as.character) 
  nba_json_nums <- c("PLAYER_ID", "TEAM_ID", "AGE", "PLAYER_HEIGHT_INCHES",
                     "PLAYER_WEIGHT", "DRAFT_YEAR", "DRAFT_NUMBER", "GP",
                     "PTS", "REB", "AST", "NET_RATING", "OREB_PCT", "DREB_PCT",
                     "USG_PCT", "TS_PCT", "AST_PCT")
  nba_json[nba_json_nums] <- lapply(nba_json[nba_json_nums], as.numeric) 
  nba_json$SEASON <- rep(season, nrow(nba_json))
  player_bios_list[[season]] <- nba_json
}
player_bios <- do.call(rbind, player_bios_list)
player_bios <- dplyr::arrange(player_bios, -SEASON)
player_bios <- as.data.frame(player_bios)


####Draft Combine Stats####
draft_combine_list <- list()
##NOTE NOTE NOTE something wrong w/ 2016-17###
for(season in c(2001:2019)){
  print(season)
  NBA_api_base <- "http://stats.nba.com/stats/"
  page_type <- "draftcombinestats"
  per_mode <- "PerGame"
  season_specific <- paste(season-1, substr(season, 3, 4), sep = "-")
  season_type <- "Regular+Season"
  league_id <- "00"
  nba_url <- paste0(NBA_api_base, page_type, "/?", "SeasonYear=", season_specific, "&", "LeagueID=", league_id)
  raw_nba_data <- readLines(nba_url, warn = "F")
  nba_json <- as.data.frame(t(sapply(raw_nba_data, fromJSON)))
  nba_json_headers <- nba_json$resultSets[[1]]$headers[[1]]
  nba_json <- as.data.frame(nba_json$resultSets[[1]]$rowSet[[1]])
  names(nba_json) <- nba_json_headers
  nba_json[nba_json_headers] <- lapply(nba_json[nba_json_headers], as.character) 
  nba_json_nums <- c("PLAYER_ID", "HEIGHT_WO_SHOES", "HEIGHT_W_SHOES",
                     "WEIGHT", "WINGSPAN", "STANDING_REACH", "BODY_FAT_PCT",
                     "HAND_LENGTH", "HAND_WIDTH", "STANDING_VERTICAL_LEAP", 
                     "MAX_VERTICAL_LEAP", "LANE_AGILITY_TIME", "MODIFIED_LANE_AGILITY_TIME",
                     "THREE_QUARTER_SPRINT", "BENCH_PRESS")
  nba_json[nba_json_nums] <- lapply(nba_json[nba_json_nums], as.numeric)
  nba_json_shooting_names <- c("SPOT_FIFTEEN_CORNER_LEFT", "SPOT_FIFTEEN_BREAK_LEFT",
                               "SPOT_FIFTEEN_TOP_KEY", "SPOT_FIFTEEN_BREAK_RIGHT", 
                               "SPOT_FIFTEEN_CORNER_RIGHT", "SPOT_COLLEGE_CORNER_LEFT", 
                               "SPOT_COLLEGE_BREAK_LEFT", "SPOT_COLLEGE_TOP_KEY", 
                               "SPOT_COLLEGE_BREAK_RIGHT", "SPOT_COLLEGE_CORNER_RIGHT", 
                               "SPOT_NBA_CORNER_LEFT", "SPOT_NBA_BREAK_LEFT", 
                               "SPOT_NBA_TOP_KEY", "SPOT_NBA_BREAK_RIGHT", 
                               "SPOT_NBA_CORNER_RIGHT", "OFF_DRIB_FIFTEEN_BREAK_LEFT", 
                               "OFF_DRIB_FIFTEEN_TOP_KEY", "OFF_DRIB_FIFTEEN_BREAK_RIGHT", 
                               "OFF_DRIB_COLLEGE_BREAK_LEFT", "OFF_DRIB_COLLEGE_TOP_KEY", 
                               "OFF_DRIB_COLLEGE_BREAK_RIGHT", "ON_MOVE_FIFTEEN", "ON_MOVE_COLLEGE")
  nba_json_shooting_names <- intersect(nba_json_shooting_names, names(nba_json))
  nba_json[nba_json_shooting_names] <- lapply(nba_json[nba_json_shooting_names],
                                              function(x){
                                                if(length(unique(x)) > 1){
                                                  df <- as.data.frame(do.call(rbind, strsplit(x, "-")))
                                                  as.numeric(as.character(df[,1]))/as.numeric(as.character(df[,2]))
                                                }
                                              }) 
  nba_json$SEASON <- rep(season, nrow(nba_json))
  draft_combine_list[[season]] <- nba_json
}
draft_combine_stats <- do.call(rbind.fill, draft_combine_list)
draft_combine_stats <- dplyr::arrange(draft_combine_stats, -SEASON)
draft_combine_stats <- as.data.frame(draft_combine_stats)


####Connect to DB and Read Files####
mydb = dbConnect(RMySQL::MySQL(), user = "root", password = "B@zm@x123!", dbname = "basketball_ref_scrape", host = "35.199.55.188")
dbWriteTable(mydb, "per_game_stats", per_game_stats, overwrite = T)
dbWriteTable(mydb, "per_36_min_stats", per_36_min_stats, overwrite = T)
dbWriteTable(mydb, "per_100_poss_stats", per_100_poss_stats, overwrite = T)
dbWriteTable(mydb, "advanced_stats", advanced_stats, overwrite = T)
dbWriteTable(mydb, "player_bios", player_bios, overwrite = T)
dbWriteTable(mydb, "draft_combine_stats", draft_combine_stats, overwrite = T)
dbDisconnect(mydb)







