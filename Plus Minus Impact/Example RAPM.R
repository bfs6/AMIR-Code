####Load Libraries####
library(ggplot2)
library(broom)
library(MASS)
library(dplyr)

####Load and Run RAPM Counting Function####
##Load
source(paste0(getwd(), "/RAPM Counting Function v2.R"))

##Run Function from 1998 to 2018
stint_list <- list()
for(i in c(1998:2018)){
  stint_list[[i]] <- rapm_counting_function(year = i)
}
names(stint_list) <- c(1998:2018)


##Practice w/ 2016
stint2k16 <- rapm_counting_function(year = 2016)
stint2k17 <- rapm_counting_function(year = 2017)

####Run RAPM 2016####
##Get Rid of Low Minutes 
year = 2016
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

low_min_players <- player_df[which(player_df$minutes_played <= 250), ]
low_min_players2 <- player_df[which(player_df$minutes_played <= 1000), ]

lowstints <- stint2k16[, -which(names(stint2k16) %in% low_min_players$player)]
lowstints3 <- lowstints[which(rowSums(abs(lowstints[,1:(length(lowstints)-1)])) == 10), ]

lowstints2 <- stint2k16[, -which(names(stint2k16) %in% low_min_players2$player)]
lowstints4 <- lowstints2[which(rowSums(abs(lowstints2[,1:(length(lowstints2)-1)])) == 10), ]


##Run APM Model
apm <- lm(formula = Margin~., data = lowstints4)
summary(apm)
sort(apm$coefficients, decreasing = T)


##Run RAPM Model
stint2k16_clean <- stint2k16[,which(colSums(abs(stint2k16)) != 0)]
rapm2 <- lm.ridge(formula = Margin~., data = stint2k16_clean, lambda = seq(0,80000,400))
r2 <- tidy(rapm2)
g <- glance(rapm2)
ggplot(r2, aes(lambda, GCV)) + geom_line() + geom_vline(xintercept = g$lambdaGCV, col = "red", lty = 2)

results <- sort(rapm2$coef[,which(as.numeric(colnames(rapm2$coef)) == g$lambdaGCV)], decreasing = T)
names(results) <- gsub("`", "", names(results))
highminresults <- results[-which(names(results) %in% low_min_players2$player)]


####Run RAPM 2017####
##Get Rid of Low Minutes 
year = 2017
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

low_min_players <- player_df[which(player_df$minutes_played <= 125), ]
low_min_players2 <- player_df[which(player_df$minutes_played <= 250), ]

lowstints <- stint2k17[, -which(names(stint2k17) %in% low_min_players$player)]
lowstints3 <- lowstints[which(rowSums(abs(lowstints[,1:(length(lowstints)-1)])) == 10), ]

lowstints2 <- stint2k17[, -which(names(stint2k17) %in% low_min_players2$player)]
lowstints4 <- lowstints2[which(rowSums(abs(lowstints2[,1:(length(lowstints2)-1)])) == 10), ]


##Run APM Model
apm <- lm(formula = Margin~., data = lowstints4)
summary(apm)
sort(apm$coefficients, decreasing = T)


##Run RAPM Model
stint2k17_clean <- stint2k17[,which(colSums(abs(stint2k17)) != 0)]
rapm2_2017 <- lm.ridge(formula = Margin~., data = stint2k17_clean, lambda = seq(0,80000,400))
r2 <- tidy(rapm2_2017)
g <- glance(rapm2)
ggplot(r2, aes(lambda, GCV)) + geom_line() + geom_vline(xintercept = g$lambdaGCV, col = "red", lty = 2)

results <- sort(rapm2_2017$coef[,which(as.numeric(colnames(rapm2_2017$coef)) == g$lambdaGCV)], decreasing = T)
names(results) <- gsub("`", "", names(results))
highminresults <- results[-which(names(results) %in% low_min_players2$player)]

