library(dplyr)

results2021  <- read.csv('/Users/hanson377/Documents/GitHub/kaggle_projects/mens_bb_2021/data_part_ii/MRegularSeasonDetailedResults.csv')
seeds2021  <- read.csv('/Users/hanson377/Documents/GitHub/kaggle_projects/mens_bb_2021/data_part_ii/MNCAATourneySeeds.csv')
names  <- read.csv('/Users/hanson377/Documents/GitHub/kaggle_projects/mens_bb_2021/data_part_ii/MTeams.csv')

seeds2021 <- subset(seeds2021,Season == '2021')
teams2021 <- seeds2021 %>% select(TeamID)

## now clean season results
season_results <- subset(results2021,Season == '2021')


cleanSeasonData <- function(data){
wins <- data %>% select(Season,DayNum,TeamID = WTeamID, OpposingTeamID = LTeamID, FGM = WFGM, FGA = WFGA,OppFGM = LFGM, OppFGA = LFGA, FGM3 = WFGM3, FGA3 = WFGA3, OppFGM3 = LFGM3, OppFGA3 = LFGA3, FTM = WFTM, FTA = WFTA, OppFTM = LFTM, OppFTA = LFTA) %>% mutate(status = 'won')

losses <- data %>% select(Season,DayNum,TeamID = LTeamID, OpposingTeamID = WTeamID,FGM = LFGM, FGA = LFGA, OppFGM = WFGM, OppFGA = WFGA, FGM3 = LFGM3, FGA3 = LFGA3, OppFGM3 = WFGM3, OppFGA3 = WFGA3, FTM = LFTM, FTA = LFTA, OppFTM = WFTM, OppFTA = WFTA) %>% mutate(status = 'lost')

games <- rbind(wins,losses)
games$diffFG <- (games$FGM-games$OppFGM)
games$diffFG2 <- (games$FGM-games$FGM3)-(games$OppFGM-games$OppFGM3)
games$diffFG3 <- (games$FGM3-games$OppFGM3)
games$diffFT <- (games$FTM-games$OppFTM)

games$diffPoints <- (games$diffFG2*2)+(games$diffFG3*2)+(games$diffFT*1)

summary <- games %>% group_by(TeamID,Season) %>% summarise(total_games = n(), FGM = sum(FGM), FGA = sum(FGA), OppFGM = sum(OppFGM), OppFGA = sum(OppFGA), FGM3 = sum(FGM3), FGA3 = sum(FGA3), OppFGM3 = sum(OppFGM3), OppFGA3 = sum(OppFGA3), FTM = sum(FTM), FTA = sum(FTA), OppFTM = sum(OppFTM), OppFTA = sum(OppFTA), meanDiffPoints = mean(diffPoints), sdDiffPoints = sd(diffPoints))

return(list(summary,games))
}
season2021 <- cleanSeasonData(season_results)[[1]]

## only keep 2021 march madness teams
season2021 <- season2021 %>% inner_join(teams2021,by='TeamID') %>% left_join()

## add names
names <- names %>% select(TeamID,TeamName)
season2021 <- season2021 %>% left_join(names,by='TeamID')

write.csv(season2021,'/Users/hanson377/Documents/GitHub/march_madness_pickem/data/team_data.csv')
