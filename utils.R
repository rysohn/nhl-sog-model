library(tidyverse)
library(nhlscraper)
library(cluster)
library(MASS)
library(caret)
library(Metrics)
library(glmnet)
library(lme4)
library(httr)
library(jsonlite)


get_team_schedule_df <- function(team_tricode, date, season = 20252026) {
  date <- as.Date(date)
  sched <- get_team_schedule(team = toupper(team_tricode), season = season) %>%
    dplyr::filter(gameType == 2) %>%
    dplyr::filter(as.Date(gameDate) < date) %>%
    dplyr::select(all_of(c("id", "gameDate", "startTimeUTC", "venueUTCOffset",
                    "awayTeam.id", "awayTeam.abbrev", "awayTeam.score",
                    "homeTeam.id", "homeTeam.abbrev", "homeTeam.score",
                    "gameOutcome.lastPeriodType")))
  return(sched)
}


teams <- get_teams() %>%
  dplyr::select('id', 'fullName', 'triCode')

get_team_id <- function(tricode) {
  id <- teams %>%
    dplyr::filter(triCode == toupper(tricode))
  id <- id$id
  if(length(id)>1)
    id <- 68
  return(id)
}


get_game_shot_report <- function(game_id, team_id) {
  pbp <- get_gc_play_by_play(game_id)
  #head(pbp, 50)
  #colnames(pbp)
  if (nrow(pbp) == 0 || !"typeCode" %in% names(pbp)) {
    return(
      tibble(
        total_shot_attempts = NA_real_,
        shots_on_goal_plus_goals = NA_real_,
        blocked_shots = NA_real_,
        missed_shots = NA_real_
      )
    )
  }
  shot_codes <- c(505, 506, 507, 508)
  ## 505-goal, 506-shot on goal, 507-missed shot, 508-blocked shot
  shot_pbp <- pbp %>%
    dplyr::filter(typeCode %in% shot_codes)
  shot_pbp <- shot_pbp %>%
    dplyr::select(any_of(c('typeCode', 'sortOrder', 'periodDescriptor.number', 
           'details.eventOwnerTeamId', 'details.shotType', 'details.homeScore', 'details.awayScore'))) %>%
    dplyr::filter(details.eventOwnerTeamId == team_id) %>%
    dplyr::filter(periodDescriptor.number %in% c(1,2,3,4))
  #print(shot_pbp$details.eventOwnerTeamId)
  #return(shot_pbp)
  shot_report <- shot_pbp %>%
    mutate(
      total_attempt = 1L,
      sog_or_goal = if_else(typeCode %in% c(505, 506), 1L, 0L),
      blocked = if_else(typeCode == 508, 1L, 0L),
      missed = if_else(typeCode == 507, 1L, 0L)
    ) %>%
    summarise(
      total_shot_attempts = sum(total_attempt, na.rm = TRUE),
      shots_on_goal_plus_goals = sum(sog_or_goal, na.rm = TRUE),
      blocked_shots = sum(blocked, na.rm = TRUE),
      missed_shots = sum(missed, na.rm = TRUE),
      .groups = "drop"
    )
  return(shot_report)
}


get_away_splits <- function(team_tricode, date){
  team_id <- get_team_id(tricode = toupper(team_tricode))
  #return(team_id)
  
  away_sched <- get_team_schedule_df(team = toupper(team_tricode), date, season = 20252026) %>%
    dplyr::filter(awayTeam.id == team_id) %>%
    dplyr::select(all_of(c("id", "gameDate", "startTimeUTC", "venueUTCOffset",
                    "awayTeam.id", "awayTeam.abbrev", "awayTeam.score",
                    "homeTeam.id", "homeTeam.abbrev", "homeTeam.score",
                    "gameOutcome.lastPeriodType")))
  n_away <- nrow(away_sched)
  
  game_log <- data.frame(matrix(ncol=4, nrow=0))
  cols <- c('total_shot_attempts', 'sog_plus_goals', 'blocked_shots', 'missed_shots')
  colnames(game_log) <- cols
  
  for (i in 1:n_away){
    game_id <- away_sched$id[nrow(away_sched)-(i-1)] 
    #print(game_id)
    
    game_report <- get_game_shot_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  
  away_mean_team_log <- game_log %>% summarise(across(where(is.numeric), mean))
  return(away_mean_team_log)
}


get_home_splits <- function(team_tricode, date){
  team_id <- get_team_id(tricode = toupper(team_tricode))
  #return(team_id)
  
  home_sched <- get_team_schedule_df(team = toupper(team_tricode), date, season = 20252026) %>%
    dplyr::filter(homeTeam.id == team_id) %>%
    dplyr::select(all_of(c("id", "gameDate", "startTimeUTC", "venueUTCOffset",
                    "awayTeam.id", "awayTeam.abbrev", "awayTeam.score",
                    "homeTeam.id", "homeTeam.abbrev", "homeTeam.score",
                    "gameOutcome.lastPeriodType")))
  n_home <- nrow(home_sched)
  
  game_log <- data.frame(matrix(ncol=4, nrow=0))
  cols <- c('total_shot_attempts', 'sog_plus_goals', 'blocked_shots', 'missed_shots')
  colnames(game_log) <- cols
  dummy <- 0
  for (i in 1:n_home){
    #print(game_id)
    game_id <- home_sched$id[nrow(home_sched)-(i-1)] 
    game_report <- get_game_shot_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  
  home_mean_team_log <- game_log %>% summarise(across(where(is.numeric), mean))
  return(home_mean_team_log)
}


get_team_shot_log <- function(team_tricode, n, date){
  sched <- get_team_schedule_df(team_tricode, date, 20252026)
  team_id <- get_team_id(team_tricode)
  game_log <- data.frame(matrix(ncol=4, nrow=0))
  cols <- c('total_shot_attempts', 'sog_plus_goals', 'blocked_shots', 'missed_shots')
  colnames(game_log) <- cols
  
  #print(team_id)
  for (i in 1:n){
    game_id <- sched$id[nrow(sched)-(i-1)] 
    #print(game_id)
    
    game_report <- get_game_shot_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  game_log <- game_log %>% summarise(across(where(is.numeric), mean))
  return(game_log)
}


get_full_home_preds <- function(tricode, date){
  l8_log <- get_team_shot_log(tricode, 8, date)
  l4_log  <- get_team_shot_log(tricode, 4, date)
  ha_split <- get_home_splits(tricode, date)
  
  l8_log <- l8_log %>%
    dplyr::select(total_shot_attempts, shots_on_goal_plus_goals, missed_shots) %>%
    rename(l8_sa = total_shot_attempts, l8_sog = shots_on_goal_plus_goals, l8_miss = missed_shots)
  l4_log <- l4_log %>%
    dplyr::select(total_shot_attempts, shots_on_goal_plus_goals, missed_shots) %>%
    rename(l4_sa = total_shot_attempts, l4_sog = shots_on_goal_plus_goals, l4_miss = missed_shots)
  ha_split <- ha_split %>%
    dplyr::select(total_shot_attempts, shots_on_goal_plus_goals, missed_shots) %>%
    rename(ha_sa = total_shot_attempts, ha_sog = shots_on_goal_plus_goals, ha_miss = missed_shots)
  
  shot_preds <- cbind(l8_log, l4_log, ha_split)
  return(shot_preds)
}
  

get_full_away_preds <- function(tricode, date){
  l8_log <- get_team_shot_log(tricode, 8, date) 
  l4_log  <- get_team_shot_log(tricode, 4, date)
  ha_split    <- get_away_splits(tricode, date)
  
  l8_log <- l8_log %>%
    dplyr::select(total_shot_attempts, shots_on_goal_plus_goals, missed_shots) %>%
    rename(l8_sa = total_shot_attempts, l8_sog = shots_on_goal_plus_goals, l8_miss = missed_shots)
  l4_log <- l4_log %>%
    dplyr::select(total_shot_attempts, shots_on_goal_plus_goals, missed_shots) %>%
    rename(l4_sa = total_shot_attempts, l4_sog = shots_on_goal_plus_goals, l4_miss = missed_shots)
  ha_split <- ha_split %>%
    dplyr::select(total_shot_attempts, shots_on_goal_plus_goals, missed_shots) %>%
    rename(ha_sa = total_shot_attempts, ha_sog = shots_on_goal_plus_goals, ha_miss = missed_shots)
  
  shot_preds <- cbind(l8_log, l4_log, ha_split)
  return(shot_preds)
}


get_game_shot_allowed_report <- function(game_id, team_id) {
  pbp <- get_gc_play_by_play(game_id)
  shot_codes <- c(505, 506, 507, 508) # goal, shot on goal, missed, blocked
  shot_pbp <- pbp %>%
    dplyr::filter(typeCode %in% shot_codes) %>%
    dplyr::select(any_of(c('typeCode', 'sortOrder', 'periodDescriptor.number',
           'details.eventOwnerTeamId', 'details.shotType', 'details.homeScore', 'details.awayScore'))) %>%
    dplyr::filter(details.eventOwnerTeamId != team_id) # events by the opponent → allowed by team_id
  shot_report <- shot_pbp %>%
    mutate(
      total_attempt = 1L,
      sog_or_goal = if_else(typeCode %in% c(505, 506), 1L, 0L),
      blocked = if_else(typeCode == 508, 1L, 0L),
      missed = if_else(typeCode == 507, 1L, 0L)
    ) %>%
    summarise(
      total_shot_attempts = sum(total_attempt, na.rm = TRUE),
      shots_on_goal_plus_goals = sum(sog_or_goal, na.rm = TRUE),
      blocked_shots = sum(blocked, na.rm = TRUE),
      missed_shots = sum(missed, na.rm = TRUE),
      .groups = "drop"
    )
  return(shot_report)
}


get_team_shot_allowed_log <- function(team_tricode, n, date) {
  sched <- get_team_schedule_df(team = toupper(team_tricode), date, season = 20252026)
  team_id <- get_team_id(tricode = toupper(team_tricode))
  num_games <- nrow(sched)
  game_log <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(game_log) <- c('total_shot_attempts', 'shots_on_goal_plus_goals', 'blocked_shots', 'missed_shots')
  for (i in 1:n) {
    game_id <- sched$id[num_games - (i - 1)]
    game_report <- get_game_shot_allowed_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  game_log <- game_log %>% summarise(across(where(is.numeric), mean))
  return(game_log)
}


get_home_allowed_splits <- function(team_tricode, date) {
  team_id <- get_team_id(tricode = toupper(team_tricode))
  home_sched <- get_team_schedule_df(team = toupper(team_tricode), date, season = 20252026) %>%
    dplyr::filter(homeTeam.id == team_id)
  n_home <- nrow(home_sched)
  game_log <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(game_log) <- c('total_shot_attempts', 'shots_on_goal_plus_goals', 'blocked_shots', 'missed_shots')
  for (i in 1:n_home) {
    game_id <- home_sched$id[n_home - (i - 1)]
    game_report <- get_game_shot_allowed_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  home_mean_allowed <- game_log %>% summarise(across(where(is.numeric), mean))
  return(home_mean_allowed)
}


get_away_allowed_splits <- function(team_tricode, date) {
  team_id <- get_team_id(tricode = toupper(team_tricode))
  away_sched <- get_team_schedule_df(team = toupper(team_tricode), date, season = 20252026) %>%
    dplyr::filter(awayTeam.id == team_id)
  n_away <- nrow(away_sched)
  game_log <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(game_log) <- c('total_shot_attempts', 'shots_on_goal_plus_goals', 'blocked_shots', 'missed_shots')
  for (i in 1:n_away) {
    game_id <- away_sched$id[n_away - (i - 1)]
    game_report <- get_game_shot_allowed_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  away_mean_allowed <- game_log %>% summarise(across(where(is.numeric), mean))
  return(away_mean_allowed)
}


get_full_against_home_preds <- function(tricode, date){
  l8_against <- get_team_shot_allowed_log(tricode, 8, date) 
  l4_against  <- get_team_shot_allowed_log(tricode, 4, date) 
  home_against <- get_home_allowed_splits(tricode, date)
  
  l8_against <- l8_against %>%
    dplyr::select(all_of(c('total_shot_attempts', 'shots_on_goal_plus_goals', 'missed_shots'))) %>%
    rename(l8_sa_ag = total_shot_attempts, l8_sog_ag = shots_on_goal_plus_goals, l8_miss_ag = missed_shots)
  l4_against <- l4_against %>%
    dplyr::select(all_of(c('total_shot_attempts', 'shots_on_goal_plus_goals', 'missed_shots'))) %>%
    rename(l4_sa_ag = total_shot_attempts, l4_sog_ag = shots_on_goal_plus_goals, l4_miss_ag = missed_shots)
  home_against <- home_against %>%
    dplyr::select(all_of(c('total_shot_attempts', 'shots_on_goal_plus_goals', 'missed_shots'))) %>%
    rename(ha_sa_ag = total_shot_attempts, ha_sog_ag = shots_on_goal_plus_goals, ha_miss_ag = missed_shots)
  
  shot_against_preds <- cbind(l8_against, l4_against, home_against)
  return(shot_against_preds)
}


get_full_against_away_preds <- function(tricode, date){
  l8_against <- get_team_shot_allowed_log(tricode, 8, date)
  l4_against  <- get_team_shot_allowed_log(tricode, 4, date)
  away_against <- get_away_allowed_splits(tricode, date)
  
  l8_against <- l8_against %>%
    dplyr::select(all_of(c('total_shot_attempts', 'shots_on_goal_plus_goals', 'missed_shots'))) %>%
    rename(l8_sa_ag = total_shot_attempts, l8_sog_ag = shots_on_goal_plus_goals, l8_miss_ag = missed_shots)
  l4_against <- l4_against %>%
    dplyr::select(all_of(c('total_shot_attempts', 'shots_on_goal_plus_goals', 'missed_shots'))) %>%
    rename(l4_sa_ag = total_shot_attempts, l4_sog_ag = shots_on_goal_plus_goals, l4_miss_ag = missed_shots)
  away_against <- away_against %>%
    dplyr::select(all_of(c('total_shot_attempts', 'shots_on_goal_plus_goals', 'missed_shots'))) %>%
    rename(ha_sa_ag = total_shot_attempts, ha_sog_ag = shots_on_goal_plus_goals, ha_miss_ag = missed_shots)
  
  shot_against_preds <- cbind(l8_against, l4_against, away_against)
  return(shot_against_preds)
}


get_rest <- function(game_id, team_tricode, max_rest = 7) {

  sched <- get_team_schedule(team = toupper(team_tricode))

  # Defensive check
  if (!"gameDate" %in% names(sched) || !"id" %in% names(sched)) {
    return(max_rest)
  }

  sched <- sched %>%
    dplyr::mutate(gameDate = as.Date(gameDate)) %>%
    dplyr::arrange(gameDate)

  rownum <- which(sched$id == game_id)

  # Game not found (future game, preseason mismatch, etc.)
  if (length(rownum) == 0) {
    last_game_date <- max(sched$gameDate, na.rm = TRUE)
    return(min(as.numeric(Sys.Date() - last_game_date) - 1, max_rest))
  }

  # First game of season
  if (rownum == 1) {
    return(max_rest)
  }

  days_rest <- as.numeric(
    sched$gameDate[rownum] - sched$gameDate[rownum - 1]
  ) - 1

  return(min(days_rest, max_rest))
}


get_daily_game_reports <- function(date) {
  slate <- get_schedule(date = date) %>%
    dplyr::select(c('id', 'gameType', 'startTimeUTC', 'awayTeam.id', 'awayTeam.abbrev',
             'homeTeam.id', 'homeTeam.abbrev')) 
  
  df <- data.frame(game_id = slate[,1], team = slate[,5], opponent = slate[,7], ha = 'home')
  names(df) <- c('game_id', 'team', 'opponent', 'h/a')
  
  df2 <- data.frame(game_id = slate[,1], team = slate[,7], opponent = slate[,5], ha='away')
  names(df2) <- c('game_id', 'team', 'opponent', 'h/a')
  games <- rbind(df, df2)
  games[c('l8_sa', 'l8_sog', 'l8_miss', 'l4_sa', 'l4_sog', 'l4_miss', 'ha_sa', 'ha_sog', 'ha_miss',
          'l8_sa_ag', 'l8_sog_ag', 'l8_miss_ag', 'l4_sa_ag', 'l4_sog_ag', 'l4_miss_ag', 
          'ha_sa_ag', 'ha_sog_ag', 'ha_miss_ag', 'rest_diff', 'true_sog')] <- NA
  #return(games)
  temp4 <- 0
  for (i in 1:nrow(games)) {
    if (games$'h/a'[i] == 'home'){
      temp <- get_full_home_preds(games$team[i], date)
      common_cols <- intersect(names(games[i,]), names(temp))
      games[i, common_cols] <- temp[1, common_cols]
      
      temp2 <- get_full_against_away_preds(games$opponent[i], date)
      common_cols_2 <- intersect(names(games[i,]), names(temp2))
      games[i, common_cols_2] <- temp2[1, common_cols_2]
      
    }else if (games$'h/a'[i] == 'away'){
      temp <- get_full_away_preds(games$team[i], date)
      common_cols <- intersect(names(games[i,]), names(temp))
      games[i, common_cols] <- temp[1, common_cols]
      
      temp2 <- get_full_against_home_preds(games$opponent[i], date)
      common_cols_2 <- intersect(names(games[i,]), names(temp2))
      games[i, common_cols_2] <- temp2[1, common_cols_2]
    }
    games$rest_diff[i] <- get_rest(games$game_id[i], games$team[i]) - get_rest(games$game_id[i], games$opponent[i])
    if (date >= Sys.Date()){
      tid <- get_team_id(games$team[i])
      true_sog <- get_game_shot_report(games$game_id[i], tid)
      games$true_sog[i] <- true_sog$shots_on_goal_plus_goals
    }
  }
  return(games)
}


