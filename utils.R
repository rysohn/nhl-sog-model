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

# Updated to teams() from get_teams()
teams_df <- teams() %>%
  dplyr::select(any_of(c('id', 'fullName', 'triCode', 'team_id', 'full_name', 'tri_code')))

get_team_id <- function(tricode) {
  # Handle both old and new 0.5.0 column naming conventions
  tri_col <- if("triCode" %in% names(teams_df)) "triCode" else "tri_code"
  id_col <- if("id" %in% names(teams_df)) "id" else "team_id"
  
  id <- teams_df %>%
    dplyr::filter(!!sym(tri_col) == toupper(tricode)) %>%
    pull(!!sym(id_col))
    
  if(length(id) > 1 || length(id) == 0) id <- 68
  return(id)
}

get_team_schedule_df <- function(team_tricode, date, season = 20252026) {
  date <- as.Date(date)
  team_id <- get_team_id(team_tricode)
  
  # Updated to team_season_schedule()
  sched <- team_season_schedule(team = team_id, season = season) 
  
  # Normalize columns to handle the 0.5.0 standardization
  names(sched) <- tolower(gsub("([A-Z])", "_\\1", names(sched)))
  names(sched) <- gsub("\\.", "_", names(sched))
  
  sched <- sched %>%
    dplyr::filter(gametype == 2 | game_type == 2) %>%
    dplyr::filter(as.Date(gamedate | game_date) < date)
    
  return(sched)
}

get_game_shot_report <- function(game_id, team_id) {
  # Updated to gc_play_by_play()
  pbp <- gc_play_by_play(game = game_id)
  
  # Normalize columns to handle the 0.5.0 standardization
  names(pbp) <- tolower(gsub("([A-Z])", "_\\1", names(pbp)))
  names(pbp) <- gsub("\\.", "_", names(pbp))
  
  # Look for either old or new type code column names
  type_col <- if("typecode" %in% names(pbp)) "typecode" else "type_code"
  team_col <- if("details_eventownerteamid" %in% names(pbp)) "details_eventownerteamid" else "event_owner_team_id"
  period_col <- if("perioddescriptor_number" %in% names(pbp)) "perioddescriptor_number" else "period_number"
  
  if (nrow(pbp) == 0 || !(type_col %in% names(pbp))) {
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
  
  shot_pbp <- pbp %>%
    dplyr::filter(!!sym(type_col) %in% shot_codes) %>%
    dplyr::filter(!!sym(team_col) == team_id) %>%
    dplyr::filter(!!sym(period_col) %in% c(1,2,3,4))
    
  shot_report <- shot_pbp %>%
    mutate(
      total_attempt = 1L,
      sog_or_goal = if_else(!!sym(type_col) %in% c(505, 506), 1L, 0L),
      blocked = if_else(!!sym(type_col) == 508, 1L, 0L),
      missed = if_else(!!sym(type_col) == 507, 1L, 0L)
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
  
  away_sched <- get_team_schedule_df(team_tricode, date, season = 20252026) %>%
    dplyr::filter(awayteam_id == team_id | away_team_id == team_id) 
    
  n_away <- nrow(away_sched)
  game_log <- data.frame(matrix(ncol=4, nrow=0))
  colnames(game_log) <- c('total_shot_attempts', 'sog_plus_goals', 'blocked_shots', 'missed_shots')
  
  for (i in 1:n_away){
    game_id <- away_sched$id[nrow(away_sched)-(i-1)] 
    if(is.null(game_id)) game_id <- away_sched$game_id[nrow(away_sched)-(i-1)]
    
    game_report <- get_game_shot_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  
  return(game_log %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))))
}

get_home_splits <- function(team_tricode, date){
  team_id <- get_team_id(tricode = toupper(team_tricode))
  
  home_sched <- get_team_schedule_df(team_tricode, date, season = 20252026) %>%
    dplyr::filter(hometeam_id == team_id | home_team_id == team_id)
    
  n_home <- nrow(home_sched)
  game_log <- data.frame(matrix(ncol=4, nrow=0))
  colnames(game_log) <- c('total_shot_attempts', 'sog_plus_goals', 'blocked_shots', 'missed_shots')
  
  for (i in 1:n_home){
    game_id <- home_sched$id[nrow(home_sched)-(i-1)] 
    if(is.null(game_id)) game_id <- home_sched$game_id[nrow(home_sched)-(i-1)]
    
    game_report <- get_game_shot_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  
  return(game_log %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))))
}

get_team_shot_log <- function(team_tricode, n, date){
  sched <- get_team_schedule_df(team_tricode, date, 20252026)
  team_id <- get_team_id(team_tricode)
  
  game_log <- data.frame(matrix(ncol=4, nrow=0))
  colnames(game_log) <- c('total_shot_attempts', 'sog_plus_goals', 'blocked_shots', 'missed_shots')
  
  for (i in 1:n){
    game_id <- sched$id[nrow(sched)-(i-1)] 
    if(is.null(game_id)) game_id <- sched$game_id[nrow(sched)-(i-1)]
    
    game_report <- get_game_shot_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  return(game_log %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))))
}

get_full_home_preds <- function(tricode, date){
  l8_log <- get_team_shot_log(tricode, 8, date) %>% rename(l8_sa = total_shot_attempts, l8_sog = sog_plus_goals, l8_miss = missed_shots, l8_blocks = blocked_shots)
  l4_log  <- get_team_shot_log(tricode, 4, date) %>% rename(l4_sa = total_shot_attempts, l4_sog = sog_plus_goals, l4_miss = missed_shots, l4_blocks = blocked_shots)
  ha_split <- get_home_splits(tricode, date) %>% rename(ha_sa = total_shot_attempts, ha_sog = sog_plus_goals, ha_miss = missed_shots, ha_blocks = blocked_shots)
  return(cbind(l8_log, l4_log, ha_split))
}
  
get_full_away_preds <- function(tricode, date){
  l8_log <- get_team_shot_log(tricode, 8, date) %>% rename(l8_sa = total_shot_attempts, l8_sog = sog_plus_goals, l8_miss = missed_shots, l8_blocks = blocked_shots)
  l4_log  <- get_team_shot_log(tricode, 4, date) %>% rename(l4_sa = total_shot_attempts, l4_sog = sog_plus_goals, l4_miss = missed_shots, l4_blocks = blocked_shots)
  ha_split <- get_away_splits(tricode, date) %>% rename(ha_sa = total_shot_attempts, ha_sog = sog_plus_goals, ha_miss = missed_shots, ha_blocks = blocked_shots)
  return(cbind(l8_log, l4_log, ha_split))
}

get_game_shot_allowed_report <- function(game_id, team_id) {
  # Updated to gc_play_by_play()
  pbp <- gc_play_by_play(game = game_id)
  names(pbp) <- tolower(gsub("([A-Z])", "_\\1", names(pbp)))
  names(pbp) <- gsub("\\.", "_", names(pbp))
  
  type_col <- if("typecode" %in% names(pbp)) "typecode" else "type_code"
  team_col <- if("details_eventownerteamid" %in% names(pbp)) "details_eventownerteamid" else "event_owner_team_id"
  
  if (nrow(pbp) == 0 || !(type_col %in% names(pbp))) {
    return(tibble(total_shot_attempts = NA_real_, shots_on_goal_plus_goals = NA_real_, blocked_shots = NA_real_, missed_shots = NA_real_))
  }
  
  shot_codes <- c(505, 506, 507, 508) 
  shot_report <- pbp %>%
    dplyr::filter(!!sym(type_col) %in% shot_codes) %>%
    dplyr::filter(!!sym(team_col) != team_id) %>%
    mutate(
      total_attempt = 1L,
      sog_or_goal = if_else(!!sym(type_col) %in% c(505, 506), 1L, 0L),
      blocked = if_else(!!sym(type_col) == 508, 1L, 0L),
      missed = if_else(!!sym(type_col) == 507, 1L, 0L)
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
  sched <- get_team_schedule_df(team_tricode, date, season = 20252026)
  team_id <- get_team_id(team_tricode)
  num_games <- nrow(sched)
  
  game_log <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(game_log) <- c('total_shot_attempts', 'shots_on_goal_plus_goals', 'blocked_shots', 'missed_shots')
  
  for (i in 1:n) {
    game_id <- sched$id[num_games - (i - 1)]
    if(is.null(game_id)) game_id <- sched$game_id[num_games - (i - 1)]
    
    game_report <- get_game_shot_allowed_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  return(game_log %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))))
}

get_home_allowed_splits <- function(team_tricode, date) {
  team_id <- get_team_id(team_tricode)
  home_sched <- get_team_schedule_df(team_tricode, date, season = 20252026) %>%
    dplyr::filter(hometeam_id == team_id | home_team_id == team_id)
    
  n_home <- nrow(home_sched)
  game_log <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(game_log) <- c('total_shot_attempts', 'shots_on_goal_plus_goals', 'blocked_shots', 'missed_shots')
  
  for (i in 1:n_home) {
    game_id <- home_sched$id[n_home - (i - 1)]
    if(is.null(game_id)) game_id <- home_sched$game_id[n_home - (i - 1)]
    game_report <- get_game_shot_allowed_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  return(game_log %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))))
}

get_away_allowed_splits <- function(team_tricode, date) {
  team_id <- get_team_id(team_tricode)
  away_sched <- get_team_schedule_df(team_tricode, date, season = 20252026) %>%
    dplyr::filter(awayteam_id == team_id | away_team_id == team_id)
    
  n_away <- nrow(away_sched)
  game_log <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(game_log) <- c('total_shot_attempts', 'shots_on_goal_plus_goals', 'blocked_shots', 'missed_shots')
  
  for (i in 1:n_away) {
    game_id <- away_sched$id[n_away - (i - 1)]
    if(is.null(game_id)) game_id <- away_sched$game_id[n_away - (i - 1)]
    game_report <- get_game_shot_allowed_report(game_id, team_id)
    game_log <- rbind(game_log, game_report)
  }
  return(game_log %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))))
}

get_full_against_home_preds <- function(tricode, date){
  l8_against <- get_team_shot_allowed_log(tricode, 8, date) %>% rename(l8_sa_ag = total_shot_attempts, l8_sog_ag = shots_on_goal_plus_goals, l8_miss_ag = missed_shots, l8_blocks_ag = blocked_shots)
  l4_against <- get_team_shot_allowed_log(tricode, 4, date) %>% rename(l4_sa_ag = total_shot_attempts, l4_sog_ag = shots_on_goal_plus_goals, l4_miss_ag = missed_shots, l4_blocks_ag = blocked_shots)
  home_against <- get_home_allowed_splits(tricode, date) %>% rename(ha_sa_ag = total_shot_attempts, ha_sog_ag = shots_on_goal_plus_goals, ha_miss_ag = missed_shots, ha_blocks_ag = blocked_shots)
  return(cbind(l8_against, l4_against, home_against))
}

get_full_against_away_preds <- function(tricode, date){
  l8_against <- get_team_shot_allowed_log(tricode, 8, date) %>% rename(l8_sa_ag = total_shot_attempts, l8_sog_ag = shots_on_goal_plus_goals, l8_miss_ag = missed_shots, l8_blocks_ag = blocked_shots)
  l4_against <- get_team_shot_allowed_log(tricode, 4, date) %>% rename(l4_sa_ag = total_shot_attempts, l4_sog_ag = shots_on_goal_plus_goals, l4_miss_ag = missed_shots, l4_blocks_ag = blocked_shots)
  away_against <- get_away_allowed_splits(tricode, date) %>% rename(ha_sa_ag = total_shot_attempts, ha_sog_ag = shots_on_goal_plus_goals, ha_miss_ag = missed_shots, ha_blocks_ag = blocked_shots)
  return(cbind(l8_against, l4_against, away_against))
}

get_b2b <- function(game_id, team_tricode) {
  team_id <- get_team_id(team_tricode)
  sched <- team_season_schedule(team = team_id, season = 20252026)
  names(sched) <- tolower(gsub("([A-Z])", "_\\1", names(sched)))
  
  date_col <- if("gamedate" %in% names(sched)) "gamedate" else "game_date"
  id_col <- if("id" %in% names(sched)) "id" else "game_id"
  
  if (!(date_col %in% names(sched)) || !(id_col %in% names(sched))) return(0)
  
  sched <- sched %>%
    dplyr::mutate(!!sym(date_col) := as.Date(!!sym(date_col))) %>%
    dplyr::arrange(!!sym(date_col))
  
  rownum <- which(sched[[id_col]] == game_id)
  if (length(rownum) == 0 || rownum == 1) return(0)
  
  days_rest <- as.numeric(sched[[date_col]][rownum] - sched[[date_col]][rownum - 1]) - 1
  return(ifelse(days_rest == 0, 1, 0))
}

get_daily_game_reports <- function(date) {
  # Updated to schedule()
  slate <- schedule(date = date) 
  names(slate) <- tolower(gsub("([A-Z])", "_\\1", names(slate)))
  names(slate) <- gsub("\\.", "_", names(slate))
  
  id_col <- if("id" %in% names(slate)) "id" else "game_id"
  away_abbrev <- if("awayteam_abbrev" %in% names(slate)) "awayteam_abbrev" else "away_team_abbrev"
  home_abbrev <- if("hometeam_abbrev" %in% names(slate)) "hometeam_abbrev" else "home_team_abbrev"
  
  df <- data.frame(game_id = slate[[id_col]], team = slate[[home_abbrev]], opponent = slate[[away_abbrev]], ha = 'home')
  names(df) <- c('game_id', 'team', 'opponent', 'h/a')
  
  df2 <- data.frame(game_id = slate[[id_col]], team = slate[[away_abbrev]], opponent = slate[[home_abbrev]], ha='away')
  names(df2) <- c('game_id', 'team', 'opponent', 'h/a')
  
  games <- rbind(df, df2)
  games[c('l8_sa', 'l8_sog', 'l8_miss', 'l8_blocks', 'l4_sa', 'l4_sog', 'l4_miss', 'l4_blocks', 'ha_sa', 'ha_sog', 'ha_miss', 'ha_blocks',
          'l8_sa_ag', 'l8_sog_ag', 'l8_miss_ag', 'l8_blocks_ag', 'l4_sa_ag', 'l4_sog_ag', 'l4_miss_ag', 'l4_blocks_ag', 
          'ha_sa_ag', 'ha_sog_ag', 'ha_miss_ag', 'ha_blocks_ag', 'is_b2b', 'opp_is_b2b', 'true_sog')] <- NA

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
    games$is_b2b[i] <- get_b2b(games$game_id[i], games$team[i])
    games$opp_is_b2b[i] <- get_b2b(games$game_id[i], games$opponent[i])
    if (date <= Sys.Date()){
      tid <- get_team_id(games$team[i])
      true_sog <- get_game_shot_report(games$game_id[i], tid)
      games$true_sog[i] <- true_sog$shots_on_goal_plus_goals
    }
  }
  return(games)
}