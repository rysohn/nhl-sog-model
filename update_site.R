# update_site.R
library(tidyverse)
library(MASS)
library(caret)
library(nhlscraper)
library(gt)
library(httr)
library(jsonlite)

# 1. Load Functions & Data
source("utils.R") 
nb_fit <- readRDS("nb_sog_model.rds")
scaler <- readRDS("sog_scaler.rds")

if(file.exists("train_data.RData")) {
  load("train_data.RData")
  mean_sog <- mean(train_data$true_sog, na.rm=TRUE)
} else {
  mean_sog <- 28.02844 
  warning("train_data.RData not found. Using default mean_sog = 28.03")
}

# 2. Get Today's Date
today <- as.Date(format(Sys.time(), tz = "America/New_York"))
print(paste("Running model for:", today))

tryCatch({
  # 3. Fetch Games
  daily_reports <- get_daily_game_reports(today)
  
  if(nrow(daily_reports) == 0) {
    stop("No games scheduled today.")
  }

  # Feature Engineering
  daily_reports$sog_trend <- daily_reports$l4_sog - daily_reports$l8_sog
  daily_reports$sog_ag_trend <- daily_reports$l4_sog_ag - daily_reports$l8_sog_ag
  daily_reports$home <- ifelse(daily_reports$'h/a' == 'home', 1, 0)
  daily_reports$shot_perc <- daily_reports$l8_sog / daily_reports$l8_sa
  daily_reports$shot_perc_ag <- daily_reports$l8_sog_ag / daily_reports$l8_sa_ag
  daily_reports$pred_sog <- NA
  
  daily_report_red <- daily_reports %>% 
    dplyr::select(c('game_id', 'team', 'opponent', 'l8_sog', 'sog_trend', 
                    'l8_sog_ag', 'sog_ag_trend', 'rest_diff', 
                    'shot_perc', 'shot_perc_ag', 'home', 'pred_sog'))
  
  # Scaling
  req_vars <- scaler$method$center
  miss_vars <- setdiff(req_vars, names(daily_report_red))
  daily_report_red[miss_vars] <- 0
  
  daily_report_red_scaled <- daily_report_red
  daily_report_red_scaled[req_vars] <- predict(scaler, daily_report_red[req_vars])
  
  daily_report_red_scaled <- daily_report_red_scaled %>%
    dplyr::select(all_of(c('game_id', 'team', 'opponent', 'l8_sog', 'sog_trend', 
                           'l8_sog_ag', 'sog_ag_trend', 'rest_diff', 'shot_perc', 
                           'shot_perc_ag', 'home', 'pred_sog')))

  # Prediction
  daily_report_red_scaled$pred_sog <- predict(nb_fit, newdata=daily_report_red_scaled, type='response')
  daily_report_red_scaled$pred_sog <- round(daily_report_red_scaled$pred_sog, 1)

  # Amplifier
  spread_factor <- 1.2
  daily_report_red_scaled$pred_sog_wide <- round(mean_sog + 
    (daily_report_red_scaled$pred_sog - mean_sog) * spread_factor, 1)

  # Floor / Ceiling
  mu_pred <- as.vector(daily_report_red_scaled$pred_sog)
  theta <- nb_fit$theta

  set.seed(42)
  simulations <- matrix(NA, nrow=length(mu_pred), ncol=1000)

  for(i in 1:length(mu_pred)){
    simulations[i,] <- MASS::rnegbin(1000, mu=mu_pred[i], theta=theta)
  }

  daily_report_red_scaled$ceiling_sog <- round(apply(simulations, 1, quantile, probs=0.75, na.rm=TRUE), 1)
  daily_report_red_scaled$floor_sog <- round(apply(simulations, 1, quantile, probs=0.25, na.rm=TRUE), 1)

  # CRITICAL FIX 1: Removed team_id function entirely
  daily_report_red_scaled <- daily_report_red_scaled %>%
  mutate(
    logo_url = paste0("<img src='https://assets.nhle.com/logos/nhl/svg/", 
      team, 
      "_dark.svg' style='height:35px; vertical-align:middle;'>"),
    location = ifelse(home == 1, 'vs', '@')
  )

  # API Pull
  API_KEY <- Sys.getenv("ODDS_API_KEY")

  if (API_KEY == "") {
    warning("API key not found.")
  }
    
  goalie_data <- data.frame()
  totals_data <- data.frame()
    
  tryCatch({
    events_url <- paste0("https://api.the-odds-api.com/v4/sports/icehockey_nhl/events?apiKey=", API_KEY, "&commenceTimeFrom=", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"))
    events <- jsonlite::fromJSON(url(events_url))
    
    if(length(events) > 0) {
      for(i in 1:nrow(events)) {
        event_id <- events$id[i]
        
        odds_url <- paste0("https://api.the-odds-api.com/v4/sports/icehockey_nhl/events/", event_id, "/odds?apiKey=", API_KEY, "&regions=us&markets=player_total_saves,team_totals&bookmakers=caesars,draftkings,fanduel,betmgm")
        odds_resp <- try(jsonlite::fromJSON(url(odds_url)), silent=TRUE)
        
        if(!inherits(odds_resp, "try-error") && length(odds_resp$bookmakers) > 0) {
          
          # Loop through whichever bookmakers the API returned
          for(b in 1:nrow(odds_resp$bookmakers)) {
             mkts <- odds_resp$bookmakers$markets[[b]]
             
             if(length(mkts) > 0) {
                for(m in 1:nrow(mkts)) {
                  key <- mkts$key[m]
                  outcomes <- mkts$outcomes[[m]]
                  
                  # Get Goalie Saves
                  if(key == "player_total_saves" && "description" %in% names(outcomes)) {
                    game_goalies <- data.frame(
                      fullName = outcomes$description, 
                      goalie_name = outcomes$name,     
                      vegas_saves = as.numeric(outcomes$point) 
                    )
                    goalie_data <- rbind(goalie_data, game_goalies)
                  }
                  
                  # Get Team Totals
                  if(key == "team_totals" && "description" %in% names(outcomes)) {
                    t_tots <- outcomes[!duplicated(outcomes$description), ]
                    game_totals <- data.frame(
                      fullName = t_tots$description,
                      vegas_goals = as.numeric(t_tots$point)
                    )
                    totals_data <- rbind(totals_data, game_totals)
                  }
                }
             }
          }
        }
        Sys.sleep(0.2)
      }
    } 
  }, error = function(e) { print(paste("Betting API Error:", e$message)) })

  team_map <- teams %>% dplyr::select(fullName, triCode)
    
  # Join Goalies to Opponent Team
  if(nrow(goalie_data) > 0) {
    goalie_data <- distinct(goalie_data, fullName, .keep_all = TRUE)
      
    goalie_map <- data.frame()
    unique_opps <- unique(daily_report_red_scaled$opponent)
        
    for(opp in unique_opps) {
      roster_url <- paste0("https://api-web.nhle.com/v1/roster/", opp, "/current")
      roster_resp <- try(jsonlite::fromJSON(url(roster_url)), silent=TRUE)
        
      if(!inherits(roster_resp, "try-error") && "goalies" %in% names(roster_resp)) {
        if(is.data.frame(roster_resp$goalies) && nrow(roster_resp$goalies) > 0) {
          temp_map <- data.frame(
            nhl_last_name = tolower(roster_resp$goalies$lastName$default),
            opponent = opp,
            stringsAsFactors = FALSE
          )
          goalie_map <- rbind(goalie_map, temp_map)
        }
      }
    }

    goalie_data$opponent <- NA
    for(i in 1:nrow(goalie_data)) {
      g_name <- tolower(goalie_data$fullName[i])
      match_idx <- which(sapply(goalie_map$nhl_last_name, function(ln) grepl(ln, g_name)))
        
      if(length(match_idx) > 0) {
        goalie_data$opponent[i] <- goalie_map$opponent[match_idx[1]]
      }
    }

    goalie_data <- goalie_data %>% 
      filter(!is.na(opponent)) %>% 
      dplyr::select(-goalie_name) %>% 
      rename(goalie_name = fullName)
        
    daily_report_red_scaled <- left_join(daily_report_red_scaled, goalie_data, by="opponent")
  } else {
    daily_report_red_scaled$vegas_saves <- NA
    daily_report_red_scaled$goalie_name <- NA
  }

  # Join Team Total to User Team
  if(nrow(totals_data) > 0) {
    totals_data <- left_join(totals_data, team_map, by="fullName") %>%
      rename(team = triCode) %>% 
      dplyr::select(team, vegas_goals) %>%
      distinct(team, .keep_all = TRUE)
        
    daily_report_red_scaled <- left_join(daily_report_red_scaled, totals_data, by="team")
  } else {
    daily_report_red_scaled$vegas_goals <- NA
  }

  # Calculate Expected Saves
  daily_report_red_scaled <- daily_report_red_scaled %>%
  mutate(
    exp_goalie_saves = pred_sog_wide - vegas_goals
  )

  # --- HTML OUTPUT ---
  if(nrow(daily_report_red_scaled)>0){
    center_val <- 28.02844
    
    max_diff <- max(abs(daily_report_red_scaled$pred_sog_wide - center_val), na.rm = TRUE)
    
    symmetric_domain <- c(center_val - max_diff, center_val + max_diff)

    table_html <- daily_report_red_scaled %>%
      dplyr::select('logo_url', 'team', 'location', 'opponent', 'pred_sog_wide', 
                    'floor_sog', 'ceiling_sog', 'vegas_goals', 'goalie_name', 
                    'vegas_saves', 'exp_goalie_saves') %>%
      gt() %>%

      fmt_number(columns=c('pred_sog_wide', 'vegas_goals', 'vegas_saves', 'exp_goalie_saves'), decimals=1) %>%
      fmt_number(columns=c('floor_sog', 'ceiling_sog'), decimals=0) %>%
      sub_missing(
        columns = c(goalie_name, vegas_saves, vegas_goals, exp_goalie_saves),
        missing_text = "—"
      ) %>%
      fmt_markdown(columns = logo_url) %>% 
      
      opt_interactive(
        active=TRUE,
        use_sorting=TRUE,
        use_highlight=TRUE,
        use_pagination=FALSE,
        height='auto'
      ) %>%

      data_color(
        columns = pred_sog_wide,
        method = "numeric",
        palette = c("#ff5555", "#333333", "#55ff55"), 
        domain = symmetric_domain
      ) %>%

      tab_options(
        table.background.color = "#121212",
        heading.background.color = "#121212",
        column_labels.background.color = "#2c2c2c",
        table.font.color = "#e0e0e0",
        table.border.top.color = "#444444",
        table.border.bottom.color = "#444444",
        heading.title.font.size = px(24),
        heading.subtitle.font.size = px(14)
      ) %>%
      
      opt_css(
        css = "
          body { background-color: #121212 !important; color: #e0e0e0; font-family: sans-serif; }
          .gt_table { background-color: #121212 !important; }
          input { background-color: #333333 !important; color: white !important; border: 1px solid #555; }
          img { filter: drop-shadow(0px 0px 3px rgba(255, 255, 255, 0.5)); }
          
          .gt_row { vertical-align: middle !important; }
        "
      ) %>%

      tab_header(title = paste("NHL Shot Predictions: ", today)) %>%
      cols_label(
        'logo_url' = "", 
        'team' = "Team",
        'location' = '',
        'opponent' = "Opponent",
        'floor_sog' = "25%ile",
        'pred_sog_wide' = "Predicted Mean",
        'ceiling_sog' = "75%ile", 
        'vegas_goals' = 'Team O/U',
        'goalie_name' = 'Opp. Goalie',
        'vegas_saves' = 'Goalie Line',
        'exp_goalie_saves' = 'Proj. Saves'
      ) %>%
      cols_align(align = "right", columns = logo_url) %>%
      cols_align(align = "left", columns = team) %>%
      cols_align(align = 'center', columns = location) %>%
      cols_width(location ~ px(35))

    gtsave(table_html, 'index.html')
  }else {
    stop("No games scheduled today.") 
  }

}, error = function(e) {
  
# Error Handler
  print("--- R SCRIPT CRASHED ---")
  print(e)
  print(paste("Script stopped (Hidden from user):", e$message))
  
  html_content <- paste0(
    "<!DOCTYPE html><html><head><title>NHL Model Status</title></head>",
    "<body style='background-color:#121212; color:#e0e0e0; text-align:center; font-family:sans-serif; padding-top:100px;'>",
    "<h1 style='font-size:40px; margin-bottom:20px;'>No NHL Games Today :(</h1>",
    "<p style='font-size:20px; color:#aaaaaa;'>Check back tomorrow!</p>",
    "</body></html>"
  )
  
  writeLines(html_content, "index.html")
})