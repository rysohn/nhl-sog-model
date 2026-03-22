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
    exp_goalie_saves = pred_sog_wide - vegas_goals,
    edge = exp_goalie_saves - vegas_saves
  )

# --- HTML CARD OUTPUT (REPLACES GT TABLE) ---
  if(nrow(daily_report_red_scaled) > 0) {
    
    # 1. Generate the HTML for each individual card
    cards_html <- apply(daily_report_red_scaled, 1, function(row) {
      
      team <- row["team"]
      opp <- row["opponent"]
      loc <- row["location"]
      goalie <- row["goalie_name"]
      
      # Handle potential NAs cleanly
      line <- suppressWarnings(as.numeric(row["vegas_saves"]))
      proj <- suppressWarnings(as.numeric(row["exp_goalie_saves"]))
      edge <- suppressWarnings(as.numeric(row["edge"]))
      
      # 2. Betting Logic & Color Routing
      if(is.na(line) || is.na(edge)) {
        bet_call <- "AWAITING LINES"
        edge_color <- "#777777" # Grey
        edge_text <- "—"
        proj_text <- "—"
      } else if(edge >= 0) {
        bet_call <- paste("OVER", line)
        edge_color <- "#4CAF50" # Modern Green
        edge_text <- paste0("+", round(edge, 1), " Edge")
        proj_text <- round(proj, 1)
      } else {
        bet_call <- paste("UNDER", line)
        edge_color <- "#E64A19" # Modern Red
        edge_text <- paste0(round(edge, 1), " Edge")
        proj_text <- round(proj, 1)
      }
      
      # 3. Build the Matchup Header based on Location
      team_logo <- paste0("https://assets.nhle.com/logos/nhl/svg/", team, "_dark.svg")
      opp_logo <- paste0("https://assets.nhle.com/logos/nhl/svg/", opp, "_dark.svg")
      
      if(loc == "@") {
        matchup_html <- paste0("<img src='", team_logo, "' class='logo'><span class='at'>@</span><img src='", opp_logo, "' class='logo'>")
      } else {
        matchup_html <- paste0("<img src='", opp_logo, "' class='logo'><span class='at'>@</span><img src='", team_logo, "' class='logo'>")
      }
      
      # 4. Construct the Card HTML
      paste0(
        "<div class='card'>",
          "<div class='matchup'>", matchup_html, "</div>",
          "<div class='goalie'>", ifelse(is.na(goalie), "Unconfirmed Goalie", goalie), "</div>",
          "<div class='bet-call' style='color:", edge_color, ";'>", bet_call, "</div>",
          "<div class='divider'></div>",
          "<div class='stats'>",
            "<div class='stat-box'><span>Proj Saves</span><strong>", proj_text, "</strong></div>",
            "<div class='stat-box'><span>Value</span><strong style='color:", edge_color, ";'>", edge_text, "</strong></div>",
          "</div>",
        "</div>"
      )
    })
    
    # 5. Assemble the Master HTML Document with Modern CSS
    full_html <- paste0(
      "<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'>",
      "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
      "<title>NHL Prop Projections</title>",
      "<style>
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;800&display=swap');
        body { 
          background-color: #0d0d0d; 
          color: #e0e0e0; 
          font-family: 'Inter', sans-serif; 
          margin: 0; padding: 20px; 
        }
        .header-container { text-align: center; margin-bottom: 30px; }
        h1 { font-weight: 800; letter-spacing: -1px; margin-bottom: 5px; }
        .subtitle { color: #888; font-size: 14px; }
        
        /* The CSS Grid that handles mobile-responsiveness */
        .grid-container {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
          gap: 20px;
          max-width: 1200px;
          margin: 0 auto;
        }
        
        /* Individual Card Styling */
        .card {
          background-color: #1a1a1a;
          border: 1px solid #333;
          border-radius: 12px;
          padding: 20px;
          text-align: center;
          box-shadow: 0 4px 6px rgba(0,0,0,0.3);
          transition: transform 0.2s;
        }
        .card:hover { transform: translateY(-3px); border-color: #555; }
        
        .matchup { display: flex; justify-content: center; align-items: center; gap: 15px; margin-bottom: 15px; }
        .logo { height: 45px; filter: drop-shadow(0px 2px 4px rgba(255,255,255,0.1)); }
        .at { color: #666; font-weight: 600; font-size: 14px; }
        
        .goalie { font-size: 16px; font-weight: 600; color: #bbb; margin-bottom: 5px; }
        .bet-call { font-size: 24px; font-weight: 800; margin-bottom: 15px; letter-spacing: -0.5px; }
        
        .divider { height: 1px; background-color: #333; margin-bottom: 15px; }
        
        .stats { display: flex; justify-content: space-around; }
        .stat-box { display: flex; flex-direction: column; }
        .stat-box span { font-size: 11px; color: #777; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 4px; }
        .stat-box strong { font-size: 18px; font-variant-numeric: tabular-nums; }
        
        .footer { text-align: center; margin-top: 40px; color: #555; font-size: 12px; }
      </style></head><body>",
      
      "<div class='header-container'>",
        "<h1>NHL Goalie Projections</h1>",
        "<div class='subtitle'>Actionable edges for ", today, "</div>",
      "</div>",
      
      "<div class='grid-container'>",
        paste(cards_html, collapse = ""),
      "</div>",
      
      "<div class='footer'>Automated Pipeline | Data: NHL & The Odds API</div>",
      "</body></html>"
    )
    
    # 6. Save the HTML file
    writeLines(full_html, "index.html")
    
  } else {
    stop("No games scheduled today.") 
  }

}, error = function(e) {
  
  print("--- R SCRIPT CRASHED ---")
  print(e)
  print(paste("Script stopped (Hidden from user):", e$message))
  
  html_content <- paste0(
    "<!DOCTYPE html><html><head><title>NHL Model Status</title></head>",
    "<body style='background-color:#0d0d0d; color:#e0e0e0; text-align:center; font-family:sans-serif; padding-top:100px;'>",
    "<h1 style='font-size:40px; margin-bottom:20px;'>No NHL Games Today :(</h1>",
    "<p style='font-size:20px; color:#aaaaaa;'>Check back tomorrow!</p>",
    "</body></html>"
  )
  
  writeLines(html_content, "index.html")
})