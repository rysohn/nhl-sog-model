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

# --- DUAL-COLUMN HTML CARD OUTPUT ---
  if(nrow(daily_report_red_scaled) > 0) {
    
    # 1. Restructure Data: Pair Away and Home teams by Game ID
    away_df <- daily_report_red_scaled %>% filter(location == "@")
    home_df <- daily_report_red_scaled %>% filter(location == "vs")
    
    games_df <- inner_join(away_df, home_df, by = "game_id", suffix = c("_away", "_home"))
    
    # 2. Generate the HTML for each Game Card
    cards_html <- apply(games_df, 1, function(row) {
      
      # Helper function to build half of the card (one team's column)
      # ADDED proj_saves to the function arguments
      build_team_column <- function(team, proj_sog, goalie, line, edge, proj_saves) {
        line_num <- suppressWarnings(as.numeric(line))
        edge_num <- suppressWarnings(as.numeric(edge))
        proj_saves_num <- suppressWarnings(as.numeric(proj_saves))
        
        # Color & Text Routing
        if(is.na(line_num) || is.na(edge_num) || is.na(proj_saves_num)) {
          bet_call <- "--"
          edge_color <- "#777777"
          combined_text <- "--"
        } else if(edge_num >= 0) {
          bet_call <- paste("O", line_num)
          edge_color <- "#4CAF50" # Green
          # Combines Projected Saves and Positive Edge
          combined_text <- paste0(round(proj_saves_num, 1), " (+", round(edge_num, 1), ")")
        } else {
          bet_call <- paste("U", line_num)
          edge_color <- "#E64A19" # Red
          # Combines Projected Saves and Negative Edge
          combined_text <- paste0(round(proj_saves_num, 1), " (", round(edge_num, 1), ")")
        }
        
        logo_url <- paste0("https://assets.nhle.com/logos/nhl/svg/", team, "_dark.svg")
        goalie_display <- ifelse(is.na(goalie), "Unconfirmed", goalie)
        
        # HTML structure for the column
        paste0(
          "<div class='team-col'>",
            "<img src='", logo_url, "' class='team-logo'>",
            "<div class='team-tricode'>", team, "</div>",
            "<div class='data-row'><span>Proj SOG</span><span class='val'>", round(as.numeric(proj_sog), 1), "</span></div>",
            "<div class='divider-sub'></div>",
            "<div class='goalie-name'>", goalie_display, "</div>",
            "<div class='data-row'><span>Line</span><span class='val'>", bet_call, "</span></div>",
            "<div class='data-row'><span>Proj Saves</span><span class='val edge-val' style='color:", edge_color, ";'>", combined_text, "</span></div>",
          "</div>"
        )
      }
      
      # Build the Away (Left) and Home (Right) columns
      # Passing 'exp_goalie_saves' into the new function argument
      away_html <- build_team_column(
        row["team_away"], row["pred_sog_wide_away"], 
        row["goalie_name_away"], row["vegas_saves_away"], 
        row["edge_away"], row["exp_goalie_saves_away"]
      )
      
      home_html <- build_team_column(
        row["team_home"], row["pred_sog_wide_home"], 
        row["goalie_name_home"], row["vegas_saves_home"], 
        row["edge_home"], row["exp_goalie_saves_home"]
      )
      
      # Combine them into a single Card wrapper
      paste0(
        "<div class='card'>",
          "<div class='card-border-glow'></div>",
          "<div class='card-content'>",
            away_html,
            "<div class='vs-divider'>@</div>",
            home_html,
          "</div>",
        "</div>"
      )
    })
    
    # 3. Assemble the Master HTML Document with CSS
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
        
        .grid-container {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(340px, 1fr));
          gap: 20px;
          max-width: 1200px;
          margin: 0 auto;
        }
        
        .card {
          position: relative;
          border-radius: 12px;
          padding: 2px; 
          background-color: #222; 
          box-shadow: 0 4px 6px rgba(0,0,0,0.3);
          transition: transform 0.2s;
          overflow: hidden; 
        }
        .card:hover { transform: translateY(-3px); }
        
        .card-border-glow {
          position: absolute;
          top: 0; left: 0; width: 100%; height: 100%;
          background: radial-gradient(
            300px circle at var(--mouse-x, 0) var(--mouse-y, 0), 
            rgba(255, 255, 255, 0.7), 
            transparent 40%
          );
          opacity: 0;
          transition: opacity 0.3s ease;
          z-index: 0;
        }
        .card:hover .card-border-glow { opacity: 1; }
        
        .card-content {
          position: relative;
          background-color: #1a1a1a;
          border-radius: 10px; 
          display: flex;
          flex-direction: row;
          align-items: stretch;
          width: 100%;
          height: 100%;
          z-index: 1;
        }
        
        .team-col {
          flex: 1;
          display: flex;
          flex-direction: column;
          align-items: center;
          padding: 20px 15px;
        }
        .vs-divider {
          display: flex;
          align-items: center;
          justify-content: center;
          font-weight: 800;
          color: #444;
          background-color: #111;
          padding: 0 10px;
          font-size: 18px;
          border-left: 1px solid #222;
          border-right: 1px solid #222;
        }
        
        .team-logo { height: 50px; margin-bottom: 5px; filter: drop-shadow(0px 2px 4px rgba(255,255,255,0.1)); }
        .team-tricode { font-size: 16px; font-weight: 800; color: #ffffff; letter-spacing: 1px; margin-bottom: 15px; }
        .goalie-name { font-size: 13px; font-weight: 600; color: #aaa; margin-bottom: 10px; text-align: center; height: 30px; display: flex; align-items: center;}
        
        .data-row {
          display: flex;
          justify-content: space-between;
          width: 100%;
          font-size: 13px;
          margin-bottom: 6px;
        }
        .data-row span { color: #777; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px;}
        .data-row .val { color: #fff; font-weight: 800; font-variant-numeric: tabular-nums; }
        .data-row .edge-val { font-size: 14px; }
        
        .divider-sub { height: 1px; width: 100%; background-color: #333; margin: 10px 0; }
        .footer { text-align: center; margin-top: 40px; color: #555; font-size: 12px; }
      </style></head><body>",
      
      "<div class='header-container'>",
        "<h1>NHL SHOT Projections</h1>",
        "<h2>", today, "</h2>",
      "</div>",
      
      "<div class='grid-container'>",
        paste(cards_html, collapse = ""),
      "</div>",
      
      "<div class='footer'>Automated Pipeline | Data: NHL & The Odds API</div>",
      
      "<script>
        document.querySelectorAll('.card').forEach(card => {
          card.addEventListener('mousemove', e => {
            const rect = card.getBoundingClientRect();
            const x = e.clientX - rect.left;
            const y = e.clientY - rect.top;
            
            // Sends the mouse coordinates directly to the CSS gradient
            card.style.setProperty('--mouse-x', `${x}px`);
            card.style.setProperty('--mouse-y', `${y}px`);
          });
        });
      </script>",
      
      "</body></html>"
    )
    
    # 4. Save the HTML file
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