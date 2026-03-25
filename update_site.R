# update_site.R
library(tidyverse)
library(MASS)
library(caret)
library(nhlscraper)
library(gt)
library(httr)
library(jsonlite)
options(warn = 1)

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
  daily_reports$blocks_perc_ag <- daily_reports$l8_blocks_ag / daily_reports$l8_sa_ag
  daily_reports$pred_sog <- NA
  
  daily_report_red <- daily_reports %>% 
    dplyr::select(c('game_id', 'team', 'opponent', 'l8_sa', 'blocks_perc_ag', 'shot_perc',
                    'home', 'is_b2b', 'opp_is_b2b', 'pred_sog'))
  
  # Scaling
  req_vars <- scaler$method$center
  miss_vars <- setdiff(req_vars, names(daily_report_red))
  daily_report_red[miss_vars] <- 0
  
  daily_report_red_scaled <- daily_report_red
  daily_report_red_scaled$is_b2b_raw <- daily_report_red$is_b2b
  daily_report_red_scaled[req_vars] <- predict(scaler, daily_report_red[req_vars])
  
  daily_report_red_scaled <- daily_report_red_scaled %>%
    dplyr::select(all_of(c('game_id', 'team', 'opponent', 'l8_sa', 'blocks_perc_ag', 'shot_perc',
                           'home', 'is_b2b', 'opp_is_b2b', 'pred_sog', 'is_b2b_raw')))

  # Prediction
  daily_report_red_scaled$pred_sog <- predict(nb_fit, newdata=daily_report_red_scaled, type='response')
  daily_report_red_scaled$pred_sog <- round(daily_report_red_scaled$pred_sog, 1)

  # Floor / Ceiling
  #mu_pred <- as.vector(daily_report_red_scaled$pred_sog)
  #raw_theta <- getME(nb_fit, "glmer.nb.theta")
  #sim_theta <- raw_theta * 0.7

  #set.seed(42)
  #simulations <- matrix(NA, nrow=length(mu_pred), ncol=1000)

  #for(i in 1:length(mu_pred)){
  #  simulations[i,] <- MASS::rnegbin(1000, mu=mu_pred[i], theta=sim_theta)
  #}

  #daily_report_red_scaled$ceiling_sog <- round(apply(simulations, 1, quantile, probs=0.75, na.rm=TRUE), 1)
  #daily_report_red_scaled$floor_sog <- round(apply(simulations, 1, quantile, probs=0.25, na.rm=TRUE), 1)

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
    
  goalie_data <- data.frame(fullName = character(), goalie_name = character(), vegas_saves = numeric(), event_id = character())
  totals_data <- data.frame(fullName = character(), vegas_goals = numeric(), event_id = character())
  
  cache_file <- "odds_cache.rds"
  skip_api <- FALSE

  if(file.exists(cache_file)) {
    cache <- readRDS(cache_file)
    if(cache$date == today) {
      goalie_data <- cache$goalies
      totals_data <- cache$totals

      if(!"event_id" %in% names(goalie_data)) goalie_data$event_id <- NA_character_
      if(!"event_id" %in% names(totals_data)) totals_data$event_id <- NA_character_
      
      expected_goalies <- nrow(daily_reports) * 2
      if(nrow(goalie_data) >= expected_goalies) {
        skip_api <- TRUE
        print(paste("All", expected_goalies, "goalie lines cached. Skipping Odds API completely."))
      }
    }
  }

  if(!skip_api && API_KEY != "") {
    print("Checking for missing goalie lines...")

    all_teams <- nhlscraper::get_teams() 
    full_name_col <- if("fullName" %in% names(all_teams)) "fullName" else "full_name"
    tri_code_col <- if("triCode" %in% names(all_teams)) "triCode" else "tri_code"
    
    team_map <- all_teams %>% 
      dplyr::select(all_of(c(full_name_col, tri_code_col))) %>%
      rename(fullName = !!sym(full_name_col), triCode = !!sym(tri_code_col))
      
    teams_playing_today <- unique(daily_reports$team)

    tryCatch({
      events_url <- paste0("https://api.the-odds-api.com/v4/sports/icehockey_nhl/events?apiKey=", API_KEY, "&commenceTimeFrom=", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"))
      events <- jsonlite::fromJSON(url(events_url))
      
      if(length(events) > 0) {
        events <- events %>%
          left_join(team_map, by = c("home_team" = "fullName")) %>%
          mutate(
            event_date = as.Date(as.POSIXct(commence_time, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"), tz="America/New_York")
          ) %>%
          filter(triCode %in% teams_playing_today & event_date == today)

        if(nrow(events) > 0) {  
          for(i in 1:nrow(events)) {
            event_id <- events$id[i]
            

            if ("event_id" %in% names(goalie_data)) {
              cached_goalies_for_game <- sum(goalie_data$event_id == event_id, na.rm = TRUE)
              if (cached_goalies_for_game >= 2) {
                print(paste("Skipping", events$home_team[i], "vs", events$away_team[i], "- already cached."))
                next 
              }
            }
            
            print(paste("Pulling odds for:", events$home_team[i], "vs", events$away_team[i]))
            
            odds_url <- paste0("https://api.the-odds-api.com/v4/sports/icehockey_nhl/events/", event_id, "/odds?apiKey=", API_KEY, "&regions=us&markets=player_total_saves,team_totals&bookmakers=caesars,draftkings,fanduel,betmgm")
            odds_resp <- try(jsonlite::fromJSON(url(odds_url)), silent=TRUE)
            
            if(!inherits(odds_resp, "try-error") && length(odds_resp$bookmakers) > 0) {
              for(b in 1:nrow(odds_resp$bookmakers)) {
                mkts <- odds_resp$bookmakers$markets[[b]]
                
                if(length(mkts) > 0) {
                    for(m in 1:nrow(mkts)) {
                      key <- mkts$key[m]
                      outcomes <- mkts$outcomes[[m]]
                      
                      if(key == "player_total_saves" && "description" %in% names(outcomes)) {
                        game_goalies <- data.frame(
                          fullName = outcomes$description, 
                          goalie_name = outcomes$name,     
                          vegas_saves = as.numeric(outcomes$point),
                          event_id = event_id 
                        )
                        goalie_data <- bind_rows(goalie_data, game_goalies)
                      }
                      
                      if(key == "team_totals" && "description" %in% names(outcomes)) {
                        t_tots <- outcomes[!duplicated(outcomes$description), ]
                        game_totals <- data.frame(
                          fullName = t_tots$description,
                          vegas_goals = as.numeric(t_tots$point),
                          event_id = event_id 
                        )
                        totals_data <- bind_rows(totals_data, game_totals)
                      }
                    }
                }
              }
            }
            Sys.sleep(0.2)
          }
        }
      } 
    }, error = function(e) { print(paste("Betting API Error:", e$message)) })

    goalie_data <- goalie_data %>% arrange(is.na(event_id)) %>% distinct(fullName, .keep_all = TRUE)
    totals_data <- totals_data %>% arrange(is.na(event_id)) %>% distinct(fullName, .keep_all = TRUE)
    
    saveRDS(list(date = today, goalies = goalie_data, totals = totals_data), cache_file)
  }

  team_map <- teams %>% dplyr::select(fullName, triCode)
    
  if(nrow(goalie_data) > 0) {
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
      distinct(opponent, .keep_all = TRUE) 
  }

  saveRDS(list(date = today, goalies = goalie_data, totals = totals_data), cache_file)

  if(nrow(goalie_data) > 0) {
    
    goalie_data_clean <- goalie_data %>%
      dplyr::select(-goalie_name) %>% 
      rename(goalie_name = fullName)
    
    daily_report_red_scaled <- left_join(daily_report_red_scaled, goalie_data_clean, by="opponent")
  } else {
    daily_report_red_scaled$vegas_saves <- NA
    daily_report_red_scaled$goalie_name <- NA
  }

  if(nrow(totals_data) > 0) {
    totals_data_clean <- left_join(totals_data, team_map, by="fullName") %>%
      rename(team = triCode) %>% 
      dplyr::select(team, vegas_goals) %>%
      distinct(team, .keep_all = TRUE)
        
    daily_report_red_scaled <- left_join(daily_report_red_scaled, totals_data_clean, by="team")
  } else {
    daily_report_red_scaled$vegas_goals <- NA
  }

  # NHL Team Primary Colors
  nhl_colors <- c(
    "ANA"="#F47A38", "BOS"="#FFB81C", "BUF"="#002654", "CGY"="#C8102E", 
    "CAR"="#CE1126", "CHI"="#CF0A2C", "COL"="#6F263D", "CBJ"="#002654", 
    "DAL"="#006847", "DET"="#CE1126", "EDM"="#FF4C00", "FLA"="#041E42", 
    "LAK"="#111111", "MIN"="#154734", "MTL"="#AF1E2D", "NSH"="#FFB81C", 
    "NJD"="#CE1126", "NYI"="#00539B", "NYR"="#0038A8", "OTT"="#E31837", 
    "PHI"="#F74902", "PIT"="#FCB514", "SJS"="#006D75", "SEA"="#99D9D9", 
    "STL"="#002F87", "TBL"="#002868", "TOR"="#00205B", "UTA"="#71AFE5", 
    "VAN"="#00205B", "VGK"="#B4975A", "WSH"="#C8102E", "WPG"="#041E42"
  )


  # Calculate Expected Saves
  daily_report_red_scaled <- daily_report_red_scaled %>%
  mutate(
    exp_goalie_saves = pred_sog - vegas_goals,
    edge = exp_goalie_saves - vegas_saves,
    team_hex = nhl_colors[team],
    
    b2b_badge = ifelse(!is.na(is_b2b_raw) & is_b2b_raw == 1, 
                       paste0("<span style='background-color: ", unname(team_hex), "; color: white; padding: 2px 6px; border-radius: 4px; font-size: 11px; font-weight: bold; margin-left: 8px; vertical-align: middle; text-shadow: 0px 1px 3px rgba(0,0,0,0.8);'>B2B</span>"), 
                       ""),
    
    bar_width_pct = pmin((abs(edge) / 5.0) * 50, 50),
    
    bar_color = ifelse(is.na(edge), "transparent", ifelse(edge > 0, "#00ffcc", "#ff4c4c")),
    

    bar_anchor = ifelse(is.na(edge), "", ifelse(edge > 0, "left: 50%;", "right: 50%;")),
    
    edge_bar = ifelse(is.na(edge), "", paste0(
      "<div style='position: relative; width: 100%; background-color: #2a2a2a; border-radius: 4px; margin-top: 10px; height: 6px; overflow: hidden;'>",
        "<div style='position: absolute; left: 50%; top: 0; bottom: 0; width: 2px; background-color: #555; z-index: 2;'></div>",
        "<div style='position: absolute; ", bar_anchor, " width: ", bar_width_pct, "%; background-color: ", bar_color, "; height: 100%; border-radius: 4px; transition: width 0.5s ease; z-index: 1;'></div>",
      "</div>"
    ))
  )




# --- HTML Output ---
  if(nrow(daily_report_red_scaled) > 0) {
    
    away_df <- daily_report_red_scaled %>% filter(location == "@")
    home_df <- daily_report_red_scaled %>% filter(location == "vs")
    
    games_df <- inner_join(away_df, home_df, by = "game_id", suffix = c("_away", "_home"))
    
    cards_html <- apply(games_df, 1, function(row) {
      

      build_team_column <- function(team, proj_sog, goalie, line, edge, proj_saves, b2b_badge, edge_bar) {
        line_num <- suppressWarnings(as.numeric(line))
        edge_num <- suppressWarnings(as.numeric(edge))
        proj_saves_num <- suppressWarnings(as.numeric(proj_saves))
        
        if(is.na(line_num) || is.na(edge_num) || is.na(proj_saves_num)) {
          line_display <- "--"
          proj_saves_display <- "--"
          edge_display <- "--"
          edge_color <- "#777777"
        } else {
          line_display <- as.character(line_num)
          proj_saves_display <- as.character(round(proj_saves_num, 1))
          
          if(edge_num >= 0) {
            edge_color <- "#4CAF50" # Green
            edge_display <- paste0("+", round(edge_num, 1))
          } else {
            edge_color <- "#E64A19" # Red
            edge_display <- as.character(round(edge_num, 1))
          }
        }
        
        logo_url <- paste0("https://assets.nhle.com/logos/nhl/svg/", team, "_dark.svg")
        goalie_display <- ifelse(is.na(goalie), "Unconfirmed", goalie)
        
        b2b_clean <- trimws(as.character(b2b_badge))
        b2b_display <- ifelse(is.na(b2b_clean) | b2b_clean %in% c("", "NA"), "", b2b_clean)

        paste0(
          "<div class='team-col'>",
            "<img src='", logo_url, "' class='team-logo'>",
            "<div class='team-tricode'>", team, b2b_display, "</div>",
            "<div class='data-row'><span>Proj SOG</span><span class='val'>", round(as.numeric(proj_sog), 1), "</span></div>",
            "<div class='divider-sub'></div>",
            "<div class='goalie-name'>", goalie_display, "</div>",
            "<div class='data-row'><span>Line</span><span class='val'>", line_display, "</span></div>",
            "<div class='data-row'><span>Proj Saves</span><span class='val'>", proj_saves_display, "</span></div>",
            "<div class='data-row'><span>Edge</span><span class='val edge-val' style='color:", edge_color, ";'>", edge_display, "</span></div>",
            ifelse(is.na(edge_bar), "", edge_bar),
          "</div>"
        )
      }
      
      away_html <- build_team_column(
        row["team_away"], row["pred_sog_away"], 
        row["goalie_name_away"], row["vegas_saves_away"], 
        row["edge_away"], row["exp_goalie_saves_away"],
        row["b2b_badge_away"], row["edge_bar_away"]
      )
      
      home_html <- build_team_column(
        row["team_home"], row["pred_sog_home"], 
        row["goalie_name_home"], row["vegas_saves_home"], 
        row["edge_home"], row["exp_goalie_saves_home"],
        row["b2b_badge_home"], row["edge_bar_home"]
      )
      
      color_away <- ifelse(row["team_away"] %in% names(nhl_colors), nhl_colors[row["team_away"]], "#ffffff")
      color_home <- ifelse(row["team_home"] %in% names(nhl_colors), nhl_colors[row["team_home"]], "#ffffff")

      paste0(
        "<div class='card' style='--away-color: ", color_home, "; --home-color: ", color_away, ";'>",
          "<div class='card-border-glow'></div>",
          "<div class='card-content'>",
            home_html,
            "<div class='vs-divider'>@</div>",
            away_html,
          "</div>",
        "</div>"
      )
    })
    
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
        .subtitle { 
          color: #888; 
          font-size: 12px; 
          font-weight: 400; 
          text-transform: uppercase; 
          letter-spacing: 1px; 
        }
        
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
          padding: 4px; 
          background-color: #000; 
          box-shadow: 0 4px 6px rgba(0,0,0,0.3);
          transition: transform 0.2s;
          overflow: hidden; 
        }
        .card:hover { transform: translateY(-3px); }
        
        .card-border-glow {
          position: absolute;
          top: 0; left: 0; width: 100%; height: 100%;

          background: linear-gradient(to right, var(--away-color), var(--home-color));

          -webkit-mask-image: radial-gradient(450px circle at var(--mouse-x, 0) var(--mouse-y, 0), black, transparent 60%);
          mask-image: radial-gradient(450px circle at var(--mouse-x, 0) var(--mouse-y, 0), black, transparent 60%);

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
        
        .team-logo { height: 75px; margin-bottom: 5px; filter: drop-shadow(0px 2px 4px rgba(255,255,255,0.1)); }
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
        "<h1>NHL Shot Projections</h1>",
        "<div class='subtitle'>", today, "</div>",
      "</div>",
      
      "<div class='grid-container'>",
        paste(cards_html, collapse = ""),
      "</div>",
      
      "<div class='footer'>Data: NHL & The Odds API</div>",
      
      "<script>
        document.querySelectorAll('.card').forEach(card => {
          card.addEventListener('mousemove', e => {
            const rect = card.getBoundingClientRect();
            const x = e.clientX - rect.left;
            const y = e.clientY - rect.top;
            
            // Just track the mouse! CSS handles the gradient colors automatically now.
            card.style.setProperty('--mouse-x', `${x}px`);
            card.style.setProperty('--mouse-y', `${y}px`);
          });
        });
      </script>",
      
      "</body></html>"
    )
    
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
