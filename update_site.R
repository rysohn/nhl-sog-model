# update_site.R
library(tidyverse)
library(MASS)
library(caret)
library(nhlscraper)
library(gt)

# 1. Load Functions & Data
source("utils.R") 
nb_fit <- readRDS("nb_sog_model.rds")
scaler <- readRDS("sog_scaler.rds")

# We need train_data to calculate the mean for the 'Wide' prediction logic
# If this file is too large for GitHub, consider hardcoding mean_sog <- 29.5 (or your actual mean)
if(file.exists("train_data.RData")) {
  load("train_data.RData")
  mean_sog <- mean(train_data$true_sog, na.rm=TRUE)
} else {
  # Fallback if file missing (Replace with your actual training mean)
  mean_sog <- 28.02844 
  warning("train_data.RData not found. Using default mean_sog = 28.03")
}

# 2. Get Today's Date
today <- '2026-02-03'
#today <- Sys.Date()
print(paste("Running model for:", today))

tryCatch({
  # 3. Fetch Games
  daily_reports <- get_daily_game_reports(today)
  
  if(nrow(daily_reports) == 0) {
    stop("No games scheduled today.")
  }

  # --- FEATURE ENGINEERING (From RUN ME Chunk) ---
  daily_reports$sog_trend <- daily_reports$l4_sog - daily_reports$l8_sog
  daily_reports$sog_ag_trend <- daily_reports$l4_sog_ag - daily_reports$l8_sog_ag
  daily_reports$home <- ifelse(daily_reports$'h/a' == 'home', 1, 0)
  daily_reports$shot_perc <- daily_reports$l8_sog / daily_reports$l8_sa
  daily_reports$shot_perc_ag <- daily_reports$l8_sog_ag / daily_reports$l8_sa_ag
  daily_reports$pred_sog <- NA
  
  # Select columns
  daily_report_red <- daily_reports %>% 
    dplyr::select(c('game_id', 'team', 'opponent', 'l8_sog', 'sog_trend', 
                    'l8_sog_ag', 'sog_ag_trend', 'rest_diff', 
                    'shot_perc', 'shot_perc_ag', 'home', 'pred_sog'))
  
  # --- SCALING ---
  req_vars <- scaler$method$center
  miss_vars <- setdiff(req_vars, names(daily_report_red))
  daily_report_red[miss_vars] <- 0
  
  daily_report_red_scaled <- daily_report_red
  daily_report_red_scaled[req_vars] <- predict(scaler, daily_report_red[req_vars])
  
  # Ensure column order matches model expectation
  daily_report_red_scaled <- daily_report_red_scaled %>%
    dplyr::select(all_of(c('game_id', 'team', 'opponent', 'l8_sog', 'sog_trend', 
                           'l8_sog_ag', 'sog_ag_trend', 'rest_diff', 'shot_perc', 
                           'shot_perc_ag', 'home', 'pred_sog')))

  # --- PREDICTION ---
  daily_report_red_scaled$pred_sog <- predict(nb_fit, newdata=daily_report_red_scaled, type='response')
  daily_report_red_scaled$pred_sog <- round(daily_report_red_scaled$pred_sog, 1)

  # --- WIDE PREDICTION (Amplifier) ---
  spread_factor <- 1.2
  daily_report_red_scaled$pred_sog_wide <- round(mean_sog + 
    (daily_report_red_scaled$pred_sog - mean_sog) * spread_factor, 1)

  # --- SIMULATIONS (Floor/Ceiling) ---
  mu_pred <- as.vector(daily_report_red_scaled$pred_sog)
  theta <- nb_fit$theta

  set.seed(42)
  simulations <- matrix(NA, nrow=length(mu_pred), ncol=1000)

  for(i in 1:length(mu_pred)){
    simulations[i,] <- MASS::rnegbin(1000, mu=mu_pred[i], theta=theta)
  }

  # Using 0.75 and 0.25 as per your code
  daily_report_red_scaled$ceiling_sog <- round(apply(simulations, 1, quantile, probs=0.75), 1)
  daily_report_red_scaled$floor_sog <- round(apply(simulations, 1, quantile, probs=0.25), 1)

  daily_report_red_scaled <- daily_report_red_scaled %>%
  mutate(
    team_id = sapply(team, get_team_id), # Uses your existing get_team_id function
    logo_url = paste0("<img src='https://assets.nhle.com/logos/nhl/svg/", 
      daily_report_red_scaled$team, 
      "_dark.svg' style='height:35px; vertical-align:middle;'>")
  )

  # --- HTML OUTPUT ---
  if(nrow(daily_report_red_scaled)>0){
    # 1. Define your target center
    center_val <- 28.02844
    
    # 2. Calculate the maximum deviation from that center in today's data
    # This ensures that 28.02844 is mathematically in the middle of the scale
    max_diff <- max(abs(daily_report_red_scaled$pred_sog_wide - center_val), na.rm = TRUE)
    
    # 3. Create a symmetric domain (e.g., if max diff is 5, domain is 23.02 to 33.02)
    symmetric_domain <- c(center_val - max_diff, center_val + max_diff)

    # 4. Generate the Table
    table_html <- daily_report_red_scaled %>%
      dplyr::select('logo_url', 'team', 'opponent', 'pred_sog_wide', 'floor_sog', 'ceiling_sog') %>%
      gt() %>%

      fmt_markdown(columns = logo_url) %>% 
      
      opt_interactive(
        active=TRUE,
        use_sorting=TRUE,
        use_highlight=TRUE,
        use_pagination=FALSE,
        height='auto'
      ) %>%

      # Apply the Symmetric Color Scale
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
      
      # 6. GLOBAL PAGE CSS (This turns the whole page black)
      opt_css(
        css = "
          body { background-color: #121212 !important; color: #e0e0e0; font-family: sans-serif; }
          .gt_table { background-color: #121212 !important; }
          input { background-color: #333333 !important; color: white !important; border: 1px solid #555; }
          img { filter: drop-shadow(0px 0px 3px rgba(255, 255, 255, 0.5)); }
          
          .gt_row { vertical-align: middle !important; }
        "
      ) %>%

      tab_header(title = paste("NHL Shot Predictions:", Sys.Date())) %>%
      cols_label(
        'logo_url' = "", 
        'team' = "Team",
        'opponent' = "Opponent",
        'floor_sog' = "25th Percentile",
        'pred_sog_wide' = "Predicted Mean",
        'ceiling_sog' = "75th Percentile"
      ) %>%
      cols_align(align = "right", columns = logo_url) %>%
      cols_align(align = "left", columns = team)

  # Save the updated file
    gtsave(table_html, 'index.html')
  }else {
    stop("No games scheduled today.") # Passes control to error handler
  }

}, error = function(e) {
  
  # We print the REAL error to the console (for you to see in GitHub logs)
  print(paste("Script stopped (Hidden from user):", e$message))
  
  # But we ALWAYS write the "No Games" message to the website
  html_content <- paste0(
    "<!DOCTYPE html><html><head><title>NHL Model Status</title></head>",
    "<body style='background-color:#121212; color:#e0e0e0; text-align:center; font-family:sans-serif; padding-top:100px;'>",
    "<h1 style='font-size:40px; margin-bottom:20px;'>No NHL Games Today :(</h1>",
    "<p style='font-size:20px; color:#aaaaaa;'>Check back tomorrow!</p>",
    "</body></html>"
  )
  
  writeLines(html_content, "index.html")
})