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
  mean_sog <- 30.0 
  warning("train_data.RData not found. Using default mean_sog = 30.0")
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
    logo_url = paste0("https://assets.nhle.com/logos/nhl/svg/", team_id, "_light.svg")
  )

  # --- HTML OUTPUT ---
  table_html <- daily_report_red_scaled %>%
    dplyr::select('logo_url', 'team', 'opponent', 'pred_sog_wide', 'floor_sog', 'ceiling_sog') %>%
    gt() %>%
    
    # 1. ADD CONDITIONAL FORMATTING (Red = Low, Green = High)
    data_color(
      columns = pred_sog_wide,
      method = "numeric",
      palette = c("#ffcccc", "#ffffff", "#ccffcc"), # Light Red -> White -> Light Green
      domain = NULL # Auto-scales based on the min/max of the data
    ) %>%

    # 2. Transform the URL column into actual images
    text_transform(
      locations = cells_body(columns = logo_url),
      fn = function(x) {
        web_image(url = x, height = 30)
      }
    ) %>%
    
    # 3. Headers and Labels
    tab_header(title = paste("NHL Shot Predictions:", Sys.Date())) %>%
    cols_label(
      'logo_url' = "", 
      'team' = "Team",
      'opponent' = "Opponent",
      'floor_sog' = "25th Percentile",
      'pred_sog_wide' = "Predicted Mean",
      'ceiling_sog' = "75th Percentile"
    ) %>%
    cols_align(align = "left", columns = c(logo_url, team)) %>%
    as_raw_html()

# Save the updated file
writeLines(table_html, "index.html")

}, error = function(e) {
  print(paste("Error:", e$message))
  writeLines(paste("<div style='text-align:center; padding-top:100px; font-family:sans-serif;'><h1 style='font-size:40px; margin-bottom:20px;'>No NHL Games Today :(</h1><p style='font-size:20px;'>"), "index.html")
})