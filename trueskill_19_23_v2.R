update_trueskill <- function(mu_winner, sigma_winner, mu_loser, sigma_loser, beta = 25/6, gamma = 25/300, epsilon = 10) {
  c <- sqrt(2 * beta^2 + sigma_winner^2 + sigma_loser^2)
  v <- function(x) { dnorm(x) / pnorm(x) }
  w <- function(x) { v(x) * (v(x) + x) }
  
  if (mu_winner == mu_loser) {
    mu_winner <- mu_winner + epsilon
  }
  
  t <- (mu_winner - mu_loser) / c
  delta_mu_winner <- (sigma_winner^2 + gamma^2) / c * v(t)
  delta_mu_loser <- -(sigma_loser^2 + gamma^2) / c * v(t)
  
  mu_winner_new <- mu_winner + delta_mu_winner
  mu_loser_new <- mu_loser + delta_mu_loser
  sigma_winner_new <- sqrt((sigma_winner^2 + gamma^2) * (1 - (sigma_winner^2 + gamma^2) / c * w(t)))
  # print((1 - (sigma_winner^2 + gamma^2) / c * w(t)))
  # print((1 - (sigma_loser^2 + gamma^2) / c * w(t)))
  sigma_loser_new <- sqrt((sigma_loser^2 + gamma^2) * (1 - (sigma_loser^2 + gamma^2) / c * w(t)))
  
  return(list(mu_winner_new = mu_winner_new, 
              sigma_winner_new = sigma_winner_new, 
              mu_loser_new = mu_loser_new, 
              sigma_loser_new = sigma_loser_new))
}

# update_trueskill <- function(mu_winner, sigma_winner, mu_loser, sigma_loser, beta = 25/6, gamma = 25/300, epsilon = 1e-6) {
#   c <- sqrt(2 * beta^2 + sigma_winner^2 + sigma_loser^2)
#   delta_mu <- (mu_winner - mu_loser) / c
#   v <- dnorm(delta_mu - epsilon) / pnorm(delta_mu - epsilon)
#   w <- v * (v + delta_mu - epsilon)
#   
#   mu_winner_new <- mu_winner + ((sigma_winner^2 + gamma^2) / c) * v
#   mu_loser_new <- mu_loser - ((sigma_loser^2 + gamma^2) / c) * v
#   
#   sigma_winner_new <- sqrt(sigma_winner^2 * (1 - (sigma_winner^2 + gamma^2) * w / (c^2)))
#   sigma_loser_new <- sqrt(sigma_loser^2 * (1 - (sigma_loser^2 + gamma^2) * w / (c^2)))
#   
#   list(mu_winner_new = mu_winner_new, sigma_winner_new = sigma_winner_new, mu_loser_new = mu_loser_new, sigma_loser_new = sigma_loser_new)
# }

# update_trueskill(25, 25/3, 25, 25/3)

export_df <- function(from, to){
  df <- data.frame()
  for (i in from:to) {
    # define file name
    if (i < 2013) {
      file_name <- paste('~/Desktop/7098/betting_data/', i,'.xls', sep = '')
    } else {
      file_name <- paste('~/Desktop/7098/betting_data/', i,'.xlsx', sep = '')
    }
    # warning will be solved later ('2.,3' in column, EXw)
    df_temp_file <- read_excel(file_name, na = c('N/A', ''))
    df_file <- read_excel(file_name, col_types = "text", na = c('N/A', ''))
    
    # Date column will be converted wrongly if col_types = "text", 
    # so re-assign Date column
    df_file$Date <- df_temp_file$Date
    
    # combine datasets
    df <- bind_rows(df, df_file)
    
    df$row_index <- 1:nrow(df)
  }
  
  # convert unknown to character type
  df$Date <- as.character(df$Date)
  
  # replace "NR" with NA
  df <- df %>%
    mutate(LRank = ifelse(LRank == "NR", NA, LRank))
  
  # handle previous warning ('2.,3' in column, EXw)
  df$EXW <- gsub(",", "", df$EXW)
  
  # convert columns type to numeric
  df <- df %>%
    mutate(across(any_of(c("ATP", "WRank", "LRank", "W1",
                           "L1", "W2", "L2", "W3",
                           "L3", "W4", "L4", "W5",
                           "L5", "Wsets", "Lsets", "CBW",
                           "CBL", "GBW", "GBL", "IWW",
                           "IWL", "SBW", "SBL" ,"B365W",
                           "B365L", "B&WW", "B&WL", "EXW",
                           "EXL", "PSW", "PSL","WPts",
                           "LPts", "UBW", "UBL", "LBW",
                           "LBL", "SJW", "SJL", "MaxW",
                           "MaxL", "AvgW", "AvgL")), 
                  ~as.numeric(coalesce(as.character(.), NA))))
  return(df)
}

# Pre-functions
export_df <- function(from, to){
  df <- data.frame()
  for (i in from:to) {
    # define file name
    if (i < 2013) {
      file_name <- paste('~/Desktop/7098/betting_data/', i,'.xls', sep = '')
    } else {
      file_name <- paste('~/Desktop/7098/betting_data/', i,'.xlsx', sep = '')
    }
    # warning will be solved later ('2.,3' in column, EXw)
    df_temp_file <- read_excel(file_name, na = c('N/A', ''))
    df_file <- read_excel(file_name, col_types = "text", na = c('N/A', ''))
    
    # Date column will be converted wrongly if col_types = "text", 
    # so re-assign Date column
    df_file$Date <- df_temp_file$Date
    
    # combine datasets
    df <- bind_rows(df, df_file)
    
    df$row_index <- 1:nrow(df)
  }
  
  # convert unknown to character type
  df$Date <- as.character(df$Date)
  
  # replace "NR" with NA
  df <- df %>%
    mutate(LRank = ifelse(LRank == "NR", NA, LRank))
  
  # handle previous warning ('2.,3' in column, EXw)
  df$EXW <- gsub(",", "", df$EXW)
  
  # convert columns type to numeric
  df <- df %>%
    mutate(across(any_of(c("ATP", "WRank", "LRank", "W1",
                           "L1", "W2", "L2", "W3",
                           "L3", "W4", "L4", "W5",
                           "L5", "Wsets", "Lsets", "CBW",
                           "CBL", "GBW", "GBL", "IWW",
                           "IWL", "SBW", "SBL" ,"B365W",
                           "B365L", "B&WW", "B&WL", "EXW",
                           "EXL", "PSW", "PSL","WPts",
                           "LPts", "UBW", "UBL", "LBW",
                           "LBL", "SJW", "SJL", "MaxW",
                           "MaxL", "AvgW", "AvgL")), 
                  ~as.numeric(coalesce(as.character(.), NA))))
  return(df)
}
data_clean_elo <- function(df){
  df %<>%
    arrange(Date)
  
  ## Select required columns, set levels, convert data type
  df <- df |>
    rename(tourney_date = Date,
           surface = Surface,
           tourney_level = Series) %>% 
    select(c(
      "tourney_date",
      "surface",
      "tourney_level",
      "Winner",
      "Loser",
      "WRank",
      "LRank",
      "row_index")) |>
    mutate_at(
      c("surface"),
      as.factor) |>
    mutate(tourney_date = ymd(tourney_date))
  
  ## Remove missing data, create new features
  df <- df |>
    na.omit()
  
  df <- df %>%
    mutate(tourney_level = ifelse(tourney_level == "Grand Slam", "G", "others"))
  
  ## Initialize Elo for winner and loser in whole dataframe
  df %<>% 
    select(-c("surface"))
  
  return (df)
}
extract_players <- function(df){
  # Define a function to create players dataframe based on the whole dataframe
  # id including winner_id and loser_id, and initialize the related rating to 1500
  df_player <- rbind(data.frame(Player = df$Winner), 
                     data.frame(Player = df$Loser))
  
  df_player %<>% 
    unique() %>% 
    mutate(mu = NA,
           sigma = NA)
  
  return(df_player)
}
split_train_test <- function(df, split_time){
  df_train <- filter(df, tourney_date < split_time)
  df_test <- filter(df, tourney_date >= split_time)
  return (list(df_train, df_test))
}

# 2019 -------------------------------------------------------
# Export data
df_init_total <- export_df(2001, 2023)

# Perform data cleaning
df_cleaned_total <- data_clean_elo(df_init_total)
df_cleaned_total %<>% 
  filter(tourney_date < dmy("01-01-2020"))
split_68_18_19_res <- split_train_test(df_cleaned_total, dmy("01-01-2019"))
df_cleaned_train <- split_68_18_19_res[[1]]
df_cleaned_val <- split_68_18_19_res[[2]]

train_data_len <- df_cleaned_train %>% nrow()
# train_data -- 49240
val_data_len <- df_cleaned_val %>% nrow()
# val_data -- 2577

data <- df_cleaned_total %>% 
  select('Winner', 'Loser', 'WRank', 'LRank', 'tourney_date')

# data <- df_cleaned_total
# data <- data_temp[1:ceiling(20000), ]

data$prob_winner <- NA
data$predicted_winner <- NA
data$mu_winner <- NA
data$mu_loser <- NA


# initial_mu <- 25
# initial_sigma <- 8.333
# ratings <- data.frame(
#   Player = unique(c(data$Winner, data$Loser)),
#   mu = initial_mu,
#   sigma = initial_sigma,
#   stringsAsFactors = FALSE
# )

ratings <- extract_players(data)

# Simulate matches and update ratings
for (i in 1:nrow(data)) {
  winner <- data$Winner[i]
  loser <- data$Loser[i]
  
  mu_winner <- ratings$mu[ratings$Player == winner]
  sigma_winner <- ratings$sigma[ratings$Player == winner]
  mu_loser <- ratings$mu[ratings$Player == loser]
  sigma_loser <- ratings$sigma[ratings$Player == loser]
  
  if (is.na(mu_winner+sigma_winner)) {
    # mu_winner <- (5000 - data$WRank[i]) / 100
    mu_winner <- 25
    if (data$WRank[i] <= 100) {
      # mu_winner <- 100
      mu_winner <- mu_winner + (5000 - data$WRank[i]) / 200
      sigma_winner <- mu_winner / 3
    } else {
      # mu_winner <- 20
      # mu_loser <- mu_loser - 5
      sigma_winner <- mu_winner / 3
    }
  }
  
  if (is.na(mu_loser+sigma_loser)) {
    # mu_loser <- (5000 - data$LRank[i]) / 100
    mu_loser <- 25
    if (data$LRank[i] <= 100) {
      # mu_loser <- 100
      mu_loser <- mu_loser + (5000 - data$LRank[i]) / 200
      sigma_loser <- mu_loser / 3
    } else {
      # mu_loser <- 20
      # mu_loser <- mu_loser - 5
      sigma_loser <- mu_loser / 3
    }
    
    # ratings$mu[ratings$Player == winner] <- mu_winner
    # ratings$sigma[ratings$Player == winner] <- sigma_winner
    # ratings$mu[ratings$Player == loser] <- mu_loser
    # ratings$sigma[ratings$Player == loser] <- sigma_loser
  }
  
  temp_prob_winner <- 1 / (1 + 10^((mu_loser - mu_winner) / 400))
  data$prob_winner[i] <- temp_prob_winner
  data$predicted_winner[i] <- ifelse(temp_prob_winner >= 0.5, winner, loser)
  data$mu_winner[i] <- mu_winner
  data$sigma_winner[i] <- sigma_winner
  data$mu_loser[i] <- mu_loser
  data$sigma_loser[i] <- sigma_loser
  
  # if (i == 25 || i == 29 || i == 30 || i == 31) {
  #   print(i)
  #   print(mu_winner)
  #   print(mu_loser)
  #   print(sigma_winner)
  #   print(sigma_loser)
  # }
  
  updated_ratings <- update_trueskill(mu_winner, sigma_winner, mu_loser, sigma_loser)
  
  ratings$mu[ratings$Player == winner] <- updated_ratings$mu_winner_new
  ratings$sigma[ratings$Player == winner] <- updated_ratings$sigma_winner_new
  ratings$mu[ratings$Player == loser] <- updated_ratings$mu_loser_new
  ratings$sigma[ratings$Player == loser] <- updated_ratings$sigma_loser_new
}

val_data <- data[train_data_len+1:ceiling(val_data_len), ]

# # Predict match outcomes
# predictions <- data %>%
#   rowwise() %>%
#   mutate(
#     mu_winner = ratings$mu[ratings$Player == Winner],
#     mu_loser = ratings$mu[ratings$Player == Loser],
#     predicted_winner = ifelse(mu_winner > mu_loser, Winner, Loser)
#   )

# Predict match outcomes and probabilities
# predictions <- data %>%
#   rowwise() %>%
#   mutate(
#     mu_winner = ratings$mu[ratings$Player == Winner],
#     mu_loser = ratings$mu[ratings$Player == Loser],
#     prob_winner = 1 / (1 + 10^((mu_loser - mu_winner) / 400)),
#     predicted_winner = ifelse(prob_winner > 0.5, Winner, Loser)
#   )
predictions <- val_data
# Calculate accuracy
accuracy <- mean(predictions$Winner == predictions$predicted_winner)
print(paste("Accuracy:", accuracy))

# Calculate calibration
# Calibration can be calculated by comparing predicted probabilities with actual outcomes
# Here, we will calculate the mean squared error between predicted probabilities and actual outcomes
calibration <- mean((ifelse(predictions$Winner == predictions$Winner, 1, 0) - predictions$prob_winner)^2)
print(paste("Calibration (MSE):", calibration))

# # Calculate calibration (Brier score as a proxy for calibration)
# brier_score <- mean((ifelse(predictions$Winner == predictions$Winner, 1, 0) - predictions$prob_winner)^2)
# print(paste("Brier score (Calibration):", brier_score))

# Calculate log-loss
library(Metrics)
log_loss_value <- logLoss(ifelse(predictions$Winner == predictions$Winner, 1, 0), predictions$prob_winner)
print(paste("Log-loss:", log_loss_value))

c(accuracy, calibration, log_loss_value)
# 0.6290260 0.2472800 0.6876987



# 2023 -------------------------------------------------------
# Export data
df_init_total <- export_df(2001, 2023)

# Perform data cleaning
df_cleaned_total <- data_clean_elo(df_init_total)
df_cleaned_total %<>% 
  filter(tourney_date < dmy("01-01-2024"))
split_68_18_19_res <- split_train_test(df_cleaned_total, dmy("01-01-2023"))
df_cleaned_train <- split_68_18_19_res[[1]]
df_cleaned_val <- split_68_18_19_res[[2]]

train_data_len <- df_cleaned_train %>% nrow()
# train_data -- 49240
val_data_len <- df_cleaned_val %>% nrow()
# val_data -- 2577

data <- df_cleaned_total %>% 
  select('Winner', 'Loser', 'WRank', 'LRank', 'tourney_date')

# data <- df_cleaned_total
# data <- data_temp[1:ceiling(20000), ]

data$prob_winner <- NA
data$predicted_winner <- NA
data$mu_winner <- NA
data$mu_loser <- NA


# initial_mu <- 25
# initial_sigma <- 8.333
# ratings <- data.frame(
#   Player = unique(c(data$Winner, data$Loser)),
#   mu = initial_mu,
#   sigma = initial_sigma,
#   stringsAsFactors = FALSE
# )

ratings <- extract_players(data)

# Simulate matches and update ratings
for (i in 1:nrow(data)) {
  winner <- data$Winner[i]
  loser <- data$Loser[i]
  
  mu_winner <- ratings$mu[ratings$Player == winner]
  sigma_winner <- ratings$sigma[ratings$Player == winner]
  mu_loser <- ratings$mu[ratings$Player == loser]
  sigma_loser <- ratings$sigma[ratings$Player == loser]
  
  if (is.na(mu_winner+sigma_winner)) {
    # mu_winner <- (5000 - data$WRank[i]) / 100
    mu_winner <- 25
    if (data$WRank[i] <= 100) {
      # mu_winner <- 100
      mu_winner <- mu_winner + (5000 - data$WRank[i]) / 200
      sigma_winner <- mu_winner / 3
    } else {
      # mu_winner <- 20
      # mu_loser <- mu_loser - 5
      sigma_winner <- mu_winner / 3
    }
  }
  
  if (is.na(mu_loser+sigma_loser)) {
    # mu_loser <- (5000 - data$LRank[i]) / 100
    mu_loser <- 25
    if (data$LRank[i] <= 100) {
      # mu_loser <- 100
      mu_loser <- mu_loser + (5000 - data$LRank[i]) / 200
      sigma_loser <- mu_loser / 3
    } else {
      # mu_loser <- 20
      # mu_loser <- mu_loser - 5
      sigma_loser <- mu_loser / 3
    }
    
    # ratings$mu[ratings$Player == winner] <- mu_winner
    # ratings$sigma[ratings$Player == winner] <- sigma_winner
    # ratings$mu[ratings$Player == loser] <- mu_loser
    # ratings$sigma[ratings$Player == loser] <- sigma_loser
  }
  
  temp_prob_winner <- 1 / (1 + 10^((mu_loser - mu_winner) / 400))
  data$prob_winner[i] <- temp_prob_winner
  data$predicted_winner[i] <- ifelse(temp_prob_winner >= 0.5, winner, loser)
  data$mu_winner[i] <- mu_winner
  data$sigma_winner[i] <- sigma_winner
  data$mu_loser[i] <- mu_loser
  data$sigma_loser[i] <- sigma_loser
  
  # if (i == 25 || i == 29 || i == 30 || i == 31) {
  #   print(i)
  #   print(mu_winner)
  #   print(mu_loser)
  #   print(sigma_winner)
  #   print(sigma_loser)
  # }
  
  updated_ratings <- update_trueskill(mu_winner, sigma_winner, mu_loser, sigma_loser)
  
  ratings$mu[ratings$Player == winner] <- updated_ratings$mu_winner_new
  ratings$sigma[ratings$Player == winner] <- updated_ratings$sigma_winner_new
  ratings$mu[ratings$Player == loser] <- updated_ratings$mu_loser_new
  ratings$sigma[ratings$Player == loser] <- updated_ratings$sigma_loser_new
}

val_data <- data[train_data_len+1:ceiling(val_data_len), ]

# # Predict match outcomes
# predictions <- data %>%
#   rowwise() %>%
#   mutate(
#     mu_winner = ratings$mu[ratings$Player == Winner],
#     mu_loser = ratings$mu[ratings$Player == Loser],
#     predicted_winner = ifelse(mu_winner > mu_loser, Winner, Loser)
#   )

# Predict match outcomes and probabilities
# predictions <- data %>%
#   rowwise() %>%
#   mutate(
#     mu_winner = ratings$mu[ratings$Player == Winner],
#     mu_loser = ratings$mu[ratings$Player == Loser],
#     prob_winner = 1 / (1 + 10^((mu_loser - mu_winner) / 400)),
#     predicted_winner = ifelse(prob_winner > 0.5, Winner, Loser)
#   )
predictions <- val_data
# Calculate accuracy
accuracy <- mean(predictions$Winner == predictions$predicted_winner)
print(paste("Accuracy:", accuracy))

# Calculate calibration
# Calibration can be calculated by comparing predicted probabilities with actual outcomes
# Here, we will calculate the mean squared error between predicted probabilities and actual outcomes
calibration <- mean((ifelse(predictions$Winner == predictions$Winner, 1, 0) - predictions$prob_winner)^2)
print(paste("Calibration (MSE):", calibration))

# # Calculate calibration (Brier score as a proxy for calibration)
# brier_score <- mean((ifelse(predictions$Winner == predictions$Winner, 1, 0) - predictions$prob_winner)^2)
# print(paste("Brier score (Calibration):", brier_score))

# Calculate log-loss
library(Metrics)
log_loss_value <- logLoss(ifelse(predictions$Winner == predictions$Winner, 1, 0), predictions$prob_winner)
print(paste("Log-loss:", log_loss_value))

c(accuracy, calibration, log_loss_value)
# 0.6644395 0.2462763 0.6856880



