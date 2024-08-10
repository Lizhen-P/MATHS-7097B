# pacman::p_load(tidyverse, lubridate, patchwork, knitr, vegalite, magrittr, readxl)

pacman::p_load(readxl, dplyr, magrittr, lubridate, tidyverse, knitr, plotly, akima)

# Betting: 2001 - 2018
# 2019

# Naive -------------------------------------------------------
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
data_clean <- function(df){
  # sort by Date
  df %<>%
    arrange(Date)
  
  df %<>% 
    filter(Comment %in% c("Completed")) %>%
    select(-c(
      "ATP", "Location", "Series",
      "Court", "Round", "Best of",
      "W1", "L1", "W2", "L2", "W3", "L3", 
      "W4", "L4", "W5", "L5", "Wsets", "Lsets",
      "Comment")) %>%
    mutate_at(
      c("Tournament", "Surface"),
      as.factor) %>% 
    rename(BWW = "B&WW",
           BWL = "B&WL")
  
  df %<>% 
    mutate(across(c("CBW", "CBL", "GBW", "GBL", "IWW",
                    "IWL", "SBW", "SBL" , "B365W","B365L", 
                    "BWW", "BWL", "EXW", "EXL", "PSW", 
                    "PSL", "WPts", "LPts", "UBW", "UBL", 
                    "LBW", "LBL", "SJW", "SJL", "MaxW", 
                    "MaxL", "AvgW", "AvgL"), 
                  ~ifelse(is.na(.), 0, .)))
  
  ## Initialize Elo for winner and loser in whole dataframe
  df %<>% 
    mutate(CB_sum = CBW + CBL,
           GB_sum = GBW + GBL,
           IW_sum = IWW + IWL,
           SB_sum = SBW + SBL,
           B365_sum = B365W + B365L,
           BW_sum = BWW + BWL,
           EX_sum = EXW + EXL,
           PS_sum = PSW + PSL,
           UB_sum = UBW + UBL,
           LB_sum = LBW + LBL,
           SJ_sum = SJW + SJL) %>% 
    mutate(num_company = rowSums(select(., CB_sum, GB_sum, IW_sum, SB_sum,
                                        B365_sum, BW_sum, EX_sum, PS_sum,
                                        UB_sum, LB_sum, SJ_sum) != 0))
  # %>% 
  #   mutate(logit_prob_W = sum(CBL/CB_sum, GBL/GB_sum, IWL/IW_sum, SBL/SB_sum,
  #                             B365L/B365_sum, BWL/BW_sum, EXL/EX_sum, PSL/PS_sum,
  #                             UBL/UB_sum, LBL/LB_sum, SJL/SJ_sum, na.rm = TRUE)/num_company,
  #          logit_prob_L = sum(CBW/CB_sum, GBW/GB_sum, IWW/IW_sum, SBW/SB_sum,
  #                             B365W/B365_sum, BWW/BW_sum, EXW/EX_sum, PSW/PS_sum,
  #                             UBW/UB_sum, LBW/LB_sum, SJW/SJ_sum, na.rm = TRUE)/num_company)
  
  numerator_cols_W <- c("CBL", "GBL", "IWL", "SBL", "B365L", "BWL", 
                        "EXL", "PSL", "UBL", "LBL", "SJL")
  numerator_cols_L <- c("CBW", "GBW", "IWW", "SBW", "B365W", "BWW", 
                        "EXW", "PSW", "UBW", "LBW", "SJW")
  denominator_cols <- c("CB_sum", "GB_sum", "IW_sum", "SB_sum", "B365_sum", "BW_sum", 
                        "EX_sum", "PS_sum", "UB_sum", "LB_sum", "SJ_sum")
  
  # calculate winning probability for winner and loser 
  df$y_W <- rowSums(df[numerator_cols_W] / df[denominator_cols], na.rm = TRUE) / df$num_company
  df$y_L <- rowSums(df[numerator_cols_L] / df[denominator_cols], na.rm = TRUE) / df$num_company
  
  df %<>% 
    mutate(win_prob_W = exp(y_W)/(1+exp(y_W)),
           win_prob_L = exp(y_L)/(1+exp(y_L)))
  
  return (df)
}
split_train_test <- function(df, split_time){
  df_train <- filter(df, tourney_date < split_time)
  df_test <- filter(df, tourney_date >= split_time)
  df_test <- filter(df_test, tourney_date < (split_time + years(1)))
  return (list(df_train, df_test))
}
transform_klassen <- function(rank){
  return (8-log2(rank))
}
sample_data_by_level <- function(df, levels_list){
  df %<>% 
    filter(tourney_level %in% levels_list)
  return (df)
}
predict_evaluate <- function(model, data){
  # Define a function to calculate three metrics
  # accuracy with 0.5 threshold
  threshold <- 0.5
  probs_val <- predict(model, data, type = 'response')
  predicted_class <- ifelse(probs_val >= threshold, 1, 0)
  acc <- mean(predicted_class == data$higher_rank_won)
  
  # calibration
  W_value <- data$higher_rank_won %>% max()
  W <- count(data, higher_rank_won) %>% 
    filter(higher_rank_won == W_value) %>% 
    select(n) %>% 
    pull(n)
  calibration <- sum(probs_val) / W
  
  # log-loss (cross entropy)
  df <- data.frame(
    prob = probs_val,
    higher_rank_won = data$higher_rank_won
  ) %>% 
    mutate(prob = ifelse(higher_rank_won == W_value, prob, 1 - prob)) %>% 
    mutate(prob = log(prob))
  
  log_loss <- -sum(df$prob)/nrow(df)
  return (c(acc, calibration, log_loss))
}
metrics_table <- function(model, train, val){
  # Define a function to create a table containing metrics for train and validation set
  metric_train <- predict_evaluate(model, train)
  metric_val <- predict_evaluate(model, val)
  table <- data.frame(
    train = metric_train,
    val = metric_val
  )
  rownames(table) <- c("accuracy", "calibration", "log-loss")
  return (table)
}

naive_model <- function(to, sample=FALSE){
  df <- export_df(2001, 2023)
  df <- data_clean(df)
  
  df %<>% 
    rename(winner_rank = WRank,
           loser_rank = LRank,
           tourney_date = Date,
           winner_rank_points = WPts,
           loser_rank_points = LPts) %>% 
    mutate(tourney_date = ymd(tourney_date))
  
  df %<>%
    filter(tourney_date >= paste(to, '-1-1', sep=''))
  df %<>%
    filter(tourney_date < paste(to+1, '-1-1', sep=''))
  
  ## Remove missing data, create new features
  df <- df |>
    na.omit() |>
    mutate(higher_rank_won = winner_rank < loser_rank) |>
    mutate(higher_rank_points = winner_rank_points * (higher_rank_won) +
             loser_rank_points * (1 - higher_rank_won)) |>
    mutate(lower_rank_points = winner_rank_points * (1 - higher_rank_won) +
             loser_rank_points * (higher_rank_won))
  # df %>% View()
  df <- df |>
    mutate(diff = higher_rank_points - lower_rank_points)
  
  ## Impute values to replace missing data
  df <- df |>
    mutate(loser_rank = replace_na(loser_rank, 100000)) |>
    mutate(winner_rank = replace_na(winner_rank, 100000))
  
  ## Transformation following paper, Klassen and Magnus (2003)
  df <- df |>
    mutate(higher_rank = winner_rank * (higher_rank_won) +
             loser_rank * (1 - higher_rank_won)) |>
    mutate(lower_rank = winner_rank * (1 - higher_rank_won) +
             loser_rank * (higher_rank_won))
  
  # transform_klassen() could be found in W3.R
  df <- df |>
    mutate(higher_rank_klassen = transform_klassen(higher_rank)) |>
    mutate(lower_rank_klassen = transform_klassen(lower_rank))
  
  df <- df |>
    mutate(diff_klassen = higher_rank_klassen - lower_rank_klassen)
  
  N <- nrow(df)
  naive_accuracy <- mean(df$higher_rank_won)
  w <- df$higher_rank_won
  # For this model, pi is constant and equal to the accuracy we have already calculated.
  pi_naive <- naive_accuracy
  log_loss_naive <- -1 / N * sum(w * log(pi_naive) +
                                   (1 - w) * log(1 - pi_naive))
  calibration_naive <- pi_naive * N / sum(w)
  validation_stats <- tibble(model = "naive", 
                             pred_acc = naive_accuracy,
                             log_loss = log_loss_naive, 
                             calibration = calibration_naive)
  kable(validation_stats)
}

naive_model(2019)
# 0.6123844 1 0.6676696

# Logit -------------------------------------------------------
logit_model <- function(split_time, sample=FALSE){
  df <- export_df(2001, 2023)
  df <- data_clean(df)
  df %<>% 
    rename(winner_rank = WRank,
           loser_rank = LRank,
           tourney_date = Date,
           winner_rank_points = WPts,
           loser_rank_points = LPts) %>% 
    mutate(tourney_date = ymd(tourney_date))
  
  ## Remove missing data, create new features
  df <- df |>
    na.omit() |>
    mutate(higher_rank_won = winner_rank < loser_rank) |>
    mutate(higher_rank_points = winner_rank_points * (higher_rank_won) +
             loser_rank_points * (1 - higher_rank_won)) |>
    mutate(lower_rank_points = winner_rank_points * (1 - higher_rank_won) +
             loser_rank_points * (higher_rank_won))
  
  df <- df |>
    mutate(diff = higher_rank_points - lower_rank_points)
  
  ## Impute values to replace missing data
  df <- df |>
    mutate(loser_rank = replace_na(loser_rank, 100000)) |>
    mutate(winner_rank = replace_na(winner_rank, 100000))
  
  ## Transformation following paper, Klassen and Magnus (2003)
  df <- df |>
    mutate(higher_rank = winner_rank * (higher_rank_won) +
             loser_rank * (1 - higher_rank_won)) |>
    mutate(lower_rank = winner_rank * (1 - higher_rank_won) +
             loser_rank * (higher_rank_won))
  
  # transform_klassen() could be found in W3.R
  df <- df |>
    mutate(higher_rank_klassen = transform_klassen(higher_rank)) |>
    mutate(lower_rank_klassen = transform_klassen(lower_rank))
  
  df <- df |>
    mutate(diff_klassen = higher_rank_klassen - lower_rank_klassen)
  
  split_res <- split_train_test(df, split_time)
  df_train <- split_res[[1]]
  df_test <- split_res[[2]]
  
  # Fit model
  model <- glm(
    higher_rank_won ~ diff_klassen + 0,
    # higher_rank_won ~ diff_klassen + surface,
    # higher_rank_won ~ diff_klassen + average_percent_win,
    # higher_rank_won ~ diff_klassen + Average_Ability_Hard + Average_Ability_Clay + Average_Ability_Grass + Average_Ability_Carpet,
    data = df_train,
    family = binomial(link = 'logit')
  )
  # print(df_train %>% nrow())
  # print(df_test %>% nrow())
  metrics_table(model, df_train, df_test)
}
logit_model(dmy("01-01-2019"))
# train       val
# accuracy    0.6608222 0.6123844
# calibration 0.9990208 1.0596427
# log-loss    0.6114394 0.6422588

# Elo_k -------------------------------------------------------
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
           tourney_level = Series,
           winner_id = Winner,
           loser_id = Loser) %>% 
    select(c(
      "tourney_date",
      "surface",
      "tourney_level",
      "winner_id",
      "loser_id",
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
    select(-c("surface")) %>% 
    mutate(Elo_winner = 1500,
           Elo_loser = 1500)
  
  return (df)
}
extract_players <- function(df){
  # Define a function to create players dataframe based on the whole dataframe
  # id including winner_id and loser_id, and initialize the related rating to 1500
  df_player <- rbind(data.frame(id = df$winner_id), 
                     data.frame(id = df$loser_id))
  
  df_player %<>% 
    unique() %>% 
    mutate(elo_rating = 1500,
           max_elo_rating = 1500,
           match_num = 0)
  
  return(df_player)
}
split_train_test <- function(df, split_time){
  df_train <- filter(df, tourney_date < split_time)
  df_test <- filter(df, tourney_date >= split_time)
  return (list(df_train, df_test))
}

# Calculate functions
update_elo_k <- function(k, elo_i, elo_j){
  # Calculate probability player i wins
  pi_i_j <- 1/(1+10**((elo_j - elo_i)/400))
  
  # Update elo rating when player i wins
  change <- k * (1 - pi_i_j)
  new_elo_i <- elo_i + change
  new_elo_j <- elo_j - change
  return(c(new_elo_i, new_elo_j, pi_i_j))
}
find_elo <- function(df_player, id, new_value, update, mode){
  # Define a function to extract elo rating based on id
  # mode = 1: elo rating
  # mode = 2: match number
  # If update is TRUE, update the player dataframe
  if (mode == 1){
    res <- df_player$elo_rating[df_player$id == id]
    if (update){
      df_player$elo_rating <- ifelse(df_player$id == id, new_value, df_player$elo_rating)
      res_max <- df_player$max_elo_rating[df_player$id == id]
      df_player[which(df_player$id == id), 'max_elo_rating'] <- ifelse(new_value > res_max, new_value, res_max)
    }
  } else if(mode == 2){
    res <- df_player$match_num[df_player$id == id]
    if (update){
      df_player$match_num <- ifelse(df_player$id == id, new_value, df_player$match_num)
    }
  }
  return (list(res, df_player))
}
add_elo_k <- function(df, df_player, k, include_c = FALSE, val_top_num = FALSE){
  # Define a function to add elo rating into the dataframe using constant k factor
  # Iterate matches
  for (i in 1:nrow(df)){
    # Find elo ratings of the player i and j
    elo_i <- find_elo(df_player, df[i, 'winner_id'], NA, FALSE, 1)[[1]]
    elo_j <- find_elo(df_player, df[i, 'loser_id'], NA, FALSE, 1)[[1]]
    
    df[i, 'Elo_winner_before'] <- elo_i
    df[i, 'Elo_loser_before'] <- elo_j
    
    # Calculate new elo ratings
    if (include_c != FALSE && df[i, 'tourney_level'] == 'G'){
      res <- update_elo_k(k*include_c, elo_i, elo_j)
    }else{
      res <- update_elo_k(k, elo_i, elo_j)
    }
    
    # Update player dataframe
    df_player <- find_elo(df_player, df[i, 'winner_id'], res[1], TRUE, 1)[[2]]
    df_player <- find_elo(df_player, df[i, 'loser_id'], res[2], TRUE, 1)[[2]]
    
    # Assign the elo ratings in the match dataframe
    df[i, 'Elo_winner'] <- res[1]
    df[i, 'Elo_loser'] <- res[2]
    df[i, 'pi_i_j'] <- res[3]
    
    if (val_top_num != FALSE){
      Elo_top_num <- (df_player$elo_rating %>% sort(decreasing = TRUE))[[val_top_num]]
      
      if (res[1] >= Elo_top_num){
        df[i, 'val_top_num_W'] <- TRUE
      }
      
      if (res[2] >= Elo_top_num){
        df[i, 'val_top_num_L'] <- TRUE
      }
    }
  }
  
  df %<>%
    mutate(elo_higher_rank_won = ifelse(Elo_winner_before >= Elo_loser_before, TRUE, FALSE)) %>% 
    mutate(prob_higher_win = ifelse(elo_higher_rank_won == TRUE, pi_i_j, 1-pi_i_j))
  return (list(df, df_player))
}
create_val_set <- function(df, df_player, k, is_538 = FALSE, include_c = FALSE, val_top_num = FALSE){
  df_players_val <- extract_players(df)
  
  df_player %<>%
    full_join(df_players_val, by = "id") %>%
    mutate(elo_rating = ifelse(is.na(elo_rating.x), 1500, elo_rating.x),
           match_num = ifelse(is.na(match_num.x), 0, match_num.x),
           max_elo_rating = ifelse(is.na(max_elo_rating.x), 1500, max_elo_rating.x)) %>%
    select(id, elo_rating, match_num, max_elo_rating)
  
  if (is_538 == FALSE){
    res_val <- add_elo_k(df, df_player, k, include_c, val_top_num)
  } else{
    res_val <- add_elo_k_538(df, df_player, k[1], k[2], k[3], include_c, val_top_num)
  }
  return (res_val)
}
add_elo_k_538 <- function(df, df_player, delta, nu, sigma, include_c = FALSE, val_top_num = FALSE){
  # Define a function to add elo rating into the dataframe using FiveThirtyEight
  # Iterate matches
  for (i in 1:nrow(df)){
    # Find elo ratings of the player i and j
    elo_i <- find_elo(df_player, df[i, 'winner_id'], NA, FALSE, 1)[[1]]
    elo_j <- find_elo(df_player, df[i, 'loser_id'], NA, FALSE, 1)[[1]]
    
    df[i, 'Elo_winner_before'] <- elo_i
    df[i, 'Elo_loser_before'] <- elo_j
    
    # Find match number of the player i and j
    m_i <- find_elo(df_player, df[i, 'winner_id'], NA, FALSE, 2)[[1]]
    m_j <- find_elo(df_player, df[i, 'loser_id'], NA, FALSE, 2)[[1]]
    
    # Calculate k_538
    k_i = delta / (m_i + nu)^sigma
    k_j = delta / (m_j + nu)^sigma
    
    # Calculate new elo ratings
    if (include_c != FALSE && df[i, 'tourney_level'] == 'G'){
      k_i <- k_i*include_c
      k_j <- k_j*include_c
    }
    
    # Calculate new elo ratings and update player dataframe
    res_i <- update_elo_k(k_i, elo_i, elo_j)
    res_j <- update_elo_k(k_j, elo_i, elo_j)
    
    # Update player dataframe
    df_player <- find_elo(df_player, df[i, 'winner_id'], res_i[1], TRUE, 1)[[2]]
    df_player <- find_elo(df_player, df[i, 'loser_id'], res_j[2], TRUE, 1)[[2]]
    df_player <- find_elo(df_player, df[i, 'winner_id'], m_i+1, TRUE, 2)[[2]]
    df_player <- find_elo(df_player, df[i, 'loser_id'], m_j+1, TRUE, 2)[[2]]
    
    # Assign the elo ratings in the match dataframe
    df[i, 'Elo_winner'] <- res_i[1]
    df[i, 'Elo_loser'] <- res_j[2]
    df[i, 'pi_i_j'] <- res_i[3]
    
    # These columns are recorded as match num after the current match
    df[i, 'Elo_winner_match_num'] <- m_i+1
    df[i, 'Elo_loser_match_num'] <- m_j+1
    
    if (val_top_num != FALSE){
      Elo_top_num <- (df_player$elo_rating %>% sort(decreasing = TRUE))[[val_top_num]]
      if (res_i[1] >= Elo_top_num){
        df[i, 'val_top_num_W'] <- TRUE
      }
      
      if (res_j[2] >= Elo_top_num){
        df[i, 'val_top_num_L'] <- TRUE
      }
    }
  }
  
  df %<>%
    mutate(elo_higher_rank_won = ifelse(Elo_winner_before >= Elo_loser_before, TRUE, FALSE)) %>% 
    mutate(prob_higher_win = ifelse(elo_higher_rank_won == TRUE, pi_i_j, 1-pi_i_j))
  return (list(df, df_player))
}

# Evaluate functions
predict_evaluate_elo <- function(data){
  # accuracy with 0.5 threshold
  threshold <- 0.5
  probs_val <- data$prob_higher_win
  predicted_class <- ifelse(probs_val >= threshold, 1, 0)
  acc <- mean(predicted_class == data$elo_higher_rank_won)
  
  # calibration
  W_value <- data$elo_higher_rank_won %>% max()
  W <- count(data, elo_higher_rank_won) %>% 
    filter(elo_higher_rank_won == W_value) %>% 
    select(n) %>% 
    pull(n)
  calibration <- sum(probs_val) / W
  
  # log-loss (cross entropy)
  df <- data.frame(
    prob = probs_val,
    elo_higher_rank_won = data$elo_higher_rank_won
  ) %>% 
    mutate(prob = ifelse(elo_higher_rank_won == W_value, prob, 1 - prob)) %>% 
    mutate(prob = log(prob))
  
  log_loss <- -sum(df$prob)/nrow(df)
  return (c(acc, calibration, log_loss))
}
metrics_table_elo <- function(train, val){
  metric_train <- predict_evaluate_elo(train)
  metric_val <- predict_evaluate_elo(val)
  table <- data.frame(
    train = metric_train,
    val = metric_val
  )
  rownames(table) <- c("accuracy", "calibration", "log-loss")
  return (table)
}

# Export data
df_init_total <- export_df(2001, 2023)

# Perform data cleaning
df_cleaned_total <- data_clean_elo(df_init_total)
split_68_18_19_res <- split_train_test(df_cleaned_total, dmy("01-01-2019"))
df_cleaned_train <- split_68_18_19_res[[1]]
df_cleaned_val <- split_68_18_19_res[[2]]
df_cleaned_val %<>% 
  filter(tourney_date < dmy("01-01-2020"))

# Extract players
df_players <- extract_players(df_cleaned_train)

# Tuning
train_K_Elo_return <- function(k){
  # cat("Prediction using K =", k, "\n")
  res_k_1_train <- add_elo_k(df_cleaned_train, df_players, k)
  df_k_1_train <- res_k_1_train[[1]]
  df_players_k_1_train <- res_k_1_train[[2]]
  
  res_k_1_val <- create_val_set(df_cleaned_val, df_players_k_1_train, k)
  df_k_1_val <- res_k_1_val[[1]]
  df_players_k_1_val <- res_k_1_val[[2]]
  metric_1 <- metrics_table_elo(df_k_1_train, df_k_1_val)
  
  # cat("include c case \n")
  # include c
  res_k_1c_train <- add_elo_k(df_cleaned_train, df_players, k, 1.1)
  df_k_1c_train <- res_k_1c_train[[1]]
  df_players_k_1c_train <- res_k_1c_train[[2]]
  
  res_k_1c_val <- create_val_set(df_cleaned_val, df_players_k_1c_train, k, FALSE, 1.1)
  df_k_1c_val <- res_k_1c_val[[1]]
  df_players_k_1c_val <- res_k_1c_val[[2]]
  metric_2 <- metrics_table_elo(df_k_1c_train, df_k_1c_val)
  # cat("\n")
  return (c(metric_1, metric_2))
}
create_df_plot <- function(all_k){
  df <- data.frame()
  for (each_k in all_k) {
    each_k_res <- train_K_Elo_return(each_k)
    df %<>% rbind(data.frame(k = each_k,
                             data_type = "train",
                             include_c = FALSE,
                             accuracy = each_k_res[1]$train[1],
                             calibration = each_k_res[1]$train[2],
                             `logloss` = each_k_res[1]$train[3]))
    
    df %<>% rbind(data.frame(k = each_k,
                             data_type = "val",
                             include_c = FALSE,
                             accuracy = each_k_res[2]$val[1],
                             calibration = each_k_res[2]$val[2],
                             `logloss` = each_k_res[2]$val[3]))
    
    df %<>% rbind(data.frame(k = each_k,
                             data_type = "train",
                             include_c = TRUE,
                             accuracy = each_k_res[3]$train[1],
                             calibration = each_k_res[3]$train[2],
                             `logloss` = each_k_res[3]$train[3]))
    
    df %<>% rbind(data.frame(k = each_k,
                             data_type = "val",
                             include_c = TRUE,
                             accuracy = each_k_res[4]$val[1],
                             calibration = each_k_res[4]$val[2],
                             `logloss` = each_k_res[4]$val[3]))
  }
  return (df)
}
all_k <- c(10:30)
df_plot_elo_k <- create_df_plot(all_k)

# Show Curve
plot_metrics <- function(df, show_include_c){
  if (show_include_c) {
    df_val <- df %>% filter(include_c == TRUE)
  } else {
    df_val <- df %>% filter(include_c == FALSE)
  }
  
  p_accuracy <- ggplot(df_val, aes(x = as.factor(k), 
                                   y = accuracy, 
                                   group = data_type, 
                                   color = as.factor(data_type))) +
    geom_line() +
    geom_point() +
    labs(title = "Accuracy against K factors", x = "K", y = "Accuracy", color = "Label") +
    theme_minimal() +
    theme(legend.position = "right")
  
  p_calibration <- ggplot(df_val, aes(x = as.factor(k), 
                                      y = calibration, 
                                      group = data_type, 
                                      color = as.factor(data_type))) +
    geom_line() +
    geom_point() +
    labs(title = "Calibration against K factors", x = "K", y = "Calibration", color = "Label") +
    theme_minimal() +
    theme(legend.position = "right")
  
  p_logloss <- ggplot(df_val, aes(x = as.factor(k), 
                                  y = logloss, 
                                  group = data_type, 
                                  color = as.factor(data_type))) +
    geom_line() +
    geom_point() +
    labs(title = "Log-loss against K factors", x = "K", y = "Log-loss", color = "Label") +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(p_accuracy)
  print(p_calibration)
  print(p_logloss)
}
plot_metrics(df_plot_elo_k, FALSE)

# Elo_538 -------------------------------------------------------
train_Elo_538_return_covid <- function(delta, nu, sigma, include_c){
  # cat("Prediction using K =", k, "\n")
  if (include_c == FALSE){
    res_k_2_train <- add_elo_k_538(df_cleaned_train, df_players, delta, nu, sigma)
    df_k_2_train <- res_k_2_train[[1]]
    df_players_k_2_train <- res_k_2_train[[2]]
    
    res_k_2_val <- create_val_set(df_cleaned_val, df_players_k_2_train, c(delta, nu, sigma), TRUE)
    df_k_2_val <- res_k_2_val[[1]]
    df_players_k_2_val <- res_k_2_val[[2]]
    metric_1 <- metrics_table_elo(df_k_2_train, df_k_2_val)
    return (metric_1)
  }else if (include_c == TRUE){
    # cat("include c case \n")
    # include c
    res_k_2c_train <- add_elo_k_538(df_cleaned_train, df_players, delta, nu, sigma, 1.1)
    df_k_2c_train <- res_k_2c_train[[1]]
    df_players_k_2c_train <- res_k_2c_train[[2]]
    
    res_k_2c_val <- create_val_set(df_cleaned_val, df_players_k_2c_train, c(delta, nu, sigma), TRUE, 1.1)
    df_k_2c_val <- res_k_2c_val[[1]]
    df_players_k_2c_val <- res_k_2c_val[[2]]
    metric_2 <- metrics_table_elo(df_k_2c_train, df_k_2c_val)
    # cat("\n")
    return (metric_2)
  }
  return (c(metric_1, metric_2))
}
create_df_plot_covid <- function(all_hyper, include_c){
  df <- data.frame()
  for (each_hyper in all_hyper) {
    delta <- each_hyper$delta
    nu <- each_hyper$nu
    sigma <- each_hyper$sigma
    each_k_res <- train_Elo_538_return_covid(delta, nu, sigma, include_c)
    
    df %<>% rbind(data.frame(delta = delta,
                             nu = nu,
                             sigma = sigma,
                             data_type = "train",
                             include_c = include_c,
                             accuracy = each_k_res[1]$train[1],
                             calibration = each_k_res[1]$train[2],
                             `logloss` = each_k_res[1]$train[3]))
    
    df %<>% rbind(data.frame(delta = delta,
                             nu = nu,
                             sigma = sigma,
                             data_type = "val",
                             include_c = include_c,
                             accuracy = each_k_res[2]$val[1],
                             calibration = each_k_res[2]$val[2],
                             `logloss` = each_k_res[2]$val[3]))
  }
  return (df)
}
create_all_hyper <- function(all_d, all_n, all_s){
  all <- list() # Initialize the list to store combinations
  all_index <- 1
  for (each_n in all_n) {
    for (each_s in all_s) {
      # For each combination, create a list and add it to the combinations list
      all[[all_index]] <- list(delta = all_d, nu = each_n, sigma = each_s)
      all_index <- all_index + 1
    }
  }
  
  return(all)
}

# Exclude
# 100, 5, 0.1
all_delta <- 100
all_nu <- seq(from = 10, to = 100, by = 20)
all_sigma <- seq(from = 0.05, to = 0.2, by = 0.025)
all_hyper <- create_all_hyper(all_delta, all_nu, all_sigma)
df_plot_elo_538_covid <- create_df_plot_covid(all_hyper, FALSE)
# 25 252500 0.02
# 0.6588235 1.031271 0.6188063


# Show Manifold
df_plot_elo_538_covid_exclude_C <- df_plot_elo_538_covid %>% filter(data_type == "val")

fig <- plot_ly(df_plot_elo_538_covid_exclude_C, 
               x = ~df_plot_elo_538_covid_exclude_C$sigma, 
               y = ~df_plot_elo_538_covid_exclude_C$nu, 
               z = ~df_plot_elo_538_covid_exclude_C$logloss, 
               type = 'scatter3d', mode = 'markers') %>%
  layout(
    scene = list(
      xaxis = list(title = 'Sigma'),
      yaxis = list(title = 'Nu'),
      zaxis = list(title = 'Log-loss')
    )
  )
fig











# BCM -------------------------------------------------------
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
data_clean <- function(df){
  # sort by Date
  df %<>%
    arrange(Date)
  
  df %<>% 
    filter(Comment %in% c("Completed")) %>%
    select(-c(
      "ATP", "Location", "Series",
      "Court", "Round", "Best of",
      "W1", "L1", "W2", "L2", "W3", "L3", 
      "W4", "L4", "W5", "L5", "Wsets", "Lsets",
      "Comment")) %>%
    mutate_at(
      c("Tournament", "Surface"),
      as.factor) %>% 
    rename(BWW = "B&WW",
           BWL = "B&WL")
  
  df %<>% 
    mutate(across(c("CBW", "CBL", "GBW", "GBL", "IWW",
                    "IWL", "SBW", "SBL" , "B365W","B365L", 
                    "BWW", "BWL", "EXW", "EXL", "PSW", 
                    "PSL", "WPts", "LPts", "UBW", "UBL", 
                    "LBW", "LBL", "SJW", "SJL", "MaxW", 
                    "MaxL", "AvgW", "AvgL"), 
                  ~ifelse(is.na(.), 0, .)))
  
  ## Initialize Elo for winner and loser in whole dataframe
  df %<>% 
    mutate(CB_sum = CBW + CBL,
           GB_sum = GBW + GBL,
           IW_sum = IWW + IWL,
           SB_sum = SBW + SBL,
           B365_sum = B365W + B365L,
           BW_sum = BWW + BWL,
           EX_sum = EXW + EXL,
           PS_sum = PSW + PSL,
           UB_sum = UBW + UBL,
           LB_sum = LBW + LBL,
           SJ_sum = SJW + SJL) %>% 
    mutate(num_company = rowSums(select(., CB_sum, GB_sum, IW_sum, SB_sum,
                                        B365_sum, BW_sum, EX_sum, PS_sum,
                                        UB_sum, LB_sum, SJ_sum) != 0))
  # %>% 
  #   mutate(logit_prob_W = sum(CBL/CB_sum, GBL/GB_sum, IWL/IW_sum, SBL/SB_sum,
  #                             B365L/B365_sum, BWL/BW_sum, EXL/EX_sum, PSL/PS_sum,
  #                             UBL/UB_sum, LBL/LB_sum, SJL/SJ_sum, na.rm = TRUE)/num_company,
  #          logit_prob_L = sum(CBW/CB_sum, GBW/GB_sum, IWW/IW_sum, SBW/SB_sum,
  #                             B365W/B365_sum, BWW/BW_sum, EXW/EX_sum, PSW/PS_sum,
  #                             UBW/UB_sum, LBW/LB_sum, SJW/SJ_sum, na.rm = TRUE)/num_company)
  
  numerator_cols_W <- c("CBL", "GBL", "IWL", "SBL", "B365L", "BWL", 
                        "EXL", "PSL", "UBL", "LBL", "SJL")
  numerator_cols_L <- c("CBW", "GBW", "IWW", "SBW", "B365W", "BWW", 
                        "EXW", "PSW", "UBW", "LBW", "SJW")
  denominator_cols <- c("CB_sum", "GB_sum", "IW_sum", "SB_sum", "B365_sum", "BW_sum", 
                        "EX_sum", "PS_sum", "UB_sum", "LB_sum", "SJ_sum")
  
  # calculate winning probability for winner and loser 
  df$y_W <- rowSums(df[numerator_cols_W] / df[denominator_cols], na.rm = TRUE) / df$num_company
  df$y_L <- rowSums(df[numerator_cols_L] / df[denominator_cols], na.rm = TRUE) / df$num_company
  
  df %<>% 
    mutate(win_prob_W = exp(y_W)/(1+exp(y_W)),
           win_prob_L = exp(y_L)/(1+exp(y_L)))
  
  return (df)
}
split_df <- function(df, from, to){
  from <- paste("01-01-", from, sep = "")
  to <- paste("31-12-", to, sep = "")
  df %<>% 
    filter(Date >= dmy(from) & Date <= dmy(to))
  return(df)
}

# Export data
df_init_total <- export_df(2001, 2023)

# # Check Comment column
# table(df_init_total$Comment)

# Perform data cleaning
df_cleaned_total <- data_clean(df_init_total)

df_cleaned_split <- df_cleaned_total
# df_cleaned_split <- split_df(df_cleaned_total, 2014, 2023)
# df_cleaned_split <- split_df(df_cleaned_total, 2001, 2010)
# df_cleaned_split <- split_df(df_cleaned_total, 2001, 2018)
df_cleaned_split <- split_df(df_cleaned_total, 2019, 2019)

# Check how many observations which do not record odds (985/61275)
df_cleaned_split_2 <- df_cleaned_split %>%
  filter(num_company != 0) %>%
  mutate(higher_prob_won = ifelse(win_prob_W >= win_prob_L, 1, 0)) %>% 
  mutate(higher_prob = ifelse(win_prob_W >= win_prob_L, win_prob_W, win_prob_L)) %>% 
  mutate(lower_prob = ifelse(win_prob_W >= win_prob_L, win_prob_L, win_prob_W))

# Evaluate functions
predict_evaluate_betting <- function(df){
  # Define a function to calculate three metrics
  probs_val <- df$higher_prob
  
  # calibration
  W_value <- df$higher_prob_won %>% max()
  W <- count(df, higher_prob_won) %>% 
    filter(higher_prob_won == W_value) %>% 
    select(n) %>% 
    pull(n)
  calibration <- sum(probs_val) / W
  
  acc <- W / nrow(df)
  
  # log-loss (cross entropy)
  df <- data.frame(
    prob = probs_val,
    higher_prob_won = df$higher_prob_won
  ) %>% 
    mutate(prob = ifelse(higher_prob_won == W_value, prob, 1 - prob)) %>% 
    mutate(prob = log(prob))
  
  log_loss <- -sum(df$prob)/nrow(df)
  return (c(acc, calibration, log_loss))
}
predict_evaluate_betting(df_cleaned_split_2)
# 0.6727926 0.9829454 0.6198793 - 2019

pacman::p_load(readxl, dplyr, magrittr, lubridate, tidyverse, knitr, plotly, akima)

# Betting: 2001 - 2022
# 2023

# Naive -------------------------------------------------------
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
data_clean <- function(df){
  # sort by Date
  df %<>%
    arrange(Date)
  
  df %<>% 
    filter(Comment %in% c("Completed")) %>%
    select(-c(
      "ATP", "Location", "Series",
      "Court", "Round", "Best of",
      "W1", "L1", "W2", "L2", "W3", "L3", 
      "W4", "L4", "W5", "L5", "Wsets", "Lsets",
      "Comment")) %>%
    mutate_at(
      c("Tournament", "Surface"),
      as.factor) %>% 
    rename(BWW = "B&WW",
           BWL = "B&WL")
  
  df %<>% 
    mutate(across(c("CBW", "CBL", "GBW", "GBL", "IWW",
                    "IWL", "SBW", "SBL" , "B365W","B365L", 
                    "BWW", "BWL", "EXW", "EXL", "PSW", 
                    "PSL", "WPts", "LPts", "UBW", "UBL", 
                    "LBW", "LBL", "SJW", "SJL", "MaxW", 
                    "MaxL", "AvgW", "AvgL"), 
                  ~ifelse(is.na(.), 0, .)))
  
  ## Initialize Elo for winner and loser in whole dataframe
  df %<>% 
    mutate(CB_sum = CBW + CBL,
           GB_sum = GBW + GBL,
           IW_sum = IWW + IWL,
           SB_sum = SBW + SBL,
           B365_sum = B365W + B365L,
           BW_sum = BWW + BWL,
           EX_sum = EXW + EXL,
           PS_sum = PSW + PSL,
           UB_sum = UBW + UBL,
           LB_sum = LBW + LBL,
           SJ_sum = SJW + SJL) %>% 
    mutate(num_company = rowSums(select(., CB_sum, GB_sum, IW_sum, SB_sum,
                                        B365_sum, BW_sum, EX_sum, PS_sum,
                                        UB_sum, LB_sum, SJ_sum) != 0))
  # %>% 
  #   mutate(logit_prob_W = sum(CBL/CB_sum, GBL/GB_sum, IWL/IW_sum, SBL/SB_sum,
  #                             B365L/B365_sum, BWL/BW_sum, EXL/EX_sum, PSL/PS_sum,
  #                             UBL/UB_sum, LBL/LB_sum, SJL/SJ_sum, na.rm = TRUE)/num_company,
  #          logit_prob_L = sum(CBW/CB_sum, GBW/GB_sum, IWW/IW_sum, SBW/SB_sum,
  #                             B365W/B365_sum, BWW/BW_sum, EXW/EX_sum, PSW/PS_sum,
  #                             UBW/UB_sum, LBW/LB_sum, SJW/SJ_sum, na.rm = TRUE)/num_company)
  
  numerator_cols_W <- c("CBL", "GBL", "IWL", "SBL", "B365L", "BWL", 
                        "EXL", "PSL", "UBL", "LBL", "SJL")
  numerator_cols_L <- c("CBW", "GBW", "IWW", "SBW", "B365W", "BWW", 
                        "EXW", "PSW", "UBW", "LBW", "SJW")
  denominator_cols <- c("CB_sum", "GB_sum", "IW_sum", "SB_sum", "B365_sum", "BW_sum", 
                        "EX_sum", "PS_sum", "UB_sum", "LB_sum", "SJ_sum")
  
  # calculate winning probability for winner and loser 
  df$y_W <- rowSums(df[numerator_cols_W] / df[denominator_cols], na.rm = TRUE) / df$num_company
  df$y_L <- rowSums(df[numerator_cols_L] / df[denominator_cols], na.rm = TRUE) / df$num_company
  
  df %<>% 
    mutate(win_prob_W = exp(y_W)/(1+exp(y_W)),
           win_prob_L = exp(y_L)/(1+exp(y_L)))
  
  return (df)
}
split_train_test <- function(df, split_time){
  df_train <- filter(df, tourney_date < split_time)
  df_test <- filter(df, tourney_date >= split_time)
  df_test <- filter(df_test, tourney_date < (split_time + years(1)))
  return (list(df_train, df_test))
}
transform_klassen <- function(rank){
  return (8-log2(rank))
}
sample_data_by_level <- function(df, levels_list){
  df %<>% 
    filter(tourney_level %in% levels_list)
  return (df)
}
predict_evaluate <- function(model, data){
  # Define a function to calculate three metrics
  # accuracy with 0.5 threshold
  threshold <- 0.5
  probs_val <- predict(model, data, type = 'response')
  predicted_class <- ifelse(probs_val >= threshold, 1, 0)
  acc <- mean(predicted_class == data$higher_rank_won)
  
  # calibration
  W_value <- data$higher_rank_won %>% max()
  W <- count(data, higher_rank_won) %>% 
    filter(higher_rank_won == W_value) %>% 
    select(n) %>% 
    pull(n)
  calibration <- sum(probs_val) / W
  
  # log-loss (cross entropy)
  df <- data.frame(
    prob = probs_val,
    higher_rank_won = data$higher_rank_won
  ) %>% 
    mutate(prob = ifelse(higher_rank_won == W_value, prob, 1 - prob)) %>% 
    mutate(prob = log(prob))
  
  log_loss <- -sum(df$prob)/nrow(df)
  return (c(acc, calibration, log_loss))
}
metrics_table <- function(model, train, val){
  # Define a function to create a table containing metrics for train and validation set
  metric_train <- predict_evaluate(model, train)
  metric_val <- predict_evaluate(model, val)
  table <- data.frame(
    train = metric_train,
    val = metric_val
  )
  rownames(table) <- c("accuracy", "calibration", "log-loss")
  return (table)
}

naive_model <- function(to, sample=FALSE){
  df <- export_df(2001, 2023)
  df <- data_clean(df)
  
  df %<>% 
    rename(winner_rank = WRank,
           loser_rank = LRank,
           tourney_date = Date,
           winner_rank_points = WPts,
           loser_rank_points = LPts) %>% 
    mutate(tourney_date = ymd(tourney_date))
  
  df %<>%
    filter(tourney_date >= paste(to, '-1-1', sep=''))
  df %<>%
    filter(tourney_date < paste(to+1, '-1-1', sep=''))
  
  ## Remove missing data, create new features
  df <- df |>
    na.omit() |>
    mutate(higher_rank_won = winner_rank < loser_rank) |>
    mutate(higher_rank_points = winner_rank_points * (higher_rank_won) +
             loser_rank_points * (1 - higher_rank_won)) |>
    mutate(lower_rank_points = winner_rank_points * (1 - higher_rank_won) +
             loser_rank_points * (higher_rank_won))
  # df %>% View()
  df <- df |>
    mutate(diff = higher_rank_points - lower_rank_points)
  
  ## Impute values to replace missing data
  df <- df |>
    mutate(loser_rank = replace_na(loser_rank, 100000)) |>
    mutate(winner_rank = replace_na(winner_rank, 100000))
  
  ## Transformation following paper, Klassen and Magnus (2003)
  df <- df |>
    mutate(higher_rank = winner_rank * (higher_rank_won) +
             loser_rank * (1 - higher_rank_won)) |>
    mutate(lower_rank = winner_rank * (1 - higher_rank_won) +
             loser_rank * (higher_rank_won))
  
  # transform_klassen() could be found in W3.R
  df <- df |>
    mutate(higher_rank_klassen = transform_klassen(higher_rank)) |>
    mutate(lower_rank_klassen = transform_klassen(lower_rank))
  
  df <- df |>
    mutate(diff_klassen = higher_rank_klassen - lower_rank_klassen)
  
  N <- nrow(df)
  naive_accuracy <- mean(df$higher_rank_won)
  w <- df$higher_rank_won
  # For this model, pi is constant and equal to the accuracy we have already calculated.
  pi_naive <- naive_accuracy
  log_loss_naive <- -1 / N * sum(w * log(pi_naive) +
                                   (1 - w) * log(1 - pi_naive))
  calibration_naive <- pi_naive * N / sum(w)
  validation_stats <- tibble(model = "naive", 
                             pred_acc = naive_accuracy,
                             log_loss = log_loss_naive, 
                             calibration = calibration_naive)
  kable(validation_stats)
}

naive_model(2023)
# 0.6400307 1 0.6534005

# Logit -------------------------------------------------------
logit_model <- function(split_time, sample=FALSE){
  df <- export_df(2001, 2023)
  df <- data_clean(df)
  df %<>% 
    rename(winner_rank = WRank,
           loser_rank = LRank,
           tourney_date = Date,
           winner_rank_points = WPts,
           loser_rank_points = LPts) %>% 
    mutate(tourney_date = ymd(tourney_date))
  
  ## Remove missing data, create new features
  df <- df |>
    na.omit() |>
    mutate(higher_rank_won = winner_rank < loser_rank) |>
    mutate(higher_rank_points = winner_rank_points * (higher_rank_won) +
             loser_rank_points * (1 - higher_rank_won)) |>
    mutate(lower_rank_points = winner_rank_points * (1 - higher_rank_won) +
             loser_rank_points * (higher_rank_won))
  
  df <- df |>
    mutate(diff = higher_rank_points - lower_rank_points)
  
  ## Impute values to replace missing data
  df <- df |>
    mutate(loser_rank = replace_na(loser_rank, 100000)) |>
    mutate(winner_rank = replace_na(winner_rank, 100000))
  
  ## Transformation following paper, Klassen and Magnus (2003)
  df <- df |>
    mutate(higher_rank = winner_rank * (higher_rank_won) +
             loser_rank * (1 - higher_rank_won)) |>
    mutate(lower_rank = winner_rank * (1 - higher_rank_won) +
             loser_rank * (higher_rank_won))
  
  # transform_klassen() could be found in W3.R
  df <- df |>
    mutate(higher_rank_klassen = transform_klassen(higher_rank)) |>
    mutate(lower_rank_klassen = transform_klassen(lower_rank))
  
  df <- df |>
    mutate(diff_klassen = higher_rank_klassen - lower_rank_klassen)
  
  split_res <- split_train_test(df, split_time)
  df_train <- split_res[[1]]
  df_test <- split_res[[2]]
  
  # Fit model
  model <- glm(
    higher_rank_won ~ diff_klassen + 0,
    # higher_rank_won ~ diff_klassen + surface,
    # higher_rank_won ~ diff_klassen + average_percent_win,
    # higher_rank_won ~ diff_klassen + Average_Ability_Hard + Average_Ability_Clay + Average_Ability_Grass + Average_Ability_Carpet,
    data = df_train,
    family = binomial(link = 'logit')
  )
  # print(df_train %>% nrow())
  # print(df_test %>% nrow())
  metrics_table(model, df_train, df_test)
}
logit_model(dmy("01-01-2023"))
# train       val
# accuracy    0.6558679 0.6400307
# calibration 1.0009627 1.0352139
# log-loss    0.6148685 0.6225173

# Elo_k -------------------------------------------------------
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
           tourney_level = Series,
           winner_id = Winner,
           loser_id = Loser) %>% 
    select(c(
      "tourney_date",
      "surface",
      "tourney_level",
      "winner_id",
      "loser_id",
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
    select(-c("surface")) %>% 
    mutate(Elo_winner = 1500,
           Elo_loser = 1500)
  
  return (df)
}
extract_players <- function(df){
  # Define a function to create players dataframe based on the whole dataframe
  # id including winner_id and loser_id, and initialize the related rating to 1500
  df_player <- rbind(data.frame(id = df$winner_id), 
                     data.frame(id = df$loser_id))
  
  df_player %<>% 
    unique() %>% 
    mutate(elo_rating = 1500,
           max_elo_rating = 1500,
           match_num = 0)
  
  return(df_player)
}
split_train_test <- function(df, split_time){
  df_train <- filter(df, tourney_date < split_time)
  df_test <- filter(df, tourney_date >= split_time)
  return (list(df_train, df_test))
}

# Calculate functions
update_elo_k <- function(k, elo_i, elo_j){
  # Calculate probability player i wins
  pi_i_j <- 1/(1+10**((elo_j - elo_i)/400))
  
  # Update elo rating when player i wins
  change <- k * (1 - pi_i_j)
  new_elo_i <- elo_i + change
  new_elo_j <- elo_j - change
  return(c(new_elo_i, new_elo_j, pi_i_j))
}
find_elo <- function(df_player, id, new_value, update, mode){
  # Define a function to extract elo rating based on id
  # mode = 1: elo rating
  # mode = 2: match number
  # If update is TRUE, update the player dataframe
  if (mode == 1){
    res <- df_player$elo_rating[df_player$id == id]
    if (update){
      df_player$elo_rating <- ifelse(df_player$id == id, new_value, df_player$elo_rating)
      res_max <- df_player$max_elo_rating[df_player$id == id]
      df_player[which(df_player$id == id), 'max_elo_rating'] <- ifelse(new_value > res_max, new_value, res_max)
    }
  } else if(mode == 2){
    res <- df_player$match_num[df_player$id == id]
    if (update){
      df_player$match_num <- ifelse(df_player$id == id, new_value, df_player$match_num)
    }
  }
  return (list(res, df_player))
}
add_elo_k <- function(df, df_player, k, include_c = FALSE, val_top_num = FALSE){
  # Define a function to add elo rating into the dataframe using constant k factor
  # Iterate matches
  for (i in 1:nrow(df)){
    # Find elo ratings of the player i and j
    elo_i <- find_elo(df_player, df[i, 'winner_id'], NA, FALSE, 1)[[1]]
    elo_j <- find_elo(df_player, df[i, 'loser_id'], NA, FALSE, 1)[[1]]
    
    df[i, 'Elo_winner_before'] <- elo_i
    df[i, 'Elo_loser_before'] <- elo_j
    
    # Calculate new elo ratings
    if (include_c != FALSE && df[i, 'tourney_level'] == 'G'){
      res <- update_elo_k(k*include_c, elo_i, elo_j)
    }else{
      res <- update_elo_k(k, elo_i, elo_j)
    }
    
    # Update player dataframe
    df_player <- find_elo(df_player, df[i, 'winner_id'], res[1], TRUE, 1)[[2]]
    df_player <- find_elo(df_player, df[i, 'loser_id'], res[2], TRUE, 1)[[2]]
    
    # Assign the elo ratings in the match dataframe
    df[i, 'Elo_winner'] <- res[1]
    df[i, 'Elo_loser'] <- res[2]
    df[i, 'pi_i_j'] <- res[3]
    
    if (val_top_num != FALSE){
      Elo_top_num <- (df_player$elo_rating %>% sort(decreasing = TRUE))[[val_top_num]]
      
      if (res[1] >= Elo_top_num){
        df[i, 'val_top_num_W'] <- TRUE
      }
      
      if (res[2] >= Elo_top_num){
        df[i, 'val_top_num_L'] <- TRUE
      }
    }
  }
  
  df %<>%
    mutate(elo_higher_rank_won = ifelse(Elo_winner_before >= Elo_loser_before, TRUE, FALSE)) %>% 
    mutate(prob_higher_win = ifelse(elo_higher_rank_won == TRUE, pi_i_j, 1-pi_i_j))
  return (list(df, df_player))
}
create_val_set <- function(df, df_player, k, is_538 = FALSE, include_c = FALSE, val_top_num = FALSE){
  df_players_val <- extract_players(df)
  
  df_player %<>%
    full_join(df_players_val, by = "id") %>%
    mutate(elo_rating = ifelse(is.na(elo_rating.x), 1500, elo_rating.x),
           match_num = ifelse(is.na(match_num.x), 0, match_num.x),
           max_elo_rating = ifelse(is.na(max_elo_rating.x), 1500, max_elo_rating.x)) %>%
    select(id, elo_rating, match_num, max_elo_rating)
  
  if (is_538 == FALSE){
    res_val <- add_elo_k(df, df_player, k, include_c, val_top_num)
  } else{
    res_val <- add_elo_k_538(df, df_player, k[1], k[2], k[3], include_c, val_top_num)
  }
  return (res_val)
}
add_elo_k_538 <- function(df, df_player, delta, nu, sigma, include_c = FALSE, val_top_num = FALSE){
  # Define a function to add elo rating into the dataframe using FiveThirtyEight
  # Iterate matches
  for (i in 1:nrow(df)){
    # Find elo ratings of the player i and j
    elo_i <- find_elo(df_player, df[i, 'winner_id'], NA, FALSE, 1)[[1]]
    elo_j <- find_elo(df_player, df[i, 'loser_id'], NA, FALSE, 1)[[1]]
    
    df[i, 'Elo_winner_before'] <- elo_i
    df[i, 'Elo_loser_before'] <- elo_j
    
    # Find match number of the player i and j
    m_i <- find_elo(df_player, df[i, 'winner_id'], NA, FALSE, 2)[[1]]
    m_j <- find_elo(df_player, df[i, 'loser_id'], NA, FALSE, 2)[[1]]
    
    # Calculate k_538
    k_i = delta / (m_i + nu)^sigma
    k_j = delta / (m_j + nu)^sigma
    
    # Calculate new elo ratings
    if (include_c != FALSE && df[i, 'tourney_level'] == 'G'){
      k_i <- k_i*include_c
      k_j <- k_j*include_c
    }
    
    # Calculate new elo ratings and update player dataframe
    res_i <- update_elo_k(k_i, elo_i, elo_j)
    res_j <- update_elo_k(k_j, elo_i, elo_j)
    
    # Update player dataframe
    df_player <- find_elo(df_player, df[i, 'winner_id'], res_i[1], TRUE, 1)[[2]]
    df_player <- find_elo(df_player, df[i, 'loser_id'], res_j[2], TRUE, 1)[[2]]
    df_player <- find_elo(df_player, df[i, 'winner_id'], m_i+1, TRUE, 2)[[2]]
    df_player <- find_elo(df_player, df[i, 'loser_id'], m_j+1, TRUE, 2)[[2]]
    
    # Assign the elo ratings in the match dataframe
    df[i, 'Elo_winner'] <- res_i[1]
    df[i, 'Elo_loser'] <- res_j[2]
    df[i, 'pi_i_j'] <- res_i[3]
    
    # These columns are recorded as match num after the current match
    df[i, 'Elo_winner_match_num'] <- m_i+1
    df[i, 'Elo_loser_match_num'] <- m_j+1
    
    if (val_top_num != FALSE){
      Elo_top_num <- (df_player$elo_rating %>% sort(decreasing = TRUE))[[val_top_num]]
      if (res_i[1] >= Elo_top_num){
        df[i, 'val_top_num_W'] <- TRUE
      }
      
      if (res_j[2] >= Elo_top_num){
        df[i, 'val_top_num_L'] <- TRUE
      }
    }
  }
  
  df %<>%
    mutate(elo_higher_rank_won = ifelse(Elo_winner_before >= Elo_loser_before, TRUE, FALSE)) %>% 
    mutate(prob_higher_win = ifelse(elo_higher_rank_won == TRUE, pi_i_j, 1-pi_i_j))
  return (list(df, df_player))
}

# Evaluate functions
predict_evaluate_elo <- function(data){
  # accuracy with 0.5 threshold
  threshold <- 0.5
  probs_val <- data$prob_higher_win
  predicted_class <- ifelse(probs_val >= threshold, 1, 0)
  acc <- mean(predicted_class == data$elo_higher_rank_won)
  
  # calibration
  W_value <- data$elo_higher_rank_won %>% max()
  W <- count(data, elo_higher_rank_won) %>% 
    filter(elo_higher_rank_won == W_value) %>% 
    select(n) %>% 
    pull(n)
  calibration <- sum(probs_val) / W
  
  # log-loss (cross entropy)
  df <- data.frame(
    prob = probs_val,
    elo_higher_rank_won = data$elo_higher_rank_won
  ) %>% 
    mutate(prob = ifelse(elo_higher_rank_won == W_value, prob, 1 - prob)) %>% 
    mutate(prob = log(prob))
  
  log_loss <- -sum(df$prob)/nrow(df)
  return (c(acc, calibration, log_loss))
}
metrics_table_elo <- function(train, val){
  metric_train <- predict_evaluate_elo(train)
  metric_val <- predict_evaluate_elo(val)
  table <- data.frame(
    train = metric_train,
    val = metric_val
  )
  rownames(table) <- c("accuracy", "calibration", "log-loss")
  return (table)
}

# Export data
df_init_total <- export_df(2001, 2023)

# Perform data cleaning
df_cleaned_total <- data_clean_elo(df_init_total)
split_68_18_19_res <- split_train_test(df_cleaned_total, dmy("01-01-2023"))
df_cleaned_train <- split_68_18_19_res[[1]]
df_cleaned_val <- split_68_18_19_res[[2]]
df_cleaned_val %<>% 
  filter(tourney_date < dmy("01-01-2024"))

# Extract players
df_players <- extract_players(df_cleaned_train)

# Tuning
train_K_Elo_return <- function(k){
  # cat("Prediction using K =", k, "\n")
  res_k_1_train <- add_elo_k(df_cleaned_train, df_players, k)
  df_k_1_train <- res_k_1_train[[1]]
  df_players_k_1_train <- res_k_1_train[[2]]
  
  res_k_1_val <- create_val_set(df_cleaned_val, df_players_k_1_train, k)
  df_k_1_val <- res_k_1_val[[1]]
  df_players_k_1_val <- res_k_1_val[[2]]
  metric_1 <- metrics_table_elo(df_k_1_train, df_k_1_val)
  
  # cat("include c case \n")
  # include c
  res_k_1c_train <- add_elo_k(df_cleaned_train, df_players, k, 1.1)
  df_k_1c_train <- res_k_1c_train[[1]]
  df_players_k_1c_train <- res_k_1c_train[[2]]
  
  res_k_1c_val <- create_val_set(df_cleaned_val, df_players_k_1c_train, k, FALSE, 1.1)
  df_k_1c_val <- res_k_1c_val[[1]]
  df_players_k_1c_val <- res_k_1c_val[[2]]
  metric_2 <- metrics_table_elo(df_k_1c_train, df_k_1c_val)
  # cat("\n")
  return (c(metric_1, metric_2))
}
create_df_plot <- function(all_k){
  df <- data.frame()
  for (each_k in all_k) {
    each_k_res <- train_K_Elo_return(each_k)
    df %<>% rbind(data.frame(k = each_k,
                             data_type = "train",
                             include_c = FALSE,
                             accuracy = each_k_res[1]$train[1],
                             calibration = each_k_res[1]$train[2],
                             `logloss` = each_k_res[1]$train[3]))
    
    df %<>% rbind(data.frame(k = each_k,
                             data_type = "val",
                             include_c = FALSE,
                             accuracy = each_k_res[2]$val[1],
                             calibration = each_k_res[2]$val[2],
                             `logloss` = each_k_res[2]$val[3]))
    
    df %<>% rbind(data.frame(k = each_k,
                             data_type = "train",
                             include_c = TRUE,
                             accuracy = each_k_res[3]$train[1],
                             calibration = each_k_res[3]$train[2],
                             `logloss` = each_k_res[3]$train[3]))
    
    df %<>% rbind(data.frame(k = each_k,
                             data_type = "val",
                             include_c = TRUE,
                             accuracy = each_k_res[4]$val[1],
                             calibration = each_k_res[4]$val[2],
                             `logloss` = each_k_res[4]$val[3]))
  }
  return (df)
}
all_k <- c(20) # previous obtained K-factor
df_plot_elo_k <- create_df_plot(all_k)

# Show Curve
plot_metrics <- function(df, show_include_c){
  if (show_include_c) {
    df_val <- df %>% filter(include_c == TRUE)
  } else {
    df_val <- df %>% filter(include_c == FALSE)
  }
  
  p_accuracy <- ggplot(df_val, aes(x = as.factor(k), 
                                   y = accuracy, 
                                   group = data_type, 
                                   color = as.factor(data_type))) +
    geom_line() +
    geom_point() +
    labs(title = "Accuracy against K factors", x = "K", y = "Accuracy", color = "Label") +
    theme_minimal() +
    theme(legend.position = "right")
  
  p_calibration <- ggplot(df_val, aes(x = as.factor(k), 
                                      y = calibration, 
                                      group = data_type, 
                                      color = as.factor(data_type))) +
    geom_line() +
    geom_point() +
    labs(title = "Calibration against K factors", x = "K", y = "Calibration", color = "Label") +
    theme_minimal() +
    theme(legend.position = "right")
  
  p_logloss <- ggplot(df_val, aes(x = as.factor(k), 
                                  y = logloss, 
                                  group = data_type, 
                                  color = as.factor(data_type))) +
    geom_line() +
    geom_point() +
    labs(title = "Log-loss against K factors", x = "K", y = "Log-loss", color = "Label") +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(p_accuracy)
  print(p_calibration)
  print(p_logloss)
}
plot_metrics(df_plot_elo_k, FALSE)

# Elo_538 -------------------------------------------------------
train_Elo_538_return_covid <- function(delta, nu, sigma, include_c){
  # cat("Prediction using K =", k, "\n")
  if (include_c == FALSE){
    res_k_2_train <- add_elo_k_538(df_cleaned_train, df_players, delta, nu, sigma)
    df_k_2_train <- res_k_2_train[[1]]
    df_players_k_2_train <- res_k_2_train[[2]]
    
    res_k_2_val <- create_val_set(df_cleaned_val, df_players_k_2_train, c(delta, nu, sigma), TRUE)
    df_k_2_val <- res_k_2_val[[1]]
    df_players_k_2_val <- res_k_2_val[[2]]
    metric_1 <- metrics_table_elo(df_k_2_train, df_k_2_val)
    return (metric_1)
  }else if (include_c == TRUE){
    # cat("include c case \n")
    # include c
    res_k_2c_train <- add_elo_k_538(df_cleaned_train, df_players, delta, nu, sigma, 1.1)
    df_k_2c_train <- res_k_2c_train[[1]]
    df_players_k_2c_train <- res_k_2c_train[[2]]
    
    res_k_2c_val <- create_val_set(df_cleaned_val, df_players_k_2c_train, c(delta, nu, sigma), TRUE, 1.1)
    df_k_2c_val <- res_k_2c_val[[1]]
    df_players_k_2c_val <- res_k_2c_val[[2]]
    metric_2 <- metrics_table_elo(df_k_2c_train, df_k_2c_val)
    # cat("\n")
    return (metric_2)
  }
  return (c(metric_1, metric_2))
}
create_df_plot_covid <- function(all_hyper, include_c){
  df <- data.frame()
  for (each_hyper in all_hyper) {
    delta <- each_hyper$delta
    nu <- each_hyper$nu
    sigma <- each_hyper$sigma
    each_k_res <- train_Elo_538_return_covid(delta, nu, sigma, include_c)
    
    df %<>% rbind(data.frame(delta = delta,
                             nu = nu,
                             sigma = sigma,
                             data_type = "train",
                             include_c = include_c,
                             accuracy = each_k_res[1]$train[1],
                             calibration = each_k_res[1]$train[2],
                             `logloss` = each_k_res[1]$train[3]))
    
    df %<>% rbind(data.frame(delta = delta,
                             nu = nu,
                             sigma = sigma,
                             data_type = "val",
                             include_c = include_c,
                             accuracy = each_k_res[2]$val[1],
                             calibration = each_k_res[2]$val[2],
                             `logloss` = each_k_res[2]$val[3]))
  }
  return (df)
}
create_all_hyper <- function(all_d, all_n, all_s){
  all <- list() # Initialize the list to store combinations
  all_index <- 1
  for (each_n in all_n) {
    for (each_s in all_s) {
      # For each combination, create a list and add it to the combinations list
      all[[all_index]] <- list(delta = all_d, nu = each_n, sigma = each_s)
      all_index <- all_index + 1
    }
  }
  
  return(all)
}

# Exclude
# 100, 5, 0.1
all_delta <- 100
all_nu <- seq(from = 50, to = 50, by = 1)
all_sigma <- seq(from = 0.175, to = 0.175, by = 1)
all_hyper <- create_all_hyper(all_delta, all_nu, all_sigma)
df_plot_elo_538_covid <- create_df_plot_covid(all_hyper, FALSE)
# 100 50 0.175
# 0.6374399    1.086598 0.6409657


# Show Manifold
df_plot_elo_538_covid_exclude_C <- df_plot_elo_538_covid %>% filter(data_type == "val")

fig <- plot_ly(df_plot_elo_538_covid_exclude_C, 
               x = ~df_plot_elo_538_covid_exclude_C$sigma, 
               y = ~df_plot_elo_538_covid_exclude_C$nu, 
               z = ~df_plot_elo_538_covid_exclude_C$logloss, 
               type = 'scatter3d', mode = 'markers') %>%
  layout(
    scene = list(
      xaxis = list(title = 'Sigma'),
      yaxis = list(title = 'Nu'),
      zaxis = list(title = 'Log-loss')
    )
  )
fig











# BCM -------------------------------------------------------
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
data_clean <- function(df){
  # sort by Date
  df %<>%
    arrange(Date)
  
  df %<>% 
    filter(Comment %in% c("Completed")) %>%
    select(-c(
      "ATP", "Location", "Series",
      "Court", "Round", "Best of",
      "W1", "L1", "W2", "L2", "W3", "L3", 
      "W4", "L4", "W5", "L5", "Wsets", "Lsets",
      "Comment")) %>%
    mutate_at(
      c("Tournament", "Surface"),
      as.factor) %>% 
    rename(BWW = "B&WW",
           BWL = "B&WL")
  
  df %<>% 
    mutate(across(c("CBW", "CBL", "GBW", "GBL", "IWW",
                    "IWL", "SBW", "SBL" , "B365W","B365L", 
                    "BWW", "BWL", "EXW", "EXL", "PSW", 
                    "PSL", "WPts", "LPts", "UBW", "UBL", 
                    "LBW", "LBL", "SJW", "SJL", "MaxW", 
                    "MaxL", "AvgW", "AvgL"), 
                  ~ifelse(is.na(.), 0, .)))
  
  ## Initialize Elo for winner and loser in whole dataframe
  df %<>% 
    mutate(CB_sum = CBW + CBL,
           GB_sum = GBW + GBL,
           IW_sum = IWW + IWL,
           SB_sum = SBW + SBL,
           B365_sum = B365W + B365L,
           BW_sum = BWW + BWL,
           EX_sum = EXW + EXL,
           PS_sum = PSW + PSL,
           UB_sum = UBW + UBL,
           LB_sum = LBW + LBL,
           SJ_sum = SJW + SJL) %>% 
    mutate(num_company = rowSums(select(., CB_sum, GB_sum, IW_sum, SB_sum,
                                        B365_sum, BW_sum, EX_sum, PS_sum,
                                        UB_sum, LB_sum, SJ_sum) != 0))
  # %>% 
  #   mutate(logit_prob_W = sum(CBL/CB_sum, GBL/GB_sum, IWL/IW_sum, SBL/SB_sum,
  #                             B365L/B365_sum, BWL/BW_sum, EXL/EX_sum, PSL/PS_sum,
  #                             UBL/UB_sum, LBL/LB_sum, SJL/SJ_sum, na.rm = TRUE)/num_company,
  #          logit_prob_L = sum(CBW/CB_sum, GBW/GB_sum, IWW/IW_sum, SBW/SB_sum,
  #                             B365W/B365_sum, BWW/BW_sum, EXW/EX_sum, PSW/PS_sum,
  #                             UBW/UB_sum, LBW/LB_sum, SJW/SJ_sum, na.rm = TRUE)/num_company)
  
  numerator_cols_W <- c("CBL", "GBL", "IWL", "SBL", "B365L", "BWL", 
                        "EXL", "PSL", "UBL", "LBL", "SJL")
  numerator_cols_L <- c("CBW", "GBW", "IWW", "SBW", "B365W", "BWW", 
                        "EXW", "PSW", "UBW", "LBW", "SJW")
  denominator_cols <- c("CB_sum", "GB_sum", "IW_sum", "SB_sum", "B365_sum", "BW_sum", 
                        "EX_sum", "PS_sum", "UB_sum", "LB_sum", "SJ_sum")
  
  # calculate winning probability for winner and loser 
  df$y_W <- rowSums(df[numerator_cols_W] / df[denominator_cols], na.rm = TRUE) / df$num_company
  df$y_L <- rowSums(df[numerator_cols_L] / df[denominator_cols], na.rm = TRUE) / df$num_company
  
  df %<>% 
    mutate(win_prob_W = exp(y_W)/(1+exp(y_W)),
           win_prob_L = exp(y_L)/(1+exp(y_L)))
  
  return (df)
}
split_df <- function(df, from, to){
  from <- paste("01-01-", from, sep = "")
  to <- paste("31-12-", to, sep = "")
  df %<>% 
    filter(Date >= dmy(from) & Date <= dmy(to))
  return(df)
}

# Export data
df_init_total <- export_df(2001, 2023)

# # Check Comment column
# table(df_init_total$Comment)

# Perform data cleaning
df_cleaned_total <- data_clean(df_init_total)

df_cleaned_split <- df_cleaned_total
# df_cleaned_split <- split_df(df_cleaned_total, 2014, 2023)
# df_cleaned_split <- split_df(df_cleaned_total, 2001, 2010)
# df_cleaned_split <- split_df(df_cleaned_total, 2001, 2018)
df_cleaned_split <- split_df(df_cleaned_total, 2023, 2023)

# Check how many observations which do not record odds (985/61275)
df_cleaned_split_2 <- df_cleaned_split %>%
  filter(num_company != 0) %>%
  mutate(higher_prob_won = ifelse(win_prob_W >= win_prob_L, 1, 0)) %>% 
  mutate(higher_prob = ifelse(win_prob_W >= win_prob_L, win_prob_W, win_prob_L)) %>% 
  mutate(lower_prob = ifelse(win_prob_W >= win_prob_L, win_prob_L, win_prob_W))

# Evaluate functions
predict_evaluate_betting <- function(df){
  # Define a function to calculate three metrics
  probs_val <- df$higher_prob
  
  # calibration
  W_value <- df$higher_prob_won %>% max()
  W <- count(df, higher_prob_won) %>% 
    filter(higher_prob_won == W_value) %>% 
    select(n) %>% 
    pull(n)
  calibration <- sum(probs_val) / W
  
  acc <- W / nrow(df)
  
  # log-loss (cross entropy)
  df <- data.frame(
    prob = probs_val,
    higher_prob_won = df$higher_prob_won
  ) %>% 
    mutate(prob = ifelse(higher_prob_won == W_value, prob, 1 - prob)) %>% 
    mutate(prob = log(prob))
  
  log_loss <- -sum(df$prob)/nrow(df)
  return (c(acc, calibration, log_loss))
}
predict_evaluate_betting(df_cleaned_split_2)
# 0.6775182 0.9785762 0.6160999 - 2023

