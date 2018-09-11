create_information_data_frame <- function(data_frame,
                                          rounds_simulated,
                                          initial_value_used,
                                          fraction_vector_used,
                                          strategy_used,
                                          lower_limit_used,
                                          upper_limit_used
                                          ){
  data_simulated <- data_frame[data_frame$Round %in% rounds_simulated,]
  balance_over_time <- simulate_returns_over_time(data_simulated,
                                                initial_value_used,
                                                fraction_vector_used,
                                                strategy_used,
                                                lower_limit_used,
                                                upper_limit_used
                                                )
  # This entails a bit of hard coding since you can only have one season in data_frame. This might be changed
  # at a later time.
  nr_returns <- length(balance_over_time)
  returns_vector <- balance_over_time[2:nr_returns]-balance_over_time[1:(nr_returns-1)]
  returns_vector <- c(0,returns_vector)
  money_spent <- balance_over_time*fraction_vector_used
  
  
  season <- rep(data_frame$Season[1],length(fraction_vector_used))
  new_data_frame <- data.frame(rounds_simulated,
                               fraction_vector_used,
                               strategy_used,
                               balance_over_time,
                               returns_vector,
                               money_spent,
                               season)
  colnames(new_data_frame) <- c("Round","Fraction spent","Strategy","Balance","Returns","Money spent","Season")
  return(new_data_frame)
} 

simulate_match_day_returns <- function(data_frame,lower_limit,upper_limit,saldo,strategy){
  strategy_name_string <- paste0(strategy,"_odds")
  odds_column <- data_frame[,strategy_name_string]
  interesting_matches <- data_frame[lower_limit <= odds_column & odds_column <= upper_limit,]
  colnames(interesting_matches)[colnames(interesting_matches)==strategy_name_string] <- "Odds"
  number_of_bets <- nrow(interesting_matches) 
  
  # Need to break in case there are no interesting matches to bet on that day
  if(number_of_bets == 0){
    return(saldo)
  }
  
  binary_result_vector <- ifelse(interesting_matches$results == strategy,1,0)
  
  # Currently hard coded strategy that you distribute the money spent evently across all interesting matches,
  # this can be changed if one wants to.
  returns_vector <- binary_result_vector*(saldo/number_of_bets)*interesting_matches$Odds
  return(sum(returns_vector))
}

simulate_returns_over_time <- function(data_frame,
                                       initial_value,
                                       fraction_vector,
                                       strategy_vector,
                                       lower_limit_vector,
                                       upper_limit_vector,
                                       start_date = data_frame$Date[1],
                                       end_date=data_frame$Date[nrow(data_frame)],
                                       stop_loss = 0){
  
  start_date_formatted <- as.Date(start_date, "%d-%m-%Y")
  end_date_formatted <- as.Date(end_date, "%d-%m-%Y")
  data_to_consider <- data_frame[start_date_formatted <= data_frame$Date & data_frame$Date <= end_date_formatted,]
  events_to_simulate <- unique(data_to_consider$Round)
  
  number_of_events_to_simulate <- length(events_to_simulate)
  # values <- c(numeric(number_of_events_to_simulate))
  # value <- initial_value
  balance_vector <- c(numeric(number_of_events_to_simulate))
  balance <- initial_value
  
  
  for(i in 1:number_of_events_to_simulate){
    matches_in_round <- data_to_consider[data_to_consider$Round==events_to_simulate[i],]
    money_to_spend <- fraction_vector[i]*balance
    updated_balance <- balance - money_to_spend
    
    returns_this_round <- simulate_match_day_returns(matches_in_round,
                                                     lower_limit_vector[i],
                                                     upper_limit_vector[i],
                                                     money_to_spend,
                                                     strategy_vector[i])
    balance <- updated_balance + returns_this_round
    balance_vector[i] <- balance
    if(balance <= stop_loss){
      #values <- values[1:i]
      balance_vector <- balance_vector[1:i]
      cat("Reached stop loss limit on", "", matches_in_round$Date[1])
      break
    }
  }
  return(balance_vector)
}