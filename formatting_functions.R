complete_year <- function(date_string){
  day_and_month <- format(as.Date(date_string, "%d/%m/%y"), "%d-%m-20%y")
  return(day_and_month)
}

format_match_data <- function(match_day_df){
  new_data_frame <- as.data.frame(matrix(0,nrow=nrow(match_day_df),ncol=9),stringsAsFactors=FALSE)
  colnames(new_data_frame) <- c("Date","favorite_odds","underdog_odds","draw_odds","favorite_name",
                                "underdog_name","results","Season","Round")
  new_data_frame$Date <- match_day_df$Date
  new_data_frame$Season <- match_day_df$Season
  new_data_frame$Round <- match_day_df$Round
  
  new_data_frame$favorite_name <- ifelse(match_day_df$home_odds > match_day_df$away_odds,
                                         as.character(match_day_df$Away),
                                         as.character(match_day_df$Home))
  new_data_frame$underdog_name <- ifelse(match_day_df$home_odds > match_day_df$away_odds,
                                         as.character(match_day_df$Home),
                                         as.character(match_day_df$Away))
  new_data_frame$favorite_odds <- ifelse(match_day_df$home_odds > match_day_df$away_odds,
                                         match_day_df$away_odds,
                                         match_day_df$home_odds)
  new_data_frame$underdog_odds <- ifelse(match_day_df$home_odds > match_day_df$away_odds,
                                         match_day_df$home_odds,
                                         match_day_df$away_odds)
  new_data_frame$draw_odds <- match_day_df$draw_odds
  
  convert_result_names <- function(result_name,home_name,away_name){
    if(result_name == "D"){
      return("draw")
    }
    ifelse(result_name=="H",
           as.character(home_name),
           as.character(away_name)
    )
  }
  
  results_temp <- mapply(convert_result_names,match_day_df$Res,match_day_df$Home,match_day_df$Away)
  
  convert_result_names_2 <- function(result,favorite_name,underdog_name){
    if(result == "draw"){
      return("draw")
    }
    ifelse(result == favorite_name,
           "favorite",
           "underdog")
  }
  new_data_frame$results <- mapply(convert_result_names_2,
                                   results_temp,
                                   new_data_frame$favorite_name,
                                   new_data_frame$underdog_name)
  
  return(new_data_frame)
}

selection_from_data_frame <- function(data_frame,operator_vector,non_odds){
  columns_to_keep <- c(non_odds,operator_vector)
  selected_parts_of_df <- subset(data_frame,select=c(columns_to_keep))
  return(selected_parts_of_df)
}

combination_and_computation_of_odds <- function(data_frame,combination_method,non_odds_vector){
  # "combination_method" can be e.g. rowMeans or rowMedians
  odds_names <- names(data_frame)[!names(data_frame) %in% non_odds_vector]
  
  home_odds_names <- odds_names[substr(odds_names,
                                       nchar(odds_names),
                                       nchar(odds_names))=="H"]
  draw_odds_names <- odds_names[substr(odds_names,
                                       nchar(odds_names),
                                       nchar(odds_names))=="D"]
  away_odds_names <- odds_names[substr(odds_names,
                                       nchar(odds_names),
                                       nchar(odds_names))=="A"]
  
  home_odds <- data_frame[,home_odds_names]
  draw_odds <- data_frame[,draw_odds_names]
  away_odds <- data_frame[,away_odds_names]
  
  if(is.vector(home_odds)){
    combined_home_odds <- round(home_odds,digits=2)
  }
  else{
    combined_home_odds <- round(combination_method(home_odds,na.rm=TRUE),digits=2)
  }
  if(is.vector(draw_odds)){
    combined_draw_odds <- round(draw_odds,digits=2)
  }
  else{
    combined_draw_odds <- round(combination_method(draw_odds,na.rm=TRUE),digits=2)
  }
  if(is.vector(away_odds)){
    combined_away_odds <- round(away_odds,digits=2)
  }
  else{
    combined_away_odds <- round(combination_method(away_odds,na.rm=TRUE),digits=2)
  }
  
  new_data_frame <- data.frame(data_frame[,non_odds_vector],combined_home_odds,combined_draw_odds,
                               combined_away_odds)
  return(new_data_frame)
}

format_dates <- function(data_frame){
  data_frame$Date <- sapply(data_frame$Date,complete_year)
  data_frame$Date <- as.Date(data_frame$Date,"%d-%m-%Y")
  return(data_frame)
}

season_from_dates <- function(start_date,end_date){
  start_year <- format(as.Date(start_date,format="%d-%m-%Y"),"%Y")
  end_year <- format(as.Date(end_date,format="%d-%m-%Y"),"%Y")
  
  season <- ifelse(start_year == end_year,
                   start_year,
                   paste0(start_year,"/",end_year))
  return(season)
}

format_season <- function(data_frame,starting_dates_vector,ending_dates_vector){
  n <- length(starting_dates_vector)
  data_frame$Season <- numeric(nrow(data_frame))
  
  starting_dates <- as.Date(starting_dates_vector)
  ending_dates <- as.Date(ending_dates_vector)
  for(i in 1:n){
    starting_date_of_season <- starting_dates[i]
    ending_date_of_season <- ending_dates[i]
    data_frame[data_frame$Date >=  starting_date_of_season & data_frame$Date <= ending_date_of_season,]$Season <- season_from_dates(starting_date_of_season,  
                                                                                                                                    ending_date_of_season)
  }
  return(data_frame)
}

format_rounds_in_season <- function(data_frame,
                                    matches_per_season_per_team,
                                    number_of_teams,
                                    number_of_play_off_rounds,
                                    number_of_games_between_teams=2){
  rounds_vector <- c(1:matches_per_season_per_team)
  number_of_matches_per_round <- number_of_teams/number_of_games_between_teams
  play_off_rounds <- c(numeric(number_of_play_off_rounds)) + matches_per_season_per_team + c(1:number_of_play_off_rounds)
  rounds_column <- c(sort(rep(rounds_vector,number_of_matches_per_round)),play_off_rounds)
  
  return(rounds_column)
}

format_rounds <- function(data_frame,
                          matches_per_season_per_team,
                          number_of_teams,
                          number_of_play_off_rounds,
                          number_of_games_between_teams=rep(2,length(number_of_teams))){
  # Not a very pretty solution but it works. Can look into making it tidier later.
  number_of_games_per_season <- (number_of_teams/2)*((number_of_teams-1)*number_of_games_between_teams) + number_of_play_off_rounds
  number_of_games_played <- sum(number_of_games_per_season) - nrow(data_frame)
  number_of_incomplete_seasons <- ceiling(number_of_games_played/nrow(data_frame))
  number_of_complete_seasons <- length(number_of_teams)-number_of_incomplete_seasons
  
  seasons <- unique(data_frame$Season)
  nr_seasons <- length(seasons)
  last_season <- seasons[nr_seasons]
  last_season_df <- data_frame[data_frame$Season == last_season,]
  nr_games_last_season <- nrow(last_season_df)
  idx_last_season <- length(number_of_teams)
  number_of_teams_this_season <- number_of_teams[idx_last_season]
  data_frame$Round <- c(numeric(nrow(data_frame)))
  
  if(number_of_complete_seasons != 0){
    for(i in 1:number_of_complete_seasons){
      season <- seasons[i]
      data_frame[data_frame$Season == season,]$Round <- format_rounds_in_season(data_frame[data_frame$Season == season,],
                                                                                matches_per_season_per_team[i],
                                                                                number_of_teams[i],
                                                                                number_of_play_off_rounds[i])
    }
  }
  # These three cases can probably be done in a tidier.
  if(nr_games_last_season == number_of_games_per_season[idx_last_season]|
     nr_games_last_season == matches_per_season_per_team[idx_last_season]){
    # The booleans correspond to no rounds missing and only play-off rounds missing in the last season, respectively.
    data_frame[data_frame$Season == last_season,]$Round <- format_rounds_in_season(data_frame[data_frame$Season == last_season,],
                                                                                   matches_per_season_per_team[idx_last_season],
                                                                                   number_of_teams[idx_last_season],
                                                                                   number_of_play_off_rounds[idx_last_season])
  }
  # Some play off rounds missing
  if(nr_games_last_season > number_of_games_per_season[idx_last_season]){
    number_of_play_offs_played <- nr_games_last_season - matches_per_season_per_team[idx_last_season]
    data_frame[data_frame$Season == last_season,]$Round <- format_rounds_in_season(data_frame[data_frame$Season == last_season,],
                                                                                   matches_per_season_per_team[idx_last_season],
                                                                                   number_of_teams[idx_last_season],
                                                                                   number_of_play_offs_played)
  }
  # Some regular rounds missing.
  if(nr_games_last_season < number_of_games_per_season[idx_last_season]){
    number_of_complete_rounds <- floor(nr_games_last_season/(number_of_teams_this_season/2))
    games_in_incomplete_round <- nr_games_last_season-number_of_complete_rounds*(number_of_teams_this_season/2)
    
    complete_rounds <- sort(rep(c(1:number_of_complete_rounds),number_of_teams_this_season/2))
    incomplete_round <- rep(number_of_complete_rounds+1,games_in_incomplete_round)
    remaining_rounds <- c(complete_rounds,incomplete_round)
    data_frame[data_frame$Season == last_season,]$Round <- remaining_rounds
  }
  
  return(data_frame)
}

data_in_simulation_format <- function(data_frame,
                                      combination_method,
                                      non_odds_vector,
                                      rounds_vector,
                                      teams_vector,
                                      play_off_rounds_vector,
                                      starting_dates=data_frame$Date[1],
                                      ending_dates=data_frame$Date[nrow(data_frame)],
                                      games_between_teams_vector = rep(2,length(rounds_vector))){
  df_with_odds <- combination_and_computation_of_odds(data_frame,
                                                      combination_method,
                                                      non_odds_vector)
  df_with_dates <- format_dates(df_with_odds)
  starting_dates <- as.Date(starting_dates)
  ending_dates <- as.Date(ending_dates)
  df_with_season <- format_season(df_with_dates,starting_dates,ending_dates)
  df_with_rounds <- format_rounds(df_with_season,rounds_vector,teams_vector,
                                  play_off_rounds_vector,
                                  games_between_teams_vector)
  colnames(df_with_rounds) <- c("Date","Home","Away","Res",
                                "home_odds","draw_odds","away_odds","Season",
                                "Round")
  formatted_df <- format_match_data(df_with_rounds)
  return(formatted_df)
}