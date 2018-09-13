# For checking installed packages -----------------------------------------
# Taken from  https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("curl","ggplot2","easyGgplot2")
# Loading functions and data ----------------------------------------------
library(curl)

source("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/formatting_functions.R")
source("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/simulation_functions.R")
source("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/visualization_functions.R")
season20172018 <- read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/SegundaDivision2017-2018.csv"),
                           stringsAsFactors = FALSE)

# Creating data frames and vectors ----------------------------------------

operator_ex <- c("B365H","B365D","B365A",
                 "BWH","BWD","BWA",
                 "IWH","IWD","IWA",
                 "LBH","LBD","LBA",
                 "PSH","PSD","PSA",
                 "WHH","WHD","WHA",
                 "VCH","VCD","VCA")
non_odds_ex <- c("Date","HomeTeam","AwayTeam","FTR")
non_odds_vec <- c("Date","Home","Away","Res")

segunda_division_table_stripped <- selection_from_data_frame(season20172018,operator_ex,
                                                    non_odds_ex)
anyNA(segunda_division_table_stripped)
which(is.na(segunda_division_table_stripped), arr.ind=TRUE)

start_dates <- c("2017-08-17")
end_dates <- c("2018-06-02")
number_of_rounds_per_season <- c(42)
number_of_teams_per_season <- c(22)
number_of_play_offs_per_season <- c(6) 
number_of_games_between_teams <- c(2)
segunda_division_format <- data_in_simulation_format(segunda_division_table_stripped,
                                            rowMeans,
                                            non_odds_ex,
                                            number_of_rounds_per_season,
                                            number_of_teams_per_season,
                                            number_of_play_offs_per_season,
                                            start_dates,
                                            end_dates,
                                            number_of_games_between_teams)
# Running and visualizing simulations -------------------------------------
rounds_to_simulate <- c(1:42)
number_of_rounds_to_simulate <- length(rounds_to_simulate)
fraction_vector_ex <- rep(0.2,number_of_rounds_to_simulate)
favorites_strategy <- rep("favorite",number_of_rounds_to_simulate)
draw_strategy <- rep("draw",number_of_rounds_to_simulate)
underdog_strategy <- rep("underdog",number_of_rounds_to_simulate)
lower_vec <- rep(2,number_of_rounds_to_simulate)
lower_vec2 <- rep(1,number_of_rounds_to_simulate)
upper_vec1 <- rep(3,number_of_rounds_to_simulate)
upper_vec2 <- rep(10,number_of_rounds_to_simulate)
upper_vec3 <- rep(5,number_of_rounds_to_simulate)
upper_vec4 <- rep(2.4,number_of_rounds_to_simulate)
initial_value_ex <- 1000

season_vector_ex <- c("2017/2018")
compare_strategy_different_seasons(segunda_division_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   favorites_strategy,
                                   lower_vec,
                                   upper_vec4)

compare_strategy_different_seasons(segunda_division_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   draw_strategy,
                                   lower_vec,
                                   upper_vec2)
compare_strategy_different_seasons(segunda_division_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   draw_strategy,
                                   lower_vec,
                                   upper_vec2,
                                   "Returns")

compare_strategy_different_seasons(segunda_division_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   underdog_strategy,
                                   lower_vec,
                                   upper_vec2)

compare_strategies_seasons(segunda_division_format,
                           c("2017/2018"),
                           1000,
                           list(rounds_to_simulate,rounds_to_simulate,rounds_to_simulate),
                           list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                           list(favorites_strategy,draw_strategy,underdog_strategy),
                           list(lower_vec,lower_vec,lower_vec),
                           list(upper_vec1,upper_vec2,upper_vec1))

compare_strategies_seasons(segunda_division_format,
                           c("2017/2018"),
                           1000,
                           list(rounds_to_simulate,rounds_to_simulate,rounds_to_simulate),
                           list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                           list(favorites_strategy,draw_strategy,underdog_strategy),
                           list(lower_vec,lower_vec,lower_vec),
                           list(upper_vec1,upper_vec2,upper_vec3))

compare_strategies_within_season(segunda_division_format,
                                 "2017/2018",
                                 1000,
                                 rounds_to_simulate,
                                 list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                                 list(favorites_strategy,draw_strategy,underdog_strategy),
                                 list(lower_vec,lower_vec,lower_vec),
                                 list(upper_vec1,upper_vec2,upper_vec3))