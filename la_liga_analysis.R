# Loading functions and data ----------------------------------------------
library(curl)

source("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/formatting_functions.R")
source("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/simulation_functions.R")
source("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/visualization_functions.R")

season20172018 <- read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/LaLiga2017-2018.csv"),
                           stringsAsFactors = FALSE)
season20162017 <- read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/LaLiga2016-2017.csv"),
                           stringsAsFactors = FALSE)
season20152016 <- read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/LaLiga2015-2016.csv"),
                           stringsAsFactors = FALSE)
season20142015 <- read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/LaLiga2014-2015.csv"),
                           stringsAsFactors = FALSE)
# It has more columns than the others so we'll remove these (assuming all others have the same columns)
columns_to_keep <- names(season20142015)[names(season20142015) %in% names(season20162017)]
season20142015 <- subset(season20142015,select=columns_to_keep)

# Check that they all have the same columns
print(names(season20142015) == names(season20152016))
print(names(season20152016) == names(season20162017))
print(names(season20162017) == names(season20172018))
# It seems like we're good to go and can combine everything into one single data frame

# Creating data frames and vectors ----------------------------------------
la_liga_table <- rbind(season20142015,
                       season20152016,
                       season20162017,
                       season20172018)

operator_ex <- c("B365H","B365D","B365A",
                 "BWH","BWD","BWA",
                 "IWH","IWD","IWA",
                 "LBH","LBD","LBA",
                 "PSH","PSD","PSA",
                 "WHH","WHD","WHA",
                 "VCH","VCD","VCA")
non_odds_ex <- c("Date","HomeTeam","AwayTeam","FTR")
non_odds_vec <- c("Date","Home","Away","Res")

la_liga_table_stripped <- selection_from_data_frame(la_liga_table,operator_ex,
                                                    non_odds_ex)
# Can check for missing values if you like

number_of_rounds_per_season <- c(38,38,38,38)
number_of_teams_per_season <- c(20,20,20,20)
number_of_play_offs_per_season <- c(0,0,0,0)
number_of_games_between_teams <- c(2,2,2,2)
start_dates <- c("2017-08-18","2016-08-19","2015-08-21","2014-08-23")
end_dates <- c("2018-05-20", "2017-05-21", "2016-05-15", "2015-05-23")
la_liga_format <- data_in_simulation_format(la_liga_table_stripped,
                                            rowMeans,
                                            non_odds_ex,
                                            number_of_rounds_per_season,
                                            number_of_teams_per_season,
                                            number_of_play_offs_per_season,
                                            start_dates,
                                            end_dates,
                                            number_of_games_between_teams)
# Running and visualizing simulations -------------------------------------
rounds_to_simulate <- c(1:38)
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

season_vector_ex <- c("2014/2015","2015/2016","2016/2017","2017/2018")
compare_strategy_different_seasons(la_liga_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   favorites_strategy,
                                   lower_vec,
                                   upper_vec4)

compare_strategy_different_seasons(la_liga_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   draw_strategy,
                                   lower_vec,
                                   upper_vec2)

compare_strategy_different_seasons(la_liga_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   draw_strategy,
                                   lower_vec,
                                   upper_vec2,
                                   "Returns")

compare_strategy_different_seasons(la_liga_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   underdog_strategy,
                                   lower_vec,
                                   upper_vec2)

compare_strategies_seasons(la_liga_format,
                           c("2016/2017","2014/2015","2017/2018"),
                           1000,
                           list(rounds_to_simulate,rounds_to_simulate,rounds_to_simulate),
                           list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                           list(favorites_strategy,draw_strategy,underdog_strategy),
                           list(lower_vec,lower_vec,lower_vec),
                           list(upper_vec1,upper_vec2,upper_vec1))

compare_strategies_seasons(la_liga_format,
                           c("2016/2017","2014/2015","2017/2018"),
                           1000,
                           list(rounds_to_simulate,rounds_to_simulate,rounds_to_simulate),
                           list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                           list(favorites_strategy,draw_strategy,underdog_strategy),
                           list(lower_vec,lower_vec,lower_vec),
                           list(upper_vec1,upper_vec2,upper_vec3))

compare_strategies_within_season(la_liga_format,
                                 "2017/2018",
                                 1000,
                                 rounds_to_simulate,
                                 list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                                 list(favorites_strategy,draw_strategy,underdog_strategy),
                                 list(lower_vec,lower_vec,lower_vec),
                                 list(upper_vec1,upper_vec2,upper_vec3))
