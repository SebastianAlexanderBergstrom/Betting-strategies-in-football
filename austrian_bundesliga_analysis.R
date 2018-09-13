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

austrian_bundesliga_data <- read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/AUT.csv"),
                             stringsAsFactors = FALSE)

# Creating data frames and vectors -----------------------------------------
op_ex_aut <- c("PH","PD","PA")
non_odds_vec <- c("Date","Home","Away","Res")
austrian_bundesliga_data_stripped <- selection_from_data_frame(austrian_bundesliga_data,op_ex_aut,non_odds_vec)

anyNA(austrian_bundesliga_data_stripped)
which(is.na(austrian_bundesliga_data_stripped), arr.ind=TRUE)
# Some missing elements which you need to rectify.

austrian_bundesliga_data_stripped[329,]$PH <- 1.07
austrian_bundesliga_data_stripped[329,]$PD <- 9.86
austrian_bundesliga_data_stripped[329,]$PA <- 18.96

austrian_bundesliga_data_stripped[415,]$PH <- 1.06
austrian_bundesliga_data_stripped[415,]$PD <- 12.57
austrian_bundesliga_data_stripped[415,]$PA <- 23.12

austrian_bundesliga_data_stripped[519,]$PH <- 1.11
austrian_bundesliga_data_stripped[519,]$PD <- 9.04
austrian_bundesliga_data_stripped[519,]$PA <- 18.94

# You can't quite wrap your head around the new format of the league so you'll ignore
# the 2018/2019 season for now.
austrian_bundesliga_data_stripped <- austrian_bundesliga_data_stripped[1:1082,]
seasons <- c("2012/2013","2013/2014","2014/2015","2015/2016","2016/2017")

# Using these calls makes sure everyting up to the end of 2016/2017 works
number_of_rounds_per_season <- c(36,36,36,36,36,36)
number_of_teams_per_season <- c(10,10,10,10,10,10)
number_of_play_offs_per_season <- c(0,0,0,0,0,2)
number_of_games_between_teams <- c(4,4,4,4,4,4)
start_dates <- c("2017-07-22","2016-07-23",
                 "2015-07-25","2014-07-19","2013-07-20",
                 "2012-07-21")
# "2019-03-07" is the last round you can access from
# https://www.bundesliga.at/de/spielplan/
end_dates <- c("2018-06-03","2017-05-28",
               "2016-05-15","2015-05-31","2014-05-12",
               "2013-05-26")
austrian_bundesliga_format <- data_in_simulation_format(austrian_bundesliga_data_stripped,
                                            rowMeans,
                                            non_odds_vec,
                                            number_of_rounds_per_season,
                                            number_of_teams_per_season,
                                            number_of_play_offs_per_season,
                                            start_dates,
                                            end_dates,
                                            number_of_games_between_teams)

# Running and visualizing simulations -------------------------------------

rounds_to_simulate <- c(1:36)
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
compare_strategy_different_seasons(austrian_bundesliga_format,
                                   season_vector_ex,
                                   initial_value_ex,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   favorites_strategy,
                                   lower_vec,
                                   upper_vec4)

compare_strategy_different_seasons(austrian_bundesliga_format,
                                   season_vector_ex,
                                   initial_value_ex,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   draw_strategy,
                                   lower_vec,
                                   upper_vec2)

compare_strategy_different_seasons(austrian_bundesliga_format,
                                   season_vector_ex,
                                   initial_value_ex,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   underdog_strategy,
                                   lower_vec,
                                   upper_vec2)

compare_strategy_different_seasons(austrian_bundesliga_format,
                                   season_vector_ex,
                                   initial_value_ex,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   underdog_strategy,
                                   lower_vec,
                                   upper_vec2,
                                   "Returns")

compare_strategies_seasons(austrian_bundesliga_format,
                         c("2016/2017","2014/2015","2017/2018"),
                         initial_value_ex,
                         list(rounds_to_simulate,rounds_to_simulate,rounds_to_simulate),
                         list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                         list(favorites_strategy,draw_strategy,underdog_strategy),
                         list(lower_vec,lower_vec,lower_vec),
                         list(upper_vec1,upper_vec2,upper_vec1))

compare_strategies_seasons(austrian_bundesliga_format,
                         c("2016/2017","2014/2015","2017/2018"),
                         initial_value_ex,
                         list(rounds_to_simulate,rounds_to_simulate,rounds_to_simulate),
                         list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                         list(favorites_strategy,draw_strategy,underdog_strategy),
                         list(lower_vec,lower_vec,lower_vec),
                         list(upper_vec1,upper_vec2,upper_vec3))

compare_strategies_within_season(austrian_bundesliga_format,
                                 "2017/2018",
                                 initial_value_ex,
                                 rounds_to_simulate,
                                 list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                                 list(favorites_strategy,draw_strategy,underdog_strategy),
                                 list(lower_vec,lower_vec,lower_vec),
                                 list(upper_vec1,upper_vec2,upper_vec3))