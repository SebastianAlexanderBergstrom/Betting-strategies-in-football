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

allsvenskan_data <- read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/Betting-strategies-in-football/master/SWE.csv"),
                             stringsAsFactors = FALSE)

# Creating data frames and vectors ----------------------------------------
op_ex_swe <- c("PH","PD","PA")
non_odds_vec <- c("Date","Home","Away","Res")
allsvenskan_data_stripped <- selection_from_data_frame(allsvenskan_data,op_ex_swe,non_odds_vec)

anyNA(allsvenskan_data_stripped)
# Identify the missing values, shows there are two matches in 2014. THe results are there but the odds are not.
which(is.na(allsvenskan_data_stripped), arr.ind=TRUE)
# http://www.oddsportal.com/soccer/sweden/allsvenskan-2014/results/page/5/
# Use this for the moment to fill in the missing values.
allsvenskan_data_stripped[705,]$PH <- 11.02
allsvenskan_data_stripped[705,]$PD <- 6.12
allsvenskan_data_stripped[705,]$PA <- 1.23

allsvenskan_data_stripped[714,]$PH <- 1.12
allsvenskan_data_stripped[714,]$PD <- 8.55
allsvenskan_data_stripped[714,]$PA <- 17.72

rounds_vec <- c(30,30,30,30,30,30,30)
teams_vec <- c(16,16,16,16,16,16,16)
number_of_play_offs_per_season <- c(2,2,2,2,2,2,2)
start_vec <- c("2018-04-01","2017-04-01","2016-04-02",
               "2015-04-04","2014-03-30","2013-03-31",
               "2012-03-31")
# The ending date of 2018 isn't available so you use some date
end_vec <- c("2018-12-24","2017-11-19","2016-11-20",
             "2015-11-08","2014-11-09","2013-11-10",
             "2012-11-17")

allsvenskan_format <- data_in_simulation_format(allsvenskan_data_stripped,
                                                rowMeans,
                                                non_odds_vec,
                                                rounds_vec,
                                                teams_vec,
                                                number_of_play_offs_per_season,
                                                start_vec,
                                                end_vec)
# Running and visualizing simulations -------------------------------------
rounds_to_simulate <- c(1:32)
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
season_vector_ex <- c("2014","2015","2016","2017")

returns_output <- "Returns"
balance_output <- "Balance"

compare_strategy_different_seasons(allsvenskan_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   favorites_strategy,
                                   lower_vec,
                                   upper_vec4)

compare_strategy_different_seasons(allsvenskan_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   draw_strategy,
                                   lower_vec,
                                   upper_vec2)

compare_strategy_different_seasons(allsvenskan_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   draw_strategy,
                                   lower_vec,
                                   upper_vec2,
                                   "Returns")


compare_strategy_different_seasons(allsvenskan_format,
                                   season_vector_ex,
                                   1000,
                                   rounds_to_simulate,
                                   fraction_vector_ex,
                                   underdog_strategy,
                                   lower_vec,
                                   upper_vec2)

# compare_returns_strategies_seasons(allsvenskan_format,
#                            c("2015","2016","2017"),
#                            1000,
#                            list(rounds_to_simulate,rounds_to_simulate,rounds_to_simulate),
#                            list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
#                            list(favorites_strategy,draw_strategy,underdog_strategy),
#                            list(lower_vec,lower_vec,lower_vec),
#                            list(upper_vec1,upper_vec2,upper_vec1))

compare_strategies_seasons(allsvenskan_format,
                           c("2015","2016","2017"),
                           1000,
                           list(rounds_to_simulate,rounds_to_simulate,rounds_to_simulate),
                           list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                           list(favorites_strategy,draw_strategy,underdog_strategy),
                           list(lower_vec,lower_vec,lower_vec),
                           list(upper_vec1,upper_vec2,upper_vec1))

compare_strategies_seasons(allsvenskan_format,
                           c("2015","2016","2017"),
                           1000,
                           list(rounds_to_simulate,rounds_to_simulate,rounds_to_simulate),
                           list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                           list(favorites_strategy,draw_strategy,underdog_strategy),
                           list(lower_vec,lower_vec,lower_vec),
                           list(upper_vec1,upper_vec2,upper_vec3))

compare_strategies_seasons(allsvenskan_format,
                           c("2015","2016","2017"),
                           1000,
                           list(rounds_to_simulate,rounds_to_simulate,rounds_to_simulate),
                           list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                           list(favorites_strategy,draw_strategy,underdog_strategy),
                           list(lower_vec,lower_vec,lower_vec),
                           list(upper_vec1,upper_vec2,upper_vec1),
                           returns_output)

compare_strategies_seasons(allsvenskan_format,
                           c("2015","2016","2017"),
                           1000,
                           list(rounds_to_simulate,rounds_to_simulate,rounds_to_simulate),
                           list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                           list(favorites_strategy,draw_strategy,underdog_strategy),
                           list(lower_vec,lower_vec,lower_vec),
                           list(upper_vec1,upper_vec2,upper_vec1),
                           balance_output)

compare_strategies_within_season(allsvenskan_format,
                                 "2017",
                                 1000,
                                 rounds_to_simulate,
                                 list(fraction_vector_ex,fraction_vector_ex,fraction_vector_ex),
                                 list(favorites_strategy,draw_strategy,underdog_strategy),
                                 list(lower_vec,lower_vec,lower_vec),
                                 list(upper_vec1,upper_vec2,upper_vec3))
