library(ggplot2)
library(easyGgplot2)

compare_strategies_within_season <- function(data_frame,
                                            season,
                                            initial_value,
                                            rounds_vector,
                                            fraction_vector_list,
                                            strategy_vector_list,
                                            lower_limit_vector_list,
                                            upper_limit_vector_list,
                                            desired_output="Balance"){
  filtered_by_season <- data_frame[data_frame$Season == season,]
  
  nr_of_strategies <- length(strategy_vector_list)
  nr_columns <- length(names(data_frame))
  new_data_frame <- as.data.frame(matrix(0,nrow=0,ncol=nr_columns),stringsAsFactors=FALSE)
  colnames(new_data_frame) <- colnames(data_frame)

  for(i in 1:nr_of_strategies){
    strategy_vector <- as.character(unlist(strategy_vector_list[i]))
    fraction_vector <- as.numeric(unlist(fraction_vector_list[i]))
    lower_limit_vector <- as.numeric(unlist(lower_limit_vector_list[i]))
    upper_limit_vector <- as.numeric(unlist(upper_limit_vector_list[i]))
    season_information <- create_information_data_frame(filtered_by_season,
                                                        rounds_vector,
                                                        initial_value,
                                                        fraction_vector,
                                                        strategy_vector,
                                                        lower_limit_vector,
                                                        upper_limit_vector)
    new_data_frame <- rbind(new_data_frame,season_information)
  }
  if(desired_output == "Balance"){
    result_plot <- ggplot(data=new_data_frame,
                          aes(x=Round,y=Balance,fill=Strategy,color=Strategy))
    return(result_plot + geom_point() + geom_line() + 
             geom_hline(yintercept = initial_value,linetype="dashed",show.legend = TRUE)+ggtitle(paste0("Season ", season)))
  }
  if(desired_output == "Returns"){
    result_plot <-  ggplot(data=new_data_frame,
                           aes(x=Returns,fill=Strategy,color=Strategy))
    return(result_plot+geom_density(alpha=0.5) + ggtitle(paste0("Season ", season)))
  }
  
}

compare_strategies_seasons <- function(data_frame,
                                     season_vector,
                                     initial_value,
                                     rounds_vector_list,
                                     fraction_vector_list,
                                     strategy_vector_list,
                                     lower_limit_vector_list,
                                     upper_limit_vector_list,
                                     desired_output = "Balance"){
  number_of_seasons <- length(season_vector)
  plots <- list()
  for(i in 1:number_of_seasons){
    season <- season_vector[i]
    rounds_vector <- as.numeric(unlist(rounds_vector_list[i]))
    plots[[i]] <- compare_strategies_within_season(data_frame,
                                                 season,
                                                 initial_value,
                                                 rounds_vector,
                                                 fraction_vector_list,
                                                 strategy_vector_list,
                                                 lower_limit_vector_list,
                                                 upper_limit_vector_list,
                                                 desired_output)
  }
  ggplot2.multiplot(plotlist=plots,cols=ceiling(sqrt(number_of_seasons)))
}

compare_strategy_different_seasons <- function(data_frame,
                                             season_vector,
                                             initial_value,
                                             rounds_vector,
                                             fraction_vector,
                                             strategy_vector,
                                             lower_limit_vector,
                                             upper_limit_vector,
                                             desired_output="Balance"){
  filtered_by_season <- data_frame[data_frame$Season %in% season_vector,]
  seasons_of_interest <- unique(filtered_by_season$Season)
  nr_of_seasons <- length(seasons_of_interest)
  nr_columns <- length(names(data_frame))
  new_data_frame <- as.data.frame(matrix(0,nrow=0,ncol=nr_columns),stringsAsFactors=FALSE)
  colnames(new_data_frame) <- colnames(data_frame)
  for(i in 1:nr_of_seasons){
    season <- season_vector[i]
    season_df <- filtered_by_season[filtered_by_season$Season == season,]
    season_information <- create_information_data_frame(season_df,
                                                        rounds_vector,
                                                        initial_value,
                                                        fraction_vector,
                                                        strategy_vector,
                                                        lower_limit_vector,
                                                        upper_limit_vector)
    new_data_frame <- rbind(new_data_frame,season_information)
  }
  
  fraction_as_percentage <- fraction_vector[1]*100
  title <- paste0("Betting on ", strategy_vector[1],"s using ",
                  fraction_as_percentage,"% of the balance in each round with \n",
                  lower_limit_vector[1]," and ", upper_limit_vector[1],
                  " as lower and upper odds limits, respectively")
  
  if(desired_output == "Balance"){
    ggplot(data=new_data_frame,
           aes(x=Round,y=Balance,fill=Season,color=Season))+
      geom_point()+
      geom_line()+
      geom_hline(yintercept = initial_value,linetype="dashed",show.legend = TRUE)+
      ggtitle(title)
  }
  if(desired_output == "Returns"){
    ggplot(data=new_data_frame,
           aes(x=Returns,fill=Season,color=Season))+
      geom_density(alpha=0.5)+
      ggtitle(title)
  }
}