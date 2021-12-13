# load library/package
library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
library(tidyr)
library(vcd)
library(grid)
# set the working directory to use relative path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the raw data
tables_html = readLines("../Data/Tables.html", warn = FALSE)

# Seasons
seasons = tables_html[grep("<td class=\"dtr1 dtc", tables_html)][1:24]

# Fetch the information of the first 20 teams
for (i in 1 : 20){
  expression = sprintf('rank_%d = tables_html[grep("<td class=\\\"dtr%d dtc", tables_html)][1:24]', i, i+1)
  eval(parse(text = expression))
}

# preprocess data
# only keep the string with more than n characters
data_processing = function(x, n){
  temp = vector()
  for(i in 1:length(x)){
    if(nchar(x[i])>n){ #if there are more than n chars, those lines are either team names or pts that we want
      temp[i] = x[i]
    }
    temp = na.exclude(temp) # remove all NAs
  }
  pattern = "(.*)\">|</td>"
  temp = gsub(pattern, "", temp) # remove useless strings
  done = temp[1:length(temp)] # keep the information we want
  return(done)
}

# Preprocess data
  seasons_clear = data_processing(seasons, 29)
  # rank 1 - 8
  for (i in 1 : 8){
  expression = sprintf('rank_%d_clear = data_processing(rank_%d, %d)', i, i, 28)
    eval(parse(text = expression))
  }
  # rank 9 - 20
  for (i in 9 : 20){
    # remove the rank column
    expression = sprintf('rank_%d = rank_%d[-1]', i, i)
    eval(parse(text = expression))
    expression = sprintf('rank_%d_clear = data_processing(rank_%d, %d)', i, i, 29)
    eval(parse(text = expression))
  }
  
  
  # Check all vectors are in correct length so that we could put them into a data frame
  all.equal(
    length(seasons_clear),
    length(rank_1_clear),
    length(rank_2_clear),
    length(rank_3_clear),
    length(rank_4_clear),
    length(rank_5_clear),
    length(rank_6_clear),
    length(rank_7_clear),
    length(rank_8_clear),
    length(rank_9_clear),
    length(rank_10_clear),
    length(rank_11_clear),
    length(rank_12_clear),
    length(rank_13_clear),
    length(rank_14_clear),
    length(rank_15_clear),
    length(rank_16_clear),
    length(rank_17_clear),
    length(rank_18_clear),
    length(rank_19_clear),
    length(rank_20_clear))
  
  # Combine all previous info together into a data frame
  tables = rbind(seasons_clear, 
                 rank_1_clear, 
                 rank_2_clear,
                 rank_3_clear,
                 rank_4_clear,
                 rank_5_clear,
                 rank_6_clear,
                 rank_7_clear,
                 rank_8_clear,
                 rank_9_clear,
                 rank_10_clear,
                 rank_11_clear,
                 rank_12_clear,
                 rank_13_clear,
                 rank_14_clear,
                 rank_15_clear,
                 rank_16_clear,
                 rank_17_clear,
                 rank_18_clear,
                 rank_19_clear,
                 rank_20_clear)
  
  colnames(tables) = c("2013-14", "Pts", "2014-15", "Pts", "2015-16", "Pts", "2016-17", "Pts", "2017-18", "Pts",
                       "2018-19", "Pts", "2019-20", "Pts", "2020-21", "Pts")
  
  rownames(tables) = c("Season", 1:20)
  
  # Table for Seasons 2013-14 until 2020-21
  head(tables)
  
  
  # Read the raw data for matches
  result_2021_html = readLines("../Data/2020_21.html", warn = FALSE)
  result_1920_html = readLines("../Data/2019_20.html", warn = FALSE)
  result_1819_html = readLines("../Data/2018_19.html", warn = FALSE)
  result_1718_html = readLines("../Data/2017_18.html", warn = FALSE)
  result_1617_html = readLines("../Data/2016_17.html", warn = FALSE)
  
  teams_processing = function(x){
    temp = x[grep("                        <td>[A-Z]|                        <td>bet", x)]
    pattern = "(.*)<td>|</td>"
    temp_2 = gsub(pattern, "", temp) # remove useless strings
    done = vector()
    for(i in 1:length(temp_2)){
      if((i %% 3)!=1){
        done[i] = temp_2[i]
      }
    }
    done = na.exclude(done) # remove all NAs
    done = done[1:760]
    return(done)
  }
  
  goals_processing = function(x){
    temp = x[grep("                                                    <td>\\d+", x)]
    pattern = "(.*)<td>|</td>"
    temp_2 = gsub(pattern, "", temp) # remove useless strings
    home = vector()
    away = vector()
    result = vector()
    for(i in 1:length(temp_2)){
      home[i] = substr(temp_2[i], 1, 1)
      away[i] = substr(temp_2[1], 5, 5)
      result[2*i-1] = home[i]
      result[2*i] = away[i]
    }
    done = na.exclude(result) # remove all NAs
    return(done)
  }
  
  matches = cbind(teams_processing(result_1617_html), goals_processing(result_1617_html),
                  teams_processing(result_1718_html), goals_processing(result_1718_html),
                  teams_processing(result_1819_html), goals_processing(result_1819_html),
                  teams_processing(result_1920_html), goals_processing(result_1920_html),
                  teams_processing(result_2021_html), goals_processing(result_2021_html))
  
  colnames(matches) = c("2016-17", "Goal(s)", "2017-18", "Goal(s)", "2018-19", "Goal(s)",
                        "2019-20", "Goal(s)", "2020-21", "Goal(s)")
  
  rownames(matches) = rep(c("Home", "Away"), 380)
  
  # All match results for Seasons 2016-17 until 2020-21
  head(matches)
  
  
  #begin of plots for data mining----------------------------------------------
  # plot the rankings of each team in the last seasons
  allSeasons =  c("2013-14", "2014-15", "2015-16", "2016-17", "2017-18", 
               "2018-19", "2019-20",  "2020-21")
  nSeasons = 8 # how many seasons do you want to see?
  
  # This function is used to find the teams appeared in ranking list in the all seasons
  find_same = function(x){
    n = ncol(x) - 1
    temp = x[,1]
    for (i in 1:n) {
      temp = temp[is.element(temp, x[, i+1])]
    }
    return(temp)
  }
  table_team = tables[-1, ]
  table_team = tables[, seq(ncol(table_team) - (nSeasons - 1) * 2 - 1, ncol(table_team), by = 2)]
  promising_team = find_same(table_team)
  # Find the rank and points of each promising team in these seasons
  table_team = tables[-1, ]
  table_team = tables[, seq(ncol(table_team) - (nSeasons - 1) * 2 - 1, ncol(table_team))]
  get_rank_and_point = function(x){
    ranks = matrix(nrow = length(x), ncol = nSeasons)
    points = matrix(nrow = length(x), ncol = nSeasons)
    for (i in 1:length(x)) {
      for (j in 1:nSeasons) {
        if (is.element( x[i], table_team[, 2*j - 1])){
          ranks[i, j] = which(table_team[, 2*j - 1] == x[i])
          points[i, j] = strtoi(table_team[ranks[i, j], 2*j])
        }
        else{
          ranks[i, j] = NA
          points[i, j] = NA
        }
        
      }
    }
    result = list(ranks, points)
    return(result)
  }
  
  result =  get_rank_and_point(promising_team)
  rank_of_proming_team = result[[1]]
  point_of_proming_team = result[[2]]
  # used to verify the data
  rownames(rank_of_proming_team) = c(promising_team)
  colnames(rank_of_proming_team) = tail(allSeasons, nSeasons)
  rownames(point_of_proming_team) = c(promising_team)
  colnames(point_of_proming_team) = tail(allSeasons, nSeasons)
  
  # create data frame for plot
  seasonsPlot = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
  rank_of_proming_team_df = data.frame(
                            Seasons = tail(seasonsPlot, nSeasons)
                            )
  for (i in 1:length(promising_team)) {
    rank_of_proming_team_df[, promising_team[i]] = rank_of_proming_team[i,]
  }
  
  rank_of_proming_team_df = melt(rank_of_proming_team_df ,  id.vars = 'Seasons', variable.name = 'Teams')
  
  point_of_proming_team_df = data.frame(
    Seasons = tail(seasonsPlot, nSeasons)
  )
  for (i in 1:length(promising_team)) {
    point_of_proming_team_df[, promising_team[i]] = point_of_proming_team[i,]
  }
  
  point_of_proming_team_df = melt(point_of_proming_team_df ,  id.vars = 'Seasons', variable.name = 'Teams')
  
  # plot the change of rank 
  (plt =  ggplot(rank_of_proming_team_df, aes(Seasons,value)) + 
          geom_line(aes(colour = Teams)) + 
          scale_y_continuous(breaks= pretty_breaks(), trans = "reverse") +
          scale_x_continuous(breaks = tail(seasonsPlot, nSeasons), label = tail(allSeasons, nSeasons)) + 
          ylab("Rank") +
          ggtitle("Every team's rank") +
          theme(plot.title = element_text(hjust = 0.5))
  )
  # plot the change of points 
  (plt =  ggplot(point_of_proming_team_df, aes(Seasons,value)) + 
      geom_line(aes(colour = Teams)) + 
      scale_y_continuous(breaks= pretty_breaks()) +
      scale_x_continuous(breaks = tail(seasonsPlot, nSeasons), label = tail(allSeasons, nSeasons)) + 
      ylab("Rank") +
      ggtitle("Every team's points") +
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  # plot the error bar of rank 
  #some teams' names are too long
  promising_team_names = lapply(promising_team, FUN = function(x){
    if (x == "MANCHESTER CITY"){
      return("MAN CITY")
    }
    if(x == "MANCHESTER UNITED"){
      return("MAN UNITED")
    }
    substr(x, 1,3)
  })
  rank_error_bar_df = data.frame(
    Teams = unlist(promising_team_names),
    RankMean = rowMeans(rank_of_proming_team),
    RankSD = apply(rank_of_proming_team, 1, sd, na.rm = TRUE)
  )
  
 (plt = ggplot(rank_error_bar_df, aes(x = Teams, y = RankMean, ymin = RankMean-RankSD, ymax =RankMean+RankSD)) +
        geom_pointrange() +
        geom_errorbar(width = 0.2) +
        geom_point(size = 1.5) +
        scale_y_continuous(breaks= 1:20, trans = "reverse") +
        ylab("Rank")
  )
  
  
  # plot the error bar of point 
  point_error_bar_df = data.frame(
    Teams = unlist(promising_team_names),
    PointMean = rowMeans(point_of_proming_team),
    PointSD = apply(point_of_proming_team, 1, sd, na.rm = TRUE)
  )
  
  (plt = ggplot(point_error_bar_df, aes(x = Teams, y = PointMean, ymin = PointMean-PointSD, ymax =PointMean+PointSD)) +
      geom_pointrange() +
      geom_errorbar(width = 0.2) +
      geom_point(size = 1.5) +
      scale_y_continuous(breaks= pretty_breaks()) +
      ylab("Point")
  )
  # end of plots for data mining--------------------------------------------------------------------
  
  # FTHG = Full Time Home Team Goals
  # FTAG = Full Time Away Team Goals
  # FTR = Full Time Result
  results_1617 <- data.frame(matrix(NA, nrow = 380, ncol = 5))
  for(i in 1:760){
    if((i %% 2)!=0){
      results_1617[(i%/%2)+1,1] = matches[i,1]
      results_1617[(i%/%2)+1,3] = matches[i,2]
    } else {
      results_1617[i/2,2] = matches[i,1]
      results_1617[i/2,4] = matches[i,2]
      
      if(results_1617[i/2,3]>results_1617[i/2,4]){
        results_1617[i/2,5] = "H"
      } else if(results_1617[i/2,3]<results_1617[i/2,4]){
        results_1617[i/2,5] = "A"
      } else{
        results_1617[i/2,5] = "D"
      }
    }
  }
  colnames(results_1617) = c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")
  
  results_1718 <- data.frame(matrix(NA, nrow = 380, ncol = 5))
  for(i in 1:760){
    if((i %% 2)!=0){
      results_1718[(i%/%2)+1,1] = matches[i,3]
      results_1718[(i%/%2)+1,3] = matches[i,4]
    } else {
      results_1718[i/2,2] = matches[i,3]
      results_1718[i/2,4] = matches[i,4]
      
      if(results_1718[i/2,3]>results_1718[i/2,4]){
        results_1718[i/2,5] = "H"
      } else if(results_1718[i/2,3]<results_1718[i/2,4]){
        results_1718[i/2,5] = "A"
      } else{
        results_1718[i/2,5] = "D"
      }
    }
  }
  colnames(results_1718) = c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")
  
  results_1819 <- data.frame(matrix(NA, nrow = 380, ncol = 5))
  for(i in 1:760){
    if((i %% 2)!=0){
      results_1819[(i%/%2)+1,1] = matches[i,5]
      results_1819[(i%/%2)+1,3] = matches[i,6]
    } else {
      results_1819[i/2,2] = matches[i,5]
      results_1819[i/2,4] = matches[i,6]
      
      if(results_1819[i/2,3]>results_1819[i/2,4]){
        results_1819[i/2,5] = "H"
      } else if(results_1819[i/2,3]<results_1819[i/2,4]){
        results_1819[i/2,5] = "A"
      } else{
        results_1819[i/2,5] = "D"
      }
    }
  }
  colnames(results_1819) = c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")
  
  results_1920 <- data.frame(matrix(NA, nrow = 380, ncol = 5))
  for(i in 1:760){
    if((i %% 2)!=0){
      results_1920[(i%/%2)+1,1] = matches[i,7]
      results_1920[(i%/%2)+1,3] = matches[i,8]
    } else {
      results_1920[i/2,2] = matches[i,7]
      results_1920[i/2,4] = matches[i,8]
      
      if(results_1920[i/2,3]>results_1920[i/2,4]){
        results_1920[i/2,5] = "H"
      } else if(results_1920[i/2,3]<results_1920[i/2,4]){
        results_1920[i/2,5] = "A"
      } else{
        results_1920[i/2,5] = "D"
      }
    }
  }
  colnames(results_1920) = c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")
  
  results_2021 <- data.frame(matrix(NA, nrow = 380, ncol = 5))
  for(i in 1:760){
    if((i %% 2)!=0){
      results_2021[(i%/%2)+1,1] = matches[i,9]
      results_2021[(i%/%2)+1,3] = matches[i,10]
    } else {
      results_2021[i/2,2] = matches[i,9]
      results_2021[i/2,4] = matches[i,10]
      
      if(results_2021[i/2,3]>results_2021[i/2,4]){
        results_2021[i/2,5] = "H"
      } else if(results_2021[i/2,3]<results_2021[i/2,4]){
        results_2021[i/2,5] = "A"
      } else{
        results_2021[i/2,5] = "D"
      }
    }
  }
  colnames(results_2021) = c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")
  
  test_data = rbind(results_1617, results_1718, results_1819, results_1920, results_2021)
  
  # Prediction {-}
  
  ## Fits Poisson Distribution ############################# 
  ## **Data Summary**
  # Summarize home power
  HomePower <- test_data %>% 
    group_by(HomeTeam) %>% 
    summarize(HWins = sum(FTR == "H"), HDraws = sum(FTR == "D"), HLoses = sum(FTR == "A"),
              HP = 3 * HWins + 1 * HDraws, HPower = HP / (19*3*3))
  
  # Summarize away power
  AwayPower <- test_data %>% 
    group_by(AwayTeam) %>% 
    summarize(AWins = sum(FTR == "A"), ADraws = sum(FTR == "D"), ALoses = sum(FTR == "A"),
              AP = 3 * AWins + 1 * ADraws, APower = AP / (19*3*3)) 
  
  # Distribution of number of home goals for each team
  HG <- test_data %>% 
    group_by(HomeTeam, FTHG) %>%
    summarize(HG = n()) %>%
    spread(FTHG, HG) 
  
  # Distribution of number of away goals for each team
  AG <- test_data %>% 
    group_by(AwayTeam, FTAG) %>%
    summarize(AG = n()) %>% 
    spread(FTAG, AG)
  
  # begin plot of match information
  #---------------------------------------------------
  #some teams' names are too long
  team_names_plot = lapply(HomePower$HomeTeam, FUN = function(x){
    if (x == "Leeds"){
      return("LE")
    }
    if(x == "Leicester"){
      return("LC")
    }
    if (x == "Man City"){
      return("MC")
    }
    if(x == "Man Utd"){
      return("MU")
    }
    if(x == "West Brom"){
      return("WB")
    }
    if(x == "West Ham"){
      return("WH")
    }
    substr(x, 1, 2)
  })
  team_names_plot = unlist(team_names_plot)
  number_of_games = numeric(3 * length(team_names_plot))
  number_of_games[seq(1, length(number_of_games), 3)] = HomePower$HWins
  number_of_games[seq(2, length(number_of_games), 3)] = HomePower$HDraws
  number_of_games[seq(3, length(number_of_games), 3)] = HomePower$HLoses
  home_match_result = data.frame(
                       Teams=rep(team_names_plot, each=3),
                       Result=rep(c("Win", "Draw", "Lose"), length(team_names_plot)),
                       Number=number_of_games)
  
  (ggplot(data=home_match_result, aes(x=Teams, y=Number, fill=Result)) +
         geom_bar(stat="identity") +
         ylab("Number of matches") +
         ggtitle("Home team performance") +
         theme(plot.title = element_text(hjust = 0.5)))
  
  team_names_away_plot = lapply(AwayPower$AwayTeam, FUN = function(x){
    if (x == "Leeds"){
      return("LE")
    }
    if(x == "Leicester"){
      return("LC")
    }
    if (x == "Man City"){
      return("MC")
    }
    if(x == "Man Utd"){
      return("MU")
    }
    if(x == "West Brom"){
      return("WB")
    }
    if(x == "West Ham"){
      return("WH")
    }
    substr(x, 1, 2)
  })
  team_names_away_plot = unlist(team_names_away_plot)
  number_of_games_away = numeric(3 * length(team_names_away_plot))
  number_of_games_away[seq(1, length(number_of_games_away), 3)] = AwayPower$AWins
  number_of_games_away[seq(2, length(number_of_games_away), 3)] = AwayPower$ADraws
  number_of_games_away[seq(3, length(number_of_games_away), 3)] = AwayPower$ALoses
  away_match_result = data.frame(
    Teams=rep(team_names_away_plot, each=3),
    Result=rep(c("Win", "Draw", "Lose"), length(team_names_away_plot)),
    Number=number_of_games_away)
  
  (ggplot(data=away_match_result, aes(x=Teams, y=Number, fill=Result)) +
      geom_bar(stat="identity") +
      ylab("Number of matches") +
      ggtitle("Away team performance") +
      theme(plot.title = element_text(hjust = 0.5)))
  
  
  #-----end of plot------------------------------------
  
  
  # But not the overall sample is not representative for every team, as strong teams like Man City will still dominate in away matches and score at least two goals most of the times.
  test_data %>% 
    group_by(AwayTeam, FTAG) %>%
    filter(AwayTeam ==  "Man City") %>% 
    summarize(FTAG) %>% 
    ggplot() +
    geom_bar(mapping = aes(FTAG))
  # Therefore we decided to estimate the distribution parameter for each team
  
  # Summarize goal number for each team in both Home and away matches
  HM <- test_data %>% 
    group_by(HomeTeam) %>% 
    summarize(FTHG) %>% 
    pivot_wider(names_from = HomeTeam, values_from = FTHG)
  
  AM <- test_data %>% 
    group_by(AwayTeam) %>% 
    summarize(FTAG) %>% 
    pivot_wider(names_from = AwayTeam, values_from = FTAG)
  
  # Vectorize the fit test function and return parameters for each team for both home and away matches
  gftest_vec <- Vectorize(goodfit, "x")
  Home_test <- data.frame(t(gftest_vec(HM, "poisson", "ML")))
  Away_test <- data.frame(t(gftest_vec(AM, "poisson", "ML")))
  pois_par <- data.frame(cbind(unlist(Home_test$par), unlist(Away_test$par)))
  colnames(pois_par) = c("Home", "Away")
  
  Names <- test_data %>% 
    arrange(HomeTeam) %>%
    select(Team = HomeTeam) %>% 
    unique()  
  
  # pois_par shows all teams poison parameters appeared in last 5 seasons
  pois_par <- cbind(Names, pois_par)
  
  # Newly promoted team Brentford (from English Football League Championship) never appeared in Premier League in last 5 seasons
  # We use the mean of Sheffield United and West Bromwich to replace Brentford, where Sheffield and West Bromwich are relegated in last season 
  brentford = data.frame(Team = "Brentford",
                         Home = (pois_par[21, ]$Home + pois_par[28, ]$Home)/2,
                         Away = (pois_par[21, ]$Away + pois_par[28, ]$Away)/2)
  
  pois_par_2122 = rbind(pois_par[1, ], pois_par[2, ], brentford, pois_par[4, ], pois_par[5, ], pois_par[7, ],
                        pois_par[8, ], pois_par[9, ], pois_par[13, ], pois_par[14, ], pois_par[15, ],
                        pois_par[16, ], pois_par[17, ], pois_par[19, ], pois_par[20, ], pois_par[22, ],
                        pois_par[23, ], pois_par[27, ], pois_par[29, ], pois_par[30, ])
  
  
  # **Monte Carlo Simulation**
  
  # Define a function for single-time simulation, with home/away lambda parameters of each team as inputs
  sim1 <- function(lambda_data){
    results_h <- data.frame(matrix(NA, nrow = 20, ncol = 20))
    results_a <- data.frame(matrix(NA, nrow = 20, ncol = 20))
    rownames(results_h) = lambda_data[1:20, 1]
    colnames(results_h) = lambda_data[1:20, 1]
    rownames(results_a) = lambda_data[1:20, 1]
    colnames(results_a) = lambda_data[1:20, 1]
    
    for (i in 1:20){
      for (j in 1:20){
        if (i == j){
          results_h[i, j] = 0
        } else {
          if (lambda_data[i, 2] > 1.5*lambda_data[j, 3]){ # when big teams against small teams
            lambda_data[i, 2] = 2*lambda_data[i, 2] # adjust both teams' lambda
            lambda_data[j, 3] = 0.5*lambda_data[j, 3]
            if(round(rpois(1, lambda_data[i, 2])) > round(rpois(1, lambda_data[j, 3]))){results_h[i, j] = 3}
            else if(round(rpois(1, lambda_data[i, 2])) < round(rpois(1, lambda_data[j, 3]))){results_h[i, j] = 0}
            else {results_h[i, j] = 1}
            lambda_data[i, 2] = lambda_data[i, 2]/2
            lambda_data[j, 3] = 2*lambda_data[j, 3]
          } else { # when teams are similar
            if(round(rpois(1, lambda_data[i, 2])) > round(rpois(1, lambda_data[j, 3]))){results_h[i, j] = 3}
            else if(round(rpois(1, lambda_data[i, 2])) < round(rpois(1, lambda_data[j, 3]))){results_h[i, j] = 0}
            else {results_h[i, j] = 1}
          }
        }
      }
    }
    
    for (i in 1:20){
      for (j in 1:20){
        if (i == j){
          results_a[i, j] = 0
        }
        else if (results_h[j, i] == 3){
          results_a[i, j] = 0
        }
        else if (results_h[j ,i] == 1){
          results_a[i, j] = 1
        }
        else{
          results_a[i, j] = 3
        }
      }
    }
    
    results_sum_h <- results_h %>% 
      rowSums()
    results_sum_a <- results_a %>% 
      rowSums()
    Table <- data.frame(results_sum_h + results_sum_a)
    colnames(Table) <- "Points"
    return(Table)
  }
  
  # Check out the single simulation result
  single_sim <- sim1(pois_par_2122)
  single_sim
  
  # Define Monte Carlo Simulation function
  MCS <- function(n = 1000, lambda_data){
    set.seed(697)
    mc <- data.frame(matrix(NA, nrow = 20, ncol = n))
    rownames(mc) = lambda_data[1:20, 1]
    for (i in 1:n){
      mc[1:20, i] <- sim1(lambda_data)
    }
    
    #------plot histogram for Monte Carlo Simulation Results
    ManCitySimulation = unname(unlist(mc[12,]))
    (plt = ggplot(data.frame(x = ManCitySimulation)) +
        geom_histogram(aes(x = x, y =..density..), color = "white", fill="red"))
    alpha = 0.05
    ManCity_point_CI_95 = quantile(ManCitySimulation, c(alpha/2, 1-alpha/2))
    ManCity_point_CI_90 = quantile(ManCitySimulation, c(alpha, 1-alpha))
    plt + geom_vline(xintercept = ManCity_point_CI_90, col = "green", linetype = "dashed") +
          geom_vline(xintercept = ManCity_point_CI_95, col = "blue", linetype = "dashed") +  
          xlab('Man City Predicted Points For 21-22 Season (n = 1000)') +
          geom_text(aes(x=80, label=" 90% confidence interval", y=0.07), colour="green", size = 4)+
          geom_text(aes(x=80, label=" 95% confidence interval", y=0.065), colour="blue", size = 4)
    
    ManUnitedSimulation = unname(unlist(mc[13,]))
    (plt = ggplot(data.frame(x = ManUnitedSimulation)) +
        geom_histogram(aes(x = x, y =..density..), color = "white", fill="red"))
    alpha = 0.05
    ManUnited_point_CI_95 = quantile(ManUnitedSimulation, c(alpha/2, 1-alpha/2))
    ManUnited_point_CI_90 = quantile(ManUnitedSimulation, c(alpha, 1-alpha))
    plt + geom_vline(xintercept = ManUnited_point_CI_90, col = "green", linetype = "dashed") +
      geom_vline(xintercept = ManUnited_point_CI_95, col = "blue", linetype = "dashed") +  
      xlab('Man United Predicted Points For 21-22 Season (n = 1000)') +
      geom_text(aes(x=80, label=" 90% confidence interval", y=0.07), colour="green", size = 4)+
      geom_text(aes(x=80, label=" 95% confidence interval", y=0.065), colour="blue", size = 4)
    
    #End of plot--------------------------------------------------
    results <- mc %>% 
      rowSums() %>% 
      as.data.frame()
    
    colnames(results) <- "Points"
    
    results_mc <- results %>%  
      mutate(Rank = dense_rank(desc(Points))) %>% 
      mutate(Points = round(Points/n, 0)) %>% 
      arrange(Rank)
    
    return(results_mc)
  }
  
  test <- MCS(1000, pois_par_2122)
  
  #-----plot the final predicted result
  
  # create data frame for plot
  # only plot the rank of first five teams
  nTeam = 5
  teamNames = row.names(test)[1:nTeam]
  teamNames = gsub("Man City", "MANCHESTER CITY", teamNames)
  teamNames = gsub("Spurs", "TOTTENHAM HOTSPUR", teamNames)
  teamNames = gsub("Leeds", "LEEDS UNITED", teamNames)
  teamNames = toupper(teamNames)
  result =  get_rank_and_point(teamNames)
  rank_of_final_team = result[[1]]
  point_of_final_team = result[[2]]
  
  # add the estimated result
  rank_of_final_team = cbind(rank_of_final_team, test$Rank[1:5])
  point_of_final_team = cbind(point_of_final_team, test$Points[1:5])
  finalSeasonsPlot = seq(from = 2013, to = 2021)
  rownames(rank_of_final_team) = teamNames
  colnames(rank_of_final_team) = finalSeasonsPlot
  rownames(point_of_final_team) = teamNames
  colnames(point_of_final_team) = finalSeasonsPlot
  
  
  rank_of_final_team_df = data.frame(
    Seasons = finalSeasonsPlot
  )
  point_of_final_team_df = data.frame(
    Seasons = finalSeasonsPlot
  )
  for (i in 1:length(teamNames)) {
    rank_of_final_team_df[, teamNames[i]] = rank_of_final_team[i,]
    point_of_final_team_df[, teamNames[i]] = point_of_final_team[i,]
  }
  
  rank_of_final_team_df = melt(rank_of_final_team_df ,  id.vars = 'Seasons', variable.name = 'Teams')
  point_of_final_team_df = melt(point_of_final_team_df ,  id.vars = 'Seasons', variable.name = 'Teams')
  
  # plot the change of rank 
  finalSeasons =  c("13-14", "14-15", "15-16", "16-17", "17-18", 
                  "18-19", "19-20",  "20-21", "Current")
  
  (plt =  ggplot(data=na.omit(rank_of_final_team_df), aes(Seasons,value)) + 
      geom_line(aes(colour = Teams)) + 
      scale_y_continuous(breaks= pretty_breaks(), trans = "reverse") +
      scale_x_continuous(breaks = finalSeasonsPlot, label = finalSeasons) + 
      ylab("Rank") +
      ggtitle("Rank Predictation") +
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  (plt =  ggplot(data=na.omit(point_of_final_team_df), aes(Seasons,value)) + 
      geom_line(aes(colour = Teams)) + 
      scale_y_continuous(breaks= pretty_breaks()) +
      scale_x_continuous(breaks = finalSeasonsPlot, label = finalSeasons) + 
      ylab("Point") +
      ggtitle("Pint Predictation") +
      theme(plot.title = element_text(hjust = 0.5))
  )
  