# load library/package
library(ggplot2)
library(reshape2)
library(scales)
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
  
  # plot the rankings of each team in the last seasons
  allSeasons =  c("2013-14", "2014-15", "2015-16", "2016-17", "2017-18", 
               "2018-19", "2019-20",  "2020-21")
  nSeasons = 8 # how many seasons do you want to see?
  #------------------------------------------------------------------------------------
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
        ranks[i, j] = which(table_team[, 2*j - 1] == x[i])
        points[i, j] = strtoi(table_team[ranks[i, j], 2*j])
      }
    }
    result = list(ranks, points)
    return(result)
  }
  
  result =  get_rank_and_point(promising_team)
  rank_of_proming_team = result[[1]]
  point_of_proming_team = result[[2]]
    
  list[rank_of_proming_team, point_of_proming_team] = get_rank_and_point(promising_team)
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