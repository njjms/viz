# Functions used to pull data from Scarblue1988's CFB API

get_pbp_data <- function(name, year, selection = c("id, offense, defense,
                                                   play_type, yards_gained, play_text")) {
 
  # Function to pull play-by-play data for a single team for a single year
  
  base_url <- paste0("https://api.collegefootballdata.com/plays?seasonType=regular&year=", year, "&week=")
  team_endpoint <- paste0("&team=", name)
  pbpdata <- data.frame(matrix(vector(), 0, length(selection), dimnames=list(c(), selection)))
  
  for (i in 1:12) {
    
    url <- paste0(base_url, i ,team_endpoint)
    week <- fromJSON(url)
    
    if (!is.null(dim(week))) {
      
      week %>% select(id, play_type, yards_gained, play_text) %>% 
        mutate(weeknumber = i) -> week
      pbpdata <- rbind(pbpdata, week)
      
    } else {
      
      
    }
  
  }
  return(pbpdata)
}

get_conf_pbp <- function(name, year="2018", selection = c("id", "offense", "offense_conference", "defense", "defense_conference",
                                                   "play_type", "yards_gained", "clock", "play_text")) {
 
  # Function to pull the play by play of an entire conference for a year
  
  pbpdata <- data.frame(matrix(vector(), 0, length(selection), dimnames=list(c(), selection)))
  
  for (week in 1:12) {
    
    url <- paste0("https://api.collegefootballdata.com/plays?seasonType=regular&year=", 
                  year,
                  "&week=",
                  week,
                  "&conference=",
                  name)
    
    conf_week_data <- fromJSON(url)
    
    if (!is.null(dim(conf_week_data))) {
      
      conf_week_data %>% select(id, offense, offense_conference, defense, defense_conference,
                                                   play_type, yards_gained, play_text) %>% 
        mutate(weeknumber = week) -> conf_week_data
      pbpdata <- rbind(pbpdata, conf_week_data)
      
    } else {
      
      
    }
  
  }
  return(pbpdata)
}

pythag_points <- function(conference, year = "2018") {
  
  # Used for the "pythagorean formula" for sports - returns a df with lots of score information
  # Points that a conference scored against a different conference in w
  
  if (!(conference %in% c("b1g", "sec", "pac", "b12", "acc"))) {
    
    stop("Invalid conference - must be one of 'Big Ten', 'SEC', 'PAC', 'Big 12', 'ACC'")
    
  } else {
    
    game_scores <- data.frame(matrix(vector(), 0, 6, dimnames=list(c(), c("home_conference",
                                                                          "home_points",
                                                                          "away_conference",
                                                                          "away_points",
                                                                          "margin of victory",
                                                                          "weeknumber"))))
    
    for (week in 1:12) {
      baseurl <- paste0("https://api.collegefootballdata.com/games?year=", year, "&week=", week, "&seasonType=regular&conference=", conference)
      week_games <- fromJSON(baseurl)
      
      week_games %>% 
        select(home_conference, home_points, away_conference, away_points) -> test
      
      margin_of_victory <- apply(test[,c("home_points", "away_points")], 1, max) - apply(test[,c("home_points", "away_points")], 1, min)
      winning_conference <- ifelse(test$home_points > test$away_points, test$home_conference, test$away_conference)
      losing_conference <- ifelse(test$home_points > test$away_points, test$away_conference, test$home_conference)
      winning_score <- ifelse(test$home_points > test$away_points, test$home_points, test$away_points)
      losing_score <- ifelse(test$home_points > test$away_points, test$away_points, test$home_points)
      test$winning_conference <- winning_conference
      test$losing_conference <- losing_conference
      test$winning_score <- winning_score
      test$losing_score <- losing_score
      test$margin_of_victory <- margin_of_victory
      test$weeknumber <- week
      
      game_scores <- rbind(game_scores, test)
    }
  }
  return(game_scores)
}

conf_points_by_team <- function(conference, year = "2018") {
  
  # Calculates points scored by teams in the provided conference
  
  if (!(conference %in% c("ind", "b1g", "sec", "pac", "b12", "acc"))) {
    
    stop("Invalid conference - must be one of 'Big Ten', 'SEC', 'PAC', 'Big 12', 'ACC'")
    
  } else {
    
    game_scores <- data.frame(matrix(vector(), 0, 6, dimnames=list(c(), c("home_conference", "home_team", "home_points",
                                                                          "away_conference", "away_team", "away_points"))))
    
    for (week in 1:13) {
      baseurl <- paste0("https://api.collegefootballdata.com/games?year=", year, "&week=", week, "&seasonType=regular&conference=", conference)
      week_games <- fromJSON(baseurl)
      
      week_games %>% 
        select(home_conference, home_team, home_points, away_conference, away_team, away_points) -> test
      
      game_scores <- rbind(game_scores, test)
    }
  }
  
  game_scores %>% 
    group_by(home_team) %>% 
    summarise(
      homepts = sum(home_points)
    ) -> homepts
  
  game_scores %>% 
    group_by(away_team) %>% 
    summarise(
      awaypts = sum(away_points)
    ) -> awaypts
  
  colnames(homepts) <- c("team", "homepts")
  colnames(awaypts) <- c("team", "awaypts")
  
  inner_join(homepts, awaypts) %>% 
    group_by(team) %>% 
    mutate(total_points = sum(homepts, awaypts)) %>% 
    select(team, total_points) -> by_team_scores
  
  return(by_team_scores)
}