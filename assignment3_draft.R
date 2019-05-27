library(tidyverse)
library(jsonlite)

# Let's start by investigating play-by-play data
# I would like to see it the internet fan forums are true - does the Big Ten really run a 1920s offense with nothing but running?
# Does the Big 12 really run a high flying offense while playing pretty much no defense?

url <- "https://api.collegefootballdata.com/plays?seasonType=regular&year=2018&week=8&team=Rutgers"
jsontest <- fromJSON(url)
colnames(jsontest)

p5 <- c("Big Ten", "Big 12", "ACC", "Pac-12", "SEC")

i <- 5

jsontest %>% 
  select(id) %>% 
  mutate(weeknumber = i) %>% 
  head

jsontest %>% select(c("id", "play_type")) %>% dim
jsontest %>% select(id, play_type) %>% dim

data2018 <- jsontest[FALSE,]

start <- Sys.time()
for (i in 1:12) {
  
  url <- paste0("https://api.collegefootballdata.com/plays?seasonType=regular&year=2018&week=", i ,"&team=Rutgers")
  week <- fromJSON(url)
  
  if (!is.null(dim(week))) {
    
    week %>% select(id, play_type, yards_gained, play_text) %>% 
      mutate(weeknumber = i) -> week
    data2018 <- rbind(data2018, week)
    print(paste("week", i, "complete"))
    
  } else {
    
    message("Bye week.")
    
  }

}
end <- Sys.time()
end-start

format(object.size(data2018), units="Kb", standard="auto")
str(data2018)

tail(data2018)

# Write function to pull out the data we want

get_pbp_data <- function(name, year, selection = c("id, offense, defense,
                                                   play_type, yards_gained, play_text")) {
  
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
      print(paste("week", i, "complete"))
      
    } else {
      
      message("Bye week.")
      
    }
  
  }
  return(pbpdata)
}

get_conf_pbp <- function(name, year, selection = c("id", "offense", "offense_conference", "defense", "defense_conference",
                                                   "play_type", "yards_gained", "clock", "play_text")) {
  
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
      print(paste("week", week, "complete"))
      
    } else {
      
      message(paste("Error at week", week, "with conference", name))
      
    }
  
  }
  return(pbpdata)
}

selection = c("id", "offense", "defense", "play_type","yards_gained", "play_text")

pbpdata <- data.frame(matrix(vector(), 0, length(selection), dimnames=list(c(), selection)))

conferences$abbreviation[1:5]
big10 <- get_conf_pbp("B1G", "2018")
acc <- get_conf_pbp("ACC", "2018")
big12 <- get_conf_pbp("B12", "2018")
pac12 <- get_conf_pbp("PAC", "2018")
sec <- get_conf_pbp("SEC", "2018")

pbpdata <- rbind(big10, acc, big12, pac12, sec)
pbpdata <- pbpdata[!duplicated(pbpdata),]
dim(pbpdata)

format(object.size(pbpdata), units="Mb")

for (conf in conferences$abbreviation[1:5]) {
  conf_data <- get_conf_pbp(conf, "2018")
  pbpdata <- rbind(pbpdata, conf_data)
  
  print(paste("done with conference", conf_data))
}

# Now that we FINALLY have the pbp data for 2018, let's see what we can do with it

unique(pbpdata$play_type)

pbpdata %>% 
  filter(offense_conference %in% c("Big Ten", "Pac-12", "Big 12", "ACC", "SEC")) %>% 
  filter(play_type %in% c("Rush", "Rushing Touchdown", "Passing Touchdown", "Pass Incompletion", "Pass Reception")) %>% 
  group_by(offense_conference) %>% 
  summarise(plays = n()) %>% 
  ggplot() + geom_bar(mapping = aes(y = plays, x = reorder(offense_conference, plays)), stat="identity")

# The Big 12 ran a lot less plays than the other conferences did.

pbpdata %>% 
  filter(offense_conference %in% c("Big Ten", "Pac-12", "Big 12", "ACC", "SEC")) %>% 
  filter(play_type %in% c("Rush", "Rushing Touchdown")) %>% 
  group_by(offense_conference) %>% 
  summarise(running_plays = n()) %>% 
  ggplot() + geom_bar(mapping = aes(y = plays, x = reorder(offense_conference, plays)), stat="identity")

pbpdata %>% 
  filter(offense_conference %in% c("Big Ten", "Pac-12", "Big 12", "ACC", "SEC")) %>% 
  filter(play_type %in% c("Passing Touchdown", "Pass Incompletion", "Pass Reception")) %>% 
  group_by(offense_conference) %>% 
  summarise(passing_plays = n()) %>% 
  ggplot() + geom_bar(mapping = aes(y = plays, x = reorder(offense_conference, plays)), stat="identity")

# The Big 12 runs less running plays than other conferences, but this doesn't tell us much - they also run less running plays.

pbpdata %>% 
  filter(offense_conference %in% c("Big Ten", "Pac-12", "Big 12", "ACC", "SEC")) %>% 
  filter(play_type %in% c("Rush", "Rushing Touchdown")) %>% 
  group_by(offense_conference) %>% 
  summarise(running_plays = n()) -> running

pbpdata %>% 
  filter(offense_conference %in% c("Big Ten", "Pac-12", "Big 12", "ACC", "SEC")) %>% 
  filter(play_type %in% c("Passing Touchdown", "Pass Incompletion", "Pass Reception")) %>% 
  group_by(offense_conference) %>% 
  summarise(passing_plays = n()) -> passing

total <- left_join(running, passing)
total %>% 
  mutate(pass_prop = passing_plays/(running_plays + passing_plays)) %>% 
  ggplot() + geom_bar(mapping = aes(x = reorder(offense_conference, pass_prop), y = pass_prop), stat="identity")

# Surprisingly, the Pac-12 runs the most passing plays. The SEC runs the most running plays.
# Expections = Subverted
# Let's check the explosiveness of plays in each conference - we can metricize this using yards per play (ypp)

pbpdata %>% 
  filter(offense_conference %in% c("Big Ten", "Pac-12", "Big 12", "ACC", "SEC")) %>% 
  filter(play_type %in% c("Rush", "Rushing Touchdown", "Pass Reception", "Passing Touchdown")) %>% 
  group_by(offense_conference, play_type) %>% 
  summarise(
    yardage = sum(yards_gained),
    plays = n(),
    ypp = yardage/plays
  ) %>% 
  ggplot() + geom_bar(mapping = aes(x = play_type, y = ypp, fill = play_type), stat="identity") + 
  geom_hline(yintercept = 11.3, color = "#f9926c", linetype = 5) +
  geom_hline(yintercept = 24.6, color = "firebrick", linetype = 2) +
  geom_hline(yintercept = 4.92, color = "#66b3ff", linetype = 3) +
  geom_hline(yintercept = 14.3, color = "blue", linetype = 4) +
  facet_grid(rows = vars(play_type))

pbpdata %>% 
  filter(offense_conference %in% p5) %>% 
  filter(play_type %in% c("Rush", "Rushing Touchdown", "Pass Reception", "Passing Touchdown")) %>% 
  group_by(offense_conference, play_type) %>% 
  summarise(
    yardage = sum(yards_gained),
    plays = n(),
    ypp = yardage/plays
  ) %>% 
  group_by(play_type) %>% 
  summarise(
    average_ypp = mean(ypp)
  )

# All of the major conferences have about the same distribution shape - what is interesting is the explosiveness of scoring plays.
# The average yardage of plays which actually score is much higher than plays which do not
# This implies that there is less methodical marches downfield to the endzone and more explosive touchdowns

game_url <- "https://api.collegefootballdata.com/games?year=2018&week=1&seasonType=regular&conference=b1g"
games_wk1 <- fromJSON(game_url)

pythag_points <- function(conference, year = "2018") {
  
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
      message(paste("complete with week", week))
    }
  }
  return(game_scores)
}

baseurl <- paste0("https://api.collegefootballdata.com/games?year=", year, "&week=", week, "&seasonType=regular&conference=", conference)
week_games <- fromJSON(baseurl)

colnames(week_games)
week_games

week_games %>% 
  mutate(
    margin_of_victory = max(home_points, away_points) - min(home_points, away_points),
    winning_conference = ifelse(home_points > away_points, home_conference, away_conference),
    losing_conference = ifelse(home_points > away_points, away_conference, home_conference)
  ) %>% 
  select(winning_conference,
         losing_conference,
         margin_of_victory)

week_games %>% 
  select(home_conference, home_points, away_conference, away_points) -> test

margin_of_victory = apply(test[,c("home_points", "away_points")], 1, max) - apply(test[,c("home_points", "away_points")], 1, min)
week_games$margin_of_victory <- margin_of_victory

week_games

year <- "2018"
week <- "1"
conference <- "b1g"

big10scores <- pythag_points("b1g")
accscores <- pythag_points("acc")
b12scores <- pythag_points("b12")
secscores <- pythag_points("sec")
pacscores <- pythag_points("pac")

scoredata <- rbind(big10scores, accscores, b12scores, secscores, pacscores)
dim(scoredata)

head(scoredata)

scoredata %>% 
  group_by(winning_conference) %>% 
  summarise(
    average_margin = mean(margin_of_victory)
  ) %>% 
  filter(winning_conference %in% c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")) %>% 
  ggplot() + geom_bar(mapping = aes(x = reorder(winning_conference, average_margin), y = average_margin), stat="identity")

# The SEC has much larger margins of victory than any other conference - they are the conference of champions after all
# The Pac-12 has much lower margins of victory than other conferences
# This would be better if we could break this out by conference-conference matchup - maybe heatmap?

scoredata %>% 
  group_by(winning_conference, losing_conference) %>% 
  summarise(
    average_margin = mean(margin_of_victory)
  ) %>% 
  filter(winning_conference %in% c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC") &
           losing_conference %in% c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")) %>% 
  ggplot() + geom_tile(mapping = aes(x = winning_conference, y = losing_conference, fill = average_margin))

# This plot is kind of neat! Consider polishing

scoredata %>% 
  group_by(winning_conference, losing_conference) %>% 
  filter(winning_conference %in% c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC") &
           losing_conference %in% c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")) %>% 
  summarise(
    average_margin = mean(margin_of_victory),
    number_of_games = n()
  )

scoredata %>% 
  mutate(
    winning_score = ifelse(home_points > away_points, home_points, away_points),
    losing_score = ifelse(home_points < away_points, home_points, away_points)
  ) %>% 
  group_by(winning_conference) %>% 
  summarise(
    points_for = sum(winning_score)
  ) -> points_for

scoredata %>% 
  mutate(
    winning_score = ifelse(home_points > away_points, home_points, away_points),
    losing_score = ifelse(home_points < away_points, home_points, away_points)
  ) %>% 
  group_by(losing_conference) %>% 
  summarise(
    points_against = sum(winning_score)
  ) -> points_against

colnames(points_against) <- c("conference", "points against")
colnames(points_for) <- c("conference", "points for")
pythagorean_points <- left_join(points_for, points_against)


pythagorean_points %>% 
  filter(conference %in% p5) %>% 
  ggplot() + geom_bar(mapping = aes(x = reorder(conference, `points for`), y = `points for`), stat="identity")

# This is interesting but also imbalanced  - the Big 12 has the fewest points
# We should adjust by the number of teams in each conference

teams_by_conference <- fromJSON("https://api.collegefootballdata.com/teams")
unique(teams_by_conference$conference)

teams_by_conference %>% 
  group_by(conference) %>% 
  summarise(
    number_of_teams = n()
  ) -> number_of_teams_by_conf

pythagorean_points <- left_join(pythagorean_points, number_of_teams_by_conf)

pythagorean_points %>% 
  filter(conference %in% p5) %>% 
  mutate(
    points_per_team = `points for`/number_of_teams
  ) %>% 
  ggplot() + geom_bar(mapping = aes(x = reorder(conference, desc(points_per_team)), y = points_per_team), stat = "identity")

# SEC scores the most points per team - the PAC-12 scores the least.
# Not entirely surprising on the far end - the SEC is probably top to bottom the best
# I am surprised though that the PAC-12 ranks the lowest - PAC12 teams are known for employing high flying offenses (think WSU air raid, UW's Jake Browning, UO)

# Let's investigate the number of points a team scores compared to how much talent they have (by composite talent rankings)

conf_points_by_team <- function(conference, year = "2018") {
  
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
      message(paste("data retrieval complete for week", week))
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

big10pbt <- conf_points_by_team("b1g")
secpbt <- conf_points_by_team("sec")
accpbt <- conf_points_by_team("acc")
b12pbt <- conf_points_by_team("b12")
pacpbt <- conf_points_by_team("pac")
indpbt <- conf_points_by_team("ind")

allpbt <- rbind(big10pbt, secpbt, accpbt, b12pbt, pacpbt, indpbt)

allpbt %>% 
  filter(total_points > 100) -> allpbt

allpbt[duplicated(allpbt$team),]

allpbt <- arrange(allpbt, desc(total_points))

allpbt <- allpbt[!duplicated(arrange(allpbt, desc(total_points))$team),]

talent <- fromJSON("https://api.collegefootballdata.com/talent?year=2018")
talent <- talent[,2:3]
colnames(talent) <- c("team", "talent")
ptsvstalent <- left_join(allpbt, talent)

teams_by_conference %>% 
  filter(conference %in% p5) %>% 
  select(school, conference) -> teams_by_conference
colnames(teams_by_conference) <- c("team", "conference")

left_join(ptsvstalent, teams_by_conference) -> ptsvstalent
head(ptsvstalent)

ggplot() +
  geom_point(ptsvstalent, mapping = aes(x = talent,
                                        y = total_points,
                                        color = conference)) +
  geom_text(ptsvstalent,mapping = aes(x = talent,
                                      y = total_points+5,
                                      label = team))

# Doesn't look like talent automatically correlates to having a better team.
# There are a lot of teams that overperform given their relatively low talent ranking and lots of teams that underperform
# Also the only teams that really rise above everyone else in talent and scoring also have the most money BY FAR
# Maybe do a future plot that has wins instead of scoring? might be more eludcidating
# TODO: write function to calculate wins from game data because for some reason I can't find an endpoint that already has team records in it.