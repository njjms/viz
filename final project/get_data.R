library(jsonlite)
library(tidyverse)
library(ggrepel)

#### Getting talent data for the past 10 years

gettalentdata <- function(year) {
  
  #Pulls out a years worth of talent data
  
  require(jsonlite)
  talenturl <- paste0("https://api.collegefootballdata.com/talent?year=", year)
  talent <- fromJSON(talenturl)
  return(talent)
}

talent2018 <- gettalentdata("2018")
talent2017 <- gettalentdata("2017")
talent2016 <- gettalentdata("2016")
talent2015 <- gettalentdata("2015")

talent <- rbind(talent2018, talent2017, talent2016, talent2015)
write.csv(talent, "talent.csv")

#### Getting number of wins for the past 10 years

getgamedata <- function(year) {
  
  # Pulls out a years worth of game data and calculates wins
  
  require(jsonlite)
  gameurl <- paste0("https://api.collegefootballdata.com/games?year=", year, "&seasonType=regular")
  games <- fromJSON(gameurl)
  games$winning_team <- with(games, ifelse(home_points > away_points, home_team, away_team))
  
  games %>% 
    group_by(season) %>% 
    count(winning_team) -> games
  
  return(games)
  
}

games2018 <- getgamedata("2018")
games2017 <- getgamedata("2017")
games2016 <- getgamedata("2016")
games2015 <- getgamedata("2015")

games <- rbind(games2018, games2017, games2016, games2015)
write.csv(games, "games.csv")

#### Getting conference information

conferenceurl <- "https://api.collegefootballdata.com/teams"
conference <- fromJSON(conferenceurl)
conference %>% select(school, conference) %>% na.omit() %>% distinct() -> conferences
conferences

#### team colors

colorurl <- "https://api.collegefootballdata.com/teams"
colors <- fromJSON(colorurl) %>% 
  select(school, color)

#### joining

head(games)
colnames(talent) <- c("season", "school", "talent rating")
colnames(games) <- c("season", "school", "wins")

wintalentdata <- inner_join(talent, games, by = c("season", "school"))
wintalentdata$`talent rating` <- as.numeric(as.character(wintalentdata$`talent rating`))
wintalentdata <- left_join(wintalentdata, conferences, by = "school")
wintalentdata <- left_join(wintalentdata, colors, by = "school")
head(wintalentdata)

unique(wintalentdata$conference)
write.csv(wintalentdata, file = "wintalentdata.csv")

wintalentdata %>% 
  filter(conference == "Conference USA") %>% 
  ggplot() +
  geom_point(mapping = aes(x = season, y = `talent rating`, fill = wins)) +
  geom_line(mapping = aes(x = season, y = `talent rating`, color = school)) +
  geom_text_repel(mapping = aes(x = season, y = `talent rating`, label = school), size = 3) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
  
wintalentdata %>% 
  filter(conference == "Big Ten") %>% 
  ggplot() +
  geom_point(mapping = aes(x = season, y = wins, size = `talent rating`), alpha = .5) +
  geom_line(mapping = aes(x = season, y = wins, color = school)) +
  geom_text(mapping = aes(x = season, y = wins - .3, label = school), size = 3)

unique(arrange(subset(wintalentdata, conference == "Pac-12"), school)$color)

unique(select(arrange(subset(wintalentdata, conference == "Pac-12"), school), c("school", "color")))$color
str(as.character(unique(select(arrange(subset(wintalentdata, conference == "Pac-12"), school), c("school", "color")))$color))

wintalentdata %>% 
  filter(conference == "Pac-12") %>% 
  ggplot() +
  geom_point(mapping = aes(x = wins, y = `talent.rating`, color = school), alpha = .5, size = 2) +
  geom_path(mapping = aes(x = wins, y = `talent.rating`, color = school), size = 1.0) +
  geom_text_repel(mapping = aes(x = wins, y = `talent.rating`, label = paste(school, season)), size = 3) + 
  scale_color_manual(
    values = as.character(unique(select(arrange(subset(wintalentdata, conference == "Pac-12"), school), c("school", "color")))$color)
  ) +
  scale_x_continuous("Number of Wins", 
                   breaks = seq(0, 12, 1)) +
  labs(
    title = "Wins vs. Talent Composite Score",
    y = "Composite Talent Score"
  ) +
  theme(
    legend.position = "none"
  )

# unique(select(arrange(subset(wintalentdata, conference == "SEC"), school), c("school", "color")))$color
# 
# wintalentdata %>% 
#   filter(conference == "Big Ten") -> bigtendata
#   
# ggplot() +
#   geom_point(bigtendata, mapping = aes(x = wins, y = `talent rating`, color = school), alpha = .5) + 
#   geom_path(bigtendata, mapping = aes(x = wins, y = `talent rating`, color = school)) +
#   geom_text_repel(subset(bigtendata, season == "2015" | season == "2018"), mapping = aes(x = wins, y = `talent rating`, label = paste(school, season)), size = 3) +
#   theme(
#     legend.position = "none"
#   )
# 
# wintalentdata %>% 
#   ggplot() +
#   geom_point(mapping = aes(x = wins, y = `talent rating`, color = school), alpha = .5) +
#   geom_path(mapping = aes(x = wins, y = `talent rating`, color = school)) +
#   geom_text_repel(mapping = aes(x = wins, y = `talent rating`, label = paste(school, season)), size = 3) + 
#   scale_x_continuous("Number of Wins", 
#                    breaks = seq(0, 12, 1)) +
#   labs(
#     title = "Wins vs. Talent Composite Score, 2015-2018",
#     y = "Composite Talent Score"
#   ) +
#   theme(
#     legend.position = "none"
#   ) 
