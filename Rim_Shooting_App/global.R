### Rim Shooting Trends by Player ###

library(tidyverse)
library(rvest)
library(janitor)
library(jsonlite)
library(readxl)
library(fuzzyjoin)
library(httr)
library(nbastatR)
library(extrafont)

setwd("~/Data Science/Basketball/Projects/Rim Shooting")

### Load theme for graphics

theme_personal <- function (x) { 
  theme_minimal(base_size=12, base_family="Avenir") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      #panel.grid.major.x = element_blank(),
      panel.grid.major = element_line(color = 'gray91', size = .5),
      plot.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
      panel.border = element_rect(fill = NA, color = 'gray91', size = 1),
      axis.title.x = element_text(vjust = -1.5, size = 14),
      axis.title.y = element_text(vjust = 3, angle = 90, size = 14),
      plot.margin = margin(25, 25, 12.5, 25),
      plot.caption = element_text(vjust = -1.7, hjust = 1, size = 9, color = 'gray50'),
      plot.title = element_text(size = 20, hjust = 0, vjust = 4.3),
      plot.subtitle = element_text(size = 12, hjust = 0.0, vjust = 5)
    )
}


## Scrape data

### PBP scrape

pbp_scrape <- function(season){
  
  pbp_url <- paste0("https://api.pbpstats.com/get-totals/nba?Season=",season,"&SeasonType=Regular%2BSeason&Type=Player")
  
  json_data <- fromJSON(paste(readLines(pbp_url), collapse=""))
  pbp_shooting <- json_data[["multi_row_table_data"]]
  
  pbp_shooting <- pbp_shooting %>%
    select(`Name`,`EntityId`,  `TeamAbbreviation`, `GamesPlayed`, `Minutes`, `ShotQualityAvg`, `AtRimFG3AFrequency`, 
           `Avg2ptShotDistance`, `Avg3ptShotDistance`, `AtRimFGM`, `AtRimFGA`, `AtRimFrequency`, `AtRimAccuracy`, 
           `UnblockedAtRimAccuracy`, `AtRimPctAssisted`, `AtRimPctBlocked`) %>%
    clean_names() %>%
    mutate(season = season) %>%
    retype()
  
  pbp_shooting[is.na(pbp_shooting)] <- 0
  
  return(pbp_shooting)
  
}

seasons_input <- c("2020-21", "2019-20", "2018-19", "2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13", "2011-12",
                   "2010-11", "2009-10", "2008-09", "2007-08", "2006-07", "2005-06", "2004-05", "2003-04", "2002-03", "2001-02",
                   "2000-01")

pbp_data <- map_df(seasons_input, pbp_scrape)



# Filter in players with enough action

pbp_data1 <- pbp_data %>%
  rename(player = name)

pbp_data1$season <- gsub("-.*", "", pbp_data1$season)

pbp_data1$season <- as.character(pbp_data1$season)


## Load in player info from NBA.com player list such as height and merge into PBP data


# Pull header code from F5

headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://www.nba.com/players',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)


# Retrieve URL and set up initial data frame

url_players <- "https://stats.nba.com/stats/playerindex?College=&Country=&DraftPick=&DraftRound=&DraftYear=&Height=&Historical=1&LeagueID=00&Season=2020-21&SeasonType=Regular%20Season&TeamID=0&Weight="

res <- GET(url = url_players, add_headers(.headers = headers))
json_resp <- fromJSON(content(res, "text"))

nba_player_info <- data.frame(json_resp$resultSets$rowSet)

columns1 <- json_resp[["resultSets"]][["headers"]][[1]]

colnames(nba_player_info) <- columns1

nba_player_info1 <- nba_player_info %>%
  clean_names() %>%
  retype() %>%
  select(1:4, 12:19) %>%
  mutate(player = paste0(player_first_name, " ", player_last_name),
         height_in = (case_when(height == "5-3" ~ 61, height == "5-5" ~ 63,
                                height == "5-6" ~ 64, height == "5-7" ~ 65,
                                height == "5-8" ~ 68, height == "5-9" ~ 69,
                                height == "5-10" ~ 70, height == "5-11" ~ 71,
                                height == "6-0" ~ 72, height == "6-1" ~ 73,
                                height == "6-2" ~ 74, height == "6-3" ~ 75,
                                height == "6-4" ~ 76, height == "6-5" ~ 77,
                                height == "6-6" ~ 78, height == "6-7" ~ 79,
                                height == "6-8" ~ 80, height == "6-9" ~ 81,
                                height == "6-10" ~ 82, height == "6-11" ~ 83,
                                height == "7-0" ~ 84, height == "7-1" ~ 85,
                                height == "7-2" ~ 86, height == "7-3" ~ 87,
                                height == "7-4" ~ 88, height == "7-5" ~ 89,
                                height == "7-6" ~ 90, height == "7-7" ~ 91))) %>%
  select(person_id, player, position, height, height_in, weight, everything())


# Merge player info and PBP data

pbp_final <- pbp_data1 %>%
  merge(nba_player_info1 %>% select(person_id, height, height_in, weight),
        by.x = "entity_id",
        by.y = "person_id",
        all.x = T)


## BBRef Scrape

bbref_scrape <- function(season = 2020){
  
  bbref_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_shooting.html")
  
  bbref_raw <- bbref_url %>% 
    read_html() %>% 
    html_table() %>%
    .[[1]]
  
  bbref_clean <- bbref_raw %>%
    row_to_names(row_number = 1) %>%
    select(1:7, 29) %>%
    clean_names() %>% 
    rename(dunks = number,
           games = g) %>%
    filter(!player == "Player") %>%
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>%
    mutate(season = season - 1) %>%
    select(-rk) %>%
    return(bbref_clean)
}

bbref_data <- map_df(1997:2021, bbref_scrape)

bbref_data$player <- str_replace_all(pattern = "\\*", replacement = "", bbref_data$player)


# Denote debut season and add in player IDs

player_ids <- read_xlsx("/Users/alexandermerg/Data Science/Basketball/Resources/player_ids.xlsx")


# Add debut season to dataset and merge all player IDs
debut_seasons <- bbref_data %>%
  group_by(player, season) %>%
  summarize(dunks = dunks) %>%
  slice(1) %>%
  mutate(debut_season = season) %>%
  select(player, debut_season) %>%
  filter(debut_season > 1999) %>%
  # Remove foreign accents and such from names
  mutate(new_name = stri_trans_general(player, 'latin-ascii')) %>%
  # Merge player IDs using all possible name combinations
  merge(player_ids %>% select(BBRefName, NBAID),
        by.x = "new_name",
        by.y = "BBRefName",
        all.x = T) %>%
  rename(bbref_source = NBAID) %>%
  merge(player_ids %>% select(NBAName, NBAID),
        by.x = "new_name",
        by.y = "NBAName",
        all.x = T) %>%
  rename(nba_source = NBAID) %>%
  merge(player_ids %>% select(ESPNName, NBAID),
        by.x = "new_name",
        by.y = "ESPNName",
        all.x = T) %>%
  rename(espn_source = NBAID) %>%
  merge(player_ids %>% select(SpotracName, NBAID),
        by.x = "new_name",
        by.y = "SpotracName",
        all.x = T) %>%
  rename(spotrac_source = NBAID) %>%
  merge(pbp_setup,
        by.x = "new_name",
        by.y = "player",
        all.x = T) %>%
  rename(pbp_source = entity_id) %>%
  mutate(across(where(is.integer), as.character)) %>%
  mutate(entity_id = coalesce(bbref_source, nba_source, espn_source, spotrac_source, pbp_source)) %>%
  select(-bbref_source, -nba_source, -espn_source, -spotrac_source, -pbp_source)


# Merge into bbref dataset

bbref_data1 <- bbref_data %>%
  merge(debut_seasons,
        by = "player")


# Compile entity IDs into one dataset to merge into bbref dataset
bbref_setup <- bbref_data1 %>%
  select(player, entity_id) %>%
  distinct(player, .keep_all = TRUE)

pbp_setup <- pbp_final %>%
  select(player, entity_id) %>%
  distinct(entity_id, .keep_all = TRUE)

entity_merge <- stringdist_left_join(bbref_setup, pbp_setup,
                                     by = c("player"),
                                     distance_col = NULL)

entity_merge1 <- entity_merge %>%
  retype() %>%
  clean_names() %>%
  mutate(entity_id = coalesce(entity_id_x, entity_id_y)) %>%
  filter(!is.na(entity_id)) %>%
  select(player_x, entity_id) %>%
  drop_na() %>%
  rename(player = player_x)


# Merge into bbref dataset
bbref_data2 <- bbref_data1 %>%
  merge(entity_merge1,
        by = "player") %>%
  select(-11) %>%
  rename(entity_id = entity_id.x) %>%
  retype() %>%
  mutate_at(vars(contains("entity")), as.character)


## Merge PBP with BBRef

merge_data <- merge(pbp_final, bbref_data1,
                    by = c("entity_id", "season"),
                    all.x = T) %>%
  drop_na() %>%
  distinct(entity_id, season, .keep_all = TRUE)


# Clean up data

merge_data1 <- merge_data %>%
  select(-c(21, 24:26)) %>%
  mutate(pos_group = (case_when(pos == "PG" ~ "1", pos == "PG-SG" ~ "1",
                                pos == "SG-PG" ~ "1", pos == "SG" ~ "Wing",
                                pos == "SF" ~ "Wing", pos == "SG-SF" ~ "Wing",
                                pos == "SF-SG" ~ "Wing", pos == "SG-PF" ~ "Wing",
                                pos == "SF-PF" ~ "4", pos == "PF-SF" ~ "4",
                                pos == "PF" ~ "4", pos == "C-PF" ~ "5",
                                pos == "PF-C" ~ "5", pos == "C" ~ "5")),
         position = (case_when(pos == "PG" ~ 1, pos == "PG-SG" ~ 1,
                               pos == "SG-PG" ~ 1, pos == "SG" ~ 2,
                               pos == "SF" ~ 3, pos == "SG-SF" ~ 3,
                               pos == "SF-SG" ~ 3, pos == "SG-PF" ~ 2,
                               pos == "SF-PF" ~ 4, pos == "PF-SF" ~ 4,
                               pos == "PF" ~ 4, pos == "C-PF" ~ 5,
                               pos == "PF-C" ~ 5, pos == "C" ~ 5)),
         height_quartile = (case_when(height_in < 76 ~ 1,
                                      height_in < 79 ~ 2,
                                      height_in < 82 ~ 3,
                                      height_in > 81 ~ 4))) %>%
  retype() %>%
  mutate(ysd = season - debut_season) %>%
  select(entity_id, player.x, season, team_abbreviation, ysd, pos_group, position, height_in, height_quartile, weight, everything()) %>%
  clean_names() %>%
  rename_with(~ gsub("at_", "", .x)) %>%
  filter(rim_fga > 25) %>%
  mutate(non_dunk_rim_fgm = rim_fgm - dunks,
         non_dunk_rim_fga = rim_fga - dunks,
         non_dunk_rim_accuracy = non_dunk_rim_fgm/non_dunk_rim_fga,
         touch_rim_fga = non_dunk_rim_fga * (1 - rim_pct_blocked),
         touch_rim_accuracy = non_dunk_rim_fgm/touch_rim_fga,
         self_created_rim_pct = 1 - rim_pct_assisted,
         rim_dunk_pct = dunks/rim_fga,
         rim_non_dunk_pct = non_dunk_rim_fga/rim_fga,
         non_dunk_rim_accuracy_spread = non_dunk_rim_accuracy - rim_accuracy,
         touch_rim_accuracy_spread = touch_rim_accuracy - rim_accuracy) %>%
  rename(player = player_x,
         team = team_abbreviation,
         games = games_played)  %>%
  # Add dunk percentage quartiles
  mutate(dunk_group = (case_when(rim_dunk_pct == 0 ~ 1,
                                 rim_dunk_pct < .1 ~ 2,
                                 rim_dunk_pct < .2 ~ 3,
                                 rim_dunk_pct < .3 ~ 4,
                                 rim_dunk_pct < .4 ~ 5,
                                 rim_dunk_pct >= .4 ~ 6))) %>%
  mutate(dunk_group2 = (case_when(rim_dunk_pct < .1 ~ 1,
                                  rim_dunk_pct < .225 ~ 2,
                                  rim_dunk_pct < .35 ~ 3,
                                  rim_dunk_pct >= .35 ~ 4))) %>%
  select(1:10,  contains("rim"), contains("dunk"), contains("minutes"), contains("games")) %>%
  mutate(player = str_replace(player, "P.J. Tucker", "PJ Tucker"),
         player = str_replace(player, "Marcus Morris Sr.", "Marcus Morris"),
         player = str_replace(player, "T.J. Warren", "TJ Warren"),
         player = str_replace(player, "Juan Hernangomez", "Juancho Hernangomez"),
         player = str_replace(player, "Kevin Knox II", "Kevin Knox"))


# Add positional averages for key metrics

rim_accuracy_pos_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season, pos_group) %>%
  summarize(rim_accuracy_pos_avg = mean(rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()

non_dunk_rim_accuracy_pos_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season, pos_group) %>%
  summarize(non_dunk_rim_accuracy_pos_avg = mean(non_dunk_rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()

touch_rim_accuracy_pos_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season, pos_group) %>%
  summarize(touch_rim_accuracy_pos_avg = mean(touch_rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()

# Add all player averages for key metrics

rim_accuracy_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season) %>%
  summarize(rim_accuracy_avg = mean(rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()

non_dunk_rim_accuracy_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season) %>%
  summarize(non_dunk_rim_accuracy_avg = mean(non_dunk_rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()

touch_rim_accuracy_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season) %>%
  summarize(touch_rim_accuracy_avg = mean(touch_rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()

# Add height averages for key metrics

rim_accuracy_height_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season, height_quartile) %>%
  summarize(rim_accuracy_height_avg = mean(rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()

non_dunk_rim_accuracy_height_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season, height_quartile) %>%
  summarize(non_dunk_rim_accuracy_height_avg = mean(non_dunk_rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()

touch_rim_accuracy_height_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season, height_quartile) %>%
  summarize(touch_rim_accuracy_height_avg = mean(touch_rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()

# Add dunk averages for key metrics

rim_accuracy_dunk_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season, dunk_group) %>%
  summarize(rim_accuracy_dunk_avg = mean(rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()

non_dunk_rim_accuracy_dunk_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season, dunk_group) %>%
  summarize(non_dunk_rim_accuracy_dunk_avg = mean(non_dunk_rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()

touch_rim_accuracy_dunk_avg <- merge_data1 %>%
  filter(rim_fga > 49) %>%
  group_by(season, dunk_group) %>%
  summarize(touch_rim_accuracy_dunk_avg = mean(touch_rim_accuracy, na.rm = T)) %>%
  drop_na() %>%
  as.data.frame()



rim_data <- merge_data1 %>%
  merge(rim_accuracy_avg, by =c('season')) %>%
  mutate(diff_vs_rim_accuracy = (rim_accuracy - rim_accuracy_avg)) %>%
  merge(non_dunk_rim_accuracy_avg, by =c('season')) %>%
  mutate(diff_vs_non_dunk_rim_accuracy = (non_dunk_rim_accuracy - non_dunk_rim_accuracy_avg)) %>%
  merge(touch_rim_accuracy_avg, by =c('season')) %>%
  mutate(diff_vs_touch_rim_accuracy = (touch_rim_accuracy - touch_rim_accuracy_avg)) %>%
  #Incorporate positional averages and differences
  merge(rim_accuracy_pos_avg, by =c('season', 'pos_group')) %>%
  mutate(diff_vs_pos_rim_accuracy = (rim_accuracy - rim_accuracy_pos_avg)) %>%
  merge(non_dunk_rim_accuracy_pos_avg, by =c('season', 'pos_group')) %>%
  mutate(diff_vs_pos_non_dunk_rim_accuracy = (non_dunk_rim_accuracy - non_dunk_rim_accuracy_pos_avg)) %>%
  merge(touch_rim_accuracy_pos_avg, by =c('season', 'pos_group')) %>%
  mutate(diff_vs_pos_touch_rim_accuracy = (touch_rim_accuracy - touch_rim_accuracy_pos_avg)) %>%
  merge(rim_accuracy_dunk_avg, by =c('season', 'dunk_group')) %>%
  mutate(diff_vs_dunk_rim_accuracy = (rim_accuracy - rim_accuracy_dunk_avg)) %>%
  merge(non_dunk_rim_accuracy_dunk_avg, by =c('season', 'dunk_group')) %>%
  mutate(diff_vs_dunk_non_dunk_rim_accuracy = (non_dunk_rim_accuracy - non_dunk_rim_accuracy_dunk_avg)) %>%
  merge(touch_rim_accuracy_dunk_avg, by =c('season', 'dunk_group')) %>%
  mutate(diff_vs_dunk_touch_rim_accuracy = (touch_rim_accuracy - touch_rim_accuracy_dunk_avg))

rim_data$pos_group <- factor(rim_data$pos_group, 
                             levels = c("1", "Wing", "4", "5"))

rim_data$dunk_group <- factor(rim_data$dunk_group, 
                              levels = c("1", "2", "3", "4", "5", "6"))

rim_data$dunk_group2 <- factor(rim_data$dunk_group2, 
                               levels = c("1", "2", "3", "4"))


write.csv(rim_data, file = "rim_data.csv")

