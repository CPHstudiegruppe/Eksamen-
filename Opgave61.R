library(mongolite)
library(dplyr)
library(tidyr)
library(jsonlite)
library(tibble)
library(ggplot2)


#Opgave 6.1 - Hermeneutik - mennesket bag facaden 

###Dataindhentning

#MongoDB forbindelser 
con_matches <- mongo("matches", db = "fb", url = "mongodb://localhost:27017")
con_events  <- mongo("events",  db = "fb", url = "mongodb://localhost:27017")
con_lineups <- mongo("lineups", db = "fb", url = "mongodb://localhost:27017")

#Henter alle kampe og deres matchid og udflader 
matches_df <- con_matches$find('{}') %>%
  jsonlite::flatten()

#Filtrerer efter FA Womens Super League og nyeste sæson 
matches_df <- matches_df %>%
  filter(
    `competition.competition_name` == "FA Women's Super League",
    `season.season_name` == "2020/2021"
  )

match_ids <- matches_df$match_id

#Henter lineups ud fra matchid
query_lineups <- toJSON(list("match_id" = list("$in" = match_ids)), auto_unbox = TRUE)
lineups_raw <- con_lineups$find(query = query_lineups)

#Unnester så hver spiller bliver sin egen rækker 
lineups_df <- lineups_raw %>%
  unnest_longer(lineup) %>%
  unnest_wider(lineup)

lineups_players_df <- lineups_df %>%
  select(player_id, player_name, match_id, team_id)

#Henter kun skud og afleveringer for kvinderne
query_events <- toJSON(list(
  "match_id" = list("$in" = match_ids),
  "type.name" = list("$in" = c("Shot", "Pass"))
), auto_unbox = TRUE)

events_df <- con_events$find(query = query_events) %>%
  jsonlite::flatten()

#Merger al data sammen 
combined_df <- events_df %>%
  left_join(matches_df, by = "match_id")

###Optælling 

#Tæller antal mål for hver spiller
mål <- combined_df %>%
  filter(`type.name` == "Shot", `shot.outcome.name` == "Goal") %>%
  count(player.id, player.name, name = "mål")

#Tæller antal afleveringer der førte til skud 
assists <- combined_df %>%
  filter(`type.name` == "Pass", `pass.shot_assist` == TRUE) %>%
  count(player.id, player.name, name = "assists")

#Tæller antal kampe for hver spiller 
kampe <- combined_df %>%
  select(player.id, player.name, match_id) %>%
  distinct() %>%
  count(player.id, player.name, name = "kampe")

#Top 10 ud fra effektivitet pr kamp ift assists og mål 
top10 <- full_join(mål, assists, by = c("player.id", "player.name")) %>%
  left_join(kampe, by = c("player.id", "player.name")) %>%
  mutate(across(c(mål, assists), ~replace_na(., 0)),
         score = mål + 0.7 * assists,
         mål_per_kamp = round(mål / kampe, 2),
         assists_per_kamp = round(assists / kampe, 2),
         score_per_kamp = round(score / kampe, 2)) %>%
  filter(kampe >= 10) %>%
  arrange(desc(score_per_kamp)) %>%
  slice_head(n = 10)

#Plot
ggplot(top10, aes(x = reorder(player.name, score_per_kamp), y = score_per_kamp)) +
  geom_col(fill = "skyblue4") +
  coord_flip() +
  labs(
    title = "Top 10 spillere i FA WSL 2020/2021",
    subtitle = "Baseret på vægtet score (mål + 0.7 × assists) pr. kamp",
    x = "Spiller",
    y = "Score per kamp",
    caption = "Kilde: StatsBomb-data via MongoDB"    
  ) +
  theme_minimal()

###Nye variabler 

#Emotional Load Score 

#Laver modstandere og deres kategorier
tophold <- c("Arsenal WFC", "Manchester City WFC", "Manchester United WFC", "Chelsea FCW")
midterhold <- c("Everton LFC", "West Ham United LFC", "Tottenham Hotspur WFC")

#Fake data: 5 spillere × 3 kampe
emotional_load_df <- tibble(
  match_id = rep(1001:1003, each = 5),
  player_id = rep(c(4641, 4961, 15570, 4645, 31539), times = 3),
  player_name = rep(c("Fran Kirby", "Samantha Kerr", "Chloe Kelly", 
                      "Isobel Christiansen", "Leah Galton"), times = 3),
  opponent = rep(c("Arsenal WFC", "Reading WFC", "Manchester United WFC"), each = 5),
  medieomtale_total = sample(5:40, size = 15, replace = TRUE)
) %>%
  mutate(
    kampvigtighed = case_when(
      opponent %in% tophold ~ 3,
      opponent %in% midterhold ~ 2,
      TRUE ~ 1
    ),
    medie_score = case_when(
      medieomtale_total < 10 ~ 1,
      medieomtale_total < 25 ~ 2,
      TRUE ~ 3
    ),
    emotional_load_score = kampvigtighed * medie_score
  )

#Life Context Score 

#Laver modstandere og deres kategorier
tophold <- c("Arsenal WFC", "Manchester City WFC", "Manchester United WFC", "Chelsea FCW")
midterhold <- c("Everton LFC", "West Ham United LFC", "Tottenham Hotspur WFC")

#Fake data
life_context_df <- tibble(
  player_name = c("Fran Kirby", "Samantha Kerr", "Chloe Kelly", 
                  "Isobel Christiansen", "Leah Galton"),
  familiekrise = c(TRUE, FALSE, FALSE, TRUE, FALSE),
  skadeshistorik = c(TRUE, TRUE, FALSE, FALSE, TRUE),
  flyttet_nyligt = c(FALSE, TRUE, FALSE, FALSE, FALSE),
  kontrakt_udløber_snart = c(TRUE, FALSE, TRUE, FALSE, TRUE)
) %>%
  mutate(
    life_context_score = 
      1 * familiekrise +
      1 * skadeshistorik +
      1 * flyttet_nyligt +
      1 * kontrakt_udløber_snart
  )




