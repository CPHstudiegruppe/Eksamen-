# 📦 Pakker og databaseforbindelse --------------------------------------

library(tidyverse)
library(progress)
library(ggplot2)
library(DBI)
library(RMariaDB)

# Opret forbindelse til databasen
con <- dbConnect(
  MariaDB(),
  dbname = "wyscout",
  host = "127.0.0.1",
  user = "root",
  password = Sys.getenv("localpw"),
  port = 3306
)

# 📥 Indlæs tabeller -----------------------------------------------------

teams <- dbReadTable(con, "wyscout_teams_sl")
matchformations <- dbReadTable(con, "wyscout_matchformations_sl")
matchdetail_base <- dbReadTable(con, "wyscout_matchdetail_base_sl")
shots <- dbReadTable(con, "wyscout_matchevents_shots_sl")
matchevents_common <- dbReadTable(con, "wyscout_matchevents_common_sl")

# 💥 Filtrer skud-events -------------------------------------------------

matchevents_shots <- matchevents_common %>% 
  filter(PRIMARYTYPE == "shot")

joined_shots <- left_join(matchevents_shots, shots, by = "EVENT_WYID")

teams_subset <- teams %>%
  select(TEAM_WYID, OFFICIALNAME) %>%
  distinct(TEAM_WYID, .keep_all = TRUE)

joined_shots_teams <- left_join(joined_shots, teams_subset, by = "TEAM_WYID")

# Filtrér sæson 2023/24
joined_shots_teams <- joined_shots_teams %>% 
  filter(SEASON_WYID == 188945)

# ⚽ Simuler kampe og beregn xP -----------------------------------------

# Funktion: simuler én kamp

sim_game <- function(mid, ht, at, joined_shots_teams) {
  oneMatch <- joined_shots_teams %>% filter(MATCH_WYID.x == mid)
  home_shots <- oneMatch %>% filter(TEAM_WYID == ht)
  away_shots <- oneMatch %>% filter(TEAM_WYID == at)
  
  home_goals <- sum(runif(nrow(home_shots)) < home_shots$SHOTXG)
  away_goals <- sum(runif(nrow(away_shots)) < away_shots$SHOTXG)
  
  if (home_goals > away_goals) return("home")
  else if (away_goals > home_goals) return("away")
  else return("draw")
}

# Funktion: beregn expected points

get_xp <- function(mid, ht, at, joined_shots_teams, n = 500) {
  results <- replicate(n, sim_game(mid, ht, at, joined_shots_teams))
  
  home_wins <- sum(results == "home")
  away_wins <- sum(results == "away")
  draws <- sum(results == "draw")
  
  home_points <- (home_wins * 3 + draws) / n
  away_points <- (away_wins * 3 + draws) / n
  
  return(c(home_points, away_points))
}

# 📋 Lav match-liste ------------------------------------------------------

match_list_season <- joined_shots_teams %>%
  select(
    MATCH_WYID = MATCH_WYID.x,
    TEAM_WYID,
    OFFICIALNAME
  ) %>%
  distinct() %>%
  group_by(MATCH_WYID) %>%
  filter(n() == 2) %>%
  summarise(
    home_team_id = first(TEAM_WYID),
    away_team_id = last(TEAM_WYID),
    home_name = first(OFFICIALNAME),
    away_name = last(OFFICIALNAME),
    .groups = "drop"
  )

# Progressbar
pb <- txtProgressBar(min = 0, max = nrow(match_list_season), style = 3)

# 🎲 Loop over kampene og beregn xP -------------------------------------

xp_results_season <- purrr::pmap_dfr(
  .l = list(
    match_id = match_list_season$MATCH_WYID,
    home_team = match_list_season$home_team_id,
    away_team = match_list_season$away_team_id
  ),
  .f = function(match_id, home_team, away_team) {
    xp <- get_xp(match_id, home_team, away_team, joined_shots_teams, n = 500)
    i <- which(match_list_season$MATCH_WYID == match_id)
    setTxtProgressBar(pb, i)
    tibble(
      MATCH_WYID = match_id,
      home_team = match_list_season$home_name[i],
      away_team = match_list_season$away_name[i],
      xp_home = round(xp[1], 2),
      xp_away = round(xp[2], 2)
    )
  }
)

# 🧮 Udregn xP-summer pr. hold -------------------------------------------

xp_summary_season <- xp_results_season %>%
  group_by(home_team) %>%
  summarise(Home_xP = sum(xp_home), .groups = "drop") %>%
  full_join(
    xp_results_season %>%
      group_by(away_team) %>%
      summarise(Away_xP = sum(xp_away), .groups = "drop"),
    by = c("home_team" = "away_team")
  ) %>%
  mutate(
    Total_xP = Home_xP + Away_xP,
    Team = home_team
  ) %>%
  select(Team, Home_xP, Away_xP, Total_xP) %>%
  arrange(desc(Total_xP))

# 🧩 Tilføj SEASON_WYID fra skuddata til kampdata ------------------------

matchdetail_base_season <- left_join(
  matchdetail_base,
  matchevents_shots %>% select(MATCH_WYID, SEASON_WYID) %>% distinct(),
  by = "MATCH_WYID"
) %>%
  filter(SEASON_WYID == 188945)

# 🧾 Find faktiske point fra kampresultater ------------------------------

teams <- teams %>% distinct(TEAM_WYID, .keep_all = TRUE)

joined_teams <- left_join(matchdetail_base_season, teams, by = "TEAM_WYID") %>%
  filter(MATCH_WYID != 5584052) %>%  # Fjern playoff-kamp
  select(MATCH_WYID, SIDE, OFFICIALNAME, SCORE) %>%
  pivot_wider(
    names_from = SIDE,
    values_from = c(SCORE, OFFICIALNAME),
    names_glue = "{SIDE}_{.value}"
  ) %>%
  mutate(
    home_points = case_when(
      home_SCORE > away_SCORE ~ 3,
      home_SCORE == away_SCORE ~ 1,
      TRUE ~ 0
    ),
    away_points = case_when(
      away_SCORE > home_SCORE ~ 3,
      home_SCORE == away_SCORE ~ 1,
      TRUE ~ 0
    )
  )

# 🧮 Saml home/away/total point per hold ---------------------------------

home_points_df <- joined_teams %>%
  select(OFFICIALNAME = home_OFFICIALNAME, points = home_points) %>%
  group_by(OFFICIALNAME) %>%
  summarise(HomePoints = sum(points, na.rm = TRUE), .groups = "drop")

away_points_df <- joined_teams %>%
  select(OFFICIALNAME = away_OFFICIALNAME, points = away_points) %>%
  group_by(OFFICIALNAME) %>%
  summarise(AwayPoints = sum(points, na.rm = TRUE), .groups = "drop")

team_points <- full_join(home_points_df, away_points_df,
                         by = "OFFICIALNAME") %>%
  mutate(
    HomePoints = replace_na(HomePoints, 0),
    AwayPoints = replace_na(AwayPoints, 0),
    TotalPoints = HomePoints + AwayPoints
  )

# 🧮 Kombiner xP og faktiske point ---------------------------------------

xp_summary_season <- xp_summary_season %>%
  rename(OFFICIALNAME = Team)

combined_summary <- left_join(team_points, xp_summary_season, by = "OFFICIALNAME") %>%
  select(OFFICIALNAME, HomePoints, AwayPoints, TotalPoints, Total_xP) %>%
  arrange(desc(TotalPoints))

# 🔍 Udregn over-/underperformance ---------------------------------------

ou_table <- combined_summary %>%
  mutate(O_U = TotalPoints - Total_xP) %>%
  arrange(O_U)

# 📊 Plot over-/underperformance -----------------------------------------

ggplot(ou_table, aes(x = O_U, y = reorder(OFFICIALNAME, O_U))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Brøndby overperformer – Midtjylland lige i hælene",
    subtitle = "Performance beregnet som forskellen mellem faktiske point og expected points (xP)",
    caption = "Kilde: Wyscout, egne beregninger",
    x = "Difference",
    y = "Hold"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "#0e1117"),
    plot.caption = element_text(size = 8.5, color = "#0e1117"),
    axis.text = element_text(color = "#0e1117"),
    axis.title = element_text(color = "#0e1117"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
