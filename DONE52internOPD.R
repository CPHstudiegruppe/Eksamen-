library(shiny)
library(dplyr)
library(tidyr)
library(jsonlite)
library(mongolite)
library(ggplot2)
library(ggsoccer)
library(ggimage)
library(tibble)
library(ggplot2)


###Dataindhentning 
# Opretter forbindelser til MongoDB
con_events     <- mongo(collection = "events",     db = "fb", url = "mongodb://localhost")
con_maleevents <- mongo(collection = "maleevents", db = "fb", url = "mongodb://localhost")
con_matches    <- mongo(collection = "matches",    db = "fb", url = "mongodb://localhost")
con_lineup     <- mongo(collection = "lineups",    db = "fb", url = "mongodb://localhost")

# Laver et indeks på matchid i begge events samlinger 
con_events$index(add = '{"match_id": 1}')
con_maleevents$index(add = '{"matchId": 1}')

# Definerer skandinaviske lande 
scandi_lande <- c("Denmark", "Sweden", "Norway", "Iceland")

# Henter alle kampe ud fra matches og flader dem ud 
matches <- flatten(con_matches$find('{}'))

# Filtrerer på kvindekampe med skandinavisk land som enten hjemme eller ude hold, opretter kamplabel til Shiny
matches_kvinder <- matches %>%
  filter(`home_team.country.name` %in% scandi_lande | `away_team.country.name` %in% scandi_lande) %>%
  filter(`home_team.home_team_gender` == "female" & `away_team.away_team_gender` == "female") %>%
  mutate(køn = "Kvinder",
         kamp_label = paste0(`home_team.country.name`, " ", home_score, "–", away_score, " ", `away_team.country.name`))

# Filtrerer på mandekampe med skandinavisk land som enten hjemme eller ude hold, opretter kamplabel, fjerner enkelt kamp 
matches_mænd <- matches %>%
  filter(`home_team.country.name` %in% scandi_lande | `away_team.country.name` %in% scandi_lande) %>%
  filter(`home_team.home_team_gender` == "male" & `away_team.away_team_gender` == "male") %>%
  mutate(køn = "Mænd",
         kamp_label = paste0(`home_team.country.name`, " ", home_score, "–", away_score, " ", `away_team.country.name`)) %>% 
  filter(match_id != "3888716")

# Gemmer matchid til skududhentning 
kvinde_ids <- matches_kvinder$match_id
mande_ids  <- matches_mænd$match_id

# Looper igennem alle kampid'er for kvinder og finder alle skud, hvor der findes en freezeframe, udflader og opretter kønskolonne og samler i df
shots_f <- data.frame()
for (i in 1:length(kvinde_ids)) {
  query <- paste0('{ "match_id": ', kvinde_ids[i], ', "type.name": "Shot", "shot.freeze_frame": { "$exists": true }}')
  d <- con_events$find(query)
  if (nrow(d) > 0) {
    d <- flatten(d)
    d$køn <- "Kvinder"
    shots_f <- bind_rows(shots_f, d)
  }
}

# Looper igennem alle kampid'er for mænd og finder alle skud med en tilhørende freezeframe, udflader og opretter kønskolonne og samler i df 
shots_m <- data.frame()
for (i in 1:length(mande_ids)) {
  query <- paste0('{ "matchId": ', mande_ids[i], ', "type.name": "Shot", "shot.freeze_frame": { "$exists": true }}')
  d <- con_maleevents$find(query)
  if (nrow(d) > 0) {
    d <- flatten(d)
    d$match_id <- d$matchId 
    d$køn <- "Mænd"
    shots_m <- bind_rows(shots_m, d)
  }
}

# Tilføjer kampnavne til de to dataframes med skud for hvert køn 
shots_f <- left_join(shots_f, matches_kvinder %>% select(match_id, kamp_label, køn), by = c("match_id", "køn"))
shots_m <- left_join(shots_m, matches_mænd %>% select(match_id, kamp_label, køn), by = c("match_id", "køn"))

# Henter alle data om spillere i kampen og udflader 
allLineups <- unnest(con_lineup$find(query = '{}', fields = '{}'))
# Laver opslagstabel så man nemt kan finde trøjenr ud fra navn 
player_lookup <- setNames(allLineups$jersey_number, allLineups$player_name)


###Opretter hjælpefunktioner 
# Funktion der beregner om en modstander står inde i skyttens måltrekant 
is_opponent_inside_triangle <- function(ox, oy, px, py) {
  gx1 <- 120; gy1 <- 36; gx2 <- 120; gy2 <- 44
  A <- abs((px*(gy1 - gy2) + gx1*(gy2 - py) + gx2*(py - gy1)) / 2)
  A1 <- abs((ox*(gy1 - gy2) + gx1*(gy2 - oy) + gx2*(oy - gy1)) / 2)
  A2 <- abs((px*(oy - gy2) + ox*(gy2 - py) + gx2*(py - oy)) / 2)
  A3 <- abs((px*(gy1 - oy) + gx1*(oy - py) + ox*(py - gy1)) / 2)
  abs(A - (A1 + A2 + A3)) < 1
}

# Funktion for at se om modstander står i vejen for en aflevering 
is_defender_blocking_pass <- function(x1, y1, x2, y2, dx, dy) {
  A <- c(x1, y1); B <- c(x2, y2); P <- c(dx, dy)
  AB <- B - A; AP <- P - A; ab_len <- sqrt(sum(AB^2))
  if (ab_len == 0) return(FALSE)
  proj_len <- sum(AP * AB) / ab_len
  if (proj_len < 0 || proj_len > ab_len) return(FALSE)
  closest <- A + (proj_len / ab_len) * AB
  sqrt(sum((P - closest)^2)) < 1.5
}

# Funktion til at beregne om et skud var egoistisk - Funktion laver dataframe med skud og ff og returnerer udvidet version med ego og egotargets
beregn_ego <- function(data) {
  data$ego <- NA # Opretter tom kolonne til ego true/false 
  data$ego_targets <- vector("list", nrow(data)) #Opretter liste med linjer til bedre medspillere 
  
  for (i in 1:nrow(data)) { #Et skud af gangen 
    skud <- data[i, ]
    
    if (is.null(skud$location[[1]]) || is.null(skud$shot.freeze_frame[[1]])) next # Spring over hvis der mangler koordinater fra skytte eller medspillere 
    
    shooter_x <- skud$location[[1]][1] #Gem skyttens position 
    shooter_y <- skud$location[[1]][2]
    
    ff <- as.data.frame(skud$shot.freeze_frame[[1]]) #Freezeframeliste laves om til df
    ff$x <- rep(NA, nrow(ff)) #To nye kolonner hvor spiller positioner gemmes
    ff$y <- rep(NA, nrow(ff))
    
    for (j in 1:nrow(ff)) { #Loop, koordinater står som tekst splittes de, hvis de står som liste læses de direkte = få alle spiller positioner ind i x og y i talform 
      loc <- ff$location[[j]]
      if (is.character(loc)) {
        spl <- strsplit(loc, ",")[[1]]
        ff$x[j] <- as.numeric(spl[1])
        ff$y[j] <- as.numeric(spl[2])
      } else {
        ff$x[j] <- loc[[1]]
        ff$y[j] <- loc[[2]]
      }
    }
    
    ff <- ff[!is.na(ff$x) & !is.na(ff$y), ] #Skiller efter om spiller er en medspiller eller modstander 
    teammates <- ff[ff$teammate == TRUE, ]
    opponents <- ff[ff$teammate == FALSE, ]
    
    if (nrow(teammates) < 1 || nrow(opponents) < 1) next
    
    shooter_opps <- 0 #Tjekker modstandere en ad gangen med funktion for at se om modstander er mellem skytten og målet 
    for (k in 1:nrow(opponents)) {
      if (is_opponent_inside_triangle(opponents$x[k], opponents$y[k], shooter_x, shooter_y)) {
        shooter_opps <- shooter_opps + 1
      }
    }
    
    ego_flag <- FALSE #Sættes til true hvis mindst en medspiller var bedre placeret 
    ego_lines <- list() #Indeholder linjer fra skytten til medspiller 
    
    # Sætter radius
    radius <- 20
    
    for (j in 1:nrow(teammates)) { #For hver medspiller
      tx <- teammates$x[j] #Hent deres koordinater
      ty <- teammates$y[j]
      afst <- sqrt((shooter_x - tx)^2 + (shooter_y - ty)^2) #Beregn afstand fra skytten 
      
      teammate_opps <- 0 #Tæl modstandere foran medspilleren 
      for (k in 1:nrow(opponents)) {
        if (is_opponent_inside_triangle(opponents$x[k], opponents$y[k], tx, ty)) {
          teammate_opps <- teammate_opps + 1
        }
      }
      
      blocked <- FALSE #Se om afleveringen er blokeret, hvis blot en modstander står i vejen = tæller ikke 
      for (k in 1:nrow(opponents)) {
        if (is_defender_blocking_pass(shooter_x, shooter_y, tx, ty, opponents$x[k], opponents$y[k])) {
          blocked <- TRUE
          break
        }
      }
      
      if (((skud$køn == "Kvinder" && teammate_opps < shooter_opps) || 
           (skud$køn == "Mænd" && teammate_opps < shooter_opps)) && 
          afst < radius && !blocked) {
        ego_flag <- TRUE #Gemmer linje til senere visualisering 
        ego_lines[[length(ego_lines) + 1]] <- data.frame(x = c(shooter_x, tx), y = c(shooter_y, ty))
      }
    }
    
    data$ego[i] <- ego_flag #Bliver true/false for skuddet
    data$ego_targets[[i]] <- ego_lines #bliver en eller flere linjer hvis ego, ellers tom 
  }
  
  return(data) #Inputdata tilbage med ekstra kolonner 
}

# Beregn ego-skud
shots_f_final <- beregn_ego(shots_f)
shots_m_final <- beregn_ego(shots_m)

# Filter for spillere fra skandinaviske lande
scandi_f <- shots_f_final %>% 
  filter(grepl("Denmark|Sweden|Norway|Iceland", team.name), !is.na(ego))

scandi_m <- shots_m_final %>% 
  filter(team.name %in% c("Denmark", "Sweden", "Norway", "Iceland"), !is.na(ego))

# Lav datasæt med andele
ego_andel <- tibble(
  Køn = c("Kvinder", "Mænd"),
  Andel = c(
    mean(scandi_f$ego),
    mean(scandi_m$ego)
  )
)


# Plot søjlediagram
ggplot(ego_andel, aes(x = Køn, y = Andel, fill = Køn)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(Andel * 100, 1), "%")), 
            vjust = -0.5, size = 5) +
  ylim(0, 1) +
  labs(
    title = "Minimal forskel i andel af egoskud mellem kønnene",
    subtitle = "Baseret på skandinaviske hold",
    y = "Andel ego-skud i procent",
    x = "",
    caption = "Kilde: StatsBomb via MongoDB"
  ) +
  scale_fill_manual(values = c("Kvinder" = "indianred", "Mænd" = "skyblue4")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(hjust = 0, face = "bold"), 
    plot.subtitle = element_text(hjust = 0), 
    plot.caption = element_text(hjust = 1, size = 10, face = "italic", margin = margin(t = 10))
  )


