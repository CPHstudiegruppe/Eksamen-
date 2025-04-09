# Opgave 3.1 - Afleveringer clustering
library(tidyverse)

# Vi henter data
wyscout_matchevents_passes_sl <- read_csv("C:/Users/emil.bengtsen/OneDrive - ferm LIVING ApS/Skrivebord/Eksamen/R.data/r.data/wyscout_matchevents_passes_sl.csv")
wyscout_matchevents_secondarytype_sl <- read_csv("C:/Users/emil.bengtsen/OneDrive - ferm LIVING ApS/Skrivebord/Eksamen/R.data/r.data/wyscout_matchevents_secondarytype_sl.csv")
wyscout_players_sl <- read_csv("C:/Users/emil.bengtsen/OneDrive - ferm LIVING ApS/Skrivebord/Eksamen/R.data/r.data/wyscout_players_sl.csv")
wyscout_matchevents_common_sl <- read_csv("C:/Users/emil.bengtsen/OneDrive - ferm LIVING ApS/Skrivebord/Eksamen/R.data/r.data/wyscout_matchevents_common_sl.csv")
wyscout_teams_sl <- read_csv("C:/Users/emil.bengtsen/OneDrive - ferm LIVING ApS/Skrivebord/Eksamen/R.data/r.data/wyscout_teams_sl.csv")

#----------------------#
# Vi renser vores data #
#----------------------#
{
# Rens player data - fjerner duplikater baseret på PLAYER_WYID
wyscout_players_sl_clean <- wyscout_players_sl %>%
  distinct(PLAYER_WYID, .keep_all = TRUE)

# Merge de to hoveddataframes - Hold kun nødvendige kolonner
allpasses <- wyscout_matchevents_secondarytype_sl %>%
  inner_join(wyscout_matchevents_passes_sl, by = "EVENT_WYID") %>%
  inner_join(wyscout_matchevents_common_sl, by = "EVENT_WYID") %>%
  mutate(MATCH_WYID = coalesce(MATCH_WYID.x, MATCH_WYID.y)) %>% # Kombinerer kolonnerne til én MATCH_WYID
  select(-MATCH_WYID.x, -MATCH_WYID.y) %>%  # Fjern de oprindelige kolonner
  select(EVENT_WYID, MATCH_WYID, everything())  # Arranger kolonnerne pænt

# Merge players
allplayers <- wyscout_players_sl_clean %>%
  inner_join(wyscout_matchdetail_players_sl, by = "PLAYER_WYID") %>% 
  filter(ROLENAME != "Goalkeeper")

# Endelig merge 
allpasses_TRUE <- allpasses %>%
  inner_join(allplayers, by = c("MATCH_WYID", "PLAYER_WYID")) %>%  # <-- VIGTIG
  filter(str_detect(PRIMARYTYPE.x, "pass"))

# Vi udvælger vores kolonner 
allpasses_cluster_all <- allpasses_TRUE %>% 
  select(MATCH_WYID,
         PRIMARYTYPE.x,
         ACCURATE,
         ANGLE,
         LENGTH,
         ENDLOCATIONX,
         ENDLOCATIONY,
         LOCATIONX,
         LOCATIONY,
         TEAM_WYID.x,
         SHORTNAME,
         ROLENAME,
         POSSESSIONDURATION)

# Vi laver en df med variable, så vi kan se spillere osv.
allpasses_cluster_mod <- allpasses_cluster_all %>%
  filter(
    !( (ENDLOCATIONX == 0 & ENDLOCATIONY == 0) |
         (ENDLOCATIONX == 100 & ENDLOCATIONY == 100) |
         (LOCATIONX == 0 & LOCATIONY == 0) |
         (LOCATIONX == 100 & LOCATIONY == 100) )
  ) 


# Vi laver en df kun med vors variable til K-means
allpasses_cluster <- allpasses_cluster_all[, 4:9]

allpasses_cluster_clean <- allpasses_cluster_all %>%
  filter(
    !( (ENDLOCATIONX == 0 & ENDLOCATIONY == 0) |
         (ENDLOCATIONX == 100 & ENDLOCATIONY == 100) |
         (LOCATIONX == 0 & LOCATIONY == 0) |
         (LOCATIONX == 100 & LOCATIONY == 100) )
  )

# Standardisér
allpasses_cluster_scaled <- scale(allpasses_cluster_clean)

#Finder afleveringer der skydes fra enten det ene hjørneflag til det andet
{
corner_counts <- allpasses_cluster %>%
  filter(
    (ENDLOCATIONX == 0 & ENDLOCATIONY == 0) |
      (ENDLOCATIONX == 100 & ENDLOCATIONY == 100)
  ) %>%
  mutate(hjørne = case_when(
    ENDLOCATIONX == 0 & ENDLOCATIONY == 0 ~ "Hjørne (0,0)",
    ENDLOCATIONX == 100 & ENDLOCATIONY == 100 ~ "Hjørne (100,100)"
  )) %>%
  group_by(hjørne) %>%
  summarise(count = n(), .groups = "drop")


Hjørne (0,0)
893

Hjørne (100,100)
802
}

}

#---------------------#
# Vi laver clustering #
#---------------------#
{
# Brug kmeans
kmeans_model <- kmeans(allpasses_cluster_scaled, centers = 4)

k = 4

wss <- sapply(1:7, function(k) {
  kmeans(allpasses_cluster_clean, centers = k)$tot.withinss
})


#Plot Elbow-metoden
plot(1:7, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Antal Clusters (k)", ylab = "WSS (Within Sum of Squares)",
     main = "Elbow-metoden for optimal k")

# Brug kmeans
kmeans_model <- kmeans(allpasses_cluster_scaled, centers = k)

allpasses_cluster_labeled <- allpasses_cluster_clean %>%
  mutate(cluster = kmeans_model$cluster)

library(dplyr)
library(ggplot2)

# Sørg for at cluster er faktor
allpasses_cluster_labeled$cluster <- as.factor(allpasses_cluster_labeled$cluster)

# Beregn gennemsnitlig vinkel for hver cluster
avg_angle_cluster <- allpasses_cluster_labeled %>%
  group_by(cluster) %>%
  summarise(mean_angle = mean(ANGLE, na.rm = TRUE))
}


#-------------------------#
# Dataframes for clusters #
#-------------------------#
{
# Cluster 1
Cluster1 <- allpasses_cluster_labeled %>% 
  filter(cluster == 1)

Cluster1_avg <- Cluster1 %>% 
  summarize(across(c(ANGLE, 
                     LENGTH, 
                     ENDLOCATIONX, 
                     ENDLOCATIONY, 
                     LOCATIONX, 
                     LOCATIONY), 
                   mean, na.rm = TRUE))
# Cluster 2
Cluster2 <- allpasses_cluster_labeled %>% 
  filter(cluster == 2)

Cluster2_avg <- Cluster2 %>% 
  summarize(across(c(ANGLE, 
                     LENGTH, 
                     ENDLOCATIONX, 
                     ENDLOCATIONY, 
                     LOCATIONX, 
                     LOCATIONY), 
                   mean, na.rm = TRUE))
# Cluster 3
Cluster3 <- allpasses_cluster_labeled %>% 
  filter(cluster == 3)

Cluster3_avg <- Cluster3 %>% 
  summarize(across(c(ANGLE, 
                     LENGTH, 
                     ENDLOCATIONX, 
                     ENDLOCATIONY, 
                     LOCATIONX, 
                     LOCATIONY), 
                   mean, na.rm = TRUE))
# Cluster 4
Cluster4 <- allpasses_cluster_labeled %>% 
  filter(cluster == 4)

Cluster4_avg <- Cluster4 %>% 
  summarize(across(c(ANGLE, 
                     LENGTH, 
                     ENDLOCATIONX, 
                     ENDLOCATIONY, 
                     LOCATIONX, 
                     LOCATIONY), 
                   mean, na.rm = TRUE))

# Alle clusters
all_clusters_avg <- allpasses_cluster_labeled %>% 
  group_by(cluster) %>% 
  summarize(across(c(ANGLE,
                     LENGTH,
                     ENDLOCATIONX,
                     ENDLOCATIONY,
                     LOCATIONX,
                     LOCATIONY),
                     mean, na.rm = TRUE))
}

#-------------------#
# Plot for clusters #
#-------------------#
{
# Indlæs nødvendige pakker
library(ggplot2)
library(ggsoccer)

# Opret fodboldbanen og plot afleveringerne med farver baseret på cluster
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "black", fill = "white") +
  geom_segment(data = all_clusters_avg_2, 
               aes(x = LOCATIONX, y = LOCATIONY, xend = ENDLOCATIONX, yend = ENDLOCATIONY, color = cluster),
               arrow = arrow(length = unit(0.15, "cm")), size = 1) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) + # Tilpas farverne efter behov
  theme_pitch() +
  coord_flip(xlim = c(0, 100), ylim = c(0, 100)) +
  ggtitle("Afleveringer visualiseret på fodboldbane med clusters") +
  theme(legend.position = "right") # Placerer legeneden til højre
}

# Plot
{
ggplot(avg_angle_cluster, aes(x = cluster, y = mean_angle, fill = cluster)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Gennemsnitlig vinkel pr. cluster",
    x = "Cluster",
    y = "Gennemsnitlig vinkel (°)"
  ) +
  theme_minimal()


library(ggplot2)

angle_dist <- allpasses_cluster_labeled %>%
  mutate(angle_round = round(ANGLE / 10) * 10) %>%
  count(cluster, angle_round)

ggplot(angle_dist, aes(x = factor(angle_round), y = n, fill = cluster)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_polar(start = pi) +
  facet_wrap(~ cluster) +
  labs(title = "Vinkelretninger fordelt på cluster", x = "Vinkel (°)", y = "Antal") +
  theme_minimal()

avg_location_cluster <- allpasses_cluster_labeled %>%
  group_by(cluster) %>%
  summarise(
    mean_x = mean(LOCATIONX, na.rm = TRUE),
    mean_y = mean(LOCATIONY, na.rm = TRUE),
    mean_xend = mean(ENDLOCATIONX, na.rm = TRUE),
    mean_yend = mean(ENDLOCATIONY, na.rm = TRUE)
  )

library(ggsoccer)

ggplot() +
  annotate_pitch(dimensions = pitch_wyscout, fill = "darkgreen", colour = "white") +
  geom_point(data = avg_location_cluster, aes(x = mean_x, y = mean_y, color = factor(cluster)), size = 6) +
  geom_text(data = avg_location_cluster, aes(x = mean_x, y = mean_y, label = cluster), color = "white", size = 4, vjust = -1) +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  theme_pitch() +
  labs(title = "Gennemsnitlige startpositioner pr. cluster",
       x = "Banelængde (%)", y = "Banebredde (%)", color = "Cluster") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

ggplot() +
  annotate_pitch(dimensions = pitch_wyscout, fill = "darkgreen", colour = "white") +
  geom_point(data = avg_location_cluster, aes(x = mean_xend, y = mean_yend, color = factor(cluster)), size = 6) +
  geom_text(data = avg_location_cluster, aes(x = mean_xend, y = mean_yend, label = cluster), color = "white", size = 4, vjust = -1) +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  theme_pitch() +
  labs(title = "Gennemsnitlige slutpositioner pr. cluster",
       x = "Banelængde (%)", y = "Banebredde (%)", color = "Cluster") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

}

# ---------------------------------------#
# Hvilke spillere ligger i hvilke klynger#
# ---------------------------------------#
{
# Vi laver en df med alle relevante variable
Players_Cluster <- allpasses_cluster_labeled2 %>% 
  left_join(allpasses_cluster_mod, by = c("ANGLE", "LENGTH", "ENDLOCATIONX", "ENDLOCATIONY", "LOCATIONX", "LOCATIONY"))

# Vi joiner teams
teams <- wyscout_teams_sl %>% 
  distinct(TEAM_WYID, .keep_all = TRUE) %>% 
  select(TEAMNAME, TEAM_WYID)

Players_Cluster <- Players_Cluster %>%
  left_join(teams %>% select(TEAM_WYID, TEAMNAME), 
            by = c("TEAM_WYID.x" = "TEAM_WYID"))

# Vi fjerner alle duplikationer fra tidligere join
Players_Cluster <- Players_Cluster %>%
  distinct(.keep_all = TRUE)

# Vi laver en df for alle clusters
Players_Cluster_1 <- Players_Cluster %>% 
  filter(cluster == 1)

Players_Cluster_2 <- Players_Cluster %>% 
  filter(cluster == 2)

Players_Cluster_3 <- Players_Cluster %>% 
  filter(cluster == 3)

Players_Cluster_4 <- Players_Cluster %>% 
  filter(cluster == 4)

Players_Cluster_5 <- Players_Cluster %>% 
  filter(cluster == 5)

# Nu laver vi statistik på de forskellige clusters

# Cluster 1
# gennemsnit og sdv
Players_Cluster_1 %>% 
  summarise(
    Total_Passes = n(),
    Successful_Passes = sum(ACCURATE, na.rm = TRUE),
    Success_Rate = round((Successful_Passes / Total_Passes) * 100, 2),
    Length_SD = sd(LENGTH, na.rm = TRUE),            # Standardafvigelse for LENGTH
    Angle_SD = sd(ANGLE, na.rm = TRUE)               # Standardafvigelse for ANGLE
)

# Rolenames + Pass accurate
Players_Cluster_1 %>%
  group_by(ROLENAME, TEAMNAME) %>%
  summarise(
    Total_Passes = n(),
    Successful_Passes = sum(ACCURATE, na.rm = TRUE),
    Success_Rate = round((Successful_Passes / Total_Passes) * 100, 2),
    Length_SD = sd(LENGTH, na.rm = TRUE),            # Standardafvigelse for LENGTH
    Angle_SD = sd(ANGLE, na.rm = TRUE)               # Standardafvigelse for ANGLE
  ) %>%
  arrange(desc(Total_Passes)) %>%
  print(n = Inf)


# Spillere
Players_Cluster_1 %>% 
  group_by(SHORTNAME, TEAMNAME) %>% 
  summarize(
    Total_Passes = n(),
    Successful_Passes = sum(ACCURATE, na.rm = TRUE),
    Success_Rate = round((Successful_Passes / Total_Passes) * 100, 2),
    Length_SD = sd(LENGTH, na.rm = TRUE),            # Standardafvigelse for LENGTH
    Angle_SD = sd(ANGLE, na.rm = TRUE)               # Standardafvigelse for ANGLE
  ) %>%
  arrange(desc(Total_Passes))
print(n = Inf)  # Printer alle rækker i konsollen%>% 
  arrange(desc(count))

# Cluster 2
# gennemsnit og sdv
Players_Cluster_2 %>% 
  summarise(
    Total_Passes = n(),
    Successful_Passes = sum(ACCURATE, na.rm = TRUE),
    Success_Rate = round((Successful_Passes / Total_Passes) * 100, 2),
    Length_SD = sd(LENGTH, na.rm = TRUE),            # Standardafvigelse for LENGTH
    Angle_SD = sd(ANGLE, na.rm = TRUE)               # Standardafvigelse for ANGLE
    )
  
Players_Cluster_2 %>%
  group_by(ROLENAME) %>%
  summarise(
    Total_Passes = n(),
    Successful_Passes = sum(ACCURATE, na.rm = TRUE),
    Success_Rate = round((Successful_Passes / Total_Passes) * 100, 2)
  ) %>%
  print(n = Inf)

Players_Cluster_2 %>% 
  group_by(SHORTNAME) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>%
  print(n = Inf)


# Cluster 3
Players_Cluster_3 %>% 
  summarise(
    Total_Passes = n(),
    Successful_Passes = sum(ACCURATE, na.rm = TRUE),
    Success_Rate = round((Successful_Passes / Total_Passes) * 100, 2),
    Length_SD = sd(LENGTH, na.rm = TRUE),            # Standardafvigelse for LENGTH
    Angle_SD = sd(ANGLE, na.rm = TRUE)               # Standardafvigelse for ANGLE
  )

Players_Cluster_3 %>%
  group_by(ROLENAME) %>%
  summarise(
    Total_Passes = n(),
    Successful_Passes = sum(ACCURATE, na.rm = TRUE),
    Success_Rate = round((Successful_Passes / Total_Passes) * 100, 2)
  ) %>%
  print(n = Inf)

Players_Cluster_3 %>% 
  group_by(SHORTNAME) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>%
  print(n = Inf)


# Cluster 4
Players_Cluster_4 %>% 
  summarise(
    Total_Passes = n(),
    Successful_Passes = sum(ACCURATE, na.rm = TRUE),
    Success_Rate = round((Successful_Passes / Total_Passes) * 100, 2),
    Length_SD = sd(LENGTH, na.rm = TRUE),            # Standardafvigelse for LENGTH
    Angle_SD = sd(ANGLE, na.rm = TRUE)               # Standardafvigelse for ANGLE
  )    
    

Players_Cluster_4 %>%
  group_by(ROLENAME) %>%
  summarise(
    Total_Passes = n(),
    Successful_Passes = sum(ACCURATE, na.rm = TRUE),
    Success_Rate = round((Successful_Passes / Total_Passes) * 100, 2)
  ) %>%
  print(n = Inf)

Players_Cluster_4 %>% 
  group_by(SHORTNAME) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>%
  print(n = Inf)


# Cluster 5
Players_Cluster_5 %>% 
  summarise(
    Total_Passes = n(),
    Successful_Passes = sum(ACCURATE, na.rm = TRUE),
    Success_Rate = round((Successful_Passes / Total_Passes) * 100, 2),
    Length_SD = sd(LENGTH, na.rm = TRUE),            # Standardafvigelse for LENGTH
    Angle_SD = sd(ANGLE, na.rm = TRUE)               # Standardafvigelse for ANGLE
  )  

Players_Cluster_5 %>%
  group_by(ROLENAME, TEAMNAME) %>%
  summarise(
    Total_Passes = n(),
    Successful_Passes = sum(ACCURATE, na.rm = TRUE),
    Success_Rate = round((Successful_Passes / Total_Passes) * 100, 2)
  ) %>%
  arrange(desc(Total_Passes))
  print(n = Inf)

Players_Cluster_5 %>% 
  group_by(SHORTNAME) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>%
  print(n = Inf)
}

#---------------------------------------------#
# Vi gemmer allpasses som csv fil til opgave 4#
#---------------------------------------------#

# Gemmer 'allpasses' som en CSV-fil på den ønskede sti
Opgave4 <- Players_Cluster

write.csv(Opgave4, file = "C:/Users/emil.bengtsen/OneDrive - ferm LIVING ApS/Skrivebord/Eksamen/Opgave4/data/Opgave4.csv", row.names = FALSE)



