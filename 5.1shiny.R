library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)

flatMaleEvents <- read.csv("flatMaleEvents.csv")
flatFemaleEvents <- read.csv("flatFemaleEvents.csv")

# ----- #
# shiny #
# ----- #

ui <- fluidPage(
  titlePanel("Statistiske forskelle på herre og kvinde fodbold"),
  tags$h4("Målt på skandinaviske hold", style = "color: #555; margin-top: -10px;"),
  
  tags$p(
    "Kvinder: Women's World Cup (2019, 2023) og UEFA Women's Euro (2022)",
    style = "color: #777; font-weight: normal; margin-top: -5px;"
  ),
  
  tags$p(
    "Mænd: FIFA World Cup (2018, 2022) og UEFA Euro (2020, 2024)", 
    style = "color: #777; font-weight: normal; margin-top: -5px;"
  ),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Vælg kategori:", choices = c("Oversigt", "Afleveringer", "Skud")),
      uiOutput("sub_category_ui"),  # Dynamisk undermenu
      br(),
      tags$p("Kilde: StatsBomb", style = "font-size: 12px; color: #888; text-align: right;")
    ),
    
    mainPanel(
      uiOutput("summary_table_ui"), 
      plotOutput("selectedPlot")  # Dynamisk plot baseret på valg
    )
  )
)

server <- function(input, output) {
  
  # --- Definer ca() funktionen; beregner vinkel for skud --- #
  
  ca <- function(p) {
    # Player's position
    player_x <- p[1]
    player_y <- p[2]
    
    # Goalpost 1 (poster 1)
    p1_x <- 120
    p1_y <- 36
    
    # Goalpost 2 (poster 2)
    p2_x <- 120
    p2_y <- 44
    
    # Vector from player to goalpost 1 (p1)
    V1_x <- p1_x - player_x
    V1_y <- p1_y - player_y
    
    # Vector from player to goalpost 2 (p2)
    V2_x <- p2_x - player_x
    V2_y <- p2_y - player_y
    
    # Dot product of V1 and V2
    dot_product <- V1_x * V2_x + V1_y * V2_y
    
    # Magnitudes of V1 and V2
    magnitude_V1 <- sqrt(V1_x^2 + V1_y^2)
    magnitude_V2 <- sqrt(V2_x^2 + V2_y^2)
    
    # Cosine of the angle
    cos_theta <- dot_product / (magnitude_V1 * magnitude_V2)
    
    # Angle in radians
    theta_rad <- acos(cos_theta)
    
    # Convert to degrees
    theta_deg <- theta_rad * (180 / pi)
    
    return(theta_deg)
  }
  
  # --- Beregn skudvinkel for kvinder og mænd --- #
  
  shooting_angle_female <- flatFemaleEvents %>%
    filter(type.name == "Shot") %>%
    mutate(shooting_angle = mapply(function(x, y) ca(c(x, y)), location_x, location_y))

  shooting_angle_male <- flatMaleEvents %>%
    filter(type.name == "Shot") %>%
    mutate(shooting_angle = mapply(function(x, y) ca(c(x, y)), location_x, location_y))
  
  # --- Beregn andel af afleveringer for kvinder og mænd --- #
  
  total_female_passes <- flatFemaleEvents %>%
    filter(type.name == "Pass") %>%
    count()
  
  total_male_passes <- flatMaleEvents %>%
    filter(type.name == "Pass") %>%
    count()
  
  # --- beregn summaries for kvinder og mænd --- #
  
  # kvinder
  
  femaleSummary <- flatFemaleEvents %>%
    summarise(
      Afleveringspræcision_procent = round(mean(ifelse(type.name == "Pass", ifelse(is.na(pass.outcome.name), 1, 0), NA), na.rm = TRUE) * 100, 2),
      Gns_afleverings_længde_meter = round(mean(ifelse(type.name == "Pass" & position.name != "Goalkeeper", pass.length, NA), na.rm = TRUE), 2),
      Gns_xg = round(mean(ifelse(type.name == "Shot", shot.statsbomb_xg, NA), na.rm = TRUE), 2),
      Gns_skudvinkel_grader = round(mean(shooting_angle_female$shooting_angle, na.rm = TRUE), 2)
    ) %>%
    mutate(
      Gns_antal_mål_pr_kamp = round(flatFemaleEvents %>%
                                    group_by(match_id) %>%
                                    summarise(goals = sum(type.name == "Shot" & shot.outcome.name == "Goal", na.rm = TRUE)) %>%
                                    summarise(mean(goals, na.rm = TRUE)) %>%
                                    pull(), 2),
      
      Gns_antal_afleveringer_pr_kamp = round(flatFemaleEvents %>%
                                     group_by(match_id) %>%
                                     summarise(passes = sum(type.name == "Pass", na.rm = TRUE)) %>%
                                     summarise(mean(passes, na.rm = TRUE)) %>%
                                     pull(), 2),
      
      Gns_antal_skud_assist_pr_kamp = round(flatFemaleEvents %>%
                                          group_by(match_id) %>%
                                          summarise(shot_assist = sum(pass.shot_assist == "TRUE", na.rm = TRUE)) %>%
                                          summarise(mean(shot_assist, na.rm = TRUE)) %>%
                                          pull(), 2),
      
      Gns_antal_gult_kort_pr_kamp = round(flatFemaleEvents %>%
                                          group_by(match_id) %>%
                                          summarise(yellow_card = sum(foul_committed.card.name == "Yellow Card", na.rm = TRUE)) %>%
                                          summarise(mean(yellow_card, na.rm = TRUE)) %>%
                                          pull(), 2),
      
      Lange_afleveringer_pr_kamp_procent = round(flatFemaleEvents %>%
                                           filter(type.name == "Pass") %>%
                                           group_by(match_id) %>%
                                           summarise(
                                             total_passes = n(),
                                             long_passes = sum(pass.length > 30, na.rm = TRUE),
                                             long_pass_percentage = (long_passes / total_passes) * 100) %>%
                                           summarise(mean(long_pass_percentage, na.rm = TRUE)) %>%
                                           pull(), 2),
      
      Gns_tid_pr_aflevering_sekund = round(flatFemaleEvents %>%
                                          filter(type.name == "Pass", position.name != "Goalkeeper") %>%
                                          group_by(match_id) %>%
                                          summarise(avg_pass_duration = mean(duration, na.rm = TRUE)) %>%
                                          summarise(mean(avg_pass_duration, na.rm = TRUE)) %>%
                                          pull(), 2)
    ) %>%
    mutate(Køn = "Kvinde") 
  
  # mænd
  
  maleSummary <- flatMaleEvents %>%
    summarise(
      Afleveringspræcision_procent = round(mean(ifelse(type.name == "Pass", ifelse(is.na(pass.outcome.name), 1, 0), NA), na.rm = TRUE) * 100, 2),
      Gns_afleverings_længde_meter = round(mean(ifelse(type.name == "Pass" & position.name != "Goalkeeper", pass.length, NA), na.rm = TRUE), 2),
      Gns_xg = round(mean(ifelse(type.name == "Shot", shot.statsbomb_xg, NA), na.rm = TRUE), 2),
      Gns_skudvinkel_grader = round(mean(shooting_angle_male$shooting_angle, na.rm = TRUE), 2)
    ) %>%
    mutate(
      Gns_antal_mål_pr_kamp = round(flatMaleEvents %>%
                                    group_by(matchId) %>%
                                    summarise(goals = sum(type.name == "Shot" & shot.outcome.name == "Goal", na.rm = TRUE)) %>%
                                    summarise(mean(goals, na.rm = TRUE)) %>%
                                    pull(), 2),
      
      Gns_antal_afleveringer_pr_kamp = round(flatMaleEvents %>%
                                     group_by(matchId) %>%
                                     summarise(passes = sum(type.name == "Pass", na.rm = TRUE)) %>%
                                     summarise(mean(passes, na.rm = TRUE)) %>%
                                     pull(), 2),
      
      Gns_antal_skud_assist_pr_kamp = round(flatMaleEvents %>%
                                          group_by(matchId) %>%
                                          summarise(shot_assist = sum(pass.shot_assist == "TRUE", na.rm = TRUE)) %>%
                                          summarise(mean(shot_assist, na.rm = TRUE)) %>%
                                          pull(), 2),
      
      Gns_antal_gult_kort_pr_kamp = round(flatMaleEvents %>%
                                          group_by(matchId) %>%
                                          summarise(yellow_card = sum(foul_committed.card.name == "Yellow Card", na.rm = TRUE)) %>%
                                          summarise(mean(yellow_card, na.rm = TRUE)) %>%
                                          pull(), 2),
      
      Lange_afleveringer_pr_kamp_procent = round(flatMaleEvents %>%
                                           filter(type.name == "Pass") %>%
                                           group_by(matchId) %>%
                                           summarise(
                                             total_passes = n(),
                                             long_passes = sum(pass.length > 30, na.rm = TRUE),
                                             long_pass_percentage = (long_passes / total_passes) * 100) %>%
                                           summarise(mean(long_pass_percentage, na.rm = TRUE)) %>%
                                           pull(), 2),
      
      Gns_tid_pr_aflevering_sekund = round(flatMaleEvents %>%
                                          filter(type.name == "Pass", position.name != "Goalkeeper") %>%
                                          group_by(matchId) %>%
                                          summarise(avg_pass_duration = mean(duration, na.rm = TRUE)) %>%
                                          summarise(mean(avg_pass_duration, na.rm = TRUE)) %>%
                                          pull(), 2)
    ) %>%
    mutate(Køn = "Mand")
  
  # --- Kombiner mænd og kvinder --- #
  
  summary_combined <- bind_rows(femaleSummary, maleSummary)
  
  summary_combined <- summary_combined %>%
    select(
      Afleveringspræcision_procent,
      Gns_antal_afleveringer_pr_kamp,
      Gns_afleverings_længde_meter,
      Lange_afleveringer_pr_kamp_procent,
      Gns_tid_pr_aflevering_sekund,
      Gns_xg,
      Gns_antal_mål_pr_kamp,
      Gns_skudvinkel_grader,
      Gns_antal_skud_assist_pr_kamp,
      Gns_antal_gult_kort_pr_kamp,
      Køn
    ) 
  
  # --- layout: Omdan til langt format med pivot_wider --- #
  
  final_summary <- summary_combined %>%
    pivot_longer(
      cols = starts_with("Afleverings") | starts_with("Gns") | starts_with("Lange"),  
      names_to = "Statistik", 
      values_to = "Value"   
    ) %>%
    pivot_wider(
      names_from = Køn,  
      values_from = Value       
    ) 
  
  # --- Dynamisk UI --- #
  
  output$summary_table_ui <- renderUI({
    if (input$category == "Oversigt") {
      renderTable({final_summary})
    }
  })
  
  output$sub_category_ui <- renderUI({
    if (input$category == "Oversigt") {
      selectInput("sub_category", "Vælg underkategori:", choices = c("Oversigt"))
    } else if (input$category == "Afleveringer") {
      selectInput("sub_category", "Vælg underkategori:", choices = c("Afleveringspræcision", "Tidsspecifik afleveringspræcision", "Afleveringslængde", "Andel af lange afleveringer (>30 meter)", "Boldbesidelse"))
    } else {
      selectInput("sub_category", "Vælg underkategori:", choices = c("Skud Udfald", "Skud over tid", "Xg over tid"))
    }
  })
  
  # --- plot --- #
  
  # successrate i afleveringer
  
  output$selectedPlot <- renderPlot({
    if (input$category == "Afleveringer" && input$sub_category == "Afleveringspræcision") {
      success_rates <- flatFemaleEvents %>%
        filter(type.name == "Pass") %>%
        mutate(pass_success = ifelse(is.na(pass.outcome.name), 1, 0), køn = "Kvinder") %>%
        bind_rows(
          flatMaleEvents %>%
            filter(type.name == "Pass") %>%
            mutate(pass_success = ifelse(is.na(pass.outcome.name), 1, 0), køn = "Mænd")
        ) %>%
        group_by(køn) %>%
        summarise(success_rate = mean(pass_success, na.rm = TRUE))
      
      ggplot(success_rates, aes(x = køn, y = success_rate, fill = køn)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = sprintf("%.2f", success_rate)),  
                  position = position_dodge(width = 0.9), 
                  size = 4, color = "white", , vjust = 1.5) +
        theme_minimal() +
        labs(title = "Mandlige fodboldspillere har en smule bedre afleveringspræcision", x = "Køn", y = "Succesrate (`%`)") +
        scale_fill_manual(values = c("Kvinder" = "red", "Mænd" = "#1E90FF"))
    } 
    
    else if (input$category == "Afleveringer" && input$sub_category == "Tidsspecifik afleveringspræcision") {
      passTimeData <- flatFemaleEvents %>%
        filter(type.name == "Pass") %>%
        filter(!is.na(minute)) %>%
        mutate(minute_bin = cut(minute, breaks = seq(0, 90, by = 10), include.lowest = TRUE),
               pass_success = ifelse(is.na(pass.outcome.name), 1, 0), køn = "Kvinder") %>%
        filter(!is.na(minute_bin)) %>% 
        bind_rows(
          flatMaleEvents %>%
            filter(type.name == "Pass") %>%
            filter(!is.na(minute)) %>%
            mutate(minute_bin = cut(minute, breaks = seq(0, 90, by = 10), include.lowest = TRUE),
                   pass_success = ifelse(is.na(pass.outcome.name), 1, 0), køn = "Mænd") %>% 
            filter(!is.na(minute_bin))
        ) %>%
        group_by(minute_bin, køn) %>%
        summarise(success_rate = mean(pass_success, na.rm = TRUE), .groups = "drop")
      
      ggplot(passTimeData, aes(x = minute_bin, y = success_rate, color = køn, group = køn)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Kvinder" = "red", "Mænd" = "#1E90FF")) +
        labs(title = "Lige inden første halvleg stiger mændendes afleveringspræcision, mens kvindernes falder", x = "Minutter", y = "Succesrate (`%`)") +
        theme_minimal()
    }
    
    # Afleveringslængde
    
    else if (input$category == "Afleveringer" && input$sub_category == "Afleveringslængde") {
      passLengthData <- flatFemaleEvents %>%
        filter(type.name == "Pass") %>%
        filter(position.name != "Goalkeeper") %>% 
        mutate(køn = "Kvinder") %>%
        bind_rows(
          flatMaleEvents %>%
            filter(type.name == "Pass") %>%
            filter(position.name != "Goalkeeper") %>% 
            mutate(køn = "Mænd")
            ) %>% 
          group_by(field_position, køn) %>%
            summarise(avg.pass.length = mean(pass.length, na.rm = TRUE))
      
      ggplot(passLengthData, aes(x = field_position, y = avg.pass.length, fill = køn)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +  
        geom_text(aes(label = round(avg.pass.length, 1), y = avg.pass.length / 2),  
                  position = position_dodge(width = 0.9),  
                  size = 4, color = "white") +  
        theme_minimal() +
        labs(title = "Generelt slår mænd længere afleveringer", 
             subtitle = "Forskellen er ikke stor, men konsekvent i alle områder af banen.",
             x = "Position på banen", 
             y = "Gennemsnitlig afleveringslængde (meter)") +
        scale_fill_manual(values = c("Mænd" = "#1E90FF", "Kvinder" = "red")) +  
        theme(axis.title = element_text(size = 14), 
              plot.title = element_text(size = 16, face = "bold"))
    }
    
    else if (input$category == "Afleveringer" && input$sub_category == "Andel af lange afleveringer (>30 meter)") {
      long_passes <- flatFemaleEvents %>%
        filter(type.name == "Pass" & pass.length > 30) %>%
        mutate(køn = "Kvinder") %>%
        bind_rows(
          flatMaleEvents %>%
            filter(type.name == "Pass" & pass.length > 30) %>%
            mutate(køn = "Mænd")
        ) %>%
        group_by(køn) %>%
        count() %>%
        mutate(percentage = case_when(
          køn == "Kvinder" ~ (n / total_female_passes$n) * 100,  # Procent for kvinder
          køn == "Mænd" ~ (n / total_male_passes$n) * 100  # Procent for mænd
        ))
      
      ggplot(long_passes, aes(x = køn, y = percentage, fill = køn)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round(percentage, 2)), colour = "white", vjust = 1.5, size = 5) + 
        labs(title = "Mandlige fodboldspillere har en større andel af lange afleveringer (>30 meter)", 
             x = "Køn", 
             y = "Procentandel") +
        theme_minimal() +
        scale_fill_manual(values = c("Mænd" = "#1E90FF", "Kvinder" = "red"))
    }
    
    # Boldbesiddelse i afleveringer
    
    else if (input$category == "Afleveringer" && input$sub_category == "Boldbesidelse") {
      passDuration <- flatFemaleEvents %>%
        filter(type.name == "Pass") %>%
        filter(position.name != "Goalkeeper") %>%
        mutate(køn = "Kvinder") %>%
        bind_rows(
          flatMaleEvents %>%
            filter(type.name == "Pass") %>%
            filter(position.name != "Goalkeeper") %>%
            mutate(køn = "Mænd")
        ) %>%
        group_by(køn) %>%  # Gruppér efter køn
        summarise(avg_pass_duration = mean(duration, na.rm = TRUE))
      
      ggplot(passDuration, aes(x = køn, y = avg_pass_duration, fill = køn)) +
        geom_bar(stat = "identity", width = 0.6) +
        geom_text(aes(label = round(avg_pass_duration, 2)), colour = "white", vjust = 1.5, size = 5) + 
        scale_fill_manual(values = c("Kvinder" = "red", "Mænd" = "#1E90FF")) + 
        theme_minimal() +
        labs(title = "Kvindlige spillere holder længere tid på bolden under afleveringer",
             x = "Køn",
             y = "Varighed (sekunder)") +
        theme(legend.position = "none")
    }
    
    # Skud udfald
    
    else if (input$category == "Skud" && input$sub_category == "Skud Udfald") {
      shotOutcome <- flatFemaleEvents %>%
        filter(type.name == "Shot") %>%
        count(shot.outcome.name) %>%
        mutate(køn = "Kvinder") %>%
        bind_rows(
          flatMaleEvents %>%
            filter(type.name == "Shot") %>%
            count(shot.outcome.name) %>%
            mutate(køn = "Mænd")
        ) %>%
        group_by(køn) %>% 
        mutate(pct = n / sum(n) * 100)
      
      ggplot(shotOutcome, aes(x = shot.outcome.name, y = pct, fill = køn)) +
        geom_bar(stat = "identity", position = "dodge", 
                 aes(alpha = ifelse(shot.outcome.name == "Goal", 1, 0.9))) +  
        labs(
          title = "Kvindelige fodboldspillere har en større konverteringsrate på målscoring",
          x = "Skududfald",
          y = "Procent",
          fill = "Køn"
        ) +
        theme_minimal() +
        scale_fill_manual(values = c("Kvinder" = "red", "Mænd" = "#1E90FF")) +
        geom_text(aes(label = round(pct, 1)), 
                  position = position_dodge(width = 0.9), 
                  vjust = -0.5, size = 4)+
        scale_alpha(guide = "none")
    }
    
    # Skud over tid
    
    else if (input$category == "Skud" && input$sub_category == "Skud over tid") {
      shotTimeData <- flatFemaleEvents %>%
        filter(type.name == "Shot") %>%
        filter(!is.na(minute)) %>%
        mutate(minute_bin = cut(minute, breaks = seq(0, 90, by = 10), include.lowest = TRUE)) %>% 
        mutate(køn = "Kvinder") %>%
        bind_rows(
          flatMaleEvents %>%
            filter(type.name == "Shot") %>%
            filter(!is.na(minute)) %>%
            mutate(minute_bin = cut(minute, breaks = seq(0, 90, by = 10), include.lowest = TRUE)) %>% 
            mutate(køn = "Mænd") %>% 
            rename(match_id = matchId)
        ) %>%
        group_by(køn, minute_bin) %>% 
        summarise(shots_per_game = n() / n_distinct(match_id)) %>% 
        filter(!is.na(minute_bin))
      
      ggplot(shotTimeData, aes(x = minute_bin, y = shots_per_game, group = køn, color = køn)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(
          title = "Kvindlige spillere har en mere svingende skudrate",
          x = "Minutinterval",
          y = "Gns. skud pr. kamp",
          color = "Køn"
        ) +
        theme_minimal() +
        scale_color_manual(values = c("Kvinder" = "red", "Mænd" = "blue"))
    }
    
    # Xg over tid 
      
      else if (input$category == "Skud" && input$sub_category == "Xg over tid") {
        shotTimeXg <- flatFemaleEvents %>%
          filter(type.name == "Shot") %>%
          filter(!is.na(minute)) %>%
          mutate(minute_bin = cut(minute, breaks = seq(0, 90, by = 10), include.lowest = TRUE)) %>% 
          mutate(køn = "Kvinder") %>%
          bind_rows(
            flatMaleEvents %>%
              filter(type.name == "Shot") %>%
              filter(!is.na(minute)) %>%
              mutate(minute_bin = cut(minute, breaks = seq(0, 90, by = 10), include.lowest = TRUE)) %>% 
              mutate(køn = "Mænd")
          ) %>%
          group_by(køn, minute_bin) %>% 
          summarise(
            mean_xg = mean(shot.statsbomb_xg, na.rm = TRUE)) %>% 
          filter(!is.na(minute_bin))
        
        ggplot(shotTimeXg, aes(x = minute_bin, y = mean_xg, group = køn, color = køn)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          labs(
            title = "Mandlige fodboldspillere har gennemsnitligt en høj Xg i kampens afsluttende fase",
            x = "Minutinterval",
            y = "Gns. xg pr. kamp",
            color = "Køn"
          ) +
          theme_minimal() +
          scale_color_manual(values = c("Kvinder" = "red", "Mænd" = "blue"))
  }
  
  })
}

shinyApp(ui, server)


#rsconnect::deployApp(appDir = ".", appPrimaryDoc = "shiny.R")






