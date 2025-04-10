library(rsconnect)
library(shiny)
library(tidyverse)
library(ggplot2)
library(ggsoccer)
library(ggimage)

shots_f_final <- readRDS("shots_f_final.rds")
shots_m_final <- readRDS("shots_m_final.rds")
matches_kvinder <- readRDS("matches_kvinder.rds")
matches_mænd <- readRDS("matches_mænd.rds")
allLineups <- readRDS("allLineups.rds")
shots_f <- readRDS("shots_f.rds")
shots_m <- readRDS("shots_m.rds")

ui <- fluidPage(
  titlePanel("Egometer App"),
  tabsetPanel(
    tabPanel("Visualiser skud",
             sidebarLayout(
               sidebarPanel(
                 selectInput("gender", "Vælg køn:", choices = c("Kvinder", "Mænd")),
                 uiOutput("match_ui"),
                 uiOutput("player_ui"),
                 actionButton("next_shot", "Næste skud"),
                 sliderInput("zoom", "Zoom (højere = tættere på)", min = 0, max = 100, value = 60),
                 helpText("NB: Hvis der ikke vises et plot, skyldes det, at skuddet mangler nødvendige data."),
               ),
               mainPanel(
                 plotOutput("shot_plot", height = "600px"),
                 h4("Medspillere og muligheder"),
                 tableOutput("pass_options"),
               ))
    ),
    tabPanel("Andel ego-skud",
             fluidRow(
               column(4, h3("Andel ego-skud"), 
                      helpText("Andelen viser hvor mange skud der blev vurderet som egoistiske ud af alle skud med gyldige data."),
                      tableOutput("summary_table")),
               column(7, offset = 0.5, h3(""),
                      plotOutput("ego_barplot"))
             )
    )
  )
)

server <- function(input, output, session) {
  
  player_lookup <- setNames(allLineups$jersey_number, allLineups$player_name)
  
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
  
  rv <- reactiveValues(index = 1) #Reaktiv variabel, husker hvilket skud bruger kigger på. Næste skud = tælles op 
  
  selected_data <- reactive({ #Returnerer skud-df for valgt køn
    if (input$gender == "Kvinder") shots_f_final else shots_m_final
  })
  
  selected_matches <- reactive({ #Returner matchdf for valgt køn 
    if (input$gender == "Kvinder") matches_kvinder else matches_mænd
  })
  
  output$match_ui <- renderUI({ #Når bruger har valgt køn, vises alle kampe for køn og kamplabel 
    kamp_df <- selected_matches()
    selectInput("match_id", "Vælg kamp:",
                choices = setNames(kamp_df$match_id, kamp_df$kamp_label))
  })
  
  output$player_ui <- renderUI({ #Når bruger har valgt kamp vises alle spillere i den kamp med ego beregnet sorteret alfabetisk 
    req(input$match_id)
    skud <- selected_data()
    players <- skud %>%
      filter(match_id == input$match_id, !is.na(ego)) %>%
      pull(player.name) %>%
      unique() %>%
      sort()
    selectInput("player", "Vælg spiller:", choices = players)
  })
  
  filtered_shots <- reactive({ #Finder alle skud for en spiller i en kamp baseret på valg i app 
    req(input$match_id, input$player) # dette er tilføjet
    
    df <- selected_data() %>%
      filter(match_id == input$match_id, player.name == input$player)
    
    req(nrow(df) > 0)
    
    df
  })
  
  observeEvent(input$next_shot, { #Når bruger klikker, hoppes der til næste skud, hvis sidste skud så startes der forfra 
    rv$index <- rv$index + 1
    if (rv$index > nrow(filtered_shots())) rv$index <- 1
  })
  
  output$shot_plot <- renderPlot({ #Reaktiv plotfunktion der laver visualisering baseret på brugers valg 
    req(nrow(filtered_shots()) > 0) #Skal være mindst et skud, ellers intet plot 
    skud <- filtered_shots()[rv$index, ] #Henter aktuelle skud baseret på brugerens klik af næste skud 
    if (is.null(skud$location[[1]]) | is.null(skud$shot.freeze_frame[[1]])) return(NULL) #Mangler skud location eller ff, springes over 
    
    shooter_x <- skud$location[[1]][1] #Henter skyttes koordinaterpå banen
    shooter_y <- skud$location[[1]][2]
    
    freeze <- as.data.frame(skud$shot.freeze_frame[[1]]) #Freeze har alle spillere på banen da skud blev taget 
    freeze$x <- sapply(freeze$location, function(loc) if (is.character(loc)) as.numeric(strsplit(loc, ",")[[1]][1]) else loc[[1]]) #Noget er tekst, andet tal, sapply bruges til at trække x og y ud af hver spiller
    freeze$y <- sapply(freeze$location, function(loc) if (is.character(loc)) as.numeric(strsplit(loc, ",")[[1]][2]) else loc[[2]])
    
    # Slår hver spiller op i player_lookup for at finde trøjenr, findes nr ikke = tomt felt 
    freeze$jersey_number <- player_lookup[freeze$player$name]
    freeze$jersey_number[is.na(freeze$jersey_number)] <- ""
    
    teammates <- freeze %>% filter(teammate == TRUE) #Deler spillere op i medspillere og modstandere
    opponents <- freeze %>% filter(teammate == FALSE)
    
    shooter_opps <- sum(mapply(is_opponent_inside_triangle, opponents$x, opponents$y, MoreArgs = list(shooter_x, shooter_y))) #Tæller antal modstandere foran skytten med funktion 
    
    radius <- 20
    
    teammate_triangles <- lapply(1:nrow(teammates), function(i) { #Gennemgår hver medspiller og beregner 
      tx <- teammates$x[i] #For hver medspiller x, y og afstand 
      ty <- teammates$y[i]
      afst <- sqrt((shooter_x - tx)^2 + (shooter_y - ty)^2)
      teammate_opps <- sum(mapply(is_opponent_inside_triangle, opponents$x, opponents$y, MoreArgs = list(tx, ty))) #Tæller modstandere foran medspiller
      blocked <- any(sapply(1:nrow(opponents), function(k) { #Ser om afleveringen er blokeret 
        is_defender_blocking_pass(shooter_x, shooter_y, tx, ty, opponents$x[k], opponents$y[k])
      }))
      better_choice <- ((skud$køn == "Kvinder" && teammate_opps < shooter_opps) || 
                          (skud$køn == "Mænd" && teammate_opps <= shooter_opps)) &&
        (afst < radius) && !blocked #Vurderer om medspiller er et bedre valg 
      data.frame( #Returnerer dataframe med tre punkter = medspiller placering + stolper 
        x = c(tx, 120, 120),
        y = c(ty, 36, 44),
        group = i,
        better_choice = better_choice #Gemmer flag til senere visualisering 
      )
    })
    
    plot <- ggplot() + #Plot til fodboldbanen 
      annotate_pitch(dimensions = pitch_statsbomb, fill = "#3ab54a", colour = "white") +
      geom_polygon(data = data.frame(x = c(shooter_x, 120, 120), y = c(shooter_y, 36, 44)),
                   aes(x = x, y = y, fill = "Skyttens trekant"), alpha = 0.3) +
      geom_polygon(data = bind_rows(teammate_triangles) %>% filter(better_choice),
                   aes(x = x, y = y, group = group, fill = "Bedre medspiller"), alpha = 0.25) +
      geom_polygon(data = bind_rows(teammate_triangles) %>% filter(!better_choice),
                   aes(x = x, y = y, group = group, fill = "Øvrig medspiller"), alpha = 0.15) +
      geom_point(data = freeze, aes(x = x, y = y, color = ifelse(teammate, "Medspiller", "Modstander")), size = 7) +
      geom_text(data = freeze, aes(x = x, y = y, label = jersey_number), color = "white", size = 3, fontface = "bold", vjust = 0.5, hjust = 0.5) +
      geom_image(aes(x = shooter_x, y = shooter_y), image = "fodbold_url.png", size = 0.08) +
      scale_fill_manual(values = c("Bedre medspiller" = "green", "Øvrig medspiller" = "blue", "Skyttens trekant" = "red"),
                        name = "Trekanttype") +
      scale_color_manual(values = c("Modstander" = "black", "Medspiller" = "blue"),
                         name = "Spillertype") +
      theme_pitch() +
      ggtitle(paste0("Skud ", rv$index, " fra ", skud$player.name, " – Ego: ", ifelse(skud$ego, "JA", "NEJ")))
    
    if (!is.null(skud$ego) && skud$ego && length(skud$ego_targets[[1]]) > 0) { #Hvis skud er ego, tegn stiplet linje til bedre placerede medspiller 
      for (line in skud$ego_targets[[1]]) {
        plot <- plot + geom_line(data = line, aes(x = x, y = y),
                                 color = "white", linetype = "dashed", linewidth = 1)
      }
    }
    
    x_min <- 0 + input$zoom
    x_max <- 125
    plot + coord_flip(xlim = c(x_min, x_max), ylim = c(0, 80)) + scale_y_reverse()
  })
  output$pass_options <- renderTable({ #Tabel til medspillere i det viste skud 
    req(nrow(filtered_shots()) > 0)
    skud <- filtered_shots()[rv$index, ]
    if (is.null(skud$location[[1]]) | is.null(skud$shot.freeze_frame[[1]])) return(NULL)
    shooter_x <- skud$location[[1]][1]
    shooter_y <- skud$location[[1]][2]
    
    radius <- 20
    
    freeze <- as.data.frame(skud$shot.freeze_frame[[1]])
    freeze$x <- sapply(freeze$location, function(loc) if (is.character(loc)) as.numeric(strsplit(loc, ",")[[1]][1]) else loc[[1]])
    freeze$y <- sapply(freeze$location, function(loc) if (is.character(loc)) as.numeric(strsplit(loc, ",")[[1]][2]) else loc[[2]])
    
    # Tilføj trøjenumre til freeze data
    freeze$jersey_number <- player_lookup[freeze$player$name]
    freeze$jersey_number[is.na(freeze$jersey_number)] <- ""
    
    teammates <- freeze %>% filter(teammate == TRUE)
    opponents <- freeze %>% filter(teammate == FALSE)
    shooter_opps <- sum(mapply(is_opponent_inside_triangle, opponents$x, opponents$y, MoreArgs = list(shooter_x, shooter_y)))
    
    radius <- 20
    
    teammate_data <- lapply(1:nrow(teammates), function(i) {
      tx <- teammates$x[i]
      ty <- teammates$y[i]
      afst <- sqrt((shooter_x - tx)^2 + (shooter_y - ty)^2)
      teammate_opps <- sum(mapply(is_opponent_inside_triangle, opponents$x, opponents$y, MoreArgs = list(tx, ty)))
      blocked <- any(sapply(1:nrow(opponents), function(k) {
        is_defender_blocking_pass(shooter_x, shooter_y, tx, ty, opponents$x[k], opponents$y[k])
      }))
      better <- ((skud$køn == "Kvinder" && teammate_opps < shooter_opps) || 
                   (skud$køn == "Mænd" && teammate_opps <= shooter_opps)) &&
        (afst < radius) && !blocked
      #Er medspiller bedre valg?
      tibble( #Laver række i tabellen for medspiller med alle oplysninger 
        `Trøjenummer` = teammates$jersey_number[i],
        Navn = teammates$player$name[i],
        Afstand = round(afst, 1),
        Modstandere_i_trekant = teammate_opps,
        Blokeret_af_forsvar = ifelse(blocked, "Ja", "Nej"),
        Bedre_valg = ifelse(better, "Ja", "Nej")
      )
    }) %>% bind_rows()
    
    teammate_data %>% arrange(desc(Bedre_valg), Afstand) 
  })
  
  output$summary_table <- renderTable({
    tibble(
      Køn = c("Kvinder", "Mænd"),
      `Ego-skud (%)` = c(
        round(
          mean(
            shots_f_final %>%
              filter(team.name %in% c("Denmark Women's", "Sweden Women's", "Norway Women's", "Iceland Women's")) %>%
              pull(ego),
            na.rm = TRUE
          ) * 100, 1
        ),
        round(
          mean(
            shots_m_final %>%
              filter(team.name %in% c("Denmark", "Sweden", "Norway", "Iceland")) %>%
              pull(ego),
            na.rm = TRUE
          ) * 100, 1
        )
      )
    )
  })
  
  
  output$ego_barplot <- renderPlot({
    df <- tibble(
      Køn = c("Kvinder", "Mænd"),
      Andel = c(
        mean(
          shots_f_final %>%
            filter(team.name %in% c("Denmark Women's", "Sweden Women's", "Norway Women's", "Iceland Women's")) %>%
            pull(ego),
          na.rm = TRUE
        ),
        mean(
          shots_m_final %>%
            filter(team.name %in% c("Denmark", "Sweden", "Norway", "Iceland")) %>%
            pull(ego),
          na.rm = TRUE
        )
      )
    )
    ggplot(df, aes(x = Køn, y = Andel, fill = Køn)) + #Søjlediagram 
      geom_col() +
      geom_text(aes(label = paste0(round(Andel * 100, 1), "%")), 
                vjust = -0.5, size = 5) +
      ylim(0, 1) +
      labs(
        title = "Mænd tager oftere egoistiske skud end kvinder",
        subtitle = "Baseret på skandinaviske hold",
        y = "Andel egoskud i procent",
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
  })
}

shinyApp(ui = ui, server = server)

rsconnect::deployApp(appDir = ".", appPrimaryDoc = "shiny_opg53.R")
