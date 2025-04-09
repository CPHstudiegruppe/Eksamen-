# Opgave 4

library(dplyr)
library(readr)

#-------------------#
# Indlæsning af data#
#-------------------#

Opgave4 <- read_csv("C:/Users/emil.bengtsen/OneDrive - ferm LIVING ApS/Skrivebord/Eksamen/Opgave4/data/Opgave4.csv")
Resultater <- read_csv("C:/Users/emil.bengtsen/OneDrive - ferm LIVING ApS/Skrivebord/Eksamen/R.data/r.data/wyscout_matchdetail_base_sl.csv")

#------------------------#
# Rensning/Merge af data #
#------------------------#
{
# Join de to dataframes på kolonnen TEAM_WYID og MATCH_WYID
Shiny_app.df <- Opgave4 %>%
  left_join(Resultater %>%
              select(MATCH_WYID, TEAM_WYID, SCORE, SIDE), 
            by = c("MATCH_WYID" = "MATCH_WYID", "TEAM_WYID.x" = "TEAM_WYID"))
}

#---------------#
# Nye variabler #
#---------------#
{
# Tilføj en kolonne, der viser resultatet for hele kampen (home vs. away)
Resultater_med_resultat <- Resultater %>%
  group_by(MATCH_WYID) %>%
  summarize(
    HomeTeam = TEAM_WYID[SIDE == "home"],
    AwayTeam = TEAM_WYID[SIDE == "away"],
    HomeScore = SCORE[SIDE == "home"],
    AwayScore = SCORE[SIDE == "away"],
    Resultat = paste0(HomeScore, "-", AwayScore)
  ) %>%
  ungroup()


# Lav en ny kolonne der viser kampen som "Hold A vs Hold B"
kamp_hold <- Shiny_app.df %>%
  group_by(MATCH_WYID) %>%
  summarise(Hold = paste(unique(TEAMNAME), collapse = " vs "))


# Slå kamp_hold sammen med din dataframe
Shiny_app.df <- Shiny_app.df %>%
  left_join(kamp_hold, by = "MATCH_WYID")


# 🎯 SLUTJOIN: Join `Resultater_med_resultat` med `Shiny_app.df` for at få kampens resultat
Shiny_app.df <- Shiny_app.df %>%
  left_join(Resultater_med_resultat, by = "MATCH_WYID")

# Vi laver en ny kolonne til modstander hold.
library(dplyr)

Shiny_app.df <- Shiny_app.df %>%
  mutate(
    Opponent = ifelse(SIDE == "home",
                      sub(".* vs ", "", Hold),  # Vælg holdet efter 'vs' når man er hjemmehold
                      sub(" vs .*", "", Hold)   # Vælg holdet før 'vs' når man er udehold
    )
  )
}

#------------------------#
# Opgave 4.1 - Shiny app #
#------------------------#
{
# Shiny app - Kun med gennemsnit
{
library(shiny)
library(ggplot2)
library(dplyr)
library(ggsoccer)

# UI
ui <- fluidPage(
  titlePanel("Opgave 4.1 - Visualisering af Clustre for Afleveringer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = NULL, multiple = TRUE),
      selectInput("team", "Vælg Hold:", choices = NULL, selected = NULL, multiple = TRUE),
      actionButton("showPlot", "Vis plot")  # Plot knap
    ),
    mainPanel(
      plotOutput("passPlot", height = "700px"),
      tableOutput("clusterSummary")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Opgave4.True
  
  # Opdater dropdowns når appen starter (med alle clusters og hold)
  updateSelectInput(session, "cluster", choices = sort(unique(data$cluster)))
  updateSelectInput(session, "team", choices = sort(unique(data$TEAMNAME)))
  
  # Reaktiv værdi til at styre, om plot skal vises
  plotVisible <- reactiveVal(FALSE)
  
  # Når knappen trykkes, vises plottet, men kun hvis der er lavet et valg
  observeEvent(input$showPlot, {
    if (length(input$cluster) > 0 | length(input$team) > 0) {
      plotVisible(TRUE)
    } else {
      showNotification("Vælg mindst ét cluster eller ét hold før du trykker 'Vis plot'.", type = "warning")
    }
  })
  
  # Filtrering af data baseret på valg
  filteredData <- reactive({
    req(plotVisible())  # Kun aktiver denne reaktive, når plottet skal vises
    
    filtered <- data
    
    if (length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    if (length(input$team) > 0) {
      filtered <- filtered %>% filter(TEAMNAME %in% input$team)
    }
    
    return(filtered)
  })
  
  # Beregning af gennemsnitlige afleveringer
  averagedData <- reactive({
    req(plotVisible())
    
    filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE),
        Antal = n()
      )
  })
  
  output$passPlot <- renderPlot({
    req(plotVisible())  # Kun render plot, hvis knappen er blevet trykket på
    
    if (nrow(averagedData()) == 0) {
      showNotification("Ingen data tilgængelig. Vælg et cluster eller hold.", type = "warning")
      return(NULL)
    }
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "black", fill = "springgreen4") +
      geom_segment(data = averagedData(), 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY, 
                       color = as.factor(cluster)),
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5) +
      scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) + 
      theme_pitch() +
      ggtitle("Gennemsnitlige afleveringer visualiseret på fodboldbane med clusters") +
      theme(legend.position = "right")
  })
  
  output$clusterSummary <- renderTable({
    req(plotVisible())
    
    if (nrow(averagedData()) == 0) return(NULL)
    
    averagedData() %>%
      select(cluster, Antal)
  })
}

# Run App
shinyApp(ui = ui, server = server)

}

# Shiny app med gennemsnit og standardafvigelse
{
library(shiny)
library(ggplot2)
library(dplyr)
library(ggsoccer)

# UI
ui <- fluidPage(
  titlePanel("Opgave 4.1 - Visualisering af Clustre for Afleveringer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = NULL, multiple = TRUE),
      selectInput("team", "Vælg Hold:", choices = NULL, selected = NULL, multiple = TRUE),
      actionButton("showPlot", "Vis plot")  # Plot knap
    ),
    mainPanel(
      plotOutput("passPlot", height = "700px"),
      tableOutput("clusterSummary")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Opgave4.True
  
  # Opdater dropdowns når appen starter (med alle clusters og hold)
  updateSelectInput(session, "cluster", choices = sort(unique(data$cluster)))
  updateSelectInput(session, "team", choices = sort(unique(data$TEAMNAME)))
  
  # Reaktiv værdi til at styre, om plot skal vises
  plotVisible <- reactiveVal(FALSE)
  
  # Når knappen trykkes, vises plottet, men kun hvis der er lavet et valg
  observeEvent(input$showPlot, {
    if (length(input$cluster) > 0 | length(input$team) > 0) {
      plotVisible(TRUE)
    } else {
      showNotification("Vælg mindst ét cluster eller ét hold før du trykker 'Vis plot'.", type = "warning")
    }
  })
  
  # Filtrering af data baseret på valg
  filteredData <- reactive({
    req(plotVisible())
    
    filtered <- data
    
    if (length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    if (length(input$team) > 0) {
      filtered <- filtered %>% filter(TEAMNAME %in% input$team)
    }
    
    return(filtered)
  })
  
  # Beregning af gennemsnitlige afleveringer og standardafvigelser
  averagedData <- reactive({
    req(plotVisible())
    
    filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE),
        SD_LOCATIONX = sd(LOCATIONX, na.rm = TRUE),
        SD_LOCATIONY = sd(LOCATIONY, na.rm = TRUE),
        SD_ENDLOCATIONX = sd(ENDLOCATIONX, na.rm = TRUE),
        SD_ENDLOCATIONY = sd(ENDLOCATIONY, na.rm = TRUE),
        Antal = n()
      )
  })
  
  output$passPlot <- renderPlot({
    req(plotVisible())
    
    if (nrow(averagedData()) == 0) {
      showNotification("Ingen data tilgængelig. Vælg et cluster eller hold.", type = "warning")
      return(NULL)
    }
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "black", fill = "springgreen4") +
      
      # Tegner gennemsnitsafleveringerne
      geom_segment(data = averagedData(), 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY, 
                       color = as.factor(cluster)),
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5) +
      
      # Tegner standardafvigelserne som "vafler"
      geom_rect(data = averagedData(),
                aes(xmin = Avg_LOCATIONX - SD_LOCATIONX,
                    xmax = Avg_LOCATIONX + SD_LOCATIONX,
                    ymin = Avg_LOCATIONY - SD_LOCATIONY,
                    ymax = Avg_LOCATIONY + SD_LOCATIONY,
                    fill = as.factor(cluster)),
                alpha = 0.2) +
      
      geom_rect(data = averagedData(),
                aes(xmin = Avg_ENDLOCATIONX - SD_ENDLOCATIONX,
                    xmax = Avg_ENDLOCATIONX + SD_ENDLOCATIONX,
                    ymin = Avg_ENDLOCATIONY - SD_ENDLOCATIONY,
                    ymax = Avg_ENDLOCATIONY + SD_ENDLOCATIONY,
                    fill = as.factor(cluster)),
                alpha = 0.2) +
      
      scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) + 
      scale_fill_manual(values = c("red", "blue", "green", "purple", "orange")) +
      theme_pitch() +
      ggtitle("Gennemsnitlige afleveringer med standardafvigelse (vafler)") +
      theme(legend.position = "right")
  })
  
  output$clusterSummary <- renderTable({
    req(plotVisible())
    
    if (nrow(averagedData()) == 0) return(NULL)
    
    averagedData() %>%
      select(cluster, Antal)
  })
}

# Run App
shinyApp(ui = ui, server = server)

}

# Shiny app med gns, sd og udvidet tabel
{
library(shiny)
library(ggplot2)
library(dplyr)
library(ggsoccer)

# UI
ui <- fluidPage(
  titlePanel("Opgave 4.1 - Visualisering af Clustre for Afleveringer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = NULL, multiple = TRUE),
      selectInput("team", "Vælg Hold:", choices = NULL, selected = NULL, multiple = TRUE),
      actionButton("showPlot", "Vis plot")  # Plot knap
    ),
    mainPanel(
      plotOutput("passPlot", height = "700px"),
      tableOutput("clusterSummary")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Shiny_app.df
  
  # Opdater dropdowns når appen starter (med alle clusters og hold)
  updateSelectInput(session, "cluster", choices = sort(unique(data$cluster)))
  updateSelectInput(session, "team", choices = sort(unique(data$TEAMNAME)))
  
  # Reaktiv værdi til at styre, om plot skal vises
  plotVisible <- reactiveVal(FALSE)
  
  # Når knappen trykkes, vises plottet, men kun hvis der er lavet et valg
  observeEvent(input$showPlot, {
    if (length(input$cluster) > 0 | length(input$team) > 0) {
      plotVisible(TRUE)
    } else {
      showNotification("Vælg mindst ét cluster eller ét hold før du trykker 'Vis plot'.", type = "warning")
    }
  })
  
  # Filtrering af data baseret på valg
  filteredData <- reactive({
    req(plotVisible())
    
    filtered <- data
    
    if (length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    if (length(input$team) > 0) {
      filtered <- filtered %>% filter(TEAMNAME %in% input$team)
    }
    
    return(filtered)
  })
  
  # Beregning af gennemsnitlige afleveringer og standardafvigelser
  averagedData <- reactive({
    req(plotVisible())
    
    filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE),
        SD_LOCATIONX = sd(LOCATIONX, na.rm = TRUE),
        SD_LOCATIONY = sd(LOCATIONY, na.rm = TRUE),
        SD_ENDLOCATIONX = sd(ENDLOCATIONX, na.rm = TRUE),
        SD_ENDLOCATIONY = sd(ENDLOCATIONY, na.rm = TRUE),
        
        # Nye værdier til tabellen
        Avg_LENGTH = mean(LENGTH, na.rm = TRUE),
        Avg_ANGLE = mean(ANGLE, na.rm = TRUE),
        SD_LENGTH = sd(LENGTH, na.rm = TRUE),
        SD_ANGLE = sd(ANGLE, na.rm = TRUE),
        
        Antal = n()
      )
  })
  
  output$passPlot <- renderPlot({
    req(plotVisible())
    
    if (nrow(averagedData()) == 0) {
      showNotification("Ingen data tilgængelig. Vælg et cluster eller hold.", type = "warning")
      return(NULL)
    }
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "black", fill = "springgreen4") +
      
      # Tegner gennemsnitsafleveringerne
      geom_segment(data = averagedData(), 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY, 
                       color = as.factor(cluster)),
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5) +
      
      # Tegner standardafvigelserne som "vafler"
      geom_rect(data = averagedData(),
                aes(xmin = Avg_LOCATIONX - SD_LOCATIONX,
                    xmax = Avg_LOCATIONX + SD_LOCATIONX,
                    ymin = Avg_LOCATIONY - SD_LOCATIONY,
                    ymax = Avg_LOCATIONY + SD_LOCATIONY,
                    fill = as.factor(cluster)),
                alpha = 0.2) +
      
      geom_rect(data = averagedData(),
                aes(xmin = Avg_ENDLOCATIONX - SD_ENDLOCATIONX,
                    xmax = Avg_ENDLOCATIONX + SD_ENDLOCATIONX,
                    ymin = Avg_ENDLOCATIONY - SD_ENDLOCATIONY,
                    ymax = Avg_ENDLOCATIONY + SD_ENDLOCATIONY,
                    fill = as.factor(cluster)),
                alpha = 0.2) +
      
      scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) + 
      scale_fill_manual(values = c("red", "blue", "green", "purple", "orange")) +
      theme_pitch() +
      ggtitle("Gennemsnitlige afleveringer med standardafvigelse (vafler)") +
      theme(legend.position = "right")
  })
  
  output$clusterSummary <- renderTable({
    req(plotVisible())
    
    if (nrow(averagedData()) == 0) return(NULL)
    
    averagedData() %>%
      select(cluster, Antal, Avg_LENGTH, SD_LENGTH, Avg_ANGLE, SD_ANGLE)
  })
}

# Run App
shinyApp(ui = ui, server = server)

}

# Shiny app med spillere kun statistik
{
library(shiny)
library(ggplot2)
library(dplyr)
library(ggsoccer)

# UI
ui <- fluidPage(
  titlePanel("Spillerstatistik - Afleveringsanalyse"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Vælg Hold:", choices = NULL, selected = NULL),
      selectInput("role", "Vælg Rolle:", choices = NULL, selected = NULL),
      actionButton("showStats", "Vis statistik")  # Knappen til at vise statistik
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Spillerstatistik", tableOutput("playerStatsTable"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Shiny_app.df
  
  # Opdater dropdowns når appen starter (med alle hold)
  updateSelectInput(session, "team", choices = sort(unique(data$TEAMNAME)))
  
  # Dynamisk opdatering af roller baseret på valgt hold
  observeEvent(input$team, {
    if (!is.null(input$team)) {
      roller <- data %>%
        filter(TEAMNAME == input$team) %>%
        pull(ROLENAME) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session, "role", choices = roller)
    }
  })
  
  # Reaktiv værdi til at styre, om statistikken skal vises
  statsVisible <- reactiveVal(FALSE)
  
  # Når knappen trykkes, vises statistikken
  observeEvent(input$showStats, {
    if (!is.null(input$team)) {
      statsVisible(TRUE)
    } else {
      showNotification("Vælg et hold før du trykker 'Vis statistik'.", type = "warning")
    }
  })
  
  # Beregning af spillerstatistik
  playerStats <- reactive({
    req(statsVisible())
    
    data %>%
      filter(TEAMNAME == input$team, ROLENAME == input$role) %>%  # Filtrér på valgt hold og rolle
      group_by(SHORTNAME, ROLENAME) %>%
      summarise(
        Afleveringer = n(),
        Succesfulde = sum(ACCURATE, na.rm = TRUE),
        Mislykkede = Afleveringer - Succesfulde,
        Succesrate = round((Succesfulde / Afleveringer) * 100, 2),
        Gennemsnitlig_Længde = round(mean(LENGTH, na.rm = TRUE), 2)
      ) %>%
      arrange(desc(Afleveringer))
  })
  
  # Render tabelen med spillerstatistik
  output$playerStatsTable <- renderTable({
    req(playerStats())
    playerStats()
  })
}

# Run App
shinyApp(ui = ui, server = server)
}


# Shiny app med spillere og klynger
{
library(shiny)
library(ggplot2)
library(dplyr)
library(ggsoccer)

# UI
ui <- fluidPage(
  titlePanel("Shiny App - Spillerstatistik og Afleveringsplot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Vælg Hold:", choices = NULL, selected = NULL),
      selectInput("role", "Vælg Rolle:", choices = NULL, selected = NULL),
      selectInput("player", "Vælg Spiller:", choices = NULL, selected = NULL, multiple = TRUE),
      selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = NULL, multiple = TRUE),
      actionButton("showStats", "Vis Statistik"),
      actionButton("showPlot", "Vis Plot")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Spillerstatistik", tableOutput("playerStatsTable")),
        tabPanel("Afleveringsplot", plotOutput("passPlot", height = "700px"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Shiny_app.df
  
  # Opdater dropdowns når appen starter (med alle hold og clusters)
  updateSelectInput(session, "team", choices = sort(unique(data$TEAMNAME)))
  updateSelectInput(session, "cluster", choices = sort(unique(data$cluster)))
  
  # Dynamisk opdatering af roller baseret på valgt hold
  observeEvent(input$team, {
    if (!is.null(input$team)) {
      roller <- data %>%
        filter(TEAMNAME == input$team) %>%
        pull(ROLENAME) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session, "role", choices = c("Alle", roller))
    }
  })
  
  # Dynamisk opdatering af spillere baseret på valgt hold og rolle
  observeEvent(input$role, {
    if (!is.null(input$role) && !is.null(input$team)) {
      
      if (input$role == "Alle") {
        spillere <- data %>%
          filter(TEAMNAME == input$team) %>%
          pull(SHORTNAME) %>%
          unique() %>%
          sort()
      } else {
        spillere <- data %>%
          filter(TEAMNAME == input$team, ROLENAME == input$role) %>%
          pull(SHORTNAME) %>%
          unique() %>%
          sort()
      }
      
      updateSelectInput(session, "player", choices = c("Alle", spillere))
    }
  })
  
  # Reaktiv værdi til at styre, om statistikken skal vises
  statsVisible <- reactiveVal(FALSE)
  
  observeEvent(input$showStats, {
    statsVisible(TRUE)
  })
  
  # Reaktiv værdi til at styre, om plottet skal vises
  plotVisible <- reactiveVal(FALSE)
  
  observeEvent(input$showPlot, {
    plotVisible(TRUE)
  })
  
  # Beregning af spillerstatistik
  playerStats <- reactive({
    req(statsVisible())
    
    filtered <- data %>%
      filter(TEAMNAME == input$team)
    
    if (input$role != "Alle") {
      filtered <- filtered %>% filter(ROLENAME == input$role)
    }
    
    filtered %>%
      group_by(SHORTNAME, ROLENAME) %>%
      summarise(
        Afleveringer = n(),
        Succesfulde = sum(ACCURATE, na.rm = TRUE),
        Mislykkede = Afleveringer - Succesfulde,
        Succesrate = round((Succesfulde / Afleveringer) * 100, 2),
        Gennemsnitlig_Længde = round(mean(LENGTH, na.rm = TRUE), 2)
      ) %>%
      arrange(desc(Afleveringer))
  })
  
  output$playerStatsTable <- renderTable({
    req(playerStats())
    playerStats()
  })
  
  # Beregning af afleveringsplot
  filteredData <- reactive({
    req(plotVisible())
    
    filtered <- data
    
    if (length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    if (!is.null(input$team)) {
      filtered <- filtered %>% filter(TEAMNAME == input$team)
    }
    
    if (input$role != "Alle" && !is.null(input$role)) {
      filtered <- filtered %>% filter(ROLENAME == input$role)
    }
    
    if (length(input$player) > 0 && !("Alle" %in% input$player)) {
      filtered <- filtered %>% filter(SHORTNAME %in% input$player)
    }
    
    return(filtered)
  })
  
  output$passPlot <- renderPlot({
    req(filteredData())
    
    if (nrow(filteredData()) == 0) return(NULL)
    
    averagedData <- filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE)
      )
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "black", fill = "springgreen4") +
      geom_segment(data = averagedData, 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY, 
                       color = as.factor(cluster)),
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5) +
      theme_pitch() +
      theme(legend.position = "right") +
      ggtitle("Afleveringsplot")
  })
}

# Run App
shinyApp(ui = ui, server = server)
}


# Shiny app med spillere og klynger (Shiny Dashboards)
{
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(ggforce)
library(reactable)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Opgave 4.2 - Visualisering af Clustre for Afleveringer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Afleveringsplot", tabName = "plot", icon = icon("chart-line"))
    ),
    selectInput("team", "Vælg Hold:", choices = NULL, selected = NULL),
    selectInput("role", "Vælg Rolle:", choices = NULL, selected = NULL),
    selectInput("player", "Vælg Spiller:", choices = NULL, selected = NULL, multiple = TRUE),
    selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = NULL, multiple = TRUE),
    actionButton("showPlot", "Vis Plot", icon = icon("play"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot",
              fluidRow(
                box(
                  width = 8,
                  title = "Afleveringsplot",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("passPlot", height = "600px")
                ),
                box(
                  width = 4,
                  title = "Statistik",
                  status = "primary",
                  solidHeader = TRUE,
                  reactableOutput("plotStatsTable")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Shiny_app.df
  
  # Opdater dropdowns når appen starter (med alle hold og clusters)
  updateSelectInput(session, "team", choices = sort(unique(data$TEAMNAME)))
  updateSelectInput(session, "cluster", choices = sort(unique(data$cluster)))
  
  # Dynamisk opdatering af roller baseret på valgt hold
  observeEvent(input$team, {
    if (!is.null(input$team)) {
      roller <- data %>%
        filter(TEAMNAME == input$team) %>%
        pull(ROLENAME) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session, "role", choices = c("Alle", roller))
    }
  })
  
  # Dynamisk opdatering af spillere baseret på valgt hold og rolle
  observeEvent(input$role, {
    if (!is.null(input$role) && !is.null(input$team)) {
      
      spillere <- data %>%
        filter(TEAMNAME == input$team, ROLENAME == input$role) %>%
        pull(SHORTNAME) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session, "player", choices = c("Alle", spillere))
    }
  })
  
  # Reaktiv værdi til at styre, om plottet skal vises
  plotVisible <- reactiveVal(FALSE)
  
  observeEvent(input$showPlot, {
    plotVisible(TRUE)
  })
  
  # Filtrering af data
  filteredData <- reactive({
    req(plotVisible())
    
    filtered <- data
    
    if (length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    if (!is.null(input$team)) {
      filtered <- filtered %>% filter(TEAMNAME == input$team)
    }
    
    if (input$role != "Alle" && !is.null(input$role)) {
      filtered <- filtered %>% filter(ROLENAME == input$role)
    }
    
    if (length(input$player) > 0 && !("Alle" %in% input$player)) {
      filtered <- filtered %>% filter(SHORTNAME %in% input$player)
    }
    
    return(filtered)
  })
  
  output$passPlot <- renderPlot({
    req(filteredData())
    
    if (nrow(filteredData()) == 0) return(NULL)
    
    averagedData <- filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE),
        SD_LOCATIONX = sd(LOCATIONX, na.rm = TRUE),
        SD_LOCATIONY = sd(LOCATIONY, na.rm = TRUE),
        SD_ENDLOCATIONX = sd(ENDLOCATIONX, na.rm = TRUE),
        SD_ENDLOCATIONY = sd(ENDLOCATIONY, na.rm = TRUE)
      )
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "white", fill = "springgreen4") +
      
      geom_segment(data = averagedData, 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, 
                       xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY), 
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5, color = "red") +
      
      geom_ellipse(data = averagedData,
                   aes(x0 = Avg_LOCATIONX, y0 = Avg_LOCATIONY,
                       a = SD_LOCATIONX, b = SD_LOCATIONY, angle = 0),
                   fill = "red", alpha = 0.2, color = NA) +
      
      geom_ellipse(data = averagedData,
                   aes(x0 = Avg_ENDLOCATIONX, y0 = Avg_ENDLOCATIONY,
                       a = SD_ENDLOCATIONX, b = SD_ENDLOCATIONY, angle = 0),
                   fill = "red", alpha = 0.2, color = NA) +
      
      theme_pitch() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA)) +
      ggtitle("Afleveringsplot med gennemsnits-afleveringer og røde ellipser")
  })
  
  output$plotStatsTable <- renderReactable({
    req(filteredData())
    
    stats <- filteredData() %>%
      summarise(
        Afleveringer = n(),
        Succesfulde = sum(ACCURATE, na.rm = TRUE),
        Succesrate = round((Succesfulde / Afleveringer) * 100, 2),
        Gennemsnits_Længde = round(mean(LENGTH, na.rm = TRUE), 2),
        SD_Længde = round(sd(LENGTH, na.rm = TRUE), 2),
        Gennemsnits_Angle = round(mean(ANGLE, na.rm = TRUE), 2),
        SD_Angle = round(sd(ANGLE, na.rm = TRUE), 2)
      )
    
    reactable(stats, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
}

# Run App
shinyApp(ui = ui, server = server)

}

# 
{
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(ggforce)
library(reactable)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Opgave 4.1 - Afleveringer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Afleveringsplot", tabName = "plot", icon = icon("chart-line"))
    ),
    selectInput("team", "Vælg Hold:", choices = NULL, selected = "Alle"),
    selectInput("role", "Vælg Rolle:", choices = NULL, selected = "Alle"),
    selectInput("player", "Vælg Spiller:", choices = NULL, selected = "Alle", multiple = TRUE),
    selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = "Alle", multiple = TRUE),
    actionButton("showPlot", "Vis Plot", icon = icon("play"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot",
              fluidRow(
                box(
                  width = 12,
                  title = "Afleveringsplot",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("passPlot", height = "700px")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Statistik",
                  status = "primary",
                  solidHeader = TRUE,
                  reactableOutput("plotStatsTable")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Shiny_app.df
  
  # Opdater dropdowns når appen starter (med alle hold og clusters)
  updateSelectInput(session, "team", choices = c("Alle", sort(unique(data$TEAMNAME))))
  updateSelectInput(session, "cluster", choices = c("Alle", sort(unique(data$cluster))))
  
  # Dynamisk opdatering af roller baseret på valgt hold
  observeEvent(input$team, {
    relevant_data <- if (input$team == "Alle") data else filter(data, TEAMNAME == input$team)
    
    roller <- relevant_data %>%
      pull(ROLENAME) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "role", choices = c("Alle", roller))
  })
  
  # Dynamisk opdatering af spillere baseret på valgt hold og rolle
  observeEvent(input$role, {
    relevant_data <- data
    
    if (input$team != "Alle") {
      relevant_data <- relevant_data %>% filter(TEAMNAME == input$team)
    }
    
    if (input$role != "Alle") {
      relevant_data <- relevant_data %>% filter(ROLENAME == input$role)
    }
    
    spillere <- relevant_data %>%
      pull(SHORTNAME) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "player", choices = c("Alle", spillere))
  })
  
  # Reaktiv værdi til at styre, om plottet skal vises
  plotVisible <- reactiveVal(FALSE)
  
  observeEvent(input$showPlot, {
    plotVisible(TRUE)
  })
  
  # Filtrering af data
  filteredData <- reactive({
    req(plotVisible())
    
    filtered <- data
    
    if (input$cluster != "Alle" && length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    if (input$team != "Alle") {
      filtered <- filtered %>% filter(TEAMNAME == input$team)
    }
    
    if (input$role != "Alle") {
      filtered <- filtered %>% filter(ROLENAME == input$role)
    }
    
    if (!("Alle" %in% input$player) && length(input$player) > 0) {
      filtered <- filtered %>% filter(SHORTNAME %in% input$player)
    }
    
    return(filtered)
  })
  
  output$passPlot <- renderPlot({
    req(filteredData())
    
    if (nrow(filteredData()) == 0) return(NULL)
    
    averagedData <- filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE),
        SD_LOCATIONX = sd(LOCATIONX, na.rm = TRUE),
        SD_LOCATIONY = sd(LOCATIONY, na.rm = TRUE),
        SD_ENDLOCATIONX = sd(ENDLOCATIONX, na.rm = TRUE),
        SD_ENDLOCATIONY = sd(ENDLOCATIONY, na.rm = TRUE)
      )
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "white", fill = "springgreen4") +
      
      geom_segment(data = averagedData, 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, 
                       xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY), 
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5, color = "red") +
      
      geom_ellipse(data = averagedData,
                   aes(x0 = Avg_LOCATIONX, y0 = Avg_LOCATIONY,
                       a = SD_LOCATIONX, b = SD_LOCATIONY, angle = 0),
                   fill = "red", alpha = 0.2, color = NA) +
      
      geom_ellipse(data = averagedData,
                   aes(x0 = Avg_ENDLOCATIONX, y0 = Avg_ENDLOCATIONY,
                       a = SD_ENDLOCATIONX, b = SD_ENDLOCATIONY, angle = 0),
                   fill = "red", alpha = 0.2, color = NA) +
      
      theme_pitch() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA)) +
      ggtitle("Afleveringsplot med gennemsnits-afleveringer og røde ellipser")
  })
  
  output$plotStatsTable <- renderReactable({
    req(filteredData())
    
    stats <- filteredData() %>%
      summarise(
        Afleveringer = n(),
        Succesfulde = sum(ACCURATE, na.rm = TRUE),
        Succesrate = round((Succesfulde / Afleveringer) * 100, 2),
        Gennemsnits_Længde = round(mean(LENGTH, na.rm = TRUE), 2),
        SD_Længde = round(sd(LENGTH, na.rm = TRUE), 2),
        Gennemsnits_Angle = round(mean(ANGLE, na.rm = TRUE), 2),
        SD_Angle = round(sd(ANGLE, na.rm = TRUE), 2)
      )
    
    reactable(stats, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
}

# Run App
shinyApp(ui = ui, server = server)
}

# Alt ovenfor med spiller stats
{
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(ggforce)
library(reactable)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Opgave 4.1 - Afleveringer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Afleveringsplot", tabName = "plot", icon = icon("chart-line"))
    ),
    selectInput("team", "Vælg Hold:", choices = NULL, selected = "Alle"),
    selectInput("role", "Vælg Rolle:", choices = NULL, selected = "Alle"),
    selectInput("player", "Vælg Spiller:", choices = NULL, selected = "Alle", multiple = TRUE),
    selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = "Alle", multiple = TRUE),
    actionButton("showPlot", "Vis Plot", icon = icon("play"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot",
              fluidRow(
                box(
                  width = 12,
                  title = "Afleveringsplot",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("passPlot", height = "700px")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Statistik (Grupperet på Spiller Niveau)",
                  status = "primary",
                  solidHeader = TRUE,
                  reactableOutput("plotStatsTable")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Shiny_app.df
  
  # Opdater dropdowns når appen starter (med alle hold og clusters)
  updateSelectInput(session, "team", choices = c("Alle", sort(unique(data$TEAMNAME))))
  updateSelectInput(session, "cluster", choices = c("Alle", sort(unique(data$cluster))))
  
  # Dynamisk opdatering af roller baseret på valgt hold
  observeEvent(input$team, {
    relevant_data <- if (input$team == "Alle") data else filter(data, TEAMNAME == input$team)
    
    roller <- relevant_data %>%
      pull(ROLENAME) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "role", choices = c("Alle", roller))
  })
  
  # Dynamisk opdatering af spillere baseret på valgt hold og rolle
  observeEvent(input$role, {
    relevant_data <- data
    
    if (input$team != "Alle") {
      relevant_data <- relevant_data %>% filter(TEAMNAME == input$team)
    }
    
    if (input$role != "Alle") {
      relevant_data <- relevant_data %>% filter(ROLENAME == input$role)
    }
    
    spillere <- relevant_data %>%
      pull(SHORTNAME) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "player", choices = c("Alle", spillere))
  })
  
  # Reaktiv værdi til at styre, om plottet skal vises
  plotVisible <- reactiveVal(FALSE)
  
  observeEvent(input$showPlot, {
    plotVisible(TRUE)
  })
  
  # Filtrering af data
  filteredData <- reactive({
    req(plotVisible())
    
    filtered <- data
    
    if (!("Alle" %in% input$cluster) && length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    if (input$team != "Alle") {
      filtered <- filtered %>% filter(TEAMNAME == input$team)
    }
    
    if (input$role != "Alle") {
      filtered <- filtered %>% filter(ROLENAME == input$role)
    }
    
    if (!(length(input$player) == 1 && input$player == "Alle") && length(input$player) > 0) {
      filtered <- filtered %>% filter(SHORTNAME %in% input$player)
    }
    
    return(filtered)
  })
  
  output$plotStatsTable <- renderReactable({
    req(filteredData())
    
    stats <- filteredData() %>%
      group_by(SHORTNAME) %>%
      summarise(
        Afleveringer = n(),
        Succesfulde = sum(ACCURATE, na.rm = TRUE),
        Succesrate = round((Succesfulde / Afleveringer) * 100, 2),
        Gennemsnits_Længde = round(mean(LENGTH, na.rm = TRUE), 2),
        SD_Længde = round(sd(LENGTH, na.rm = TRUE), 2),
        Gennemsnits_Angle = round(mean(ANGLE, na.rm = TRUE), 2),
        SD_Angle = round(sd(ANGLE, na.rm = TRUE), 2)
      )
    
    reactable(stats, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
  
  output$passPlot <- renderPlot({
    req(filteredData())
    
    if (nrow(filteredData()) == 0) return(NULL)
    
    averagedData <- filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE)
      )
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "white", fill = "springgreen4") +
      geom_segment(data = averagedData, 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, 
                       xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY), 
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5, color = "red") +
      theme_pitch() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA)) +
      ggtitle("Afleveringsplot med gennemsnits-afleveringer")
  })
}

# Run App
shinyApp(ui = ui, server = server)
}


# Oven på med top 5 i siden
{
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(ggforce)
library(reactable)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Opgave 4.1 - Afleveringer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Afleveringsplot", tabName = "plot", icon = icon("chart-line"))
    ),
    selectInput("team", "Vælg Hold:", choices = NULL, selected = "Alle"),
    selectInput("role", "Vælg Rolle:", choices = NULL, selected = "Alle"),
    selectInput("player", "Vælg Spiller:", choices = NULL, selected = "Alle", multiple = TRUE),
    selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = "Alle", multiple = TRUE),
    actionButton("showPlot", "Vis Plot", icon = icon("play"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot",
              fluidRow(
                box(
                  width = 9,
                  title = "Afleveringsplot",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("passPlot", height = "700px")
                ),
                box(
                  width = 3,
                  title = "Top 5 Bedste Spillere",
                  status = "primary",
                  solidHeader = TRUE,
                  reactableOutput("top5Table")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Statistik (Grupperet på Spiller Niveau)",
                  status = "primary",
                  solidHeader = TRUE,
                  reactableOutput("plotStatsTable")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Shiny_app.df
  
  # Opdater dropdowns når appen starter (med alle hold og clusters)
  updateSelectInput(session, "team", choices = c("Alle", sort(unique(data$TEAMNAME))))
  updateSelectInput(session, "cluster", choices = c("Alle", sort(unique(data$cluster))))
  
  # Dynamisk opdatering af roller baseret på valgt hold
  observeEvent(input$team, {
    relevant_data <- if (input$team == "Alle") data else filter(data, TEAMNAME == input$team)
    
    roller <- relevant_data %>%
      pull(ROLENAME) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "role", choices = c("Alle", roller))
  })
  
  # Dynamisk opdatering af spillere baseret på valgt hold og rolle
  observeEvent(input$role, {
    relevant_data <- data
    
    if (input$team != "Alle") {
      relevant_data <- relevant_data %>% filter(TEAMNAME == input$team)
    }
    
    if (input$role != "Alle") {
      relevant_data <- relevant_data %>% filter(ROLENAME == input$role)
    }
    
    spillere <- relevant_data %>%
      pull(SHORTNAME) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "player", choices = c("Alle", spillere))
  })
  
  # Reaktiv værdi til at styre, om plottet skal vises
  plotVisible <- reactiveVal(FALSE)
  
  observeEvent(input$showPlot, {
    plotVisible(TRUE)
  })
  
  # Filtrering af data
  filteredData <- reactive({
    req(plotVisible())
    
    filtered <- data
    
    if (!("Alle" %in% input$cluster) && length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    if (input$team != "Alle") {
      filtered <- filtered %>% filter(TEAMNAME == input$team)
    }
    
    if (input$role != "Alle") {
      filtered <- filtered %>% filter(ROLENAME == input$role)
    }
    
    if (!(length(input$player) == 1 && input$player == "Alle") && length(input$player) > 0) {
      filtered <- filtered %>% filter(SHORTNAME %in% input$player)
    }
    
    return(filtered)
  })
  
  output$top5Table <- renderReactable({
    req(filteredData())
    
    top5 <- filteredData() %>%
      group_by(SHORTNAME) %>%
      summarise(
        Afleveringer = n(),
        Succesrate = round((sum(ACCURATE, na.rm = TRUE) / Afleveringer) * 100, 2)
      ) %>%
      arrange(desc(Afleveringer)) %>%
      slice_head(n = 5)
    
    reactable(top5, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
  
  output$plotStatsTable <- renderReactable({
    req(filteredData())
    
    stats <- filteredData() %>%
      group_by(SHORTNAME) %>%
      summarise(
        Afleveringer = n(),
        Succesfulde = sum(ACCURATE, na.rm = TRUE),
        Succesrate = round((Succesfulde / Afleveringer) * 100, 2),
        Gennemsnits_Længde = round(mean(LENGTH, na.rm = TRUE), 2),
        SD_Længde = round(sd(LENGTH, na.rm = TRUE), 2),
        Gennemsnits_Angle = round(mean(ANGLE, na.rm = TRUE), 2),
        SD_Angle = round(sd(ANGLE, na.rm = TRUE), 2)
      ) %>%
      arrange(desc(Afleveringer))  # Sortér tabel efter antal afleveringer
    
    reactable(stats, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
  
  output$passPlot <- renderPlot({
    req(filteredData())
    
    if (nrow(filteredData()) == 0) return(NULL)
    
    averagedData <- filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE)
      )
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "white", fill = "springgreen4") +
      geom_segment(data = averagedData, 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, 
                       xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY), 
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5, color = "red") +
      theme_pitch() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA)) +
      ggtitle("Afleveringsplot med gennemsnits-afleveringer")
  })
}

# Run App
shinyApp(ui = ui, server = server)
}
}

# Færdig 4.1 !!
{
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(ggforce)
library(reactable)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Opgave 4.1 - Afleveringer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Afleveringsplot", tabName = "plot", icon = icon("chart-line"))
    ),
    selectInput("team", "Vælg Hold:", choices = NULL, selected = "Alle"),
    selectInput("role", "Vælg Rolle:", choices = NULL, selected = "Alle"),
    selectInput("player", "Vælg Spiller:", choices = NULL, selected = "Alle", multiple = TRUE),
    selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = "Alle", multiple = TRUE),
    actionButton("showPlot", "Vis Plot", icon = icon("play"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot",
              fluidRow(
                box(
                  width = 9,
                  title = "Afleveringsplot",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("passPlot", height = "700px")
                ),
                box(
                  width = 3,
                  title = "Top 5 Bedste Spillere",
                  status = "primary",
                  solidHeader = TRUE,
                  reactableOutput("top5Table")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Statistik (Grupperet på Spiller Niveau)",
                  status = "primary",
                  solidHeader = TRUE,
                  reactableOutput("plotStatsTable")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Shiny_app.df
  
  # Opdater dropdowns når appen starter (med alle hold og clusters)
  updateSelectInput(session, "team", choices = c("Alle", sort(unique(data$TEAMNAME))))
  updateSelectInput(session, "cluster", choices = c("Alle", sort(unique(data$cluster))))
  
  # Dynamisk opdatering af roller baseret på valgt hold
  observeEvent(input$team, {
    relevant_data <- if (input$team == "Alle") data else filter(data, TEAMNAME == input$team)
    
    roller <- relevant_data %>%
      pull(ROLENAME) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "role", choices = c("Alle", roller))
  })
  
  # Dynamisk opdatering af spillere baseret på valgt hold og rolle
  observeEvent(input$role, {
    relevant_data <- data
    
    if (input$team != "Alle") {
      relevant_data <- relevant_data %>% filter(TEAMNAME == input$team)
    }
    
    if (input$role != "Alle") {
      relevant_data <- relevant_data %>% filter(ROLENAME == input$role)
    }
    
    spillere <- relevant_data %>%
      pull(SHORTNAME) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "player", choices = c("Alle", spillere))
  })
  
  # Reaktiv værdi til at styre, om plottet skal vises
  plotVisible <- reactiveVal(FALSE)
  
  observeEvent(input$showPlot, {
    plotVisible(TRUE)
  })
  
  # Filtrering af data
  filteredData <- reactive({
    req(plotVisible())
    
    filtered <- data
    
    if (!("Alle" %in% input$cluster) && length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    if (input$team != "Alle") {
      filtered <- filtered %>% filter(TEAMNAME == input$team)
    }
    
    if (input$role != "Alle") {
      filtered <- filtered %>% filter(ROLENAME == input$role)
    }
    
    if (!(length(input$player) == 1 && input$player == "Alle") && length(input$player) > 0) {
      filtered <- filtered %>% filter(SHORTNAME %in% input$player)
    }
    
    return(filtered)
  })
  
  output$top5Table <- renderReactable({
    req(filteredData())
    
    top5 <- filteredData() %>%
      group_by(SHORTNAME) %>%
      summarise(
        Afleveringer = n(),
        Succesrate = round((sum(ACCURATE, na.rm = TRUE) / Afleveringer) * 100, 2)
      ) %>%
      arrange(desc(Afleveringer)) %>%
      slice_head(n = 5)
    
    reactable(top5, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
  
  output$plotStatsTable <- renderReactable({
    req(filteredData())
    
    stats <- filteredData() %>%
      group_by(SHORTNAME) %>%
      summarise(
        Afleveringer = n(),
        Succesfulde = sum(ACCURATE, na.rm = TRUE),
        Succesrate = round((Succesfulde / Afleveringer) * 100, 2),
        Gennemsnits_Længde = round(mean(LENGTH, na.rm = TRUE), 2),
        SD_Længde = round(sd(LENGTH, na.rm = TRUE), 2),
        Gennemsnits_Angle = round(mean(ANGLE, na.rm = TRUE), 2),
        SD_Angle = round(sd(ANGLE, na.rm = TRUE), 2)
      ) %>%
      arrange(desc(Afleveringer))
    
    reactable(stats, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
  
  output$passPlot <- renderPlot({
    req(filteredData())
    
    if (nrow(filteredData()) == 0) return(NULL)
    
    averagedData <- filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE),
        SD_LOCATIONX = sd(LOCATIONX, na.rm = TRUE),
        SD_LOCATIONY = sd(LOCATIONY, na.rm = TRUE),
        SD_ENDLOCATIONX = sd(ENDLOCATIONX, na.rm = TRUE),
        SD_ENDLOCATIONY = sd(ENDLOCATIONY, na.rm = TRUE)
      )
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "white", fill = "springgreen4") +
      geom_segment(data = averagedData, 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, 
                       xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY), 
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5, color = "red") +
      geom_ellipse(data = averagedData, 
                   aes(x0 = Avg_LOCATIONX, y0 = Avg_LOCATIONY, a = SD_LOCATIONX, b = SD_LOCATIONY, angle = 0), 
                   fill = "red", color = NA, alpha = 0.2) +
      geom_ellipse(data = averagedData, 
                   aes(x0 = Avg_ENDLOCATIONX, y0 = Avg_ENDLOCATIONY, a = SD_ENDLOCATIONX, b = SD_ENDLOCATIONY, angle = 0), 
                   fill = "red", color = NA, alpha = 0.2) +
      theme_pitch() +
      ggtitle("Afleveringsplot med gennemsnit og standardafvigelse")
  })
}

# Run App
shinyApp(ui = ui, server = server)

}

#---------------------------------#
# Opgave 4.2 - Shiny app m. kampe #
#---------------------------------#
{
{
library(shiny)
library(ggplot2)
library(dplyr)
library(ggsoccer)

# UI
ui <- fluidPage(
  titlePanel("Opgave 4.2 - Visualisering af Clustre for Afleveringer givet en Kamp"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Vælg Hold:", choices = NULL, selected = NULL),
      selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = NULL, multiple = TRUE),
      selectInput("match", "Vælg Kamp:", choices = NULL, selected = NULL, multiple = TRUE),
      actionButton("showPlot", "Vis plot")  # Plot knap
    ),
    mainPanel(
      plotOutput("passPlot", height = "700px"),
      tableOutput("clusterSummary")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Shiny_app.df
  
  # Tilføj en kolonne der kombinerer Hold og Resultat til visning i dropdown-menuen
  data <- data %>%
    mutate(Match_Info = paste(Hold, " - Resultat:", Resultat))
  
  # Opdater dropdown for Hold (unikke holdnavne)
  updateSelectInput(session, "team", choices = sort(unique(data$TEAMNAME)))
  
  # Filtrer kampe baseret på valgt hold
  observeEvent(input$team, {
    if (!is.null(input$team)) {
      relevant_matches <- data %>%
        filter(TEAMNAME == input$team) %>%
        select(Match_Info) %>%
        unique()
      
      updateSelectInput(session, "match", choices = sort(relevant_matches$Match_Info))
    }
  })
  
  # Opdater dropdowns når appen starter (med alle clusters)
  updateSelectInput(session, "cluster", choices = sort(unique(data$cluster)))
  
  # Reaktiv værdi til at styre, om plot skal vises
  plotVisible <- reactiveVal(FALSE)
  
  # Når knappen trykkes, vises plottet, men kun hvis der er lavet et valg
  observeEvent(input$showPlot, {
    if (length(input$cluster) > 0 | length(input$match) > 0) {
      plotVisible(TRUE)
    } else {
      showNotification("Vælg mindst ét cluster eller en kamp før du trykker 'Vis plot'.", type = "warning")
    }
  })
  
  # Filtrering af data baseret på valg
  filteredData <- reactive({
    req(plotVisible())
    
    filtered <- data
    
    if (length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    if (length(input$match) > 0) {
      filtered <- filtered %>% filter(Match_Info %in% input$match)
    }
    
    return(filtered)
  })
  
  # Beregning af gennemsnitlige afleveringer og standardafvigelser
  averagedData <- reactive({
    req(plotVisible())
    
    filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE),
        
        # Standardafvigelser for start- og slutpunkter
        SD_LOCATIONX = sd(LOCATIONX, na.rm = TRUE),
        SD_LOCATIONY = sd(LOCATIONY, na.rm = TRUE),
        SD_ENDLOCATIONX = sd(ENDLOCATIONX, na.rm = TRUE),
        SD_ENDLOCATIONY = sd(ENDLOCATIONY, na.rm = TRUE),
        
        Avg_LENGTH = mean(LENGTH, na.rm = TRUE),
        Avg_ANGLE = mean(ANGLE, na.rm = TRUE),
        SD_LENGTH = sd(LENGTH, na.rm = TRUE),
        SD_ANGLE = sd(ANGLE, na.rm = TRUE),
        
        Antal = n()
      )
  })
  
  output$passPlot <- renderPlot({
    req(plotVisible())
    
    if (nrow(averagedData()) == 0) {
      showNotification("Ingen data tilgængelig. Vælg et cluster eller en kamp.", type = "warning")
      return(NULL)
    }
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "black", fill = "springgreen4") +
      
      # Tegner gennemsnitsafleveringerne
      geom_segment(data = averagedData(), 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY, 
                       color = as.factor(cluster)),
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5) +
      
      # Tegner SD-området ved STARTpunktet (som før)
      geom_rect(data = averagedData(),
                aes(xmin = Avg_LOCATIONX - SD_LOCATIONX,
                    xmax = Avg_LOCATIONX + SD_LOCATIONX,
                    ymin = Avg_LOCATIONY - SD_LOCATIONY,
                    ymax = Avg_LOCATIONY + SD_LOCATIONY,
                    fill = as.factor(cluster)),
                alpha = 0.2) +
      
      # Tegner SD-området ved ENDpunktet (NYT!)
      geom_rect(data = averagedData(),
                aes(xmin = Avg_ENDLOCATIONX - SD_ENDLOCATIONX,
                    xmax = Avg_ENDLOCATIONX + SD_ENDLOCATIONX,
                    ymin = Avg_ENDLOCATIONY - SD_ENDLOCATIONY,
                    ymax = Avg_ENDLOCATIONY + SD_ENDLOCATIONY,
                    fill = as.factor(cluster)),
                alpha = 0.2) +
      
      scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) + 
      scale_fill_manual(values = c("red", "blue", "green", "purple", "orange")) +
      theme_pitch() +
      ggtitle("Gennemsnitlige afleveringer med standardafvigelse for både start- og slutpunkter") +
      theme(legend.position = "right")
  })
  
  output$clusterSummary <- renderTable({
    req(plotVisible())
    
    if (nrow(averagedData()) == 0) return(NULL)
    
    averagedData() %>%
      select(cluster, Antal, Avg_LENGTH, SD_LENGTH, Avg_ANGLE, SD_ANGLE)
  })
}

# Run App
shinyApp(ui = ui, server = server)

}
# Pæn version af Shiny
{
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(ggsoccer)

app_theme <- shinytheme("superhero")

# UI
ui <- navbarPage(
  theme = app_theme,
  "xG Model Explorer",
  
  tabPanel("Skudanalyse",
           fluidPage(
             div(style = "background-color: #2c3e50; padding: 15px; color: white; text-align: center; font-size: 36px; font-weight: bold; padding-bottom: 20px;",
                 "Opgave 4.2 - Visualisering af Clustre for Afleveringer givet en Kamp"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("team", "Vælg Hold:", choices = NULL, selected = NULL),
                 selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = NULL, multiple = TRUE),
                 selectInput("match", "Vælg Kamp:", choices = NULL, selected = NULL, multiple = TRUE),
                 actionButton("showPlot", "Vis plot")  # Plot knap
               ),
               mainPanel(
                 plotOutput("passPlot", height = "700px"),
                 tableOutput("clusterSummary")
               )
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  # Indlæs data direkte fra miljøet
  data <- Shiny_app.df
  
  # Tilføj en kolonne der kombinerer Hold og Resultat til visning i dropdown-menuen
  data <- data %>%
    mutate(Match_Info = paste(Hold, " - Resultat:", Resultat))
  
  # Opdater dropdown for Hold (unikke holdnavne)
  updateSelectInput(session, "team", choices = sort(unique(data$TEAMNAME)))
  
  # Filtrer kampe baseret på valgt hold
  observeEvent(input$team, {
    if (!is.null(input$team)) {
      relevant_matches <- data %>%
        filter(TEAMNAME == input$team) %>%
        select(Match_Info) %>%
        unique()
      
      updateSelectInput(session, "match", choices = sort(relevant_matches$Match_Info))
    }
  })
  
  # Opdater dropdowns når appen starter (med alle clusters)
  updateSelectInput(session, "cluster", choices = sort(unique(data$cluster)))
  
  # Reaktiv værdi til at styre, om plot skal vises
  plotVisible <- reactiveVal(FALSE)
  
  # Når knappen trykkes, vises plottet, men kun hvis der er lavet et valg
  observeEvent(input$showPlot, {
    if (length(input$cluster) > 0 | length(input$match) > 0) {
      plotVisible(TRUE)
    } else {
      showNotification("Vælg mindst ét cluster eller en kamp før du trykker 'Vis plot'.", type = "warning")
    }
  })
  
  # Filtrering af data baseret på valg
  filteredData <- reactive({
    req(plotVisible())
    
    filtered <- data
    
    if (length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    if (length(input$match) > 0) {
      filtered <- filtered %>% filter(Match_Info %in% input$match)
    }
    
    return(filtered)
  })
  
  # Beregning af gennemsnitlige afleveringer og standardafvigelser
  averagedData <- reactive({
    req(plotVisible())
    
    filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE),
        
        # Standardafvigelser for start- og slutpunkter
        SD_LOCATIONX = sd(LOCATIONX, na.rm = TRUE),
        SD_LOCATIONY = sd(LOCATIONY, na.rm = TRUE),
        SD_ENDLOCATIONX = sd(ENDLOCATIONX, na.rm = TRUE),
        SD_ENDLOCATIONY = sd(ENDLOCATIONY, na.rm = TRUE),
        
        Avg_LENGTH = mean(LENGTH, na.rm = TRUE),
        Avg_ANGLE = mean(ANGLE, na.rm = TRUE),
        SD_LENGTH = sd(LENGTH, na.rm = TRUE),
        SD_ANGLE = sd(ANGLE, na.rm = TRUE),
        
        Antal = n()
      )
  })
  
  output$passPlot <- renderPlot({
    req(plotVisible())
    
    if (nrow(averagedData()) == 0) {
      showNotification("Ingen data tilgængelig. Vælg et cluster eller en kamp.", type = "warning")
      return(NULL)
    }
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "black", fill = "springgreen4") +
      
      geom_segment(data = averagedData(), 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY, 
                       color = as.factor(cluster)),
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5) +
      
      geom_rect(data = averagedData(),
                aes(xmin = Avg_LOCATIONX - SD_LOCATIONX,
                    xmax = Avg_LOCATIONX + SD_LOCATIONX,
                    ymin = Avg_LOCATIONY - SD_LOCATIONY,
                    ymax = Avg_LOCATIONY + SD_LOCATIONY,
                    fill = as.factor(cluster)),
                alpha = 0.2) +
      
      geom_rect(data = averagedData(),
                aes(xmin = Avg_ENDLOCATIONX - SD_ENDLOCATIONX,
                    xmax = Avg_ENDLOCATIONX + SD_ENDLOCATIONX,
                    ymin = Avg_ENDLOCATIONY - SD_ENDLOCATIONY,
                    ymax = Avg_ENDLOCATIONY + SD_ENDLOCATIONY,
                    fill = as.factor(cluster)),
                alpha = 0.2) +
      
      scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) + 
      scale_fill_manual(values = c("red", "blue", "green", "purple", "orange")) +
      theme_pitch() +
      ggtitle("Gennemsnitlige afleveringer med standardafvigelse for både start- og slutpunkter") +
      theme(legend.position = "right")
  })
  
  output$clusterSummary <- renderTable({
    req(plotVisible())
    
    if (nrow(averagedData()) == 0) return(NULL)
    
    averagedData() %>%
      select(cluster, Antal, Avg_LENGTH, SD_LENGTH, Avg_ANGLE, SD_ANGLE)
  })
}

# Run App
shinyApp(ui = ui, server = server)
}
}

# Færdig 4.2 !!
{
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(reactable)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Opgave 4.2 - Afleveringer med Kampe"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Afleveringsplot med kampe", tabName = "kamp_plot", icon = icon("futbol"))
    ),
    selectInput("team", "Vælg Hold:", choices = NULL, selected = "Alle"),
    selectInput("match", "Vælg Kamp:", choices = NULL, selected = "Alle"),
    selectInput("role", "Vælg Position:", choices = NULL, selected = "Alle"),
    selectInput("player", "Vælg Spiller:", choices = NULL, selected = "Alle"),
    selectInput("cluster", "Vælg Cluster:", choices = NULL, selected = "Alle", multiple = TRUE),
    actionButton("showPlot", "Vis Plot", icon = icon("play"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "kamp_plot",
              fluidRow(
                box(
                  width = 9,
                  title = "Afleveringsplot for Kamp",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("kampPassPlot", height = "700px")
                ),
                box(
                  width = 3,
                  title = "Top 5 Spillere i Kamp",
                  status = "primary",
                  solidHeader = TRUE,
                  reactableOutput("kampTop5Table")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Statistik for Kamp (Spiller Niveau)",
                  status = "primary",
                  solidHeader = TRUE,
                  reactableOutput("kampPlotStatsTable")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data <- Shiny_app.df
  
  # Tilføj Match_Info kolonne (Hold - Resultat) til data
  data <- data %>% mutate(Match_Info = paste(Hold, " - Resultat:", Resultat))
  
  # Opdatering af dropdowns
  updateSelectInput(session, "team", choices = c("Alle", sort(unique(data$TEAMNAME))))
  
  # Filtrer kampe baseret på valgt hold
  observeEvent(input$team, {
    relevant_matches <- data %>%
      filter(TEAMNAME == input$team) %>%
      pull(Match_Info) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "match", choices = c("Alle", relevant_matches))
  })
  
  # Filtrer positioner baseret på valgt kamp
  observeEvent(input$match, {
    relevant_positions <- data %>%
      filter(Match_Info == input$match) %>%
      pull(ROLENAME) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "role", choices = c("Alle", relevant_positions))
  })
  
  # Filtrer spillere baseret på valgt position og hold
  observeEvent(input$role, {
    relevant_players <- data %>%
      filter(TEAMNAME == input$team, ROLENAME == input$role) %>%
      pull(SHORTNAME) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "player", choices = c("Alle", relevant_players))
  })
  
  # Filtrer clusters baseret på valgt position
  observeEvent(input$role, {
    relevant_clusters <- data %>%
      filter(ROLENAME == input$role) %>%
      pull(cluster) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "cluster", choices = c("Alle", relevant_clusters))
  })
  
  # Reaktiv værdi til at styre, om plottet skal vises
  plotVisible <- reactiveVal(FALSE)
  
  observeEvent(input$showPlot, {
    plotVisible(TRUE)
  })
  
  filteredData <- reactive({
    req(plotVisible())
    
    filtered <- data
    
    if (input$team != "Alle") {
      filtered <- filtered %>% filter(TEAMNAME == input$team)
    }
    
    if (input$match != "Alle") {
      filtered <- filtered %>% filter(Match_Info == input$match)
    }
    
    if (input$role != "Alle") {
      filtered <- filtered %>% filter(ROLENAME == input$role)
    }
    
    if (input$player != "Alle") {
      filtered <- filtered %>% filter(SHORTNAME == input$player)
    }
    
    if (!("Alle" %in% input$cluster) && length(input$cluster) > 0) {
      filtered <- filtered %>% filter(cluster %in% input$cluster)
    }
    
    filtered
  })
  
  output$kampPassPlot <- renderPlot({
    req(filteredData())
    
    if (nrow(filteredData()) == 0) return(NULL)
    
    averagedData <- filteredData() %>%
      group_by(cluster) %>%
      summarise(
        Avg_LOCATIONX = mean(LOCATIONX, na.rm = TRUE),
        Avg_LOCATIONY = mean(LOCATIONY, na.rm = TRUE),
        Avg_ENDLOCATIONX = mean(ENDLOCATIONX, na.rm = TRUE),
        Avg_ENDLOCATIONY = mean(ENDLOCATIONY, na.rm = TRUE)
      )
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "white", fill = "springgreen4") +
      geom_segment(data = averagedData, 
                   aes(x = Avg_LOCATIONX, y = Avg_LOCATIONY, 
                       xend = Avg_ENDLOCATIONX, yend = Avg_ENDLOCATIONY), 
                   arrow = arrow(length = unit(0.15, "cm")), linewidth = 1.5, color = "red") +
      theme_pitch() +
      ggtitle("Afleveringsplot for Kamp")
  })
  
  output$kampTop5Table <- renderReactable({
    req(filteredData())
    
    top5 <- filteredData() %>%
      group_by(SHORTNAME) %>%
      summarise(
        Afleveringer = n(),
        Succesrate = round((sum(ACCURATE, na.rm = TRUE) / Afleveringer) * 100, 2)
      ) %>%
      arrange(desc(Afleveringer)) %>%
      slice_head(n = 5)
    
    reactable(top5, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
  
  output$kampPlotStatsTable <- renderReactable({
    req(filteredData())
    
    stats <- filteredData() %>%
      group_by(SHORTNAME) %>%
      summarise(
        Afleveringer = n(),
        Succesrate = round((sum(ACCURATE, na.rm = TRUE) / Afleveringer) * 100, 2)
      ) %>%
      arrange(desc(Afleveringer))
    
    reactable(stats, bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
}

shinyApp(ui = ui, server = server)
}

# Vi gemmer rds
saveRDS(Shiny_app.df, "Shiny_app.rds")


