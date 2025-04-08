library(shiny)
library(shinythemes)
library(ggplot2)
library(gganimate)
library(ggsoccer)
library(gifski)
library(caret)        
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(stringr)
library(pROC)
library(ggsoccer)
library(xgboost)
library(themis)
library(RMariaDB)


# Læs din model og data ind
xgb_model_cv <- readRDS("xgb_model_cv.rds")

app_theme <- shinytheme("superhero")

ui <- navbarPage(
  theme = app_theme,
  "xG Model Explorer",
  
  tabPanel("Skudanalyse",
           fluidPage(
             div(style = "background-color: #2c3e50; padding: 15px; color: white; text-align: center; font-size: 36px; font-weight: bold; padding-bottom: 20px;",
                 "⚽ Forventet Målmodel (xG) Explorer"),
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("x", "Boldens X-position:", min = 0, max = 100, value = 80),
                 sliderInput("y", "Boldens Y-position:", min = 0, max = 100, value = 50),
                 selectInput("foot", "Skudtype (Fod):", 
                             choices = c("right", "left", "head_or_other")),
                 selectInput("preferred", "Foretrukken fod?", 
                             choices = c("Preferred", "Non-Preferred", "Non-Foot")),
                 selectInput("role", "Roller:", 
                             choices = c("Angriber", "Forsvar/Midtbane"))
               ),
               mainPanel(
                 div(style = "border: 10px solid black; border-radius: 15px; padding: 10px; background-color: #000; position: relative;", 
                     plotOutput("pitch_plot", height = "700px"),
                     absolutePanel(
                       top = 20, right = 30, draggable = FALSE,
                       div(style = "background-color: #111; padding: 10px 20px; border-radius: 10px; color: white; font-size: 24px; font-weight: bold;",
                           textOutput("xg_output")
                       )
                     )
                 )
               )
             )
           )
  )
)

server <- function(input, output, session) {
  output$pitch_plot <- renderPlot({
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, fill = "darkgreen", colour = "white") +
      
      # 🎯 Opdateret bold - hvid med sort kant
      geom_point(aes(x = input$x, y = input$y), 
                 shape = 21, fill = "yellow2", color = "black", size = 7) +
      
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
      theme_pitch()
  })
  
  output$xg_output <- renderText({
    # Beregn vinkel og afstand
    goal_x <- 100
    goal_y <- 50
    
    angle_calc <- function(x, y) {
      p1 <- c(100, 44)
      p2 <- c(100, 56)
      v1 <- c(p1[1] - x, p1[2] - y)
      v2 <- c(p2[1] - x, p2[2] - y)
      cos_theta <- sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
      angle <- acos(cos_theta) * (180 / pi)
      return(angle)
    }
    
    angle <- angle_calc(input$x, input$y)
    length <- sqrt((goal_x - input$x)^2 + (goal_y - input$y)^2)
    
    # Lav dataframe med inputs
    new_data <- data.frame(
      LENGTH = length,
      ANGLE = angle,
      SHOTBODYPART = input$foot,
      PREFERRED_FOOT = input$preferred,
      ROLEGROUP = input$role
    )
    
    # Forudsig xG
    xg_prob <- predict(xgb_model_cv, newdata = new_data, type = "prob")[, "Yes"]
    paste0("xG (sandsynlighed for mål): ", round(xg_prob * 100, 2), "%")
  })
}

shinyApp(ui, server)
