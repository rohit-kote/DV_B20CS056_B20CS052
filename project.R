library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(plotly)
library(dplyr)
# Source the functions
source("D:/Btech/year4_sem2/Data_visulisation/project/Ronaldo_vs_messi/shots_Data.R")
source("D:/Btech/year4_sem2/Data_visulisation/project/Ronaldo_vs_messi/messi_plots.R")
source("D:/Btech/year4_sem2/Data_visulisation/project/Ronaldo_vs_messi/ronaldo_plots.R")

# Define UI using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Ronaldo vs Messi Performance Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Messi", tabName = "messi", icon = icon("user")),
      menuItem("Ronaldo", tabName = "ronaldo", icon = icon("user")),
      menuItem("Comparison", tabName = "comparison", icon = icon("users"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "messi",
              selectInput("messi_plotType", "Choose a plot type:",
                          choices = list(

                            "Yellow and Red Cards per Season" = "cards",
                            "xG vs. Actual Goals" = "xg_goals",
                            "Distribution of Shot Outcomes" = "shot_outcomes",
                            "Heat Map of Shot Locations" = "shot_locations",
                            "Boxplot of Goals Scored by Competition" = "boxplot",
                            "Goal Involvement Analysis" = "goal_involvement",
                            "Goals per Club" = "goals_per_club",
                            "Density Plot of Goals per Minute" = "goals_per_minute",
                            "Goals per Opponent" = "goals_per_opponent",
                            "Goals Assist" = "goals_assist",
                            "Goals Distribution by Playing Position" = "goals_position",
                            "Goals Distribution by Playing Position (Donut Chart)" = "goals_position_donut"
                          )),
              plotlyOutput("messiPlot")
      ),
      tabItem(tabName = "ronaldo",
              selectInput("ronaldo_plotType", "Choose a plot type:",
                          choices = list(
                           
                            "Yellow and Red Cards per Season" = "cards",
                            "xG vs. Actual Goals" = "xg_goals",
                            "Distribution of Shot Outcomes" = "shot_outcomes",
                            "Heat Map of Shot Locations" = "shot_locations",
                            "Boxplot of Goals Scored by Competition" = "boxplot",
                            "Goal Involvement Analysis" = "goal_involvement",
                            "Goals per Club" = "goals_per_club",
                            "Density Plot of Goals per Minute" = "goals_per_minute",
                            "Goals per Opponent" = "goals_per_opponent",
                            "Goals Assist" = "goals_assist",
                            "Goals Distribution by Playing Position" = "goals_position",
                            "Goals Distribution by Playing Position (Donut Chart)" = "goals_position_donut"
                          )),
              plotlyOutput("ronaldoPlot")
      ),
      tabItem(tabName = "comparison",
              selectInput("comparison_plotType", "Choose Comparison Type:",
                          choices = c("Performance Over Seasons" = "performance",
                                      "Goals and Assists" = "goals_assists",
                                      "xG vs. Actual Goals" = "xg_goals",
                                      "Yellow and Red Cards" = "cards",
                                      "Shot Comparison" = "shot_comparison",
                                      "Key Passes" = "key_passes")),
              selectInput("comparison_player", "Select Player:",
                          choices = c("Both" = "Both", "Ronaldo" = "Cristiano", "Messi" = "Messi")),
              plotlyOutput("comparisonPlot")
      )
    )
  )
)

server <- function(input, output) {
  # Load Messi and Ronaldo data upon app start-up
  messiData <- loadMessiData()
  ronaldoData <- loadRonaldoData()
  
  output$messiPlot <- renderPlotly({
    req(input$messi_plotType)
    p <- generateMessiPlot(messiData, input$messi_plotType)
    ggplotly(p)
  })
  
  output$ronaldoPlot <- renderPlotly({
    req(input$ronaldo_plotType)
    p <- generateRonaldoPlot(ronaldoData, input$ronaldo_plotType)
    ggplotly(p)
  })
  
  output$comparisonPlot <- renderPlotly({
    req(input$comparison_plotType, input$comparison_player)
    
    # Prepare data based on player selection for comparison
    filtered_data <- if (input$comparison_player == "Both") {
      # Assuming data structures are similar and can be combined directly
      bind_rows(
        messiData$season_data %>% mutate(Player = "Messi"),
        ronaldoData$season_data %>% mutate(Player = "Ronaldo")
      )
    } else if (input$comparison_player == "Cristiano") {
      ronaldoData$season_data %>% mutate(Player = "Ronaldo")
    } else {
      messiData$season_data %>% mutate(Player = "Messi")
    }
    
    # Generate the comparison plot based on selected plot type
    p <- generatePlot(filtered_data, input$comparison_plotType)
    ggplotly(p)
  })
}

shinyApp(ui, server)