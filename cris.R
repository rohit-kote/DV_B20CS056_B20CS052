library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Load data
cris_season_data <- read.csv("D:/Btech/year4_sem2/Data_visulisation/project/messi_season_wise_meta_df.csv")
cris_shots_data <- read.csv("D:/Btech/year4_sem2/Data_visulisation/project/messi_shots_df.csv")

cr7_data <- read.csv("D:/Btech/year4_sem2/Data_visulisation/project/cr7.csv", header = TRUE)

ui <- fluidPage(
  titlePanel("Cristiano Ronaldo Performance Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Choose a plot type:",
                  choices = c("Performance Over Seasons" = "performance",
                              "Yellow and Red Cards per Season" = "cards",
                              "xG vs. Actual Goals" = "xg_goals",
                              "Distribution of Shot Outcomes" = "shot_outcomes",
                              "Heat Map of Shot Locations" = "shot_locations",
                              "Boxplot of Goals Scored by Competition" = "boxplot",
                              "Goal Involvement Analysis" = "goal_involvement",
                              "Goals per Club" = "goals_per_club"))
    ),
    mainPanel(
      plotlyOutput("selectedPlot")
    )
  )
)


server <- function(input, output) {
  observe({
    req(input$plotType)
    
    # Assuming data is loaded as 'cr7_data' for Cristiano Ronaldo
    cr7_data <- read.csv("path/to/your/cr7_data.csv") # You should replace this path with the correct file path
    
    if (input$plotType == "performance") {
      # Existing code for performance...
    } else if (input$plotType == "boxplot") {
      cr7_data$Goals_Scored <- as.numeric(sub(":.*", "", cr7_data$At_score))
      
      p <- ggplot(data = cr7_data, aes(x = Competition, y = Goals_Scored)) +
        geom_boxplot(fill = "yellow") +
        labs(x = "Competition", y = "Goals Scored", title = "Distribution of Goals Scored per Competition (Boxplot)") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    } else if (input$plotType == "goal_involvement") {
      cr7_data$Goals_Scored <- as.numeric(sub(":.*", "", cr7_data$At_score))
      cr7_data$Goal_assist <- as.numeric(cr7_data$Goal_assist)
      cr7_data$Goal_assist[is.na(cr7_data$Goal_assist)] <- 0
      cr7_data$Goal_Involvement <- cr7_data$Goals_Scored + cr7_data$Goal_assist
      
      total_goal_involvement_per_season <- aggregate(cr7_data$Goal_Involvement, by=list(cr7_data$Season), FUN=sum)
      
      p <- ggplot(data = total_goal_involvement_per_season, aes(x = Group.1, y = x)) +
        geom_line() +
        geom_point() +
        labs(x = "Season", y = "Total Goal Involvement", title = "Trend of Total Goal Involvement Over Seasons") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plotType == "goals_per_club") {
      p <- ggplot(data = cr7_data, aes(x = Club, fill = Club)) +
        geom_bar(stat = "count") +
        labs(x = "Club", y = "Goals", title = "Goals per Club") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    output$selectedPlot <- renderPlotly({
      ggplotly(p)
    })
  })
}

# Run the application


# Run the application 
shinyApp(ui = ui, server=server)
