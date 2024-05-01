library(plotly)
library(dplyr)
library(shiny)
library(shinydashboard)
library(RColorBrewer)

# Load Data
shots_df <- read.csv("D:/Btech/year4_sem2/Data_visulisation/project/shots_df.csv")
season_wise_meta_df <- read.csv("D:/Btech/year4_sem2/Data_visulisation/project/season_wise_meta_data.csv")

# Color palette
color_palette <- RColorBrewer::brewer.pal(3, "Set2")

# Plot 1: Shot Result Comparison
plot1 <- plot_ly(data = shots_df, x = ~result, color = ~Player, colors = color_palette, type = "histogram") %>%
  layout(title = "Shot Result Comparison", barmode = "group")

# Plot 2: Shot Play-pattern Comparison
plot2 <- plot_ly(data = shots_df, x = ~situation, color = ~Player, colors = color_palette, type = "histogram") %>%
  layout(title = "Shot Play-pattern Comparison", barmode = "group")

# Plot 3: Home-Away Goals Comparison
goal_shots_df <- shots_df[shots_df$result == "Goal",]
plot3 <- plot_ly(data = goal_shots_df, x = ~h_a, color = ~Player, colors = color_palette, type = "histogram") %>%
  layout(title = "Home-Away Goals Comparison", barmode = "group")

# Scatter plot with different symbols for each season
avg_goals <- mean(season_wise_meta_df$goals, na.rm = TRUE)
avg_shots <- mean(season_wise_meta_df$shots, na.rm = TRUE)
size_factor <- 5 # Adjust the size scaling factor as needed
plot4 <- plot_ly(data = season_wise_meta_df, x = ~shots, y = ~goals,
                 type = 'scatter', mode = 'markers',
                 hoverinfo = 'text',
                 marker = list(size = ~npg * 0.7, opacity = 0.5, symbol = ~as.numeric(season))) %>%
  add_trace(x = c(min(season_wise_meta_df$shots, na.rm = TRUE), max(season_wise_meta_df$shots, na.rm = TRUE)),
            y = rep(avg_goals, 2), mode = "lines", line = list(color = 'red'), name = "Avg. Goals") %>%
  add_trace(x = rep(avg_shots, 2),
            y = c(min(season_wise_meta_df$goals, na.rm = TRUE), max(season_wise_meta_df$goals, na.rm = TRUE)),
            mode = "lines", line = list(color = 'blue'), name = "Avg. Shots") %>%
  layout(title = "Shots vs Goals",
         xaxis = list(title = "Shots"),
         yaxis = list(title = "Goals"),
         showlegend = TRUE, legend = list(x = 1, xanchor = 'right', y = 1))

# Define Shiny UI using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Football Stats Analysis"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(width = 6, plotlyOutput("plot1")),
      column(width = 6, plotlyOutput("plot2"))
    ),
    fluidRow(
      column(width = 6, plotlyOutput("plot3")),
      column(width = 6, plotlyOutput("plot4"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$plot1 <- renderPlotly({ plot1 })
  output$plot2 <- renderPlotly({ plot2 })
  output$plot3 <- renderPlotly({ plot3 })
  plot4 %>%
    add_text(x = season_wise_meta_df$shots, y = season_wise_meta_df$goals,
             text = paste(season_wise_meta_df$Player),
             hoverinfo = "text", showlegend = FALSE, textposition = "top left")
  output$plot4 <- renderPlotly({ plot4 })
}

# Run the application
shinyApp(ui, server)
