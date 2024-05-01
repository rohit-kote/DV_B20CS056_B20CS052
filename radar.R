library(shiny)
library(plotly)
library(dplyr)

# Define the UI
ui <- fluidPage(
  titlePanel("Cristiano vs Messi Comparison"),
 
    
    mainPanel(
      plotlyOutput("radarPlot")
    )
  
)

# Define the server logic
server <- function(input, output) {
  
  # Function to generate radar plot
  output$radarPlot <- renderPlotly({
    
    # Data preprocessing
    num_cols <- c('goals', 'shots', 'xG', 'assists', 'xA', 'key_passes', 'npg', 'npxG', 'xGChain', 'xGBuildup')
    
    # Assuming you have a dataframe season_wise_meta_df available
    # Group by player and calculate sum of numeric columns
    season_wise_meta_df <- read.csv("D:/Btech/year4_sem2/Data_visulisation/project/season_wise_meta_data.csv")
    radar_df <- season_wise_meta_df %>%
      group_by(Player) %>%
      summarise(across(all_of(num_cols), sum)) %>%
      ungroup()
    
    # Radar plot
    fig <- plot_ly()
    
    # Add Cristiano's radar plot
    fig <- fig %>% 
      add_trace(
        type = 'scatterpolar',
        r = radar_df %>% filter(Player == "Cristiano") %>% select(all_of(num_cols)) %>% unlist() %>% as.vector(),
        theta = num_cols,
        fill = 'toself',
        name = 'Cristiano'
      )
    
    # Add Messi's radar plot
    fig <- fig %>% 
      add_trace(
        type = 'scatterpolar',
        r = radar_df %>% filter(Player == "Messi") %>% select(all_of(num_cols)) %>% unlist() %>% as.vector(),
        theta = num_cols,
        fill = 'toself',
        name = 'Messi'
      )
    
    # Layout customization
    fig <- fig %>% 
      layout(
        title = "Cristiano vs Messi",
        polar = list(radialaxis = list(visible = TRUE)),
        showlegend = TRUE
      )
    
    fig
  })
}

# Run the application
shinyApp(ui = ui, server = server)