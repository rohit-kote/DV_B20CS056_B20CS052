library(ggplot2)
library(dplyr)
library(plotly)

# Function to load data
loadData <- function() {
  data <- read.csv("D:/Btech/year4_sem2/Data_visulisation/project/season_wise_meta_data.csv")
  if (nrow(data) == 0) {
    stop("Data failed to load or is empty.")
  }
  return(data)
}

# Function to generate plots
generatePlot <- function(filtered_data, plotType) {
  if (nrow(filtered_data) == 0) {
    return(NULL)
  }
  
  # Generate plots based on input
  p <- NULL
  if (plotType  == "performance") {
    p <- ggplot(filtered_data, aes(x = as.factor(season), group = Player, color = Player)) +
      geom_line(aes(y = goals, linetype = "Goals")) +
      geom_line(aes(y = assists, linetype = "Assists")) +
      labs(title = "Performance Over Seasons", x = "Season", y = "Count") +
      scale_linetype_manual(values = c("Goals" = "solid", "Assists" = "dotted")) +
      theme_minimal()
  } else if (plotType  == "goals_assists") {
    p <- ggplot(filtered_data, aes(x = as.factor(season), fill = Player)) +
      geom_bar(aes(y = goals), stat = "identity", position = "dodge", color = "black") +
      geom_bar(aes(y = assists), stat = "identity", position = "dodge", color = "black", alpha = 0.5) +
      labs(title = "Goals and Assists Comparison", x = "Season", y = "Count") +
      scale_fill_manual(values = c("Cristiano" = "lightblue", "Messi" = "#FA7070")) +
      theme_minimal()
  } else if (plotType  == "xg_goals") {
    p <- ggplot(filtered_data, aes(x = xG, y = goals, color = Player)) +
      geom_point(size = 2) +
      geom_smooth(method = "lm") +
      labs(title = "xG vs. Actual Goals", x = "Expected Goals (xG)", y = "Actual Goals") +
      theme_minimal()
  } else if (plotType  == "cards") {
    p <- ggplot(filtered_data, aes(x = as.factor(season))) +
      geom_bar(aes(y = yellow, fill = "Yellow"), stat = "identity", position = position_dodge(width = 0.8), alpha = 0.5) +
      geom_bar(aes(y = red, fill = "Red"), stat = "identity", position = position_dodge(width = 0.8), alpha = 0.5) +
      scale_fill_manual(values = c("Yellow" = "#FFEC9E", "Red" = "#FA7070")) +
      labs(title = "Yellow and Red Cards Comparison", x = "Season", y = "Number of Cards") +
      theme_minimal()
  } else if (plotType  == "shot_comparison") {
    p <- ggplot(filtered_data, aes(x = as.factor(season), group = Player, color = Player)) +
      geom_line(aes(y = shots, linetype = "Shots")) +
      labs(title = "Shot Comparison Over Seasons", x = "Season", y = "Count") +
      scale_linetype_manual(values = c("Shots" = "solid")) +
      theme_minimal()
  } else if (plotType  == "key_passes") {
    p <- ggplot(filtered_data, aes(x = as.factor(season), y = key_passes, fill = Player)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      labs(title = "Key Passes per Season", x = "Season", y = "Key Passes") +
      theme_minimal()
  }
  
  return(p)
}