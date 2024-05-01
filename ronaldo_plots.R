library(ggplot2)
library(plotly)
library(dplyr)

# Assume we have a function to load necessary Ronaldo data, which you can call in the main app.
loadRonaldoData <- function() {
  season_data <- read.csv("D:/Btech/year4_sem2/Data_visulisation/project/cris_season_wise_meta_df.csv")
  shots_data <- read.csv("D:/Btech/year4_sem2/Data_visulisation/project/shots_df.csv")
  cr7_data <- read.csv("D:/Btech/year4_sem2/Data_visulisation/project/cr7.csv")
  return(list(season_data = season_data, shots_data = shots_data, cr7_data = cr7_data))
}

# Function to generate plots for Cristiano Ronaldo
generateRonaldoPlot <- function(data, plotType) {
  season_data <- data$season_data
  shots_data <- data$shots_data
  cr7_data <- data$cr7_data
  
  p <- ggplot()  # Default empty ggplot object
  
  if (plotType == "performance") {
    p <- ggplot(data = season_data, aes(x = as.factor(season))) +
      geom_line(aes(y = goals, colour = "Goals")) +
      geom_line(aes(y = assists, colour = "Assists")) +
      geom_line(aes(y = xG, colour = "Expected Goals (xG)")) +
      labs(title = "Ronaldo Performance Over Seasons", x = "Season", y = "Count") +
      scale_color_manual(values = c("Goals" = "blue", "Assists" = "green", "Expected Goals (xG)" = "red"))
  } else if (plotType == "cards") {
        p <- ggplot(data = season_data, aes(x = as.factor(season))) +
          geom_bar(aes(y = yellow, fill = "Yellow Cards"), stat = "identity") +
          geom_bar(aes(y = red, fill = "Red Cards"), position = "dodge", stat = "identity") +
          labs(title = "Yellow and Red Cards per Season", x = "Season", y = "Number of Cards") +
          scale_fill_manual(values = c("Yellow Cards" = "yellow", "Red Cards" = "red"))
      } else if (plotType == "xg_goals") {
        p <- ggplot(data = season_data, aes(x = xG, y = goals)) +
          geom_point(aes(color = as.factor(season)), size = 3) +
          geom_smooth(method = "lm") +
          labs(title = "Relationship Between Expected Goals and Actual Goals", x = "Expected Goals (xG)", y = "Actual Goals")
      } else if (plotType == "shot_outcomes") {
        p <- ggplot(data = shots_data, aes(x = result)) +
          geom_bar(aes(fill = result)) +
          labs(title = "Distribution of Shot Outcomes", x = "Shot Outcome", y = "Count")
      } else if (plotType == "shot_locations") {
        p <- ggplot(data = shots_data, aes(x = X, y = Y)) +
          geom_bin2d(bins = 30, alpha = 0.5) +
          scale_fill_viridis_c() +
          labs(title = "Heat Map of Shot Locations", x = "Field X Coordinate", y = "Field Y Coordinate")
      } else if (plotType == "boxplot") {
        cr7_data$Goals_Scored <- as.numeric(sub(":.*", "", cr7_data$At_score))
        
        p <- ggplot(data = cr7_data, aes(x = Competition, y = Goals_Scored)) +
          geom_boxplot(fill = "yellow") +
          labs(x = "Competition", y = "Goals Scored", title = "Distribution of Goals Scored per Competition (Boxplot)") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      } else if (plotType == "goal_involvement") {
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
      } else if (plotType == "goals_per_club") {
        p <- ggplot(data = cr7_data, aes(x = Club, fill = Club)) +
          geom_bar(stat = "count") +
          labs(x = "Club", y = "Goals", title = "Goals per Club") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else if (plotType == "goals_per_minute") {
        # Data manipulation for density plot
        cr7_data$Minute <- as.character(cr7_data$Minute)
        cr7_data$Minute <- gsub("^45\\+.*", "45.5", cr7_data$Minute)
        cr7_data$Minute <- gsub("^90\\+.*", "90.5", cr7_data$Minute)
        cr7_data$Minute <- gsub("^120\\+.*", "120.5", cr7_data$Minute)
        cr7_data$Minute <- as.numeric(cr7_data$Minute)
        
        p <- ggplot(cr7_data, aes(x = Minute)) +
          geom_density(fill = "blue", alpha = 0.5) +
          labs(title = "Density of Goals per Minute",
               x = "Minute of the Game",
               y = "Density")
      } else if (plotType == "goals_per_opponent") {
        # Aggregate data
        opponent_data <- cr7_data %>%
          group_by(Opponent) %>%
          summarise(Goals = n(), .groups = 'drop') %>%
          arrange(desc(Goals))
        
        # Plot all data but focus initially on the first 15
        p <- ggplot(opponent_data, aes(x = reorder(Opponent, -Goals), y = Goals, fill = Opponent)) +
          geom_bar(stat = "identity") +
          labs(title = "Goals per Opponent", x = "Opponent", y = "Number of Goals") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                legend.position = "none") +  # Remove the legend
          coord_cartesian(xlim = c(1, 15), clip = "off")  # Limit view to first 15 but allow panning
        
        # Make the plot interactive with Plotly
        p <- ggplotly(p, tooltip = "y")
      } else if (plotType == "goals_assist") {
        # Prepare data: Count assists and sort by frequency
        assist_data <- cr7_data %>%
          filter(!is.na(Goal_assist)) %>%
          group_by(Goal_assist) %>%
          summarise(Count = n(), .groups = 'drop') %>%
          arrange(desc(Count))
        
        # Create the plot without a legend and allowing panning
        p <- ggplot(assist_data, aes(x = reorder(Goal_assist, -Count), y = Count, fill = Goal_assist)) +
          geom_bar(stat = "identity") +
          labs(title = "Goals Assist", x = "Number of Assists", y = "Frequency") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                legend.position = "none") +
          coord_cartesian(xlim = c(1, 15), clip = "off")  # Limit view to first 15 but allow panning
        
        # Make the plot interactive with Plotly
        p <- ggplotly(p, tooltip = "y")
      } else if (plotType == "goals_position") {
        # Calculate the count of goals scored per playing position
        goals_per_position <- cr7_data$Playing_Position %>% table()
        
        # Remove NaN values
        goals_per_position <- goals_per_position[!is.nan(as.numeric(names(goals_per_position))) & 
                                                   names(goals_per_position) != "" &
                                                   as.numeric(goals_per_position) != 0]  
        
        # Create a pie chart
        pie_chart <- plot_ly(labels = names(goals_per_position), values = as.numeric(goals_per_position), type = 'pie', 
                             marker = list(colors = RColorBrewer::brewer.pal(length(goals_per_position), "Set3")),
                             textinfo = 'percent+label')
        
        return(pie_chart)
      } else if (plotType == "goals_position_donut") {
        # Calculate the count of goals scored per playing position
        goals_per_position <- cr7_data$Type %>% table()
        
        # Remove NaN values
        goals_per_position <- goals_per_position[!is.nan(as.numeric(names(goals_per_position))) & 
                                                   names(goals_per_position) != "" &
                                                   as.numeric(goals_per_position) != 0]        
        
        # Calculate the percentage of each partition
        partition_percentage <- as.numeric(goals_per_position) / sum(as.numeric(goals_per_position))
        
        # Set a threshold for partition size to determine whether to place the label inside or outside
        threshold <- 0.05  # You can adjust this threshold as needed
        
        # Create a list to store text positions
        text_positions <- ifelse(partition_percentage > threshold, 'inside', 'outside')
        
        # Create a donut chart with dynamic text positions
        donut_chart <- plot_ly(labels = names(goals_per_position), values = as.numeric(goals_per_position), type = 'pie', 
                               marker = list(colors = RColorBrewer::brewer.pal(length(goals_per_position), "Set3")),
                               hole = 0.6,
                               textinfo = 'percent+label')
        return(donut_chart)
      }
      
  return(p)
}
