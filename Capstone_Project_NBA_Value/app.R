#data
read.csv("/Users/home/Desktop/Capstone Git/current_player_preds.csv")

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",  
    bg = "#f8f9fa",      
    fg = "#212529",         
    primary = "#0d6efd", 
    base_font = font_google("Montserrat")
  ),
  
  # Page layout
  fluidRow(
    column(
      width = 12,
      align = "center",
      h2("NBA Performance Value Dashboard"),
      selectInput("team", "Select a Team:",
                  choices = sort(unique(current_players_preds$Team)),
                  selected = "LAL",
                  width = "300px")
    )
  ),
  br(),
  
  # Plot 1: Salary comparison
  fluidRow(
    column(
      width = 8, offset = 2,
      h4("True vs. Estimated Player Salaries", align = "center"),
      plotOutput("salary_plot"),
      p("This chart visualizes the difference between a players true salary, and their performance value. 
        It illustrates which players are under or over performing their contracts, and by how much."),
      br(), tags$hr(), br()
    )
  ),
  
  # Plot 2: Performance multiplier
  fluidRow(
    column(
      width = 8, offset = 2,
      h4("Performance Multipliers Relative to Salary", align = "center"),
      plotOutput("multiplier_plot"),
      p("This chart demonstrates the performance value of each player as a multiplier of their current contract. 
        Where the previous plot shows the raw distance, this adjusts to scale based on the size of the contract.")
    )
  )
)

server <- function(input, output) {
  
  team_data <- reactive({
    current_players_preds %>%
      filter(Team == input$team) %>%
      arrange(True.Salary) %>%
      mutate(
        Player = factor(Player, levels = Player),
        line_color = case_when(
          abs(True.Salary - Pred.Salary) <= 1e6 ~ "gold2",
          True.Salary > Pred.Salary ~ "red",
          TRUE ~ "green4"
        ),
        up_down = as.factor(case_when(
          abs(True.Salary - Pred.Salary) <= 1e6 ~ 23,  # Square
          True.Salary > Pred.Salary ~ 25,  # Down
          TRUE ~ 24  # Up
        ))
      )
  })
  
  output$salary_plot <- renderPlot({
    df <- team_data()
    ggplot(df, aes(x = Player)) +
      geom_segment(aes(x = Player, xend = Player, y = True.Salary, yend = Pred.Salary, color = line_color), linewidth = 1) +
      geom_point(aes(y = True.Salary, color = "black"), size = 2.5) +
      geom_point(aes(y = Pred.Salary, fill = line_color, shape = up_down),
                 color = df$line_color, size = 2) +
      labs(
        subtitle = "Black = Contract Value; Arrow = Performance Value; Yellow = Values Within 1M",
        x = "Player",
        y = "Salary"
      ) +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
      scale_color_identity() +
      scale_fill_identity() +
      scale_shape_manual(values = c("25" = 25, "24" = 24, "23" = 23)) +
      guides(shape = "none") +
      theme_minimal(base_family = "Montserrat") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$multiplier_plot <- renderPlot({
    df <- team_data()
    ggplot(df, aes(x = reorder(Player, True.Salary))) +
      geom_col(aes(y = Performance.Multiplier - 1), color = "darkgrey", fill = "lightgrey") +
      geom_hline(yintercept = 0, color = "grey4") +
      geom_text(aes(
        y = Performance.Multiplier - 1,
        label = paste0(round(Performance.Multiplier, 2), "x"),
        vjust = ifelse(Performance.Multiplier >= 1, -0.5, 1.5)
      ),
      size = 3,
      fontface = "italic",
      family = "Montserrat") +
      scale_y_continuous(
        breaks = seq(
          floor(min(df$Performance.Multiplier - 1, na.rm = TRUE)),
          ceiling(max(df$Performance.Multiplier - 1, na.rm = TRUE)),
          by = 1
        ),
        labels = function(x) paste0(x + 1, "x")
      ) +
      labs(
        subtitle = "A player's performance value as a multiplier of their true salary",
        x = "Player",
        y = "Multiplier"
      ) +
      theme_minimal(base_family = "Montserrat") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)

