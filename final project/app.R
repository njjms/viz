library(shiny)
library(tidyverse)
library(ggrepel)

wintalentdata <- read.csv("wintalentdata.csv")

ui <- fluidPage(
  selectInput(inputId = "conference", label = "Select a conference",
              choices = unique(wintalentdata$conference)[order(unique(wintalentdata$conference))][-12],
              selected = "All Conferences"
              ),
  checkboxGroupInput(inputId = "Teams", label = "Available teams",
              choices = wintalentdata$school
  ),
  mainPanel(
    plotOutput(outputId = "plot", height = "700px", width = "1200px")
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    wintalentdata %>% 
      filter(conference == input$conference) %>% 
      ggplot() +
      geom_point(mapping = aes(x = wins, y = `talent.rating`, color = school), alpha = .5, size = 2) +
      geom_path(mapping = aes(x = wins, y = `talent.rating`, color = school), size = 1.0) +
      geom_text_repel(mapping = aes(x = wins, y = `talent.rating`, label = paste(school, season)), size = 3) + 
      scale_color_manual(
        values = as.character(unique(select(arrange(subset(wintalentdata, conference == input$conference), school), c("school", "color")))$color)
      ) +
      scale_x_continuous("Number of Wins", 
                       breaks = seq(0, 12, 1)) +
      labs(
        title = "Wins vs. Talent Composite Score",
        y = "Composite Talent Score"
      ) +
      theme(
        legend.position = "none"
      )
  })
}

shinyApp(ui = ui, server = server)