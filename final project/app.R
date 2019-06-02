library(shiny)
library(tidyverse)
library(ggrepel)

wintalentdata <- read.csv("wintalentdata.csv")
as.character(unique(wintalentdata$school[wintalentdata$conference == "Big Ten"]))[!is.na(as.character(unique(wintalentdata$school[wintalentdata$conference == "Big Ten"])))]

ui <- fluidPage(
  
  sidebarPanel(
    selectInput(inputId = "conference", label = "Select a conference",
                choices = unique(wintalentdata$conference)[order(unique(wintalentdata$conference))][-12]
                ),
    conditionalPanel(
      condition = "input.conference == 'Big Ten'",
      checkboxGroupInput(inputId = "b10_teams", label = "Teams",
                         choices = c("Ohio State",
                                     "Michigan",
                                     "Penn State",
                                     "Maryland",
                                     "Nebraska",
                                     "Michigan State",
                                     "Wisconsin",
                                     "Iowa",
                                     "Northwestern",
                                     "Minnesota",
                                     "Indiana",
                                     "Illinois",
                                     "Rutgers",
                                     "Purdue"),
                         selected = c()
    )
  ),
    conditionalPanel(
      condition = "input.conference == 'SEC'",
      checkboxGroupInput(inputId = "sec_teams", label = "Teams",
                         choices = c(
                           "Alabama",
                           "Georgia",
                           "LSU",
                           "Florida",
                           "Auburn",
                           "Tennessee",
                           "Texas A&M",
                           "South Carolina",
                           "Ole Miss",
                           "Mississippi State",
                           "Arkansas",
                           "Kentucky",
                           "Vanderbilt",
                           "Missouri"
                         ),
                         selected = c()
    )
  ),
    conditionalPanel(
      condition = "input.conference == 'Big 12'",
      checkboxGroupInput(inputId = "b12_teams", label = "Teams",
                         choices = c(
                           "Texas",
                           "Oklahoma",
                           "TCU",
                           "Baylor",
                           "West Virginia",
                           "Oklahoma State",
                           "Iowa State",
                           "Texas Tech",
                           "Kansas State",
                           "Kansas"
                         ),
                         selected = c()
    )
  ),
  conditionalPanel(
      condition = "input.conference == 'Pac-12'",
      checkboxGroupInput(inputId = "p12_teams", label = "Teams",
                         choices = c(
                           "USC",
                           "Stanford",
                           "Washington",
                           "UCLA",
                           "Oregon",
                           "Arizona State",
                           "California",
                           "Utah",
                           "Arizona",
                           "Colorado",
                           "Washington State",
                           "Oregon State"
                         ),
                         selected = c()
    )
  ),
  conditionalPanel(
      condition = "input.conference == 'ACC'",
      checkboxGroupInput(inputId = "acc_teams", label = "Teams",
                         choices = c(
                           "Clemson",
                           "Florida State",
                           "Miami",
                           "North Carolina",
                           "Virginia Tech",
                           "Pittsburgh",
                           "Duke",
                           "Louisville",
                           "NC State",
                           "Georgia Tech",
                           "Syracuse",
                           "Virginia",
                           "Boston College",
                           "Wake Forest"
                         ),
                         selected = c()
    )
  ),
  conditionalPanel(
      condition = "input.conference == 'FBS Independents'",
      checkboxGroupInput(inputId = "ind_teams", label = "Teams",
                         choices = c(
                           "Notre Dame",
                           "BYU",
                           "UMass",
                           "Army"
                         ),
                         selected = c()
    )
  ),
  conditionalPanel(
      condition = "input.conference == 'American Athletic'",
      checkboxGroupInput(inputId = "aac_teams", label = "Teams",
                         choices = c(
                           "Houston",
                           "UCF",
                           "South Florida",
                           "Cincinnati",
                           "SMU",
                           "Memphis",
                           "Temple",
                           "Tulane",
                           "East Carolina",
                           "Connecticut",
                           "Tulsa",
                           "Navy"
                         ),
                         selected = c()
    )
  ),
  conditionalPanel(
      condition = "input.conference == 'Conference USA'",
      checkboxGroupInput(inputId = "cusa_teams", label = "Teams",
                         choices = c(
                           "Florida Atlantic",
                           "Louisiana Tech",
                           "Florida International",
                           "Marshall",
                           "Middle Tennessee",
                           "UT San Antonio",
                           "UAB",
                           "Southern Mississippi",
                           "Rice",
                           "North Texas",
                           "Western Kentucky",
                           "UTEP",
                           "Old Dominion",
                           "Charlotte"
                         ),
                         selected = c()
    )
  ),
  conditionalPanel(
      condition = "input.conference == 'Mid-American'",
      checkboxGroupInput(inputId = "mid_teams", label = "Teams",
                         choices = c(
                           "Toledo",
                           "Eastern Michigan",
                           "Western Michigan",
                           "Northern Illinois",
                           "Miami (OH)",
                           "Central Michigan",
                           "Bowling Green",
                           "Buffalo",
                           "Ball State",
                           "Akron",
                           "Kent State",
                           "Ohio"
                         ),
                         selected = c()
    )
  ),
  conditionalPanel(
      condition = "input.conference == 'Sun Belt'",
      checkboxGroupInput(inputId = "sun_teams", label = "Teams",
                         choices = c(
                           "Appalachian State",
                           "Arkansas State",
                           "Coastal Carolina",
                           "Georgia Southern",
                           "Georgia State",
                           "Louisiana",
                           "Louisiana Monroe",
                           "South Alabama",
                           "Texas State",
                           "Troy"
                         ),
                         selected = c()
    )
  ),
  conditionalPanel(
      condition = "input.conference == 'Mountain West'",
      checkboxGroupInput(inputId = "mw_teams", label = "Teams",
                         choices = c(
                           "Boise State",
                           "Colorado State", 
                           "San Diego State",
                           "Fresno State",
                           "Nevada",
                           "San José State",
                           "New Mexico",
                           "UNLV",
                           "Utah State",
                           "Wyoming",
                           "Hawai'i",
                           "Air Force"
                         ),
                         selected = c()
    )
  )
  ),
  mainPanel(
    plotOutput(outputId = "plot", height = "700px", width = "1200px"),
    textOutput(outputId = "teams")
  )
)


server <- function(input, output) {
  
  output$plot <- renderPlot({
    wintalentdata %>% 
      filter(school %in% c(input$b10_teams,
                           input$b12_teams,
                           input$sec_teams,
                           input$acc_teams,
                           input$p12_teams,
                           input$mid_teams,
                           input$aac_teams,
                           input$sun_teams,
                           input$mw_teams,
                           input$cusa_teams,
                           input$ind_teams)) %>% 
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
  
  output$teams <- renderPrint(input$teams)
}

shinyApp(ui = ui, server = server)