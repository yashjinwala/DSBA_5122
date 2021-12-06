library(shiny)
library(tidyverse)
library(dplyr)
library(tidyverse)
library(socviz)
library(tidyr)
library(ggplot2)

df <- read_csv("NBA_stats.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("NBA Seasonal Statistics R Shiny Project"),
  
  tabsetPanel(
    tabPanel(
      "Points Statistics",
      fluid = TRUE,
      sidebarLayout(sidebarPanel(radioButtons(
        "Year",
        "Select Year:",
        c(2018, 2019, 2020, 2021)
      )),
      
      mainPanel(tabsetPanel(
        tabPanel("Bar Chart", plotOutput("barchart")),
        tabPanel("Box Plot", plotOutput("boxplot")),
      )))
    ),
    tabPanel(
      "Age Statistics",
      fluid = TRUE,
      sidebarLayout(sidebarPanel(selectInput(
        "Age",
        "Select Age:",
        c("19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40")
      )),
      
      mainPanel(plotOutput("Ageplot")),
      )
    ),
    tabPanel(
      "Position Statistics",
      fluid = TRUE,
      sidebarLayout(sidebarPanel(radioButtons(
        "Pos",
        "Select Position:",
        c("PG", "SG", "SF", "PF", "C")
      )),
      
      mainPanel(plotOutput("Posplot")),
      )
    ),
    tabPanel(
      "Interactive Dataset",
      fluid = TRUE,
      sidebarLayout(sidebarPanel(radioButtons(
        "iYear",
        "Select Year:",
        c(2018, 2019, 2020, 2021)
      ),
      selectInput(
        "iTeam",
        "Select Team:",
        c("ATL", "BOS", "BRK", "CHI", "CHO", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
      )
      ),
      mainPanel(tableOutput("dataset")))
    ),
    tabPanel(
      "About",
      fluid = TRUE,
      mainPanel(
        h4("App built by: Ty Birling, Elijah Ellison, and Yash Jinwala"),
        br(),
        p("This dataset has statistics of all NBA Players in a per game format over the years 2018 to 2021. The data contains statistics such as points, assists, rebounds, age, games played and many more categories for each player. Our app does its best job to tell stories of how the NBA has looked over these years and highlights individual players performance as well.")
      )
    )
    
    
  ),)
server <- function(input, output) {
  output$barchart <- renderPlot({
    
    df2 <- df %>%
      filter(Year == input$Year) %>%
      mutate(seasonPoints = Points*82)
    
    ggplot(df2, aes(x = Team, y = seasonPoints)) +
      geom_bar(stat = "identity",
               width = .5,
               fill = "tomato3") +
      theme(axis.text.x = element_text(angle = 65, vjust = 0.6))
  })
  
  output$boxplot <- renderPlot({
    df2 <- df %>%
      filter(Year == input$Year)
    
    ggplot(df2, aes(x = Team, y = Points)) +
      geom_boxplot() +
      coord_flip() +
      labs(
        subtitle = "Distribution of Team's Points",
        x = "Team",
        y = "Average Points by Player"
      )
  })
  
  output$Ageplot <- renderPlot({
    df3 <- df %>%
      filter(Age == input$Age)
    
    ggplot(data = df3, mapping = aes(x = `Minutes Played`, y=`Points`, color = `Age`)) + 
      geom_point()
  })
  
  output$Posplot <- renderPlot({
    df4 <- df %>%
      filter(Pos == input$Pos)
    
    ggplot(data = df4, mapping = aes(x = `Minutes Played`, y=`Points`, color = `Pos`)) + 
      geom_point()
  })
  
  selected_year <- reactive({input$Year})
  df_Year <- reactive({
    df %>%
      filter(Year == input$iYear) %>%
      filter(Team == input$iTeam) %>%
      select(Player, Team, Age, Pos, Games, Points, Assists,`Total Rebounds`, Steals, Blocks, Turnovers, Year)
      
  })
  output$dataset <- renderTable({
    df_Year()

  })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)
