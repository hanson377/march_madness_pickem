## in this file, I generate a shiny app that allows users to visualize two proportions with a beta distribution
## for simplicity purposes, we assume a flat prior (jeffreys prior of beta(1/2,1/2))
## additionally, the distribution of the % difference between the two proportions from 1m simulations is visualized

library(shiny)
library(dplyr)
library(knitr)
library(kableExtra)
library(kable)
library(ggplot2)
library(gridExtra)

data <- read.csv('/Users/hanson377/Documents/GitHub/march_madness_pickem/data/team_data.csv')
source('/Users/hanson377/Documents/GitHub/march_madness_pickem/functions.R')

# Define UI ----
ui <- fluidPage(
  titlePanel("March Madness 2021: Pick'em Bayesian Style"),
  sidebarLayout(
    sidebarPanel(

      h2('Select Your Teams'),
      textInput("team1", h3("Team #1"), value = "Kansas"),
      textInput("team2", h3("Team #2"), value = "E Washington"),

      h2('Select Offensive Weighting'),
      numericInput("weighting", "Weighting", min = -1, max = 1, value = 0)
    ),

    mainPanel(
      h1('Table Summary'),
      tableOutput("table_summary"),

      h1('Scoring Differentials'),
      splitLayout(cellWidths = c("25%", "25%", "25%", '25%'), plotOutput("points_diff"),plotOutput("fgm2_diff"), plotOutput("fgm3_diff"),plotOutput("ftm_diff")),

      h1('Points Scored: Prior, Likelihood, Posterior'),
      splitLayout(cellWidths = c("100%"), plotOutput("points_model")),

      h1('FGM2: Prior, Likelihood, Posterior'),
      splitLayout(cellWidths = c("100%"), plotOutput("fgm2_model")),

      h1('FGM3: Prior, Likelihood, Posterior'),
      splitLayout(cellWidths = c("100%"), plotOutput("fgm3_model")),

      h1('FTM: Prior, Likelihood, Posterior'),
      splitLayout(cellWidths = c("100%"), plotOutput("ftm_model"))
    )
  )
)

# Define server logic ----
server <- function(input, output,session) {

model_sims <- reactive({

  modelGen(input$weighting,input$team1,input$team2)

  })


  output$points_diff<-renderPlot({
    ggplot(model_sims(),aes(x=PointsDiff)) + geom_histogram(binwidth=1) + geom_vline(xintercept=0,linetype='dashed',colour='red')
  })

  output$fgm2_diff<-renderPlot({
    ggplot(model_sims(),aes(x=FGM2Diff)) + geom_histogram(binwidth=1) + geom_vline(xintercept=0,linetype='dashed',colour='red')
  })

  output$fgm3_diff<-renderPlot({
    ggplot(model_sims(),aes(x=FGM3Diff)) + geom_histogram(binwidth=1) + geom_vline(xintercept=0,linetype='dashed',colour='red')
  })

  output$ftm_diff<-renderPlot({
    ggplot(model_sims(),aes(x=FTMDiff)) + geom_histogram(binwidth=1) + geom_vline(xintercept=0,linetype='dashed',colour='red')
  })

  output$points_model<-renderPlot({
    genViewPoints(input$weighting,input$team1,input$team2)
  })

  output$fgm2_model<-renderPlot({
    genViewFGM2(input$weighting,input$team1,input$team2)
  })

  output$fgm3_model<-renderPlot({
    genViewFGM3(input$weighting,input$team1,input$team2)
  })

  output$ftm_model<-renderPlot({
    genViewFTM(input$weighting,input$team1,input$team2)
  })

  output$table_summary<-renderTable({
    genTable(input$weighting,input$team1,input$team2)
  })


}

# Run the app ----
shinyApp(ui = ui, server = server)
