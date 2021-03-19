## in this file, I generate a shiny app that allows users to visualize two proportions with a beta distribution
## for simplicity purposes, we assume a flat prior (jeffreys prior of beta(1/2,1/2))
## additionally, the distribution of the % difference between the two proportions from 1m simulations is visualized

library(shiny)
library(dplyr)
library(knitr)
library(ggplot2)
library(gridExtra)

data <- read.csv('data/team_data_new2.csv')
source('functions.R')

#data <- read.csv('/Users/hanson377/Documents/GitHub/march_madness_pickem/data/team_data_new2.csv')
#source('/Users/hanson377/Documents/GitHub/march_madness_pickem/functions.R')

## generate list of names for input
choices <- data %>% select(var = TeamName) %>% arrange(var) %>% mutate(num = row_number())

mylist <- as.list(choices$num)
# Name it
names(mylist) <- choices$var

# Define UI ----
ui <- fluidPage(
  titlePanel("March Madness 2021: Pick'em Bayesian Style"),
  sidebarLayout(
    sidebarPanel(

      h2('Select Your Teams'),
      selectInput("team1",label = h3("Select Team #1"),choices = mylist, selected = '28'),
      selectInput("team2",label = h3("Select Team #2"),choices = mylist, selected = '15'),

      h2('Select Offensive Weighting'),
      numericInput("weighting", "Weighting", min = -1, max = 1, value = .1)
    ),

    mainPanel(
      h1('Table Summary'),
      tableOutput("table_summary"),

      h1('Scoring Differentials: Results from Simulations'),
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

  modelGen(choices,input$weighting,input$team1,input$team2)

  })

  model_sims_points <- reactive({

    simulations <- modelGen(choices,input$weighting,input$team1,input$team2)

    prior <- simulations %>% select(value = T1Points_prior) %>% mutate(model = 'prior')
    likelihood <- simulations %>% select(value = T1Points_likelihood) %>% mutate(model = 'likelihood')
    posterior <- simulations %>% select(value = T1Points) %>% mutate(model = 'posterior')

    sample1 <- rbind(prior,likelihood,posterior)
    sample1$model <- factor(sample1$model,levels = c('prior','likelihood','posterior'))
    sample1$team <- simulations$Team1


    prior <- simulations %>% select(value = T2Points_prior) %>% mutate(model = 'prior')
    likelihood <- simulations %>% select(value = T2Points_likelihood) %>% mutate(model = 'likelihood')
    posterior <- simulations %>% select(value = T2Points) %>% mutate(model = 'posterior')

    sample2 <- rbind(prior,likelihood,posterior)
    sample2$model <- factor(sample2$model,levels = c('prior','likelihood','posterior'))
    sample2$team <- simulations$Team2

    combo <- rbind(sample1,sample2)
    return(combo)

    })


    model_sims_fg2 <- reactive({

      simulations <- modelGen(choices,input$weighting,input$team1,input$team2)

      prior <- simulations %>% select(value = T1FGM2_prior) %>% mutate(model = 'prior')
      likelihood <- simulations %>% select(value = T1FGM2_likelihood) %>% mutate(model = 'likelihood')
      posterior <- simulations %>% select(value = T1FGM2_posterior) %>% mutate(model = 'posterior')

      sample1 <- rbind(prior,likelihood,posterior)
      sample1$model <- factor(sample1$model,levels = c('prior','likelihood','posterior'))
      sample1$team <- simulations$Team1


      prior <- simulations %>% select(value = T2FGM2_prior) %>% mutate(model = 'prior')
      likelihood <- simulations %>% select(value = T2FGM2_likelihood) %>% mutate(model = 'likelihood')
      posterior <- simulations %>% select(value = T2FGM2_posterior) %>% mutate(model = 'posterior')

      sample2 <- rbind(prior,likelihood,posterior)
      sample2$model <- factor(sample2$model,levels = c('prior','likelihood','posterior'))
      sample2$team <- simulations$Team2

      combo <- rbind(sample1,sample2)
      return(combo)

      })


      model_sims_fg3 <- reactive({

        simulations <- modelGen(choices,input$weighting,input$team1,input$team2)

        prior <- simulations %>% select(value = T1FGM3_prior) %>% mutate(model = 'prior')
        likelihood <- simulations %>% select(value = T1FGM3_likelihood) %>% mutate(model = 'likelihood')
        posterior <- simulations %>% select(value = T1FGM3_posterior) %>% mutate(model = 'posterior')

        sample1 <- rbind(prior,likelihood,posterior)
        sample1$model <- factor(sample1$model,levels = c('prior','likelihood','posterior'))
        sample1$team <- simulations$Team1


        prior <- simulations %>% select(value = T2FGM3_prior) %>% mutate(model = 'prior')
        likelihood <- simulations %>% select(value = T2FGM3_likelihood) %>% mutate(model = 'likelihood')
        posterior <- simulations %>% select(value = T2FGM3_posterior) %>% mutate(model = 'posterior')

        sample2 <- rbind(prior,likelihood,posterior)
        sample2$model <- factor(sample2$model,levels = c('prior','likelihood','posterior'))
        sample2$team <- simulations$Team2

        combo <- rbind(sample1,sample2)
        return(combo)

        })


        model_sims_ftm <- reactive({

          simulations <- modelGen(choices,input$weighting,input$team1,input$team2)

          prior <- simulations %>% select(value = T1FTM_prior) %>% mutate(model = 'prior')
          likelihood <- simulations %>% select(value = T1FTM_likelihood) %>% mutate(model = 'likelihood')
          posterior <- simulations %>% select(value = T1FTM_posterior) %>% mutate(model = 'posterior')

          sample1 <- rbind(prior,likelihood,posterior)
          sample1$model <- factor(sample1$model,levels = c('prior','likelihood','posterior'))
          sample1$team <- simulations$Team1


          prior <- simulations %>% select(value = T2FTM_prior) %>% mutate(model = 'prior')
          likelihood <- simulations %>% select(value = T2FTM_likelihood) %>% mutate(model = 'likelihood')
          posterior <- simulations %>% select(value = T2FTM_posterior) %>% mutate(model = 'posterior')

          sample2 <- rbind(prior,likelihood,posterior)
          sample2$model <- factor(sample2$model,levels = c('prior','likelihood','posterior'))
          sample2$team <- simulations$Team2

          combo <- rbind(sample1,sample2)
          return(combo)

          })


  output$points_diff<-renderPlot({
    ggplot(model_sims(),aes(x=PointsDiff)) + geom_histogram(binwidth=1) + geom_vline(xintercept=0,linetype='dashed',colour='red') + xlab('Points') + ylab('Volume from Simluations')
  })

  output$fgm2_diff<-renderPlot({
    ggplot(model_sims(),aes(x=FGM2Diff)) + geom_histogram(binwidth=1) + geom_vline(xintercept=0,linetype='dashed',colour='red') + xlab('FGM2') + ylab('')
  })

  output$fgm3_diff<-renderPlot({
    ggplot(model_sims(),aes(x=FGM3Diff)) + geom_histogram(binwidth=1) + geom_vline(xintercept=0,linetype='dashed',colour='red') + xlab('FGM3') + ylab('')
  })

  output$ftm_diff<-renderPlot({
    ggplot(model_sims(),aes(x=FTMDiff)) + geom_histogram(binwidth=1) + geom_vline(xintercept=0,linetype='dashed',colour='red') + xlab('FTM') + ylab('')
  })

  output$points_model<-renderPlot({

    xmin <- min(model_sims_points()$value)
    xmax <- max(model_sims_points()$value)

    ggplot(model_sims_points(),aes(x=value,colour=model,fill=model)) + geom_density(alpha=.75)  + theme(legend.position = 'none') + xlab('') + ylab('') + coord_cartesian(xlim=c(xmin,xmax)) + facet_wrap(~team,nrow=2) + theme(legend.position='bottom',legend.title=element_blank())
  })

  output$fgm2_model<-renderPlot({

    xmin <- min(model_sims_fg2()$value)
    xmax <- max(model_sims_fg2()$value)


    ggplot(model_sims_fg2(),aes(x=value,colour=model,fill=model)) + geom_density(alpha=.75)  + theme(legend.position = 'none') + xlab('') + ylab('') + coord_cartesian(xlim=c(xmin,xmax)) + facet_wrap(~team,nrow=2) + theme(legend.position='bottom',legend.title=element_blank())
  })

  output$fgm3_model<-renderPlot({

    xmin <- min(model_sims_fg3()$value)
    xmax <- max(model_sims_fg3()$value)


    ggplot(model_sims_fg3(),aes(x=value,colour=model,fill=model)) + geom_density(alpha=.75)  + theme(legend.position = 'none') + xlab('') + ylab('') + coord_cartesian(xlim=c(xmin,xmax)) + facet_wrap(~team,nrow=2) + theme(legend.position='bottom',legend.title=element_blank())

  })

  output$ftm_model<-renderPlot({

    xmin <- min(model_sims_ftm()$value)
    xmax <- max(model_sims_ftm()$value)


    ggplot(model_sims_ftm(),aes(x=value,colour=model,fill=model)) + geom_density(alpha=.75)  + theme(legend.position = 'none') + xlab('') + ylab('') + coord_cartesian(xlim=c(xmin,xmax)) + facet_wrap(~team,nrow=2) + theme(legend.position='bottom',legend.title=element_blank())

  })

  output$table_summary<-renderTable({
    ##genTable(input$weighting,input$team1,input$team2)
    prob_win <- sum(model_sims()$T1Points >= model_sims()$T2Points)/nrow(model_sims())

    medianT1_points <- median(model_sims()$T1Points)
    lowerT1_points <- quantile(model_sims()$T1Points,.025)
    upperT1_points <- quantile(model_sims()$T1Points,.975)

    medianT2_points <- median(model_sims()$T2Points)
    lowerT2_points <- quantile(model_sims()$T2Points,.025)
    upperT2_points <- quantile(model_sims()$T2Points,.975)

    summary <- data.frame(prob_win,medianT1_points,lowerT1_points,upperT1_points,medianT2_points,lowerT2_points,upperT2_points)
  })


}

# Run the app ----
shinyApp(ui = ui, server = server)
