library(dplyr)
library(ggplot2)
library(gridExtra)

modelGen <- function(weighting,team1_name,team2_name){

team1 <- data %>% filter(TeamName == team1_name)
team2 <- data %>% filter(TeamName == team2_name)

OMultiplier <- 1+weighting
DMultiplier <- 1-weighting

## first teams stats for points made
T1Games <- max(team1$total_games)

T1FGM <- max(team1$FGM)*OMultiplier
T1FGM3 <- max(team1$FGM3)*OMultiplier
T1FGM2 <- (T1FGM-T1FGM3)*OMultiplier
T1FTM <- max(team1$FTM)*OMultiplier

## winning points afforded
T1OppFGM <- max(team1$OppFGM)*DMultiplier
T1OppFGM3 <- max(team1$OppFGM3)*DMultiplier
T1OppFGM2 <- (T1OppFGM-T1OppFGM3)*DMultiplier
T1OppFTM <- max(team1$OppFTM)*DMultiplier

## second teams stats
T2Games <- max(team2$total_games)

T2FGM <- max(team2$FGM)*OMultiplier
T2FGM3 <- max(team2$FGM3)*OMultiplier
T2FGM2 <- (T2FGM-T2FGM3)*OMultiplier
T2FTM <- max(team2$FTM)*OMultiplier

## losing stats yes
T2OppFGM <- max(team2$OppFGM)*DMultiplier
T2OppFGM3 <- max(team2$OppFGM3)*DMultiplier
T2OppFGM2 <- (T2OppFGM-T2OppFGM3)*DMultiplier
T2OppFTM <- max(team2$OppFTM)*DMultiplier

## generate models
trials <- 50000

## general models with gamma distribution
T1FGM3_posterior = rgamma(trials,(T1FGM3+T2OppFGM3),((T1Games*OMultiplier)+(T2Games*DMultiplier)))
T1FGM3_prior = rgamma(trials,T2OppFGM3,(T2Games*DMultiplier))
T1FGM3_likelihood = rgamma(trials,T1FGM3,(T1Games*OMultiplier))

T1FGM2_posterior = rgamma(trials,(T1FGM2+T2OppFGM2),((T1Games*OMultiplier)+(T2Games*DMultiplier)))
T1FGM2_prior = rgamma(trials,T2OppFGM2,(T2Games*DMultiplier))
T1FGM2_likelihood = rgamma(trials,T1FGM2,(T1Games*OMultiplier))

T1FTM_posterior = rgamma(trials,(T1FTM+T2OppFTM),((T1Games*OMultiplier)+(T2Games*DMultiplier)))
T1FTM_prior = rgamma(trials,T2OppFTM,(T2Games*DMultiplier))
T1FTM_likelihood = rgamma(trials,T1FTM,(T1Games*OMultiplier))


T2FGM3_posterior = rgamma(trials,(T2FGM3+T1OppFGM3),((T1Games*DMultiplier)+(T2Games*OMultiplier)))
T2FGM3_prior = rgamma(trials,T1OppFGM3,(T1Games*DMultiplier))
T2FGM3_likelihood = rgamma(trials,T2FGM3,(T2Games*OMultiplier))

T2FGM2_posterior = rgamma(trials,(T2FGM2+T1OppFGM2),((T1Games*DMultiplier)+(T2Games*OMultiplier)))
T2FGM2_prior = rgamma(trials,T1OppFGM2,(T1Games*DMultiplier))
T2FGM2_likelihood = rgamma(trials,T2FGM2,(T2Games*OMultiplier))

T2FTM_posterior = rgamma(trials,(T2FTM+T1OppFTM),((T1Games*DMultiplier)+(T2Games*OMultiplier)))
T2FTM_prior = rgamma(trials,T1OppFTM,(T1Games*DMultiplier))
T2FTM_likelihood = rgamma(trials,T2FTM,(T2Games*OMultiplier))

model <- data.frame(T1FGM3_posterior,T1FGM2_posterior,T1FTM_posterior,T2FGM3_posterior,T2FGM2_posterior,T2FTM_posterior,T1FGM3_prior,T1FGM3_likelihood,T1FGM2_prior,T1FGM2_likelihood,T2FGM2_likelihood,T2FGM2_prior,T2FGM3_prior,T2FGM3_likelihood,T2FTM_prior,T2FTM_likelihood,T1FTM_prior,T1FTM_likelihood,T2FTM_prior,T2FTM_likelihood)

model$T1Points <- (model$T1FGM2_posterior*2)+(model$T1FGM3_posterior*3)+(model$T1FTM_posterior*1)
model$T1Points_prior <- (model$T1FGM2_prior*2)+(model$T1FGM3_prior*3)+(model$T1FTM_prior*1)
model$T1Points_likelihood <- (model$T1FGM2_likelihood*2)+(model$T1FGM3_likelihood*3)+(model$T1FTM_likelihood*1)

model$T2Points <- (model$T2FGM2_posterior*2)+(model$T2FGM3_posterior*3)+(model$T2FTM_posterior*1)
model$T2Points_prior <- (model$T2FGM2_prior*2)+(model$T2FGM3_prior*3)+(model$T2FTM_prior*1)
model$T2Points_likelihood <- (model$T2FGM2_likelihood*2)+(model$T2FGM3_likelihood*3)+(model$T2FTM_likelihood*1)

model$PointsDiff <- model$T1Points-model$T2Points
model$FGM2Diff <- model$T1FGM2_posterior-model$T2FGM2_posterior
model$FGM3Diff <- model$T1FGM3_posterior-model$T2FGM3_posterior
model$FTMDiff <- model$T1FTM_posterior-model$T2FTM_posterior

return(model)
}

genViewPoints <- function(weighting,team1,team2){

  simulations <- modelGen(weighting,team1,team2)

  prior <- simulations %>% select(value = T1Points_prior) %>% mutate(model = 'prior')
  likelihood <- simulations %>% select(value = T1Points_likelihood) %>% mutate(model = 'likelihood')
  posterior <- simulations %>% select(value = T1Points) %>% mutate(model = 'posterior')

  sample1 <- rbind(prior,likelihood,posterior)
  sample1$model <- factor(sample1$model,levels = c('prior','likelihood','posterior'))
  sample1$team <- team1


  prior <- simulations %>% select(value = T2Points_prior) %>% mutate(model = 'prior')
  likelihood <- simulations %>% select(value = T2Points_likelihood) %>% mutate(model = 'likelihood')
  posterior <- simulations %>% select(value = T2Points) %>% mutate(model = 'posterior')

  sample2 <- rbind(prior,likelihood,posterior)
  sample2$model <- factor(sample2$model,levels = c('prior','likelihood','posterior'))
  sample2$team <- team2

  combo <- rbind(sample1,sample2)

  xmin <- min(combo$value)
  xmax <- max(combo$value)

  view1 <- ggplot(combo,aes(x=value,colour=model,fill=model)) + geom_density(alpha=.75)  + theme(legend.position = 'none') + xlab('') + ylab('') + coord_cartesian(xlim=c(xmin,xmax)) + facet_wrap(~team,nrow=2) + theme(legend.position='bottom',legend.title=element_blank())

  return(view1)

}

genViewFGM2 <- function(weighting,team1,team2){

  simulations <- modelGen(weighting,team1,team2)

  prior <- simulations %>% select(value = T1FGM2_prior) %>% mutate(model = 'prior')
  likelihood <- simulations %>% select(value = T1FGM2_likelihood) %>% mutate(model = 'likelihood')
  posterior <- simulations %>% select(value = T1FGM2_posterior) %>% mutate(model = 'posterior')

  sample1 <- rbind(prior,likelihood,posterior)
  sample1$model <- factor(sample1$model,levels = c('prior','likelihood','posterior'))
  sample1$team <- team1


  prior <- simulations %>% select(value = T2FGM2_prior) %>% mutate(model = 'prior')
  likelihood <- simulations %>% select(value = T2FGM2_likelihood) %>% mutate(model = 'likelihood')
  posterior <- simulations %>% select(value = T2FGM2_posterior) %>% mutate(model = 'posterior')

  sample2 <- rbind(prior,likelihood,posterior)
  sample2$model <- factor(sample2$model,levels = c('prior','likelihood','posterior'))
  sample2$team <- team2

  combo <- rbind(sample1,sample2)

  xmin <- min(combo$value)
  xmax <- max(combo$value)

  view1 <- ggplot(combo,aes(x=value,colour=model,fill=model)) + geom_density(alpha=.75)  + theme(legend.position = 'none') + xlab('') + ylab('') + coord_cartesian(xlim=c(xmin,xmax)) + facet_wrap(~team,nrow=2) + theme(legend.position='bottom',legend.title=element_blank())

  return(view1)

}

genViewFGM3 <- function(weighting,team1,team2){

  simulations <- modelGen(weighting,team1,team2)

  prior <- simulations %>% select(value = T1FGM3_prior) %>% mutate(model = 'prior')
  likelihood <- simulations %>% select(value = T1FGM3_likelihood) %>% mutate(model = 'likelihood')
  posterior <- simulations %>% select(value = T1FGM3_posterior) %>% mutate(model = 'posterior')

  sample1 <- rbind(prior,likelihood,posterior)
  sample1$model <- factor(sample1$model,levels = c('prior','likelihood','posterior'))
  sample1$team <- team1


  prior <- simulations %>% select(value = T2FGM3_prior) %>% mutate(model = 'prior')
  likelihood <- simulations %>% select(value = T2FGM3_likelihood) %>% mutate(model = 'likelihood')
  posterior <- simulations %>% select(value = T2FGM3_posterior) %>% mutate(model = 'posterior')

  sample2 <- rbind(prior,likelihood,posterior)
  sample2$model <- factor(sample2$model,levels = c('prior','likelihood','posterior'))
  sample2$team <- team2

  combo <- rbind(sample1,sample2)

  xmin <- min(combo$value)
  xmax <- max(combo$value)

  view1 <- ggplot(combo,aes(x=value,colour=model,fill=model)) + geom_density(alpha=.75)  + theme(legend.position = 'none') + xlab('') + ylab('') + coord_cartesian(xlim=c(xmin,xmax)) + facet_wrap(~team,nrow=2) + theme(legend.position='bottom',legend.title=element_blank())

  return(view1)

}

genViewFTM <- function(weighting,team1,team2){

  simulations <- modelGen(weighting,team1,team2)

  prior <- simulations %>% select(value = T1FTM_prior) %>% mutate(model = 'prior')
  likelihood <- simulations %>% select(value = T1FTM_likelihood) %>% mutate(model = 'likelihood')
  posterior <- simulations %>% select(value = T1FTM_posterior) %>% mutate(model = 'posterior')

  sample1 <- rbind(prior,likelihood,posterior)
  sample1$model <- factor(sample1$model,levels = c('prior','likelihood','posterior'))
  sample1$team <- team1


  prior <- simulations %>% select(value = T2FTM_prior) %>% mutate(model = 'prior')
  likelihood <- simulations %>% select(value = T2FTM_likelihood) %>% mutate(model = 'likelihood')
  posterior <- simulations %>% select(value = T2FTM_posterior) %>% mutate(model = 'posterior')

  sample2 <- rbind(prior,likelihood,posterior)
  sample2$model <- factor(sample2$model,levels = c('prior','likelihood','posterior'))
  sample2$team <- team2

  combo <- rbind(sample1,sample2)

  xmin <- min(combo$value)
  xmax <- max(combo$value)

  view1 <- ggplot(combo,aes(x=value,colour=model,fill=model)) + geom_density(alpha=.75)  + theme(legend.position = 'none') + xlab('') + ylab('') + coord_cartesian(xlim=c(xmin,xmax)) + facet_wrap(~team,nrow=2) + theme(legend.position='bottom',legend.title=element_blank())

  return(view1)

}

genTable <- function(weighting,team1,team2){

  simulations <- modelGen(weighting,team1,team2)

  prob_win <- sum(simulations$T1Points >= simulations$T2Points)/nrow(simulations)

  medianT1_points <- median(simulations$T1Points)
  lowerT1_points <- quantile(simulations$T1Points,.025)
  upperT1_points <- quantile(simulations$T1Points,.975)

  medianT2_points <- median(simulations$T2Points)
  lowerT2_points <- quantile(simulations$T2Points,.025)
  upperT2_points <- quantile(simulations$T2Points,.975)

  summary <- data.frame(prob_win,medianT1_points,lowerT1_points,upperT1_points,medianT2_points,lowerT2_points,upperT2_points)

  return(summary)

}
