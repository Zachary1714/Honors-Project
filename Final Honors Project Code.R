##*------------------------------------------------------------------------------------*
##* Code is written by Zach Houghtaling, current student at the University of Akron
##* Copyright 2022

##PACKAGES - Install if necessary then load the following packages
#devtools::install_github("BillPetti/baseballr") #Bill Petti put this package together with many functions to analyze baseball data using R
#install.packages("purrr")
#install.packages("furrr")
#install.packages("tictoc")
#install.packages("rlang")
#install.packages("readr")
#install.packages("lubridate")
#devtools::install_github("rasmusab/bayesian_first_aid")
#install.packages("rjags")
#install.packages("ggpubr")
#install.packages("rpsychi")
#install.packages("HH")
#install.packages("bayestestR")
#install.packages("BEST")
#install.packages("pracma")

library(baseballr)
library(purrr)
library(furrr)
library(plyr)
library(dplyr)
library(tibble)
library(data.table)
library(randomForestSRC) # random forests for survival, regression and classification
#library(ggRandomForests) # ggplot2 random forest figures
library(foreign)
library(ggplot2)
library("RColorBrewer")
#library(rjags)
#library(rethinking)
library(tidyverse)
library(magrittr)
library(readxl)
#library(ggpubr)
#library(rpsychi)
library(HH)
library(bayestestR)
library(BEST)
library(pracma)

setwd("~/Desktop")
#games <- get_game_pks_mlb(dates[27],levelids[1])
get_games_all_dates <- function(dates,m){
  i=1
  games <- get_game_pks_mlb(dates[i],levelids[m])
  games <- subset(games,select=c(game_pk,teams.away.score,teams.away.team.id,teams.away.team.name,teams.home.score,teams.home.team.id,teams.home.team.name,seriesDescription))
  i=2
  for (i in 2:length(dates)){
    tryCatch({
      games2 <- get_game_pks_mlb(dates[i],levelids[m])
      games2 <- subset(games2,select=c(game_pk,teams.away.score,teams.away.team.id,teams.away.team.name,teams.home.score,teams.home.team.id,teams.home.team.name,seriesDescription))
      games <- rbind(games2,games)
    }, error=function(e){i<-i+1})
  }
  return(games)
}

#Using this code acquired from https://www.reddit.com/r/Sabermetrics/comments/oh2zpq/trying_to_figure_out_how_to_query_the_mlb_api/ via reddit user u/metlover
dates = seq.Date(from=as.Date('2019-03-01'),to=as.Date('2021-11-01'),by='day') #All dates of games from 2019-2021
#plan(multisession, workers = 6)
#tictoc::tic()
#games <- get_games_all_dates(dates,c(1,11,12,13,14,15))
#tictoc::toc()
#gamesMLB <- get_games_all_dates(dates,c(1))
#games = get_game_pks_mlb(date = ., level_ids = c(1,11,12,13,14,15))
#games = future_map_dfr(dates, ~get_game_pks_mlb_possibly(date = ., level_ids = c(1,11,12,13,14,15)) #Pulls all games from 2019-2021
#gamesMLB = future_map_dfr(dates, ~get_game_pks_mlb_possibly(date = ., level_ids = c(1)))
#gamesTRIPLEA = future_map_dfr(dates, ~get_game_pks_mlb_possibly(date = ., level_ids = c(11)))
#gamesDOUBLEA = future_map_dfr(dates, ~get_game_pks_mlb_possibly(date = ., level_ids = c(12)))
#gamesHIGHA = future_map_dfr(dates, ~get_game_pks_mlb_possibly(date = ., level_ids = c(13)))
#gamesA = future_map_dfr(dates, ~get_game_pks_mlb_possibly(date = ., level_ids = c(14)))
#gamesLOWA = future_map_dfr(dates, ~get_game_pks_mlb_possibly(date = ., level_ids = c(15)))
#tictoc::toc()
#write.csv(games, file="Total Games.csv")
games <- read.csv("Total Games.csv")
games <- subset(games, select=-c(X))
#write.csv(gamesMLB, file="Games MLB.csv")
gamesMLB <- read.csv("Games MLB.csv")
gamesMLB <- subset(gamesMLB, select=-c(X))
#write.csv(gamesTRIPLEA, file="Games Triple A.csv")
gamesTRIPLEA <- read.csv("Games Triple A.csv")
gamesTRIPLEA <- subset(gamesTRIPLEA, select=-c(X))
#write.csv(gamesDOUBLEA, file="Games Double A.csv")
gamesDOUBLEA <- read.csv("Games Double A.csv")
gamesDOUBLEA <- subset(gamesDOUBLEA, select=-c(X))
#write.csv(gamesHIGHA, file="Games High A.csv")
gamesHIGHA <- read.csv("Games High A.csv")
gamesHIGHA <- subset(gamesHIGHA, select=-c(X))
#write.csv(gamesA, file="Games A.csv")
gamesA <- read.csv("Games A.csv")
gamesA <- subset(gamesA, select=-c(X))
#write.csv(gamesLOWA, file="Games Low A.csv")
gamesLOWA <- read.csv("Games Low A.csv")
gamesLOWA <- subset(gamesLOWA, select=-c(X))

dates19 = seq.Date(from=as.Date('2019-03-01'),to=as.Date('2019-11-20'),by='day')
#get_game_pks_mlb_possibly = possibly(get_game_pks_mlb, data.frame())
#plan(multisession, workers = 6)
#tictoc::tic()
#games_19 = future_map_dfr(dates19, ~get_game_pks_mlb_possibly(date = ., level_ids = c(1,11,12,13,14,15)))
#gamesMLB_19 = future_map_dfr(dates19, ~get_game_pks_mlb_possibly(date = ., level_ids = c(1)))
#gamesTRIPLEA_19 = future_map_dfr(dates19, ~get_game_pks_mlb_possibly(date = ., level_ids = c(11)))
#gamesDOUBLEA_19 = future_map_dfr(dates19, ~get_game_pks_mlb_possibly(date = ., level_ids = c(12)))
#gamesHIGHA_19 = future_map_dfr(dates19, ~get_game_pks_mlb_possibly(date = ., level_ids = c(13)))
#gamesA_19 = future_map_dfr(dates19, ~get_game_pks_mlb_possibly(date = ., level_ids = c(14)))
#gamesLOWA_19 = future_map_dfr(dates19, ~get_game_pks_mlb_possibly(date = ., level_ids = c(15)))
#tictoc::toc()

#write.csv(games_19, file="Total Games 2019.csv")
games_19 <- read.csv("Total Games 2019.csv")
games_19 <- subset(games_19, select=-c(X))
#write.csv(gamesMLB_19, file="Games MLB 2019.csv")
gamesMLB_19 <- read.csv("Games MLB 2019.csv")
gamesMLB_19 <- subset(gamesMLB_19, select=-c(X))
#write.csv(gamesTRIPLEA_19, file="Games Triple A 2019.csv")
gamesTRIPLEA_19 <- read.csv("Games Triple A 2019.csv")
gamesTRIPLEA_19 <- subset(gamesTRIPLEA_19, select=-c(X))
#write.csv(gamesDOUBLEA_19, file="Games Double A 2019.csv")
gamesDOUBLEA_19 <- read.csv("Games Double A 2019.csv")
gamesDOUBLEA_19 <- subset(gamesDOUBLEA_19, select=-c(X))
#write.csv(gamesHIGHA_19, file="Games High A 2019.csv")
gamesHIGHA_19 <- read.csv("Games High A 2019.csv")
gamesHIGHA_19 <- subset(gamesHIGHA_19, select=-c(X))
#write.csv(gamesA_19, file="Games A 2019.csv")
gamesA_19 <- read.csv("Games A 2019.csv")
gamesA_19 <- subset(gamesA_19, select=-c(X))
#write.csv(gamesLOWA_19, file="Games Low A 2019.csv")
gamesLOWA_19 <- read.csv("Games Low A 2019.csv")
gamesLOWA_19 <- subset(gamesLOWA_19, select=-c(X))

dates20 = seq.Date(from=as.Date('2020-03-01'),to=as.Date('2020-11-20'),by='day')
#get_game_pks_mlb_possibly = possibly(get_game_pks_mlb, data.frame())
#plan(multisession, workers = 6)
#tictoc::tic()
#games_20 = future_map_dfr(dates20, ~get_game_pks_mlb_possibly(date = ., level_ids = c(1)))
#write.csv(games_20, file="Total Games 2020.csv")
games_20 <- read.csv("Total Games 2020.csv")
games_20 <- subset(games_20, select=-c(X))
#tictoc::toc()

dates21 = seq.Date(from=as.Date('2021-03-01'),to=as.Date('2021-11-20'),by='day')
dates21_noPO = seq.Date(from=as.Date('2021-03-01'),to=as.Date('2021-10-04'),by='day')
get_game_pks_mlb_possibly = possibly(get_game_pks_mlb, data.frame())
#plan(multisession, workers = 6)
#tictoc::tic()
#games_21 = future_map_dfr(dates21, ~get_game_pks_mlb_possibly(date = ., level_ids = c(1,11,12,13,14,15)))
gamesMLB_21 = future_map_dfr(dates21_noPO, ~get_game_pks_mlb_possibly(date = ., level_ids = c(1)))
#gamesTRIPLEA_21 = future_map_dfr(dates21, ~get_game_pks_mlb_possibly(date = ., level_ids = c(11)))
#gamesDOUBLEA_21 = future_map_dfr(dates21, ~get_game_pks_mlb_possibly(date = ., level_ids = c(12)))
#gamesHIGHA_21 = future_map_dfr(dates21, ~get_game_pks_mlb_possibly(date = ., level_ids = c(13)))
#gamesA_21 = future_map_dfr(dates21, ~get_game_pks_mlb_possibly(date = ., level_ids = c(14)))
#gamesLOWA_21 = future_map_dfr(dates21, ~get_game_pks_mlb_possibly(date = ., level_ids = c(15)))
#tictoc::toc()
#write.csv(games_21, file="Total Games 2021.csv")
games_21 <- read.csv("Total Games 2021.csv")
games_21 <- subset(games_21, select=-c(X))
#write.csv(gamesMLB_21, file="Games MLB 2021.csv")
gamesMLB_21 <- read.csv("Games MLB 2021.csv")
gamesMLB_21 <- subset(gamesMLB_21, select=-c(X))
#write.csv(gamesTRIPLEA_21, file="Games Triple A 2021.csv")
gamesTRIPLEA_21 <- read.csv("Games Triple A 2021.csv")
gamesTRIPLEA_21 <- subset(gamesTRIPLEA_21, select=-c(X))
#write.csv(gamesDOUBLEA_21, file="Games Double A 2021.csv")
gamesDOUBLEA_21 <- read.csv("Games Double A 2021.csv")
gamesDOUBLEA_21 <- subset(gamesDOUBLEA_21, select=-c(X))
#write.csv(gamesHIGHA_21, file="Games High A 2021.csv")
gamesHIGHA_21 <- read.csv("Games High A 2021.csv")
gamesHIGHA_21 <- subset(gamesHIGHA_21, select=-c(X))
#write.csv(gamesA_21, file="Games A 2021.csv")
gamesA_21 <- read.csv("Games A 2021.csv")
gamesA_21 <- subset(gamesA_21, select=-c(X))
plan(sequential)
#-----------------------

#I then get the games that I want and will pull the Play-by-play for All MLB and MiLB pitches from 2019, 2020 (No MiLB), and 2021 with no Exhibition & Spring Training Games
#gamepk <-games$game_pk[gamesMLBOLD$status.codedGameState=="F" & !(games$seriesDescription == "Spring Training" | games$seriesDescription == "Exhibition")]
#gamepk <- gamepk[!duplicated(gamepk)]

#--------------------------------------

#i=1 #Used as index in loop I'm setting up. Need to do the first one outside to make rbind possible. We pull the play-by-play and variables we want then for the events we want
#pbp <- get_pbp_mlb(gamepk[i]) #baseballr function that pulls each pitch as recorded by gameday stringers from the MLB API
#pbp <- subset(pbp, select = c(game_pk,game_date,fielding_team,batting_team,  #Game Info for linking
#                              about.halfInning,about.inning,about.atBatIndex,pitchNumber,  #Time in game event occurred
#                              details.event,result.event,details.description,details.runnerGoing,details.call.description,result.description, #Event details
#                              player.id,matchup.batter.id,matchup.batter.fullName,matchup.batSide.code,matchup.pitcher.id,matchup.pitcher.fullName,matchup.pitchHand.code, #Players involved information
#                              count.balls.start,count.strikes.start,count.outs.start,result.rbi, #Balls Strike Outs and RsBI
#                              result.awayScore,result.homeScore
#                              #                              ,matchup.postOnFirst.id,matchup.postOnFirst.fullName,matchup.postOnSecond.id,matchup.postOnSecond.fullName,matchup.postOnThird.id,matchup.postOnThird.fullName #Runners on base information
#)) #Score
#sbhr1 <- pbp %>% filter(details.event == "Stolen Base 2B"|details.event == "Stolen Base 3B"|details.event == "Stolen Base Home"|details.event == "Caught Stealing 2B"|details.event == "Caught Stealing Home"|details.event == "Caught Stealing 3B"|details.event=="Pickoff 1B"|details.event=="Pickoff 2B"|details.event=="Pickoff 3B"|result.event=="Home Run" & details.description=="In play, run(s)")
#i=2
#N <- length(gamepk) #Get the length for when the loop should end

#tictoc::tic()
#while(i < N){ #Loop while the index is less than the length that will acquire the play-by-play and append it to one data frame. The run time is approx. 8 hours for 2019-21
#  pbp <- get_pbp_mlb(gamepk[i]) #baseballr function that pulls each pitch as recorded by gameday stringers from the MLB API
#  pbp <- subset(pbp, select = c(game_pk,game_date,fielding_team,batting_team,  #Game Info for linking
#                                about.halfInning,about.inning,about.atBatIndex,pitchNumber,  #Time in game event occurred
#                                details.event,result.event,details.description,details.runnerGoing,details.call.description,result.description, #Event details
#                                player.id,matchup.batter.id,matchup.batter.fullName,matchup.batSide.code,matchup.pitcher.id,matchup.pitcher.fullName,matchup.pitchHand.code, #Players involved information
#                                count.balls.start,count.strikes.start,count.outs.start,result.rbi, #Balls Strike Outs and RsBI
#                                result.awayScore,result.homeScore
#                                ,matchup.postOnFirst.id,matchup.postOnFirst.fullName,matchup.postOnSecond.id,matchup.postOnSecond.fullName,matchup.postOnThird.id,matchup.postOnThird.fullName #Runners on base information
#  )) #Score
#  sbhr2 <- pbp %>% filter(details.event == "Stolen Base 2B"|details.event == "Stolen Base 3B"|details.event == "Stolen Base Home"|details.event == "Caught Stealing 2B"|details.event == "Caught Stealing Home"|details.event == "Caught Stealing 3B"|details.event=="Pickoff 1B"|details.event=="Pickoff 2B"|details.event=="Pickoff 3B"|result.event=="Home Run" & details.description=="In play, run(s)")
#  sbhr1 <- rbind(sbhr1,sbhr2)
#  i <- i+1
#}
#tictoc::toc()
#write.csv(sbhr1, file="New SBHR Redone.csv") #Saves the data done into a file
#SBHRredone <- sbhr1 #New name used for code when pulling the csv so we will convert if it was run and not loaded in
#----------------------

#This below can be used to get the data again
#SBHR <- read.csv("New SBHR.csv")
#SBHR <- subset(SBHR, select=-c(X))
#SBHRredone <- read.csv("New SBHR Redone.csv")
#SBHRredone <- subset(SBHRredone, select=-c(X))

#i <- nrow(SBHRredone)

#------------------------
## Start here after complete or if you have the csv
#-------------------------

SBHR <- read.csv("New SBHR Sorted Redone.csv") #Use "New SBHR Sorted.csv" for old one
SBHR <- subset(SBHR, select=-c(X))
#############################################################################################
#YOU MUST RUN PITCHER HAND GRAPH MAKER BEFORE PROCEEDING!!!!
league=c("All Leagues","MLB","Triple-A","Double-A","High-A","A","Low-A")
year=c("2019 to 2021", "2019", "2020","2021")

#---------------------------------------------------------------------------------------------------
#RHP vs LHP analysis
SB_LR_Split_All <- graph_maker_byP.handness(SBHR,games,league="All Leagues",year="2019 to 2021")
SB_LR_Split_MLB <- graph_maker_byP.handness(SBHR,gamesMLB,league="MLB",year="2019 to 2021")
SB_LR_Split_TRIPLEA <- graph_maker_byP.handness(SBHR,gamesTRIPLEA,league="Triple-A",year="2019 to 2021")
SB_LR_Split_DOUBLEA <- graph_maker_byP.handness(SBHR,gamesDOUBLEA,league="Double-A",year="2019 to 2021")
SB_LR_Split_HIGHA <- graph_maker_byP.handness(SBHR,gamesHIGHA,league="High-A",year="2019 to 2021")
SB_LR_Split_A <- graph_maker_byP.handness(SBHR,gamesA,league="A",year="2019 to 2021")
SB_LR_Split_LOWA <- graph_maker_byP.handness(SBHR,gamesLOWA,league="Low-A",year="2019 to 2021")

SB_LR_Split_All_19 <- graph_maker_byP.handness(SBHR,games_19,league="All Leagues",year="2019")
SB_LR_Split_MLB_19 <-graph_maker_byP.handness(SBHR,gamesMLB_19,league="MLB",year="2019")
SB_LR_Split_TRIPLEA_19 <-graph_maker_byP.handness(SBHR,gamesTRIPLEA_19,league="Triple-A",year="2019")
SB_LR_Split_DOUBLEA_19 <-graph_maker_byP.handness(SBHR,gamesDOUBLEA_19,league="Double-A",year="2019")
SB_LR_Split_HIGHA_19 <-graph_maker_byP.handness(SBHR,gamesHIGHA_19,league="High-A",year="2019")
SB_LR_Split_A_19 <-graph_maker_byP.handness(SBHR,gamesA_19,league="A",year="2019")
SB_LR_Split_LOWA_19 <-graph_maker_byP.handness(SBHR,gamesLOWA_19,league="Low-A",year="2019")

SB_LR_Split_MLB_20 <- graph_maker_byP.handness(SBHR,games_20,league="MLB",year="2020")
SB_LR_Split_All_21 <- graph_maker_byP.handness(SBHR,games_21,league="All Leagues",year="2021")
SB_LR_Split_MLB_21 <- graph_maker_byP.handness(SBHR,gamesMLB_21,league="MLB",year="2021")
SB_LR_Split_TRIPLEA_21 <- graph_maker_byP.handness(SBHR,gamesTRIPLEA_21,league="Triple-A",year="2021")
SB_LR_Split_DOUBLEA_21 <- graph_maker_byP.handness(SBHR,gamesDOUBLEA_21,league="Double-A",year="2021")
SB_LR_Split_HIGHA_21 <- graph_maker_byP.handness(SBHR,gamesHIGHA_21,league="High-A",year="2021")
SB_LR_Split_A_21 <- graph_maker_byP.handnessADJUST(SBHR,gamesA_21,league="A",year="2021") #Adjustments necessary since no one was caught stealing home in A ball in 2021

Complete_SB_LR_Split <- rbind(SB_LR_Split_All,SB_LR_Split_MLB,SB_LR_Split_TRIPLEA,SB_LR_Split_DOUBLEA,SB_LR_Split_HIGHA,SB_LR_Split_A, SB_LR_Split_LOWA,
                              SB_LR_Split_All_19,SB_LR_Split_MLB_19,SB_LR_Split_TRIPLEA_19,SB_LR_Split_DOUBLEA_19,SB_LR_Split_HIGHA_19,SB_LR_Split_A_19,
                              SB_LR_Split_LOWA_19,SB_LR_Split_MLB_20,SB_LR_Split_All_21,SB_LR_Split_MLB_21,SB_LR_Split_TRIPLEA_21,SB_LR_Split_DOUBLEA_21,
                              SB_LR_Split_HIGHA_21,SB_LR_Split_A_21)
#write.csv(Complete_SB_LR_Split,file="All SBpercents by Pitch Hand.csv")
SBsplits <- read.csv("All SBpercents by Pitch Hand2.csv")
SBsplits <- subset(SBsplits,select=-c(X))
SBsplits <- as.data.frame(SBsplits)

g=1
SBpercen <- (SBsplits$L[g]+SBsplits$R[g])/(SBsplits$L.2[g]+SBsplits$R.2[g])
SBpercenTotal <- SBpercen
g<-g+1
while(g<=nrow(SBsplits)){
  SBpercen <- (SBsplits$L[g]+SBsplits$R[g])/(SBsplits$L.2[g]+SBsplits$R.2[g])
  SBpercenTotal <- rbind(SBpercenTotal,SBpercen)
  g<-g+1
}
Complete_gamesTable <- rbind(nrow(games),nrow(gamesMLB),nrow(gamesTRIPLEA),nrow(gamesDOUBLEA),nrow(gamesHIGHA),nrow(gamesA), nrow(gamesLOWA),
                             nrow(games_19),nrow(gamesMLB_19),nrow(gamesTRIPLEA_19),nrow(gamesDOUBLEA_19),nrow(gamesHIGHA_19),nrow(gamesA_19),
                             nrow(gamesLOWA_19),nrow(games_20),nrow(games_21),nrow(gamesMLB_21),nrow(gamesTRIPLEA_21),nrow(gamesDOUBLEA_21),
                             nrow(gamesHIGHA_21),nrow(gamesA_21))
SBdata <- cbind(SBpercenTotal,SBsplits,Complete_gamesTable)
SBdata <- mutate(SBdata, SB=L+R,CS=L.1+R.1,SBA=R.2+L.2,SBApG=(L.2+R.2)/Complete_gamesTable)
SBdata <- subset(SBdata,select=-c(L,R,L.1,R.1,L.2,R.2))
All_names <- c("All","MLB","Triple-A","Double-A","High-A","A","Low-A",
  "All 19","MLB 19","Triple-A 19","Double-A 19","High-A 19","A 19","Low-A 19",
  "MLB 20","All 21","MLB 21","Triple-A 21","Double-A 21","High-A 21","A 21")
rownames(SBdata) <- All_names
colnames(SBdata) <- cbind("Stolen Base %","G", "SB", "CS","SBA","SBA/G")

#---------------------------------------------------------------------------------------------------
#Ignore, used for seeing all LR percentages
#SBsplits <- cbind(SBpercenTotal,SBsplits)
#
#rownames(SBsplits) <- All_names
#colnames(SBsplits) <- cbind("Stolen Base %","SB_L", "SB_R", "CS_L", "CS_R", "SBA_L", "SBA_R")
#---------------------------------------------------------------------------------------------------

SBpercDiff_All <- SBdata$`Stolen Base %`[16]-SBdata$`Stolen Base %`[8]              #Difference in Stolen Base Percentage of Each Variable
SBpercDiff_MLB1 <- SBdata$`Stolen Base %`[17]-SBdata$`Stolen Base %`[9]
SBpercDiff_MLB2 <- SBdata$`Stolen Base %`[17]-SBdata$`Stolen Base %`[15]
SBpercDiff_MLB3 <- SBdata$`Stolen Base %`[15]-SBdata$`Stolen Base %`[9]
SBpercDiff_TRIPLEA <- SBdata$`Stolen Base %`[18]-SBdata$`Stolen Base %`[10]
SBpercDiff_DOUBLEA <- SBdata$`Stolen Base %`[19]-SBdata$`Stolen Base %`[11]
SBpercDiff_HIGHA <- SBdata$`Stolen Base %`[20]-SBdata$`Stolen Base %`[12]
SBpercDiff_A <- SBdata$`Stolen Base %`[21]-SBdata$`Stolen Base %`[13]
SBpercDiff_Total <- rbind(SBpercDiff_All,SBpercDiff_MLB1,SBpercDiff_MLB2,SBpercDiff_MLB3,SBpercDiff_TRIPLEA,SBpercDiff_DOUBLEA,SBpercDiff_HIGHA,SBpercDiff_A)
rownames(SBpercDiff_Total) <- c("ALL","MLB","MLB 2021-2020","MLB 2020-2019","Triple-A","Double-A","High-A","A")
colnames(SBpercDiff_Total) <- "Difference in SB Success %"

SBApropDiff_All <- SBdata$`SBA/G`[16]-SBdata$`SBA/G`[8]
SBApropDiff_MLB1 <- SBdata$`SBA/G`[17]-SBdata$`SBA/G`[9]
SBApropDiff_MLB2 <- SBdata$`SBA/G`[17]-SBdata$`SBA/G`[15]
SBApropDiff_MLB3 <- SBdata$`SBA/G`[15]-SBdata$`SBA/G`[9]
SBApropDiff_TRIPLEA <- SBdata$`SBA/G`[18]-SBdata$`SBA/G`[10]
SBApropDiff_DOUBLEA <- SBdata$`SBA/G`[19]-SBdata$`SBA/G`[11]
SBApropDiff_HIGHA <- SBdata$`SBA/G`[20]-SBdata$`SBA/G`[12]
SBApropDiff_A <- SBdata$`SBA/G`[21]-SBdata$`SBA/G`[13]
SBApropDiff_Total <- rbind(SBApropDiff_All,SBApropDiff_MLB1,SBApropDiff_MLB2,SBApropDiff_MLB3,SBApropDiff_TRIPLEA,SBApropDiff_DOUBLEA,SBApropDiff_HIGHA,SBApropDiff_A)
rownames(SBApropDiff_Total) <- c("ALL","MLB","MLB 2021-2020","MLB 2020-2019","Triple-A","Double-A","High-A","A")
colnames(SBApropDiff_Total) <- "    Difference in SBA/G proportion"

SBDiff <- cbind(SBpercDiff_Total,SBApropDiff_Total)
SBdata
SBDiff
#---------------------------------------------------------------------------------------------------
#Win-Loss Analysis
SBWin <- read.csv("Winners.csv")
SBWin <- subset(SBWin,select=-c(X))

SB_WL_Split_All <- graph_maker_byWinner(SBHR,games,league="All Leagues",year="2019 to 2021")
SB_WL_Split_MLB <- graph_maker_byWinner(SBHR,gamesMLB,league="MLB",year="2019 to 2021")
SB_WL_Split_TRIPLEA <- graph_maker_byWinner(SBHR,gamesTRIPLEA,league="Triple-A",year="2019 to 2021")
SB_WL_Split_DOUBLEA <- graph_maker_byWinner(SBHR,gamesDOUBLEA,league="Double-A",year="2019 to 2021")
SB_WL_Split_HIGHA <- graph_maker_byWinner(SBHR,gamesHIGHA,league="High-A",year="2019 to 2021")
SB_WL_Split_A <- graph_maker_byWinner(SBHR,gamesA,league="A",year="2019 to 2021")
SB_WL_Split_LOWA <- graph_maker_byWinner(SBHR,gamesLOWA,league="Low-A",year="2019 to 2021")

SB_WL_Split_All_19 <- graph_maker_byWinner(SBHR,games_19,league="All Leagues",year="2019")
SB_WL_Split_MLB_19 <-graph_maker_byWinner(SBHR,gamesMLB_19,league="MLB",year="2019")
SB_WL_Split_TRIPLEA_19 <-graph_maker_byWinner(SBHR,gamesTRIPLEA_19,league="Triple-A",year="2019")
SB_WL_Split_DOUBLEA_19 <-graph_maker_byWinner(SBHR,gamesDOUBLEA_19,league="Double-A",year="2019")
SB_WL_Split_HIGHA_19 <-graph_maker_byWinner(SBHR,gamesHIGHA_19,league="High-A",year="2019")
SB_WL_Split_A_19 <-graph_maker_byWinner(SBHR,gamesA_19,league="A",year="2019")
SB_WL_Split_LOWA_19 <-graph_maker_byWinner(SBHR,gamesLOWA_19,league="Low-A",year="2019")

SB_WL_Split_MLB_20 <- graph_maker_byWinnerADJUST2(SBHR,games_20,league="MLB",year="2020")
SB_WL_Split_All_21 <- graph_maker_byWinner(SBHR,games_21,league="All Leagues",year="2021")
SB_WL_Split_MLB_21 <- graph_maker_byWinner(SBHR,gamesMLB_21,league="MLB",year="2021")
SB_WL_Split_TRIPLEA_21 <- graph_maker_byWinner(SBHR,gamesTRIPLEA_21,league="Triple-A",year="2021")
SB_WL_Split_DOUBLEA_21 <- graph_maker_byWinnerADJUST(SBHR,gamesDOUBLEA_21,league="Double-A",year="2021")
SB_WL_Split_HIGHA_21 <- graph_maker_byWinner(SBHR,gamesHIGHA_21,league="High-A",year="2021")
SB_WL_Split_A_21 <- graph_maker_byWinner(SBHR,gamesA_21,league="A",year="2021") #Adjustments necessary since no one was caught stealing home in A ball in 2021

Complete_SB_WL_Split <- rbind(SB_WL_Split_All,SB_WL_Split_MLB,SB_WL_Split_TRIPLEA,SB_WL_Split_DOUBLEA,SB_WL_Split_HIGHA,SB_WL_Split_A, SB_WL_Split_LOWA,
                              SB_WL_Split_All_19,SB_WL_Split_MLB_19,SB_WL_Split_TRIPLEA_19,SB_WL_Split_DOUBLEA_19,SB_WL_Split_HIGHA_19,SB_WL_Split_A_19,
                              SB_WL_Split_LOWA_19,SB_WL_Split_MLB_20,SB_WL_Split_All_21,SB_WL_Split_MLB_21,SB_WL_Split_TRIPLEA_21,SB_WL_Split_DOUBLEA_21,
                              SB_WL_Split_HIGHA_21,SB_WL_Split_A_21)
Complete_SB_WL_Split <- as.data.frame(Complete_SB_WL_Split)
#write.csv(Complete_SB_WL_Split,file="All Winners.csv")
SBwinners <- read.csv("All Winners.csv")
SBdata2 <- mutate(SBwinners, SB=FALSE.+TRUE.,CS=FALSE..1+TRUE..1,SBA=TRUE..2+FALSE..2,SBApG=(FALSE..2+TRUE..2)/Complete_gamesTable)
SBdata2 <- subset(SBdata2,select=-c(X,FALSE.,TRUE.,FALSE..1,TRUE..1,FALSE..2,TRUE..2))

g=1
SBpercen2 <- (SBwinners$FALSE.[g]+SBwinners$TRUE.[g])/(SBwinners$FALSE..2[g]+SBwinners$TRUE..2[g])
SBpercenTotal2 <- SBpercen2
g<-g+1
while(g<=nrow(SBwinners)){
  SBpercen2 <- (SBwinners$FALSE.[g]+SBwinners$TRUE.[g])/(SBwinners$FALSE..2[g]+SBwinners$TRUE..2[g])
  SBpercenTotal2 <- rbind(SBpercenTotal2,SBpercen2)
  g<-g+1
}

SBdata2 <- cbind(SBpercenTotal2,Complete_gamesTable,SBdata2)
rownames(SBdata2) <- All_names
colnames(SBdata2) <- cbind("Stolen Base %","G", "SB", "CS","SBA","SBA/G")

SBpercDiff2_All <- SBdata2$`Stolen Base %`[16]-SBdata2$`Stolen Base %`[8]
SBpercDiff2_MLB1 <- SBdata2$`Stolen Base %`[17]-SBdata2$`Stolen Base %`[9]
SBpercDiff2_MLB2 <- SBdata2$`Stolen Base %`[17]-SBdata2$`Stolen Base %`[15]
SBpercDiff2_MLB3 <- SBdata2$`Stolen Base %`[15]-SBdata2$`Stolen Base %`[9]
SBpercDiff2_TRIPLEA <- SBdata2$`Stolen Base %`[18]-SBdata2$`Stolen Base %`[10]
SBpercDiff2_DOUBLEA <- SBdata2$`Stolen Base %`[19]-SBdata2$`Stolen Base %`[11]
SBpercDiff2_HIGHA <- SBdata2$`Stolen Base %`[20]-SBdata2$`Stolen Base %`[12]
SBpercDiff2_A <- SBdata2$`Stolen Base %`[21]-SBdata2$`Stolen Base %`[13]
SBpercDiff2_Total <- rbind(SBpercDiff2_All,SBpercDiff2_MLB1,SBpercDiff2_MLB2,SBpercDiff2_MLB3,SBpercDiff2_TRIPLEA,SBpercDiff2_DOUBLEA,SBpercDiff2_HIGHA,SBpercDiff2_A)

rownames(SBpercDiff2_Total) <- c("ALL","MLB","MLB 2021-2020","MLB 2020-2019","Triple-A","Double-A","High-A","A")
colnames(SBpercDiff2_Total) <- "Difference in SB Success %"

SBApropDiff2_All <- SBdata2$`SBA/G`[16]-SBdata2$`SBA/G`[8]
SBApropDiff2_MLB1 <- SBdata2$`SBA/G`[17]-SBdata2$`SBA/G`[9]
SBApropDiff2_MLB2 <- SBdata2$`SBA/G`[17]-SBdata2$`SBA/G`[15]
SBApropDiff2_MLB3 <- SBdata2$`SBA/G`[15]-SBdata2$`SBA/G`[9]
SBApropDiff2_TRIPLEA <- SBdata2$`SBA/G`[18]-SBdata2$`SBA/G`[10]
SBApropDiff2_DOUBLEA <- SBdata2$`SBA/G`[19]-SBdata2$`SBA/G`[11]
SBApropDiff2_HIGHA <- SBdata2$`SBA/G`[20]-SBdata2$`SBA/G`[12]
SBApropDiff2_A <- SBdata2$`SBA/G`[21]-SBdata2$`SBA/G`[13]
SBApropDiff2_Total <- rbind(SBApropDiff2_All,SBApropDiff2_MLB1,SBApropDiff2_MLB2,SBApropDiff2_MLB3,SBApropDiff2_TRIPLEA,SBApropDiff2_DOUBLEA,SBApropDiff2_HIGHA,SBApropDiff2_A)
rownames(SBApropDiff2_Total) <- c("ALL","MLB","MLB 2021-2020","MLB 2020-2019","Triple-A","Double-A","High-A","A")
colnames(SBApropDiff2_Total) <- "    Difference in SBA/G proportion"

SBDiff2 <- cbind(SBpercDiff2_Total,SBApropDiff2_Total)

SBdata
SBDiff
SBdata2
SBDiff2
SBdataDiff <- SBdata-SBdata2

SBdata
SBdata2
SBdataDiff
#---------------------------------------------------------------------------------------------------
#CORRECT DATA INPUTTED DIRECTLY VIA BASEBALL REFERENCE
SB_19 <- c(2280,3761,3147,3147,3461,1496)
CS_19 <- c(832,1763,1489,1590,1611,726)
G_19 <- c(4858,6080,4154,4086,4150,1666)
SB_20 <- c(884)
CS_20 <- c(292)
G_20 <- c(1796)
SB_21 <- c(2213,2752,2760,3821,4357)
CS_21 <- c(711,887,1135,1219,1300)
G_21 <- c(4858,3850,3482,3544,3554)

SBdata19 <- cbind(G_19,SB_19,CS_19)
SBdata19 <- rbind(SBdata19,colSums(SBdata19))
SBdata20 <- cbind(G_20,SB_20,CS_20)
SBdata21 <- cbind(G_21,SB_21,CS_21)
SBdata21 <- rbind(SBdata21,colSums(SBdata21))
SBdata3 <- rbind(SBdata19,SBdata20,SBdata21)
colnames(SBdata3) <- cbind("G", "SB", "CS")
SBdata3 <- as.data.frame(SBdata3)
SBdata3 <- mutate(SBdata3, SBA=SB+CS,SBApG=(SB+CS)/G)
SBpercentage <- SBdata3$SB/SBdata3$SBA
SBdata3 <- cbind(SBpercentage,SBdata3)
League_names <- c("MLB 19","Triple-A 19","Double-A 19","High-A 19","A 19","Low-A 19","All 19",
                  "MLB 20","MLB 21","Triple-A 21","Double-A 21","High-A 21","A 21", "All 21")
rownames(SBdata3) <- League_names
colnames(SBdata3) <- cbind("Stolen Base %","G", "SB", "CS","SBA","SBA/G")

SBpercDiff3_MLB1 <- SBdata3$`Stolen Base %`[9]-SBdata3$`Stolen Base %`[1]
SBpercDiff3_MLB2 <- SBdata3$`Stolen Base %`[8]-SBdata3$`Stolen Base %`[1]
SBpercDiff3_MLB3 <- SBdata3$`Stolen Base %`[9]-SBdata3$`Stolen Base %`[8]
SBpercDiff3_TRIPLEA <- SBdata3$`Stolen Base %`[10]-SBdata3$`Stolen Base %`[2]
SBpercDiff3_DOUBLEA <- SBdata3$`Stolen Base %`[11]-SBdata3$`Stolen Base %`[3]
SBpercDiff3_HIGHA <- SBdata3$`Stolen Base %`[12]-SBdata3$`Stolen Base %`[4]
SBpercDiff3_A <- SBdata3$`Stolen Base %`[13]-SBdata3$`Stolen Base %`[5]
SBpercDiff3_All <- SBdata3$`Stolen Base %`[14]-SBdata3$`Stolen Base %`[7]
SBpercDiff3_Total <- rbind(SBpercDiff3_MLB1,SBpercDiff3_MLB2,SBpercDiff3_MLB3,SBpercDiff3_TRIPLEA,SBpercDiff3_DOUBLEA,SBpercDiff3_HIGHA,SBpercDiff3_A,SBpercDiff3_All)

rownames(SBpercDiff3_Total) <- c("MLB","MLB 2021-2020","MLB 2020-2019","Triple-A","Double-A","High-A","A","All")
colnames(SBpercDiff3_Total) <- "Difference in SB Success %"

SBApropDiff3_MLB1 <- SBdata3$`SBA/G`[9]-SBdata3$`SBA/G`[1]
SBApropDiff3_MLB2 <- SBdata3$`SBA/G`[8]-SBdata3$`SBA/G`[1]
SBApropDiff3_MLB3 <- SBdata3$`SBA/G`[9]-SBdata3$`SBA/G`[7]
SBApropDiff3_TRIPLEA <- SBdata3$`SBA/G`[10]-SBdata3$`SBA/G`[2]
SBApropDiff3_DOUBLEA <- SBdata3$`SBA/G`[11]-SBdata3$`SBA/G`[3]
SBApropDiff3_HIGHA <- SBdata3$`SBA/G`[12]-SBdata3$`SBA/G`[4]
SBApropDiff3_A <- SBdata3$`SBA/G`[13]-SBdata3$`SBA/G`[5]
SBApropDiff3_A <- SBdata3$`SBA/G`[14]-SBdata3$`SBA/G`[7]
SBApropDiff3_Total <- rbind(SBApropDiff3_MLB1,SBApropDiff3_MLB2,SBApropDiff3_MLB3,SBApropDiff3_TRIPLEA,SBApropDiff3_DOUBLEA,SBApropDiff3_HIGHA,SBApropDiff3_A,SBApropDiff3_All)
rownames(SBApropDiff3_Total) <- c("MLB","MLB 2021-2020","MLB 2020-2019","Triple-A","Double-A","High-A","A","All")
colnames(SBApropDiff3_Total) <- "    Difference in SBA/G proportion"

SBdata3
SBpercDiff3_Total
SBApropDiff3_Total
#---------------------------------------------------------------------------------------------------
#95% Confidence Intervals
League_names2 <- c("MLB 19","Triple-A 19","Double-A 19","High-A 19","A 19","Low-A 19",
                  "MLB 20","MLB 21","Triple-A 21","Double-A 21","High-A 21","A 21")

m=1
phat <-SBdata3$`Stolen Base %`[m]
ME <- sqrt((phat*(1-phat))/SBdata3$SBA[m])
low <- phat-(1.96*ME)
hi <- phat+(1.96*ME)
std2 <- sqrt((phat)*(1-phat)/SBdata3$SBA[m])
all_CI <- c(phat,ME,low,hi)
m <- m+1
while(m<=nrow(SBdata3)){
  phat <-SBdata3$`Stolen Base %`[m]
  ME <- sqrt((phat*(1-phat))/SBdata3$SBA[m])
  low <- phat-(1.96*ME)
  hi <- phat+(1.96*ME)
  std <- sqrt((phat)*(1-phat)/SBdata3$SBA[m])
  CI <- c(phat,ME,low,hi)
  all_CI <- rbind(all_CI,CI)
  std2 <- rbind(std2,std)
  m <- m+1
}
rownames(all_CI) <- League_names
colnames(all_CI) <- c("P-Hat Value","Margin of Error","Lower Bound","Upper Bound")
all_CI
rownames(std2)<- All_names
colnames(std2) <- "Standard Deviation"
std2

g=1
devi <-SBdata3$`SBA/G`[g]-SBdata3$`SBA/G`[7]
std3 <- sqrt(abs((devi/SBdata3$G[7])))
low2 <- devi-(1.96*std3)
hi2 <- devi+(1.96*std3)
all_CI2 <- c(devi,std3,low2,hi2)
g <- g+1
while(g<nrow(SBdata19-1)){
  devi <-SBdata3$`SBA/G`[g]-SBdata3$`SBA/G`[7]
  std4 <- sqrt(abs((devi/SBdata3$G[7])))
  low2 <- devi-(1.96*std4)
  hi2 <- devi+(1.96*std4)
  CI2 <- c(devi,std4,low2,hi2)
  all_CI2 <- rbind(all_CI2,CI2)
  std3 <- rbind(std3,std4)
  g <- g+1
}
all_CI3 <- all_CI2
all_CI3
std5 <- std3
g <- g+1
while(g<nrow(SBdata3-1)){
  devi <-SBdata3$`SBA/G`[g]-SBdata3$`SBA/G`[14]
  std4 <- sqrt(abs((devi/SBdata3$G[14])))
  low2 <- devi-(1.96*std4)
  hi2 <- devi+(1.96*std4)
  CI2 <- c(devi,std4,low2,hi2)
  all_CI3 <- rbind(all_CI3,CI2)
  std5 <- rbind(std5,std4)
  g <- g+1
}
rownames(all_CI3) <- League_names2
colnames(all_CI3) <- c("Deviation","Margin of Error","Lower Bound","Upper Bound")
all_CI3 <- cbind(group=League_names2,all_CI3)
rownames(std5)<- League_names2
colnames(std5) <- "Standard Deviation"
std5
SBdata3
plot(SBdata3$`Stolen Base %`,SBdata3$`SBA/G`,xlab="Stolen Base %",ylab="SBA/G",main="Stolen Base Attempts per Game by Stolen Base Percentage")
reg <- lm(`SBA/G`~`Stolen Base %`,data=SBdata3)
summary(reg)
cor(SBdata3$`SBA/G`,SBdata3$`Stolen Base %`)
SBdata19 <- as.data.frame(SBdata19)
SBdata19 <- mutate(SBdata19,`Stolen Base %`=SB_19/(SB_19+CS_19),`SBA/G`=(SB_19+CS_19)/G_19)
SBdata19
plot(SBdata19$`Stolen Base %`,SBdata19$`SBA/G`,xlab="Stolen Base %",ylab="SBA/G",main="Stolen Base Attempts per Game by Stolen Base Percentage in 2019")
reg19 <- lm(`SBA/G`~`Stolen Base %`,data=SBdata19)
summary(reg19)
SBdata21 <- as.data.frame(SBdata21)
SBdata21 <- mutate(SBdata21,`Stolen Base %`=SB_21/(SB_21+CS_21),`SBA/G`=(SB_21+CS_21)/G_21)
SBdata21
plot(SBdata21$`Stolen Base %`,SBdata21$`SBA/G`,xlab="Stolen Base %",ylab="SBA/G",main="Stolen Base Attempts per Game by Stolen Base Percentage in 2021")
reg21 <- lm(`SBA/G`~`Stolen Base %`,data=SBdata21)
summary(reg21)

#---------------------------------------------------------------------------------------------------
#Two Sample Z-Test
prop.test(x=c(SBdata3$SB[4],SBdata3$SB[12]),n=c(SBdata3$SBA[4],SBdata3$SBA[12]))
#2-sample test for equality of proportions with continuity correction
#data:  c(SBdata3$SB[4], SBdata3$SB[12]) out of c(SBdata3$SBA[4], SBdata3$SBA[12])
#X-squared = 104.45, df = 1, p-value < 2.2e-16
#alternative hypothesis: two.sided
#95 percent confidence interval:
#  -0.11190032 -0.07568048
#sample estimates:
#  prop 1(SB% High-A 2019)          prop 2 (SB% High-A 2021)
#0.6643445                           0.7581349 
#---------------------------------------------------------------------------------------------------
#Two Sample T-Test
t.test.from.summary.data <- function(mean1, sd1, n1, mean2, sd2, n2, ...) {
  data1 <- scale(1:n1)*sd1 + mean1
  data2 <- scale(1:n2)*sd2 + mean2
  t.test(data1, data2, ...)
}
std2
SBdata3
t.test.from.summary.data(mean1=SBdata3$`SBA/G`[5],sd1=std5[5],n1=SBdata3$G[5],mean2=SBdata3$`SBA/G`[13],sd2=std5[12],n2=SBdata3$G[13])
#---------------------------------------------------------------------------------------------------
#ANOVA
##  Significance codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#year <- c("All","All","All","All","All","All","All",2019,2019,2019,2019,2019,2019,2019,2020,2021,2021,2021,2021,2021,2021)
year <- c("2019","2019","2019","2019","2019","2019","2019","2020","2021","2021","2021","2021","2021","2021")
df <- cbind(group=League_names,data.frame(SBA = SBdata3[,5],mean = SBdata3[,1],sd = std2[,]),year)
df
ggplot(df,aes(x=League_names,fill=year))+geom_boxplot(aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity") + xlab("League") + ylab("SB Success %") + ggtitle("Stolen Base Success Rates By League and Year")
one.way <- anovaMean(df$group,df$SBA,df$mean,df$sd,ylabel="SB Success %")
one.way
#Analysis of Variance Table
#Response: SB Success %
#Terms added sequentially (first to last)
#                   Df     Sum of Sq   Mean Sq   F value   Pr(F)    
#SB Success %       13     128.395     9.8765    330075    <2.2e-16 ***
#  Residuals        94078  2.815       0.0000                         

#TukeyHSD(one.way,conf.level = .95)

#df$Is.Nineteen <- grepl("19",df$group, ignore.case = T)
df19<- df[df$year == "2019",]
ggplot(df19,aes(x=df19$group,fill=year))+geom_boxplot(aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity") + xlab("League") + ylab("SB Success %") + ggtitle("Stolen Base Success Rates By League and Year")
one.way19 <- anovaMean(df19$group,df19$SBA,df19$mean,df19$sd,ylabel="SB Success %")
one.way19

#                   Df   Sum of Sq   Mean Sq   F value    Pr(F)     
#SB Success %       5     9.6345      1.92691   37770     <2.2e-16 ***
#  Residuals      25297   1.2906      0.00005     

#df$Is.Twentyone <- grepl("21",df$group, ignore.case = T)
df21<- df[df$year == "2021",]
ggplot(df21,aes(x=df21$group))+geom_boxplot(aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity") + xlab("League") + ylab("SB Success %") + ggtitle("Stolen Base Success Rates By League and Year")
one.way21 <- anovaMean(df21$group,df21$SBA,df21$mean,df21$sd,ylabel="SB Success %")
one.way21
#                   Df   Sum of Sq   Mean Sq   F value   Pr(F)
#SB Success %     4      9.5318     2.38294    53904     <2.2e-16 ***
#  Residuals    21150    0.9350     0.00004
all_CI3 <- as.data.frame(all_CI3)
ggplot(all_CI3, aes(group, Deviation)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin =`Lower Bound`, ymax = `Upper Bound`)) +
#  ylim(c(-0.2,0.5)) + 
  geom_hline(yintercept = 0,col='red',size=3)

#---------------------------------------------------------------------------------------------------
#Bayesian Analysis Code

##### PRIOR
r=8 #MLB 2021 value
trials <- SBdata3$SBA[r]
p <- SBdata3$`Stolen Base %`[r+1]
successes <- SBdata3$SB[r]
population_mean=all_CI[r]
population_sd=std2[r]

lower_bound <- population_mean - population_sd
upper_bound <- population_mean + population_sd
x <- seq(-4, 4, length = 1000) * population_sd + population_mean

rangeP <- seq(0, 1, length.out = 1000)  #Sequence from 0 to 1 split by each hundredth for the range of P
#Plot our Prior Distribution
plot(rangeP, dbinom(x = successes, prob = rangeP, size = trials),
     type = "l", xlab = "P(SB Safe)", ylab = "Density", main="Probability Distribution")
#Let's Zoom in on that
plot(rangeP, dbinom(x = successes, prob = rangeP, size = trials),
     type = "l", xlab = "P(SB Safe)", ylab = "Density", main="Probability Distribution",xlim=c(0.735,0.775),ylim=c(0,.08))
##### LIKELIHOOD
#Plot our Likelihood
lines(rangeP, dnorm(x = rangeP, mean = p, sd = std2[r+1])/trials,
      col = "red")

lik <- dbinom(x = successes, prob = rangeP, size = trials)
prior <- dnorm(x = rangeP, mean = p, sd = std2[r+1])/40
##### POSTIERIOR
#lines(rangeP, lik*prior, col = "green")
unstdPost <- lik * prior
stdPost <- unstdPost / sum(unstdPost)
stdPost
lines(rangeP, stdPost, col = "blue")
legend("topleft", legend = c("Likelihood", "Prior", "Poster","HDI Boundaries", "CI Bounds"),
       text.col = c(1,2,4,3,6), bty = "n")
post <- (stdPost~rangeP)
summary(post)


auc(rangeP[740:767],stdPost[740:767])*999
#0.740 to 0.7675 is our credible interval and also our HDI
abline(v=0.74,col="green")
abline(v=0.7675,col="green")
abline(v=0.)

prop.test(x=c(SBdata3$SB[9]+SBdata3$SB[8]),n=c(SBdata3$SBA[9]+SBdata3$SBA[8]))
SBdata3
abline(v=0.7410486,col="purple")
abline(v=0.7694006,col="purple")
