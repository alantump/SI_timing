# load libraries
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(cowplot)
library(knitr)



# read file/datbase
the_sheet <- read_excel(filename,sheet = 2)

# filter players with 120 trials
n_sheet <- the_sheet %>% group_by(playerNr)  %>% summarise(n = length(period))
the_sheet_filtered <- the_sheet %>% filter(playerNr %in%  n_sheet$playerNr[n_sheet$n==120])

# add all additional variables needed for analysis
# choices orange/blue/5050
finalSheet <- the_sheet_filtered %>%  mutate(choiceOrange = ifelse(rating==1 | rating==2 | rating==3 | rating==4 | rating==5,1,0), choiceBlue = ifelse(rating==7 | rating==8 | rating==9 | rating==10 | rating==11,1,0), choice5050 = ifelse(rating==6,1,0))
# translate confidence
finalSheet <- finalSheet %>%  mutate(confidence = case_when(xor(rating==1, rating==11) ~ 100, xor(rating==2, rating==10) ~ 90, xor(rating==3, rating==9) ~ 80, xor(rating==4, rating==8) ~ 70, xor(rating==5, rating==7) ~ 60, rating==6 ~ 50))
# brierScore
finalSheet <- finalSheet %>% mutate(brierScore = (confidence/100-correctChoice)^2)
# correctSccore (should only be needed in the first batch of the main experiment)
finalSheet <- finalSheet %>% mutate (correctScore = case_when( payoff==25 | payoff==24 | payoff==21 | payoff==16 | payoff==9 | payoff==-11 | payoff==-24 | payoff==-39 | payoff==-56 | payoff==-75| tooLate==1 ~ payoff, payoff==0&rating==6 ~ 0, payoff==0&tooLate!=1&xor(rating==1, rating==11) ~ -75))
# goup conditions to noSI and SI
finalSheet <- finalSheet %>% mutate(socialInformation = ifelse(SI_timing=='early' | SI_timing=='late','SI','noSI'))
# make 5050 choices NA in accuracy
finalSheet$correctChoice[finalSheet$confidence==50]=NA
#scale confidence wrong/correct 0-100
finalSheet <- finalSheet %>% mutate(confidenceScaled = case_when(xor(rating==1, rating==11)&correctChoice==1 ~ 100,
                                                                 xor(rating==1, rating==11)&correctChoice==0 ~ 0,
                                                                 xor(rating==2, rating==10)&correctChoice==1 ~ 90,
                                                                 xor(rating==2, rating==10)&correctChoice==0 ~ 10,
                                                                 xor(rating==3, rating==9)&correctChoice==1 ~ 80, 
                                                                 xor(rating==3, rating==9)&correctChoice==0 ~ 20,
                                                                 xor(rating==4, rating==8)&correctChoice==1 ~ 70,
                                                                 xor(rating==4, rating==8)&correctChoice==0 ~ 30,
                                                                 xor(rating==5, rating==7)&correctChoice==1 ~ 60,
                                                                 xor(rating==5, rating==7)&correctChoice==0 ~ 40,
                                                                 rating==6 ~ 50))
