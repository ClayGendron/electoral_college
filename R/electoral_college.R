#load packages
library(tidymodels)
library(tidyverse)
library(here)
library(corrr)
library(corrplot)

#data pull
pe_2016 <- read.csv(here::here("Data/Presidential Election Data (2016).csv"))
colnames(pe_2016)
summary(pe_2016)

pe_corr_df <- pe_2016 %>% select("VoteMarginPercentABS", "VoteMarginPercent", "TurnOut", "RuralPopulation","SmallTownPopulation", "SuburbPopulation", "UrbanPopulation","White", "Black", "Hispanic","AmericanIndian.AlaskaNative", "Asian", "NativeHawaiian.OtherPacificIslander", "TwoOrMoreRaces", "HighSchool", "BachelorsDegree","AdvancedDegree")


white_m <- mean(pe_2016$White)
white_sd <- sd(pe_2016$White)

#analysis
turnout_lm <- lm(TurnOut ~
                   VoteMarginPercentABS,
  data = pe_2016
)
summary(turnout_lm)

margin_lm <- lm(VoteMarginPercent ~
                  ((White / white_m) * white_sd) +
                  Black +
                  Hispanic +
                  UrbanPopulation +
                  BachelorsDegree,
                data = pe_2016)

summary(margin_lm)

pe_corr <- cor(pe_corr_df)
corrplot(pe_corr)
