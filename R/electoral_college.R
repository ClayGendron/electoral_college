#load packages
library(tidymodels)
library(tidyverse)
library(here)
library(corrr)
library(corrplot)

#data pull
pe_2016 <- read.csv(here::here("Data/Presidential Election Data (2016).csv"))
colnames(pe_2016)
# ("State" "Year" "Democrat.Votes" "Democrat.EC", "Republican.Votes", "Republican.EC", "Third.Party.Votes", "Third.Party.EC", "Vote.Margin..ABS.", "Vote.Margin", "Vote.Margin....ABS.", "Vote.Margin..", "Total.Votes.Minus.Third.Party", "Total.Votes", "Total.EC", "Registered.Voters", "Voting.Aged.Population", "State.Population", "Electoral.Power.Group", "State.Population.Over.EC", "Turn.Out", "Rural.Population", "Small.Town.Population", "Suburb.Population", "Urban.Population", "White", "Black", "Hispanic", "Asian", "Two.or.More.Races.Other", "High.School", "Bachelors.Degree", "Advanced.Degree", "Children.0.18", "Adults.19.25", "Adults.26.34", "Adults.35.54", "Adults.55.64", "Adults.65.", "Below.Poverty....100..", "Above.Poverty..100...199..", "Above.Poverty..200.399..", "Abover.Poverty..400...", "Male", "Female")
summary(pe_2016)

pe_corr_df <- pe_2016 %>% select("Vote.Margin..", "Turn.Out", "Rural.Population", "Small.Town.Population", "Suburb.Population", "Urban.Population", "White", "Black", "Hispanic", "Asian", "Two.or.More.Races.Other","No.High.School", "High.School", "Bachelors.Degree", "Advanced.Degree", "Children.0.18", "Adults.19.25", "Adults.26.34", "Adults.35.54", "Adults.55.64", "Adults.65.", "Below.Poverty....100..", "Above.Poverty..100...199..", "Above.Poverty..200.399..", "Abover.Poverty..400...", "Male", "Female")


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
pe_corr
corrplot(pe_corr)
