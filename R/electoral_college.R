#load packages
library(tidymodels)
library(tidyverse)
library(here)
library(corrr)
library(corrplot)

#data pull
pe_2016 <- read.csv(here::here("Data/Presidential Election Data (2016).csv"))
pe_2016_m_dc <- pe_2016 %>% filter(State != "District of Columbia")
pe_2016_m_dc_hw <- pe_2016 %>% filter(State != "District of Columbia") %>%  filter(State != "Hawaii") 
colnames(pe_2016)
# ("State" "Year" "Democrat.Votes" "Democrat.EC", "Republican.Votes", "Republican.EC", "Third.Party.Votes", "Third.Party.EC", "Vote.Margin..ABS.", "Vote.Margin", "Vote.Margin....ABS.", "Vote.Margin..", "Total.Votes.Minus.Third.Party", "Total.Votes", "Total.EC", "Registered.Voters", "Voting.Aged.Population", "State.Population", "Electoral.Power.Group", "State.Population.Over.EC", "Turn.Out", "Rural.Population", "Small.Town.Population", "Suburb.Population", "Urban.Population", "White", "Black", "Hispanic", "Asian", "Two.or.More.Races.Other", "High.School", "Bachelors.Degree", "Advanced.Degree", "Children.0.18", "Adults.19.25", "Adults.26.34", "Adults.35.54", "Adults.55.64", "Adults.65.", "Below.Poverty....100..", "Above.Poverty..100...199..", "Above.Poverty..200.399..", "Abover.Poverty..400...", "Male", "Female")
summary(pe_2016)

pe_corr_df <- pe_2016 %>% select("Vote.Margin..", "Turn.Out", "Rural.Population", "Small.Town.Population", "Suburb.Population", "Urban.Population", "White", "Black", "Hispanic", "Asian", "Two.or.More.Races.Other","No.High.School", "High.School", "Bachelors.Degree", "Advanced.Degree", "Children.0.18", "Adults.19.25", "Adults.26.34", "Adults.35.54", "Adults.55.64", "Adults.65.", "Below.Poverty....100..", "Above.Poverty..100...199..", "Above.Poverty..200.399..", "Abover.Poverty..400...", "Male", "Female")


#analysis
turnout_lm <- lm(TurnOut ~
                   VoteMarginPercentABS,
  data = pe_2016_m_dc
)
summary(turnout_lm)




margin_lm <- lm(VoteMarginPercent ~
                  White +
                  Black +
                  Hispanic +
                  UrbanPopulation +
                  BachelorsDegree,
                data = pe_2016)

summary(margin_lm)

pe_corr <- cor(pe_corr_df)
pe_corr
corrplot(pe_corr)


#model creation 
pva_t <- 
  pva %>% 
  dplyr::select(HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, NGIFTALL, CARDGIFT, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, AVGGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE, GIFTAMNT, TOTALGIFTAMNT) %>% 
  initial_split()

#train and test
train <- training(pva_t) # training dataset
test <- testing(pva_t) # testing dataset

#recipes
mod_rec_gift_amt <- recipe(GIFTAMNT~ ., data = train) %>% 
  step_center(
    HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, NGIFTALL, CARDGIFT, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, AVGGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE
  ) %>%
  step_scale(
    HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, NGIFTALL, CARDGIFT, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, AVGGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE
  )

# training model
# all variables
gift_amt_lm_vars <- qc(HOMEOWNER, STATEGOV, NGIFTALL, LASTGIFT, AVGGIFT, CLUSTER2)
gift_amt_function_var <- "GIFTAMNT"
glm_formula <- as.formula(paste(sprintf("%s ~", gift_amt_function_var), paste(gift_amt_lm_vars [!gift_amt_lm_vars  %in% "y"], collapse = " + ")))

#bake data
train_prep <- prep(mod_rec_gift_amt, training = train)
train_data <- bake(train_prep, train)

# all variables function
gift_amount_lm <- lm(
  formula = glm_formula
  , data = train_data
)


# quick summary
summary(gift_amount_lm)

# test model
test_data <- bake(train_prep, test)

test_p <- predict(gift_amount_lm, test_data)
test_predict <- cbind(test_data, test_p)
test_sse <- (test_predict$GIFTAMNT - test_predict$test_p)^2
test_sst <- (test_predict$GIFTAMNT - mean(test_predict$GIFTAMNT))^2
test_r2 <- 1 - (sum(test_sse)/sum(test_sst))
test_r2


# k means model

#recipe

mod_rec_k_means <- recipe(TOTALGIFTAMNT ~ ., data = pva) %>% 
  step_center(
    HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE, TOTALGIFTAMNT
  ) %>%
  step_scale(
    HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE, TOTALGIFTAMNT
  )

mod_rec_pe_2016 <- recipe(VoteMarginPercent ~ ., data = pe_2016) %>% 
  step_center(
    VoteMarginPercentABS, VoteMarginPercent, TurnOut, RuralPopulation, SmallTownPopulation, SuburbPopulation, UrbanPopulation, White, Black, Hispanic, AmericanIndian.AlaskaNative, Asian, NativeHawaiian.OtherPacificIslander, TwoOrMoreRaces, HighSchool, BachelorsDegree, AdvancedDegree
    ) %>% 
  step_scale(
    VoteMarginPercentABS, VoteMarginPercent, TurnOut, RuralPopulation, SmallTownPopulation, SuburbPopulation, UrbanPopulation, White, Black, Hispanic, AmericanIndian.AlaskaNative, Asian, NativeHawaiian.OtherPacificIslander, TwoOrMoreRaces, HighSchool, BachelorsDegree, AdvancedDegree
    )


k_prep <- prep(mod_rec_k_means, training = pva)

cluster_prep <- prep(mod_rec_pe_2016, training = pe_2016)
cluster_table <- bake(cluster_prep, pe_2016_m_dc_hw)
cluster_table <- cluster_table %>% dplyr::select(VoteMarginPercent, RuralPopulation, UrbanPopulation, White, Black, Hispanic, BachelorsDegree)

k_table <- bake(k_prep, pva)
k_table <- k_table %>% dplyr::select(HOMEOWNER, HIT, MALEVET, VIETVETS, WWIIVETS, LOCALGOV, STATEGOV, FEDGOV, CARDPROM, NUMPROM, CARDPM12, NUMPRM12, MINRAMNT, MINRDATE_T, MAXRAMNT, MAXRDATE_T, LASTGIFT, CONTROLN, HPHONE_D, CLUSTER2, CHILDREN, AGE, TOTALGIFTAMNT)
colnames(k_table)

# model creation

state_cluster_mod <- kmeans(cluster_table, centers = 5, nstart = 10000)
state_cluster_mod

# build table

state_cluster_table <- cbind(pe_2016_m_dc_hw, state_cluster_mod$cluster)
names(state_cluster_table)[32] <- "Cluster"
st <- state_cluster_table %>% select(State,VoteMarginPercent,Cluster)

pva_clusters <- cbind(pva, k_mod$cluster)




