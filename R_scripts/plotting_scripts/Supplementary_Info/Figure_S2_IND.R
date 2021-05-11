
####### Replication code for Fig. S2 - Validation tests using Area under the ROC curve ####### 

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)

## This R.script makes logistic regression applied to each country survey dataset we build, 
## determining how well the model is able to classify household who have spent in air conditioning
## for their primary residence. This is good for proving reviewers our prediction are NOT CASUAL. 
## Here we focus on INDIA and last wave NSS 2012


## Install required R packages if required before loading

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc()                  # frees up memory resources

library(data.table)
library(plyr)
library(dplyr)
library(reshape)
library(reshape2)
library(tidyr)
library(readstata13)
library(stringr)
library(glmnet)
library(caTools)
library(ROCR)
library(boot)
library(Zelig)

## Set path to work directory

#user <- 'username'

if (user=='username') {
  stub <- 'C:/Users/Standard/Google Drive/5-CountryExpertsExchange/Comparative_paper/'
}

dta_dir              <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/input_data_files/', sep='')
SI_plot_dir          <- paste(stub,'Data_codes_to_be_submitted_for_online_ver/output_table_figures/Supplementary_Info/', sep='')


### Load Data ###

HH_4C <- read.dta13(paste(dta_dir,'/HH_4countries_extended.dta', sep=''))

# Choose India
HH_India <- HH_4C %>% filter(country == "India")
HH_India <- HH_India %>% filter(year == 2012)

# Arrange some vars
HH_India$state <- as.factor(HH_India$state)
HH_India$urban <- as.factor(HH_India$urban)
HH_India$ac <- as.factor(HH_India$ac)
HH_India$fan <- as.factor(HH_India$fan)
HH_India$refrigerator <- as.factor(HH_India$refrigerator)
HH_India$ownership <- as.factor(HH_India$ownership)
HH_India$edu_head_2 <- as.factor(HH_India$edu_head_2)
HH_India$sex_head <- as.factor(HH_India$sex_head)
HH_India$occupation_head <- as.factor(HH_India$occupation_head)
HH_India$year <- as.factor(HH_India$year)

# Only those with not missing AC
HH_India <- HH_India[complete.cases(HH_India$ac), ]

# Only who has electricity access
HH_India <- HH_India %>% filter(ely_access == 1)
HH_India <- HH_India %>% filter(occupation_head != 1)

# log total exp
HH_India$ln_total_exp_usd_2011 <- log(HH_India$total_exp_usd_2011)
HH_India$ln_total_exp_usd_2011[HH_India$total_exp_usd_2011 == 0] <- 0


### Train and Test Sets ###
# set the seed
set.seed(2)

# create the sample split vector
train_label <- sample.split(HH_India$ac, SplitRatio = 3/5)

# Split the sample and check that the split is balanced
# Train
train_India <- HH_India[train_label, ]
mean(as.numeric(train_India$ac)-1)

# Test
test_India <- HH_India[!train_label, ]
mean(as.numeric(test_India$ac)-1)

# Full Model specification
full.formula <- ac ~ mean_CDD_wb * ln_total_exp_usd_2011 + urban + n_members + sh_under16 + 
                ownership + edu_head_2 + occupation_head + age_head + sex_head + state


###########################
### LOGISTIC REGRESSION ###
###########################

# Fit the logisitic regression
log.fit <- glm(full.formula, data = train_India, family = "binomial"); summary(log.fit)

# Predict the probabilities in the test set
test_India <- test_India %>% filter(state != 31) # we have to drop it to predict because R omitted it (empty category)
log.probs <- predict(log.fit, test_India, type = "response")

# test set ROC curve and AUC
predob <- prediction(log.probs, test_India$ac)
perf <- performance(predob, "tpr", "fpr")

# Figure S2 India - ROC curve
png(paste0(SI_plot_dir, 'Figure_S2_IND.png',sep=''), height=6.75, width=13, units='in', res=1200)

plot(perf, colorize = TRUE, main = "Logistic Regression",
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
dev.off()

# AUC
as.numeric(performance(predob, "auc")@y.values) # 0.89

text(0.53, 0.6, "AUC = ")
text(0.6, 0.6, round(as.numeric(performance(predob, "auc")@y.values), digits = 3))
