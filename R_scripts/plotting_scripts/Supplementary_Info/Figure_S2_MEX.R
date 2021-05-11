
####### Replication code for Fig. S2 - Validation tests using Area under the ROC curve ####### 

## Pavanello et al. (2021): "Air-Conditioning and the Adaptation Cooling Deficit in Emerging Economies"
## (DOI of paper to be included upon acceptance)

## This R.script makes logistic regression applied to each country survey dataset we build, 
## determining how well the model is able to classify household who have spent in air conditioning
## for their primary residence. This is good for proving reviewers our prediction are NOT CASUAL. 
## Here we focus on MEXICO and last wave ENIGH 2016


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

# Choose Mexico
HH_Mexico <- HH_4C %>% filter(country == "Mexico")
HH_Mexico <- HH_Mexico %>% filter(year == 2016)

# Arrange some vars
HH_Mexico$state <- as.factor(HH_Mexico$state)
HH_Mexico$urban <- as.factor(HH_Mexico$urban)
HH_Mexico$ac <- as.factor(HH_Mexico$ac)
HH_Mexico$fan <- as.factor(HH_Mexico$fan)
HH_Mexico$refrigerator <- as.factor(HH_Mexico$refrigerator)
HH_Mexico$ownership_d <- as.factor(HH_Mexico$ownership)
HH_Mexico$edu_head_2 <- as.factor(HH_Mexico$edu_head_2)
HH_Mexico$sex_head <- as.factor(HH_Mexico$sex_head)
HH_Mexico$housing_index_lab <- as.factor(HH_Mexico$housing_index_lab)
HH_Mexico$occupation_head <- as.factor(HH_Mexico$occupation_head)

# Only those with not missing AC
HH_Mexico <- HH_Mexico[complete.cases(HH_Mexico$ac), ]

# Only who has electricity access
HH_Mexico <- HH_Mexico %>% filter(ely_access == 1)

# log total exp
HH_Mexico$ln_total_exp_usd_2011 <- log(HH_Mexico$total_exp_usd_2011)
HH_Mexico$ln_total_exp_usd_2011[HH_Mexico$total_exp_usd_2011 == 0] <- 0


### Train and Test Sets ###
# set the seed
set.seed(2)

# create the sample split vector
train_label <- sample.split(HH_Mexico$ac, SplitRatio = 3/5)

# Split the sample and check that the split is balanced
# Train
train_Mexico <- HH_Mexico[train_label, ]
mean(as.numeric(train_Mexico$ac)-1)

# Test
test_Mexico <- HH_Mexico[!train_label, ]
mean(as.numeric(test_Mexico$ac)-1)

# Full Model specification
full.formula <- ac ~ mean_CDD_wb * ln_total_exp_usd_2011 + urban + n_members + sh_under16 + housing_index_lab +
                     ownership_d + edu_head_2 + occupation_head + age_head + sex_head + state


###########################
### LOGISTIC REGRESSION ###
###########################

# Fit the logisitic regression
log.fit <- glm(full.formula, data = train_Mexico, family = "binomial"); summary(log.fit)

# Predict the probabilities in the test set
log.probs <- predict(log.fit, test_Mexico, type = "response")

# test set ROC curve and AUC
predob <- prediction(log.probs, test_Mexico$ac)
perf <- performance(predob, "tpr", "fpr")

# Figure S2 Mexico - ROC curve
png(paste0(SI_plot_dir, 'Figure_S2_MEX.png',sep=''), height=6.75, width=13, units='in', res=1200)

plot(perf, colorize = TRUE, main = "Logistic Regression",
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
dev.off()

# AUC
as.numeric(performance(predob, "auc")@y.values) # 0.92

text(0.53, 0.6, "AUC = ")
text(0.6, 0.6, round(as.numeric(performance(predob, "auc")@y.values), digits = 3))
