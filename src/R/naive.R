#=========================================
#  A Naive Approach for Data Processing
#=========================================

# Clear work space: 
rm(list = ls())

# Load data: 
library(tidyverse)

df_train <- read_csv("data/train.csv")

df_train %>% mutate_if(is.character, function(x) {str_to_upper(x)}) -> df_train

df_test <- read_csv("data/test.csv")

df_test %>% 
  mutate_if(is.character, function(x) {str_to_upper(x)}) %>% 
  mutate(FIELD_36 = as.logical(FIELD_36)) -> df_test
df_test

# Conduct binning variables: 
library(scorecard)

# Generates optimal binning for all variables/features: 
bins_var <- woebin(df_train %>% select(-id), y = "label", no_cores = 4, positive = "label|1")
# bins_var <- woebin(df_train %>% select(-id, -province, -district, -maCv, -FIELD_7), y = "label", no_cores = 8, positive = "label|1")

# IV for variables/features: 

do.call("rbind", bins_var) %>% 
  as.data.frame() %>% 
  filter(!duplicated(variable)) %>% 
  rename(iv_var = total_iv) %>% 
  arrange(iv_var) %>% 
  mutate(variable = factor(variable, levels = variable)) -> iv_values

# Features have IV >= 0: 

iv_values %>% 
  filter(iv_var >= 0) %>% 
  pull(variable) %>% 
  as.character() -> var_IV_10

# Conduct data transformation based on IV/WoE and filter features with IV > 0.1: 

train_woe <- woebin_ply(df_train %>% select(-id), bins_var) %>% 
  as.data.frame() %>% 
  select(c("label", paste0(var_IV_10, "_", "woe")))


# Data transformation for actual test data: 

test_woe <- woebin_ply(df_test %>% select(-id), bins_var) %>% 
  as.data.frame() %>% 
  select(paste0(var_IV_10, "_", "woe"))

# A function imputes NA observations: 

replace_na_categorical <- function(x, y) {
  
  y %>% 
    table() %>% 
    as.data.frame() %>% 
    arrange(-Freq) -> my_df
  
  n_obs <- sum(my_df$Freq)
  
  pop <- my_df$. %>% as.character() %>% as.numeric()
  
  set.seed(29)
  
  x[is.na(x)] <- sample(pop, sum(is.na(x)), replace = TRUE, prob = my_df$Freq)
  
  return(x)
}

# Use the function: 

test_woe %>% 
  mutate(FIELD_13_woe = replace_na_categorical(FIELD_13_woe, train_woe$FIELD_13_woe)) %>% 
  mutate(maCv_woe = replace_na_categorical(maCv_woe, train_woe$maCv_woe)) %>%
  mutate(district_woe = replace_na_categorical(district_woe, train_woe$district_woe)) %>%
  mutate(FIELD_7_woe = replace_na_categorical(FIELD_7_woe, train_woe$FIELD_7_woe)) %>%
  mutate(FIELD_41_woe = replace_na_categorical(FIELD_41_woe, train_woe$FIELD_41_woe)) %>% 
  mutate(FIELD_10_woe = replace_na_categorical(FIELD_10_woe, train_woe$FIELD_10_woe)) %>% 
  mutate(FIELD_39_woe = replace_na_categorical(FIELD_39_woe, train_woe$FIELD_39_woe)) %>% 
  mutate(FIELD_11_woe = replace_na_categorical(FIELD_11_woe, train_woe$FIELD_11_woe)) %>% 
  mutate(FIELD_9_woe = replace_na_categorical(FIELD_9_woe, train_woe$FIELD_9_woe)) %>% 
  mutate(FIELD_12_woe = replace_na_categorical(FIELD_12_woe, train_woe$FIELD_12_woe)) -> test_woe_imputed


#======================================================
# Attempt 4: Default Random Forest with Scaled Data
#======================================================

# For convinience, convert binary target variable to factor: 

train_woe %>% 
  mutate(label = case_when(label == 1 ~ "Bad", TRUE ~ "Good")) %>% 
  mutate(label = as.factor(label)) -> df_forGBM 

# Scale our data: 

df_forGBM %>% 
  mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}) -> df_forGBM_Scaled

test_woe_imputed %>% 
  mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}) -> df_test_Scaled

# Train Random Forest: 
library(ranger)
library(caret)
library(doParallel)
library(data.table)
RF_default <- ranger(label ~ ., data = df_forGBM_Scaled, probability = TRUE, importance = "impurity_corrected")
m_importance <- importance(RF_default)
# RF_default <- train(label ~ ., data = df_forGBM_Scaled,  method = "ranger", probability = TRUE, allowParallel = TRUE, nthread = 1)
# varImp(RF_default)

prDf = data.table(name = names(m_importance), rank = m_importance)
prDf

write.csv(prDf, file = file.path("out", "feature_importance.csv"), row.names = FALSE)


# Use the RF Classifier for predicting PD (Probability of Default): 
pd_sub_RF <- predict(RF_default, df_test_Scaled, type = "response")

# Save results for submission: 
pd_sub_RF$predictions %>% as.data.frame() %>% pull(Bad) -> pd_sub_RF
df_sub <- data.frame(id = 30000:49999, label = pd_sub_RF)
write_csv(df_sub, "submission_RandomForest_ScaledData.csv")
