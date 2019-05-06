ST <- Sys.time()
library(pacman)
p_load(tidyverse,lubridate,caret,e1071,xgboost,Metrics,modelr,vtreat,magrittr,here)


train <- read_csv(here("100_data_raw-input","train.csv"))
test <- read_csv(here("100_data_raw-input","test.csv"))

#z <- train
#problems(train)

#str(train)
#summary(train)

#train %>% filter(is.na(Employment.Type)) %>% group_by(loan_default) %>% summarize(c= n()) 

#colSums(is.na(train))

train$Date.of.Birth <- dmy(train$Date.of.Birth)
train$DisbursalDate <- dmy(train$DisbursalDate)
train$age <- as.numeric(floor((difftime(train$DisbursalDate, train$Date.of.Birth, units = "days"))/365.25))
train$age <- ifelse(train$age < 0, round(median(train$age)), train$age)
test$Date.of.Birth <- dmy(test$Date.of.Birth)
test$DisbursalDate <- dmy(test$DisbursalDate)
test$age <- as.numeric(floor((difftime(test$DisbursalDate, test$Date.of.Birth, units = "days"))/365.25))
test$age <- ifelse(test$age < 0, round(median(test$age)), test$age)


train <- separate(train, CREDIT.HISTORY.LENGTH, into = c("Year", "Month"))
train$Year <- as.numeric(str_remove(train$Year,"yrs")) #substr(train$Year, -1, 1)
train$Month <- round(as.numeric(str_remove(train$Month,"mon"))/12, digits = 2)
train$CREDIT.HISTORY.LENGTH <- train$Year+train$Month

test <- separate(test, CREDIT.HISTORY.LENGTH, into = c("Year", "Month"))
test$Year <- as.numeric(str_remove(test$Year,"yrs")) #substr(test$Year, -1, 1)
test$Month <- round(as.numeric(str_remove(test$Month,"mon"))/12, digits = 2)
test$CREDIT.HISTORY.LENGTH <- test$Year+test$Month


train$loan_default <- as.factor(train$loan_default)


train <- train %>% mutate(disbursed_pct = round((disbursed_amount/asset_cost)*100, digits = 2),
                          Total_Id_Flag = MobileNo_Avl_Flag + Aadhar_flag + PAN_flag + VoterID_flag + Driving_flag + Passport_flag)
test <- test %>% mutate(disbursed_pct = round((disbursed_amount/asset_cost)*100, digits = 2),
                        Total_Id_Flag = MobileNo_Avl_Flag + Aadhar_flag + PAN_flag + VoterID_flag + Driving_flag + Passport_flag)


train$Risk_Level <- case_when(
  train$PERFORM_CNS.SCORE.DESCRIPTION %in% c("A-Very Low Risk", "B-Very Low Risk", "C-Very Low Risk", "D-Very Low Risk",
                                             "E-Low Risk", "F-Low Risk", "G-Low Risk") ~ "Low Risk",
  train$PERFORM_CNS.SCORE.DESCRIPTION %in% c("H-Medium Risk", "I-Medium Risk") ~ "Medium Risk",
  train$PERFORM_CNS.SCORE.DESCRIPTION %in% c("J-High Risk", "K-High Risk", "L-Very High Risk",
                                             "M-Very High Risk") ~ "High Risk",
  train$PERFORM_CNS.SCORE.DESCRIPTION == "No Bureau History Available" ~ "History Not available",
  TRUE  ~ "Not Scored"
)
#train$Risk_Level <- as.factor(train$Risk_Level)

test$Risk_Level <- case_when(
  test$PERFORM_CNS.SCORE.DESCRIPTION %in% c("A-Very Low Risk", "B-Very Low Risk", "C-Very Low Risk", "D-Very Low Risk",
                                            "E-Low Risk", "F-Low Risk", "G-Low Risk") ~ "Low Risk",
  test$PERFORM_CNS.SCORE.DESCRIPTION %in% c("H-Medium Risk", "I-Medium Risk") ~ "Medium Risk",
  test$PERFORM_CNS.SCORE.DESCRIPTION %in% c("J-High Risk", "K-High Risk", "L-Very High Risk",
                                            "M-Very High Risk") ~ "High Risk",
  test$PERFORM_CNS.SCORE.DESCRIPTION == "No Bureau History Available" ~ "History Not available",
  TRUE  ~ "Not Scored"
)

#test$Risk_Level <- as.factor(test$Risk_Level)

train$Employment.Type <- ifelse(is.na(train$Employment.Type), "Missing", train$Employment.Type)
test$Employment.Type <- ifelse(is.na(test$Employment.Type), "Missing", test$Employment.Type)

#cor(a)
#cor <- data.frame(cor(train[, sapply(train, is.numeric)]))


train_final <- train[, c("disbursed_amount", "disbursed_pct", "branch_id", "manufacturer_id",
                         "State_ID","Employment.Type", "Risk_Level", "PRI.CURRENT.BALANCE","PRIMARY.INSTAL.AMT",
                         "DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS", "CREDIT.HISTORY.LENGTH",
                         "NO.OF_INQUIRIES", "age", "Total_Id_Flag",
                         "loan_default")]


tot <- nrow(train_final)
n_train <- round(0.80*tot)
train_indices <- sample(1:tot, n_train)
train_default <- train_final[train_indices, ]
test_default <- train_final[-train_indices, ]


# XGBoost


vars <- c("disbursed_amount", "disbursed_pct", "branch_id","manufacturer_id", "State_ID",
          "Employment.Type", "Risk_Level", "PRI.CURRENT.BALANCE", "PRIMARY.INSTAL.AMT",
          "DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS", "CREDIT.HISTORY.LENGTH", "NO.OF_INQUIRIES", "age",  "Total_Id_Flag")

treatplan <- designTreatmentsZ(train_default, vars)

newvars <- treatplan %>%  use_series(scoreFrame) %>% filter(code %in% c("clean", "lev")) %>%
  use_series(varName)

train_default.treat <- prepare(treatplan, train_default, varRestriction  = newvars)
test_default.treat <- prepare(treatplan, test_default, varRestriction  = newvars)


train_default$loan_default <- as.integer(train_default$loan_default) - 1

cv <- xgb.cv(data = as.matrix(train_default.treat),
             label = train_default$loan_default,
             nrounds  = 100,
             nfold = 5,
             objective = "binary:logistic",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             gamma = 7,
             verbose = TRUE)

elog <- as.data.frame(cv$evaluation_log)
elog
elog %>% 
  summarize(ntrees.train = which.min(elog$train_error_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(elog$test_error_mean))  # find the index of min(test_rmse_mean)

ntrees <- 13

child_weight <- 0:50
gama <- 0:20 

hyper_grid <- expand.grid(child_weight = child_weight, gama = gama)
hyper_grid
aucs <- c()
for(i in 1:nrow(hyper_grid)){
  xgb_model <- xgboost(data = as.matrix(train_default.treat),
                       label = train_default$loan_default,
                       nrounds  = 39,
                       objective = "binary:logistic",
                       eta = 0.3,
                       max_depth = 6,
                       gamma = hyper_grid$gama[i],
                       min_child_weight = hyper_grid$child_weight[i],
                       verbose = TRUE) 
  
  
  pred <- predict(xgb_model, as.matrix(test_default.treat))
  aucs[i] <- auc(actual = test_default$loan_default, predicted = pred)
  
}

opt_i <- which.max(aucs)

print(hyper_grid[opt_i,])

hyper_grid <- cbind(hyper_grid, aucs)

xgb_model <- xgboost(data = as.matrix(train_default.treat),
                     label = train_default$loan_default,
                     nrounds  = 39,
                     objective = "binary:logistic",
                     eta = 0.3,
                     max_depth = 6,
                     gamma = 3,
                     min_child_weight = 41,
                     verbose = TRUE) 

  
pred <- predict(xgb_model, as.matrix(test_default.treat))
auc(actual = test_default$loan_default, predicted = pred)

head(pred)
pred_labels <- ifelse(pred > 0.15, 1, 0)

confusionMatrix(as.factor(pred_labels), test_default$loan_default)

importance_matrix <- xgb.importance(names(train_default.treat), xgb_model)
xgb.plot.importance(importance_matrix)


# Final Predictions


test_1 <- test[, c("disbursed_amount", "disbursed_pct", "branch_id", "manufacturer_id",
                   "State_ID","Employment.Type", "Risk_Level", "PRI.CURRENT.BALANCE","PRIMARY.INSTAL.AMT",
                   "DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS", "CREDIT.HISTORY.LENGTH",
                   "NO.OF_INQUIRIES", "age", "Total_Id_Flag")]
train_1 <- train[, c("disbursed_amount", "disbursed_pct", "branch_id", "manufacturer_id",
                   "State_ID","Employment.Type", "Risk_Level", "PRI.CURRENT.BALANCE","PRIMARY.INSTAL.AMT",
                   "DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS", "CREDIT.HISTORY.LENGTH",
                   "NO.OF_INQUIRIES", "age", "Total_Id_Flag")]

test_1.treat <- prepare(treatplan, test_1, varRestriction  = newvars)
train_1.treat <- prepare(treatplan, train_1, varRestriction  = newvars)

train$loan_default <- as.integer(train$loan_default) - 1


cv <- xgb.cv(data = as.matrix(train_1.treat),
             label = train$loan_default,
             nrounds  = 100,
             nfold = 5,
             objective = "binary:logistic",
             eta = 0.3,
             max_depth = 6,
             gamma = 3,
             min_child_weight = 41,
             verbose = TRUE)

elog <- as.data.frame(cv$evaluation_log)

elog %>% 
  summarize(ntrees.train = which.min(elog$train_error_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(elog$test_error_mean))  # find the index of min(test_rmse_mean)



xgb_model <- xgboost(data = as.matrix(train_1.treat),
                     label = train$loan_default,
                     nrounds  = 42,
                     objective = "binary:logistic",
                     eta = 0.4,
                     max_depth = 6,
                     gamma = 7,
                     min_child_weight = 35,
                     verbose = TRUE) 

pred <- predict(xgb_model, as.matrix(test_1.treat))


test_export <- cbind(test, pred) 
test_export <- test_export[,c(1,47)]
colnames(test_export)[2] <- "loan_default"
write_excel_csv(test_export, here("120_data_output"
                                  ,paste0("XGB-copied_",
                                          strftime(Sys.time(), format="%Y%m%d_%H%M%S")
                                          ,".csv"))
                , na = 'NA')


summary(pred)
ST
Sys.time()