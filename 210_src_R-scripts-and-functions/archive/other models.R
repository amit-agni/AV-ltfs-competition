############## this code was removed ############## 
############## ############## ############## ############## 





##################### LOAN AMOUNTS DIFFERENT APPROACH 16Apr19 #####################

brks <- seq(0,100000,10000)
DT$disbursed_amount.bin <-  fct_explicit_na(cut(DT$disbursed_amount,
                                                breaks = brks,
                                                labels = brks[-1]),na_level = "outlier")
prop.table(table(DT$disbursed_amount.bin))



summary(DT$asset_cost)
DT$asset_cost.bin <-  fct_explicit_na(cut(DT$asset_cost,
                                          breaks = brks,
                                          labels = brks[-1]),na_level = "outlier")
prop.table(table(DT$asset_cost.bin))





## loan amounts ##
# PRI.DISBURSED.AMOUNT has high correlation with SANCTIONED
# Secondary amounts correlation
# SEC.CURRENT.BALANCE is all 0, hence ignore

correl <- cor(DT[,.(PRI.SANCTIONED.AMOUNT,PRI.DISBURSED.AMOUNT,PRI.CURRENT.BALANCE,
                    SEC.SANCTIONED.AMOUNT,SEC.DISBURSED.AMOUNT,SEC.CURRENT.BALANCE,
                    PRIMARY.INSTAL.AMT,SEC.INSTAL.AMT
)])
ggcorrplot::ggcorrplot(correl,lab=TRUE)




summary(DT$PRI.SANCTIONED.AMOUNT)

brks <- c(min(DT$PRI.SANCTIONED.AMOUNT),seq(0,10000,1000),max(DT$PRI.SANCTIONED.AMOUNT))

DT$PRI.SANCTIONED.AMOUNT.bin <- fct_explicit_na(cut(DT$PRI.SANCTIONED.AMOUNT,
                                                    breaks = brks,
                                                    labels = brks[-1]),na_level = "outlier")
prop.table(table(DT$PRI.SANCTIONED.AMOUNT.bin))
prop.table(table(DT$PRI.SANCTIONED.AMOUNT.bin,DT$loan_default))



summary(DT$PRI.CURRENT.BALANCE)
brks <- c(min(DT$PRI.CURRENT.BALANCE),seq(-1000,100000,10000), max(DT$PRI.CURRENT.BALANCE))
DT$PRI.CURRENT.BALANCE.bin <- fct_explicit_na(cut(DT$PRI.CURRENT.BALANCE,
                                                  breaks = brks,
                                                  labels = brks[-1]),na_level = "outlier")
prop.table(table(DT$PRI.CURRENT.BALANCE.bin))
prop.table(table(DT$PRI.CURRENT.BALANCE.bin,DT$loan_default))


summary(DT$SEC.SANCTIONED.AMOUNT)
brks <- c(seq(min(DT$SEC.SANCTIONED.AMOUNT),1000000,100000),max(DT$SEC.SANCTIONED.AMOUNT))
DT$SEC.SANCTIONED.AMOUNT.bin <- fct_explicit_na(cut(DT$SEC.SANCTIONED.AMOUNT,
                                                    breaks = brks,
                                                    labels = brks[-1]) ,na_level = "outlier")

prop.table(table(DT$SEC.SANCTIONED.AMOUNT.bin))
prop.table(table(DT$SEC.SANCTIONED.AMOUNT.bin,DT$loan_default))



#installment amounts

summary(DT$PRIMARY.INSTAL.AMT)
brks <- c(seq(0,10000,1000),max(DT$PRIMARY.INSTAL.AMT))
DT$PRIMARY.INSTAL.AMT.bin <- fct_explicit_na(cut(DT$PRIMARY.INSTAL.AMT,
                                                 breaks = brks,
                                                 labels = brks[-1]),na_level = "outlier")
prop.table(table(DT$PRIMARY.INSTAL.AMT.bin))
prop.table(table(DT$PRIMARY.INSTAL.AMT.bin,DT$loan_default))



summary(DT$SEC.INSTAL.AMT)
brks <- c(seq(0,10000,1000),max(DT$SEC.INSTAL.AMT))

DT$SEC.INSTAL.AMT.bin <- fct_explicit_na(cut(DT$SEC.INSTAL.AMT,
                                             breaks = brks,
                                             labels = brks[-1]),na_level = "outlier")
prop.table(table(DT$SEC.INSTAL.AMT.bin))
prop.table(table(DT$SEC.INSTAL.AMT.bin,DT$loan_default))


###### Zero and negative amount flag ####
temp <- c("disbursed_amount","asset_cost","PRI.SANCTIONED.AMOUNT","PRI.DISBURSED.AMOUNT"
          ,"PRI.CURRENT.BALANCE","SEC.SANCTIONED.AMOUNT","SEC.DISBURSED.AMOUNT","SEC.CURRENT.BALANCE",
          "PRIMARY.INSTAL.AMT","SEC.INSTAL.AMT")
temp

DT[,(paste0(temp,"_zero")):= lapply(.SD,function(x) as.factor(if_else(x==0,1,0))), .SDcols = temp]

DT[,(paste0(temp,"_negative")):= lapply(.SD,function(x) as.factor(if_else(x<0,1,0))), .SDcols = temp]


# remove the converted 

DT$disbursed_amount <- NULL
DT$asset_cost <-NULL

DT$PRI.SANCTIONED.AMOUNT <- NULL
DT$PRI.DISBURSED.AMOUNT <- NULL
DT$PRI.CURRENT.BALANCE <- NULL


DT$SEC.SANCTIONED.AMOUNT <- NULL
DT$SEC.DISBURSED.AMOUNT <- NULL
DT$SEC.CURRENT.BALANCE <- NULL

DT$PRIMARY.INSTAL.AMT <- NULL
DT$SEC.INSTAL.AMT <-NULL

summary(DT$ltv)
brks <- c(seq(0,100,10))
DT$ltv.bin <- fct_explicit_na(cut(DT$ltv,breaks = brks,labels = brks[-1]),na_level = "outlier")
prop.table(table(DT$ltv.bin))
prop.table(table(DT$ltv.bin,DT$loan_default))








##################### Convert Characters to FACTORS  #####################


#characters
temp <- names(which(lapply(DT, is.character)==TRUE))
temp
DT[, (temp) := lapply(.SD, as.factor),.SDcols = temp]





##################### DROP FEATURES  #####################




#drop PERFORM_CNS.SCORE as it captured in the description

DT$PERFORM_CNS.SCORE <- NULL

#Remove features with only 1 factor level
temp <- names(which(lapply(DT,function(x) length(levels(x)))==1))
DT[,(temp) := NULL]






############## ############## ############## ############## 
############## ############## ############## ############## 



###################
This is modularised

#Current branch_id with their total loans and default pct
temp <- DT[flag=="train",.(.N,disbursed_amount=sum(disbursed_amount)),by=.(branch_id,loan_default)][
    ,.(loan_default,
       branch_id.tot_loans = sum(N),
       tot_disbursed_amount =sum(disbursed_amount),
       avg_loan_amount=sum(disbursed_amount)/sum(N),
       branch_id.default.pct = N/sum(N)),
    by = branch_id][loan_default == 1][order(-branch_id.tot_loans)]

temp[,branch_id.default_rate := cut(temp$branch_id.default.pct, breaks=c(quantile(temp$branch_id.default.pct, probs = seq(0, 1, by = 1/3))), 
                                    labels=c("Low","Med","High"),include.lowest = TRUE)]

temp[,branch_id.volume := cut(temp$branch_id.tot_loans, breaks=c(quantile(temp$branch_id.tot_loans, probs = seq(0, 1, by = 1/3))), 
                              labels=c("Low","Med","High"),include.lowest = TRUE)]

temp[,branch_id.avg_loan_amount := cut(temp$avg_loan_amount, breaks=c(quantile(temp$avg_loan_amount, probs = seq(0, 1, by = 1/3))), 
                                       labels=c("Low","Med","High"),include.lowest = TRUE)]

temp[,`:=`(loan_default=NULL,branch_id.tot_loans=NULL,tot_disbursed_amount=NULL,avg_loan_amount=NULL,branch_id.default.pct=NULL)]




##################### LOAN AMOUNTS #####################

#bin amounts has huge outliers but below  may not be the right approach

DT$PRI.SANCTIONED.AMOUNT <- dplyr::if_else(DT$PRI.SANCTIONED.AMOUNT <= 0,0,log(DT$PRI.SANCTIONED.AMOUNT))
DT$PRI.DISBURSED.AMOUNT <- dplyr::if_else(DT$PRI.DISBURSED.AMOUNT <= 0,0,log(DT$PRI.DISBURSED.AMOUNT))
DT$PRI.CURRENT.BALANCE <- dplyr::if_else(DT$PRI.CURRENT.BALANCE <= 0,0,log(DT$PRI.CURRENT.BALANCE))

DT$SEC.SANCTIONED.AMOUNT <- dplyr::if_else(DT$SEC.SANCTIONED.AMOUNT <= 0,0,log(DT$SEC.SANCTIONED.AMOUNT))
DT$SEC.DISBURSED.AMOUNT <- dplyr::if_else(DT$SEC.DISBURSED.AMOUNT <= 0,0,log(DT$SEC.DISBURSED.AMOUNT))
DT$SEC.CURRENT.BALANCE <- dplyr::if_else(DT$SEC.CURRENT.BALANCE <= 0,0,log(DT$SEC.CURRENT.BALANCE))


DT$PRI.SANCTIONED.AMOUNT.quantiles <- cut(DT$PRI.SANCTIONED.AMOUNT,
                                          breaks = quantile(DT[PRI.SANCTIONED.AMOUNT >0,]$PRI.SANCTIONED.AMOUNT, probs = c(0,0.25,0.5,0.75,1))
                                          ,labels = c("Q1","Q2","Q3","Q4"))
DT[,PRI.SANCTIONED.AMOUNT.quantiles := if_else(is.na(PRI.SANCTIONED.AMOUNT.quantiles),"Q0",as.character(PRI.SANCTIONED.AMOUNT.quantiles))]
table(DT$PRI.SANCTIONED.AMOUNT.quantiles)


DT$PRI.DISBURSED.AMOUNT.quantiles <- cut(DT$PRI.DISBURSED.AMOUNT,
                                         breaks = quantile(DT[PRI.DISBURSED.AMOUNT >0,]$PRI.DISBURSED.AMOUNT, probs = c(0,0.25,0.5,0.75,1))
                                         ,labels = c("Q1","Q2","Q3","Q4"))
DT[,PRI.DISBURSED.AMOUNT.quantiles := if_else(is.na(PRI.DISBURSED.AMOUNT.quantiles),"Q0",as.character(PRI.DISBURSED.AMOUNT.quantiles))]
table(DT$PRI.DISBURSED.AMOUNT.quantiles)

DT$PRI.CURRENT.BALANCE.quantiles <- cut(DT$PRI.CURRENT.BALANCE,
                                        breaks = quantile(DT[PRI.CURRENT.BALANCE >0,]$PRI.CURRENT.BALANCE, probs = c(0,0.25,0.5,0.75,1))
                                        ,labels = c("Q1","Q2","Q3","Q4"))
DT[,PRI.CURRENT.BALANCE.quantiles := if_else(is.na(PRI.CURRENT.BALANCE.quantiles),"Q0",as.character(PRI.CURRENT.BALANCE.quantiles))]
table(DT$PRI.CURRENT.BALANCE.quantiles)



DT$SEC.SANCTIONED.AMOUNT.quantiles <- cut(DT$SEC.SANCTIONED.AMOUNT,
                                          breaks = quantile(DT[SEC.SANCTIONED.AMOUNT >0,]$SEC.SANCTIONED.AMOUNT, probs = c(0,0.25,0.5,0.75,1))
                                          ,labels = c("Q1","Q2","Q3","Q4"))
DT[,SEC.SANCTIONED.AMOUNT.quantiles := if_else(is.na(SEC.SANCTIONED.AMOUNT.quantiles),"Q0",as.character(SEC.SANCTIONED.AMOUNT.quantiles))]
table(DT$SEC.SANCTIONED.AMOUNT.quantiles)


DT$SEC.DISBURSED.AMOUNT.quantiles <- cut(DT$SEC.DISBURSED.AMOUNT,
                                         breaks = quantile(DT[SEC.DISBURSED.AMOUNT >0,]$SEC.DISBURSED.AMOUNT, probs = c(0,0.25,0.5,0.75,1))
                                         ,labels = c("Q1","Q2","Q3","Q4"))
DT[,SEC.DISBURSED.AMOUNT.quantiles := if_else(is.na(SEC.DISBURSED.AMOUNT.quantiles),"Q0",as.character(SEC.DISBURSED.AMOUNT.quantiles))]
table(DT$SEC.DISBURSED.AMOUNT.quantiles)

DT$SEC.CURRENT.BALANCE.quantiles <- cut(DT$SEC.CURRENT.BALANCE,
                                        breaks = quantile(DT[SEC.CURRENT.BALANCE >0,]$SEC.CURRENT.BALANCE, probs = c(0,0.25,0.5,0.75,1))
                                        ,labels = c("Q1","Q2","Q3","Q4"))
DT[,SEC.CURRENT.BALANCE.quantiles := if_else(is.na(SEC.CURRENT.BALANCE.quantiles),"Q0",as.character(SEC.CURRENT.BALANCE.quantiles))]
table(DT$SEC.CURRENT.BALANCE.quantiles)




DT$PRIMARY.INSTAL.AMT.quantiles <- cut(DT$PRIMARY.INSTAL.AMT,
                                       breaks = quantile(DT[PRIMARY.INSTAL.AMT >0,]$PRIMARY.INSTAL.AMT, probs = c(0,0.25,0.5,0.75,1))
                                       ,labels = c("Q1","Q2","Q3","Q4"))
DT[,PRIMARY.INSTAL.AMT.quantiles := if_else(is.na(PRIMARY.INSTAL.AMT.quantiles),"Q0",as.character(PRIMARY.INSTAL.AMT.quantiles))]
table(DT$PRIMARY.INSTAL.AMT.quantiles)

DT$SEC.INSTAL.AMT.quantiles <- cut(DT$SEC.INSTAL.AMT,
                                   breaks = quantile(DT[SEC.INSTAL.AMT >0,]$SEC.INSTAL.AMT, probs = c(0,0.25,0.5,0.75,1))
                                   ,labels = c("Q1","Q2","Q3","Q4"))
DT[,SEC.INSTAL.AMT.quantiles := if_else(is.na(SEC.INSTAL.AMT.quantiles),"Q0",as.character(SEC.INSTAL.AMT.quantiles))]
table(DT$SEC.INSTAL.AMT.quantiles)







xgb <- xgboost(data = data.matrix(sparse_matrix[,-1]), 
               label = loan_default, 
               eta = 0.01,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)





####################################################
##########           Submission           ##########
####################################################


nrow(DT[flag == "test"])


test_data <- DT[flag =="test",]
test_data$flag <- NULL
test_data$loan_default <- NULL

predicted<-predict.train(object=model,newdata = test_data ,type="prob")
predicted

prob_pred <- ifelse(predicted$Yes > 0.5,1,0)

table(prob_pred)

get_unique_ids <- fread(here::here("100_data_raw-input","test.csv"))

dim(get_unique_ids)


submit_kaggle <- as.data.frame(cbind(get_unique_ids$UniqueID,prob_pred))

colnames(submit_kaggle) <- c("UniqueID","loan_default")
write.csv(submit_kaggle,here(paste0("EC2_GBM_",Sys.Date(),".csv")), row.names = FALSE)





################### GBM using caret  #####################


control <- trainControl(method="repeatedcv", number=5, repeats=3,classProbs = TRUE)
control <- trainControl(classProbs = TRUE)

metric <- "Kappa"
#preproc <-  c("center", "scale")

#defautls GBM
# n.trees = 100
# bag.fraction = 0.5
# n.minobsinnode = 10
# interaction.depth = 1
# shrinkage = 0.1
# train.fraction = 1
# cv.folds = 0


grid <-  expand.grid(n.trees = c(100,500)
                     ,n.minobsinnode = 1
                     ,interaction.depth = 10
                     ,shrinkage = c(0.1,0.01)
                     ,cv.folds = 5
)
Sys.time()
model <- train(loan_default ~ ., data = train
               ,method = "gbm" 
               ,trControl = control
               ## This last option is actually one
               ## for gbm() that passes throug
               ,metric=metric
               #                ,tuneGrid = grid
               ,allowParallel = TRUE
               ,verbose = FALSE)

model

warnings()


########################################################
################### Random Forest  #####################
########################################################


DT$loan_default <- factor(DT$loan_default,labels = c("No","Yes"))

# Algorithm Tune (tuneRF)
# Execute the tuning process
p_load(randomForest)
set.seed(1)              
res <- tuneRF(x = subset(train, select = -loan_default),
              y = train$loan_default,
              ntreeTry = 500)

print(res)

# Find the mtry value that minimizes OOB Error
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]

print(mtry_opt)


# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(3, ncol(train) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- round(nrow(train) * c(0.7, 0.8))

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()



# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
    
    
    
    # Train a Random Forest model
    model <- randomForest(formula = loan_default ~ ., 
                          data = train,
                          
                          mtry = hyper_grid$mtry[i],
                          nodesize = hyper_grid$nodesize[i],
                          sampsize = hyper_grid$sampsize[i])
    
    # Store OOB error for the model                      
    oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])




##########################################################
###################         xgb      #####################
##########################################################

control <- trainControl(method="repeatedcv", number=5, repeats=2)
metric <- "Kappa"

parametersGrid <-  expand.grid(eta = c(0.01,0.001), 
                               #colsample_bytree=c(0.1,0.9),
                               max_depth=c(3,6),
                               nrounds=c(100,500),
                               gamma=5,
                               min_child_weight=c(1,2),
                               #subsample = c(0.1,0.9),
                               eval_metric = "error")


dummies <- dummyVars(loan_default ~ ., data = train)
train_dummy <- as.data.frame(predict(dummies,newdata = train))


str(train_dummy)

fit.xg <- train(y =factor(train$loan_default) 
                ,x = train_dummy
                ,nthread = 1
                ,method = "xgbTree"
                ,eta = 1
                ,max_depth = 2)




fit.xg <- train(loan_default~.
                ,data = train
                ,method = "xgbTree"
                #                trControl = control,
                #                metric = metric,
                ,allowParallel = TRUE
                ,params=parametersGrid)


fit.xg <- train(y = factor(train$loan_default)
                ,x = dtrain
                ,method = "xgbTree"
                #                trControl = control,
                #                metric = metric,)
                
                ,allowParallel = TRUE)

                ,params=parametersGrid)





