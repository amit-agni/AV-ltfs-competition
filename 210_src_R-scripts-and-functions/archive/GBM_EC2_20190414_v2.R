#install.packages("pacman")
library(pacman)
p_load(caret,data.table,dplyr,lubridate
       ,here,DescTools,DataExplorer,tidyverse
       ,googledrive,e1071,ggcorrplot,forcats)

options(scipen=999) #remove scientific notation in printing 

####### parallel on windows #######
p_load(doParallel)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(1)
#registerDoParallel(cl)


# turn parallel processing off and run sequentially again:
# registerDoSEQ()

#####################################


gc()

#####################################
# parallel::detectCores()
# 
# p_load(doMC)
# 
# registerDoMC(cores = 4)
# 
# dribble <- drive_find(pattern="train.csv")
# drive_download(as_dribble(dribble),overwrite = TRUE)
# 
# dribble <- drive_find(pattern="test.csv")
# drive_download(as_dribble(dribble),overwrite = TRUE)
# 
# DT_train <- fread(here::here("train.csv"))
# DT_test <- fread(here::here("test.csv"))

#####################################


DT_train <- fread(here::here("100_data_raw-input","train.csv"))
DT_test <- fread(here::here("100_data_raw-input","test.csv"))


nrow(DT_test)
#[1] 112392
prop.table(table(DT_train$loan_default))


DT_train$flag <- "train"
DT_test$flag <- "test"

DT_test$loan_default <- 1

DT <- rbind(DT_train,DT_test)

rm(DT_train)
rm(DT_test)



##################### Basic Cleanup #####################

#to avoid date of births like 2068 instead of 1968
DT$Date.of.Birth <- dplyr::if_else(dmy(DT$Date.of.Birth) > Sys.Date(),
                                   dmy(DT$Date.of.Birth) - years(100),
                                   dmy(DT$Date.of.Birth))

DT$DisbursalDate <- dmy(DT$DisbursalDate)


#Some Employment types are blank
#Set them fo Self Employed (TO DO : Missing value Imputation)
DT[flag=="train",.N,by=.(Employment.Type,loan_default)]
DT[flag=="test",.N,by=.(Employment.Type,loan_default)]
DT[,Employment.Type := ifelse(Employment.Type == "","Self employed",as.character(Employment.Type))]


#drop mobile no, not useful
DT$MobileNo_Avl_Flag <- NULL

#convert into months


DT[,avg.acct.age.months := as.integer(str_extract_all(AVERAGE.ACCT.AGE,"[0-9]+",simplify = TRUE)[,1]) * 12 + as.integer(str_extract_all(AVERAGE.ACCT.AGE,"[0-9]+",simplify = TRUE)[,2])]

DT[,.(AVERAGE.ACCT.AGE,avg.acct.age.months)]


DT[,credit.history.length.months := as.integer(str_extract_all(CREDIT.HISTORY.LENGTH,"[0-9]+",simplify = TRUE)[,1]) * 12 + as.integer(str_extract_all(CREDIT.HISTORY.LENGTH,"[0-9]+",simplify = TRUE)[,2])]

DT[,.(CREDIT.HISTORY.LENGTH,credit.history.length.months)]


DT$AVERAGE.ACCT.AGE <- NULL
DT$CREDIT.HISTORY.LENGTH <- NULL


#remove ID column
DT$UniqueID <- NULL





##################### NO OF ACCOUNTS  #####################
#Convert them to factors

unique(DT$PRI.ACTIVE.ACCTS)
unique(DT$PRI.NO.OF.ACCTS)

group_category(data = DT, feature = "PRI.NO.OF.ACCTS", threshold = 0.1,update = TRUE)
group_category(data = DT, feature = "PRI.ACTIVE.ACCTS", threshold = 0.1,update = TRUE)
group_category(data = DT, feature = "PRI.OVERDUE.ACCTS", threshold = 0.001,update = TRUE)

group_category(data = DT, feature = "SEC.NO.OF.ACCTS", threshold = 0.002,update = TRUE)
group_category(data = DT, feature = "SEC.ACTIVE.ACCTS", threshold = 0.0005,update = TRUE)
group_category(data = DT, feature = "SEC.OVERDUE.ACCTS", threshold = 0.0001,update = TRUE)

group_category(data = DT, feature = "NEW.ACCTS.IN.LAST.SIX.MONTHS", threshold = 0.002,update = TRUE)
group_category(data = DT, feature = "DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS", threshold = 0.0005,update = TRUE)
group_category(data = DT, feature = "NO.OF_INQUIRIES", threshold = 0.005,update = TRUE)










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



##################### DATES  #####################



#source :https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates/1996404
DT$age.at.disbursal <- interval(DT$Date.of.Birth,DT$DisbursalDate) %/% months(1)


#option did not work
#https://stackoverflow.com/questions/10836503/convert-difftime-time-to-years-months-and-days
#DT$age.at.disbursal <- as.POSIXct(c(difftime(DT$DisbursalDate,DT$Date.of.Birth, units = "secs")),origin = DT$Date.of.Birth)

DT[,.(age.at.disbursal,avg.acct.age.months,credit.history.length.months)]

#Evertyhing in 2018 ?
DT$disbursal.year <- year(DT$DisbursalDate)
unique(DT$disbursal.year)
DT$disbursal.year <- NULL

DT$disbursal.month <- month(DT$DisbursalDate)
DT$disbursal.month <- as.factor(as.character(DT$disbursal.month))


##################### NUMERIC to FACTORS  #####################


#Some columns have categorical data and can be converted to factors


#Suppliers with their total loans and default pct
unique(DT$loan_default)
temp <- DT[flag=="train",.N,by=.(supplier_id,loan_default)][
    ,.(supplier_id.tot_loans = sum(N),loan_default,supplier_id.default.pct = N/sum(N)),by = supplier_id][loan_default == 1]

temp[,loan_default:=NULL]

DT <- merge(DT,temp,by="supplier_id",all.x = TRUE)


#Current pin code with their total loans and default pct
temp <- DT[flag=="train",.N,by=.(Current_pincode_ID,loan_default)][
    ,.(Current_pincode_ID.tot_loans = sum(N),loan_default,Current_pincode_ID.default.pct = N/sum(N)),by = Current_pincode_ID][loan_default == 1][order(-Current_pincode_ID.tot_loans)]

temp[,loan_default:=NULL]

DT <- merge(DT,temp,by="Current_pincode_ID",all.x = TRUE)


#Current branch_id with their total loans and default pct
temp <- DT[flag=="train",.N,by=.(branch_id,loan_default)][
    ,.(branch_id.tot_loans = sum(N),loan_default,branch_id.default.pct = N/sum(N)),by = branch_id][loan_default == 1][order(-branch_id.tot_loans)]

temp[,loan_default:=NULL]

DT <- merge(DT,temp,by="branch_id",all.x = TRUE)


#Current Employee_code_ID with their total loans and default pct
temp <- DT[flag=="train",.N,by=.(Employee_code_ID,loan_default)][
    ,.(Employee_code_ID.tot_loans = sum(N),loan_default,Employee_code_ID.default.pct = N/sum(N)),by = Employee_code_ID][loan_default == 1][order(-Employee_code_ID.tot_loans)]

temp[,loan_default:=NULL]

DT <- merge(DT,temp,by="Employee_code_ID",all.x = TRUE)


#Current State_ID with their total loans and default pct
temp <- DT[flag=="train",.N,by=.(State_ID,loan_default)][
    ,.(State_ID.tot_loans = sum(N),loan_default,State_ID.default.pct = N/sum(N)),by = State_ID][loan_default == 1][order(-State_ID.tot_loans)]

temp[,loan_default:=NULL]

DT <- merge(DT,temp,by="State_ID",all.x = TRUE)


#Score description ignore
DT[,.(count = .N,
      minscore = min(PERFORM_CNS.SCORE),
      maxscore = max(PERFORM_CNS.SCORE)), by =PERFORM_CNS.SCORE.DESCRIPTION ][order(minscore)]




##################### Convert Characters to FACTORS  #####################


#characters
temp <- names(which(lapply(DT, is.character)==TRUE))
temp
DT[, (temp) := lapply(.SD, as.factor),.SDcols = temp]


#some additional columns
temp <- c("manufacturer_id","Employment.Type","Aadhar_flag","PAN_flag","VoterID_flag","Driving_flag","Passport_flag")
DT[, (temp) := lapply(.SD, as.factor),.SDcols = temp]



##################### play with the remove the default pct features #####################

#DT$cross_pin_employee <- DT$Current_pincode_ID.default.pct * DT$Employee_code_ID.default.pct

summary(DT$Current_pincode_ID.default.pct)
brks <- c(seq(0,1,0.1))
DT$Current_pincode_ID.default.pct.bin <- fct_explicit_na(cut(DT$Current_pincode_ID.default.pct,
                                                 breaks = brks,
                                                 labels = brks[-1]),na_level = "outlier")
prop.table(table(DT$Current_pincode_ID.default.pct.bin))
prop.table(table(DT$Current_pincode_ID.default.pct.bin,DT$loan_default))

summary(DT$Employee_code_ID.default.pct)
brks <- c(seq(0,1,0.1))
DT$Employee_code_ID.default.pct.bin <- fct_explicit_na(cut(DT$Employee_code_ID.default.pct,
                                                             breaks = brks,
                                                             labels = brks[-1]),na_level = "outlier")
prop.table(table(DT$Employee_code_ID.default.pct.bin))
prop.table(table(DT$Employee_code_ID.default.pct.bin,DT$loan_default))


summary(DT$ltv)
brks <- c(seq(0,100,10))
DT$ltv.bin <- fct_explicit_na(cut(DT$ltv,breaks = brks,labels = brks[-1]),na_level = "outlier")
prop.table(table(DT$ltv.bin))
prop.table(table(DT$ltv.bin,DT$loan_default))


# DT$Current_pincode_ID.default.pct <- NULL
# DT$Employee_code_ID.default.pct <- NULL
# DT$ltv <- NULL



##################### DROP FEATURES  #####################

#factor features
temp <- c("branch_id","supplier_id","Current_pincode_ID","State_ID","Employee_code_ID")

DT[,(temp) := NULL]

#date features
temp <- c("Date.of.Birth","DisbursalDate")
DT[,(temp) := NULL]


#drop PERFORM_CNS.SCORE as it captured in the description

DT$PERFORM_CNS.SCORE <- NULL

#Remove features with only 1 factor level
temp <- names(which(lapply(DT,function(x) length(levels(x)))==1))
DT[,(temp) := NULL]


##################### BOX COX  #####################
#Box Cox transformation of numeric columns

temp <-names(which(lapply(DT,is.numeric)==TRUE))

preProcValues <- preProcess(DT[,(temp),with = FALSE], method = "BoxCox")
DT_tran <- predict(preProcValues, DT[,(temp),with = FALSE])

#remove the columns from DT and cbind the transformed columns
DT[,(temp) := NULL]

DT <- cbind(DT,DT_tran)

rm(DT_tran)
rm(preProcValues)
rm(temp)

str(DT)

################### Modeling Prep  #####################

#XGB gives this error 
#Error in setinfo.xgb.DMatrix(dmat, names(p), p[[1]]) : The length of labels must equal to the number of rows in the input data

DT[is.na(DT)] <- 0
#index <- createDataPartition(y=DT[flag =="train",]$loan_default, p=1, list=FALSE) 
#train <-DT[flag =="train"][index,]

train <- DT[flag =="train"]
train$flag <- NULL

#test <-DT[flag =="train",][-index,]
#test$flag <- NULL


#rm(index)
gc(reset=TRUE)


str(train)
####################################################
###############      MODELIING     #################
####################################################


### xgb without caret ##
#source : https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/


p_load(xgboost)
labels <- train$loan_default
# new_tr <- model.matrix(~.+0,data = train[,-c("loan_default"),with=F]) 
p_load(Matrix)
new_tr <- sparse.model.matrix(loan_default~.-1,data = train) 
#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 

rm(new_tr)



# pmap(list(
#     x = seq_len(3),
#     y = c("thing", "items", "objects"),
#     z = c(".", "?", "!")
# ),
# .f = temp)
# temp <- function(x, y, z) { paste(x, paste0(y, z)) }



# df_grid <- data.frame(expand.grid(eta = c(1,0.05,0.001)
#                                   
#                                   ,max_depth = c(6,8,10,15)
#                                   ,min_child_weight = c(3,7)
#                                   
#                                   ,subsample = c(1,0.2,0.6,0.8)
#                                   ,colsample_bytree = c(1,0.2,0.6,0.8)
#                       
#                                   ,gamma=c(0,10)
#                                   ,alpha = c(0,10)
#                                   ,lambda = c(1,10)
#             
#                                   ,max_delta_step = c(0,10)
#                                   ,scale_pos_weight = c(0,10)))



x= list(eta = c(1,0.05)
        
        ,max_depth = c(6,8,10)
        ,min_child_weight = c(2,7)
        
        ,subsample = c(1,0.2,0.7)
        ,colsample_bytree = c(1,0.2,0.7)
        
        ,gamma=c(0,10)
        ,alpha = c(0,10)
        ,lambda = c(1,10)
        
        ,max_delta_step = c(0,10)
        ,scale_pos_weight = c(0,10))

rm(df_grid)
df_grid <- do.call(CJ, args = x)

df_grid


set.seed(1)
df_grid <- sample_n(df_grid,100)
ST <- Sys.time()
output <- pmap(.l=as.list(df_grid),.f=fn_grid)
out_df<-data.frame()
for(i in 1:nrow(df_grid)) {
    out_df <- rbind(out_df,output[i][[1]])
}
ST 
Sys.time()

i <- 1

fn_grid <- function(eta
                    ,max_depth
                    ,min_child_weight
                    ,subsample
                    ,colsample_bytree
                    ,gamma
                    ,alpha 
                    ,lambda
                    ,max_delta_step
                    ,scale_pos_weight){
                    
                    gc()
                    print(paste0("Iteration no : ", i))
                    assign("i",i+1)
                    params <- list(booster = "gbtree"
                                   , objective = "binary:logistic"
                                   , eval_metric = "error"
                                   
                                   , eta=eta
                                   , gamma=gamma
                                   , alpha = alpha
                                   , lambda = lambda
                                   , max_delta_step = max_delta_step
                                   , max_depth=max_depth
                                   , min_child_weight=min_child_weight
                                   , subsample=subsample
                                   , colsample_bytree=colsample_bytree
                                   , scale_pos_weight = scale_pos_weight
                                    )
                    
                    
                    xgbcv <- xgb.cv( params = params
                                     , data = dtrain
                                     , nrounds = 100
                                     , nfold = 5
                                     , showsd = T
                                     , stratified = T
                                     , print_every_n = 1
                                     , early_stopping_rounds = 20
                                     , maximize = F
                                     , nthread = 1)
                    
                    
                    data.frame(eta=eta
                        ,gamma=gamma
                        ,alpha = alpha
                        ,lambda = lambda
                        ,max_delta_step = max_delta_step
                        ,max_depth=max_depth
                        ,min_child_weight=min_child_weight
                        ,subsample=subsample
                        ,colsample_bytree=colsample_bytree
                        ,scale_pos_weight = scale_pos_weight
                        ,best_iteration = xgbcv$best_iteration
                        ,mean_train_err=mean(xgbcv$evaluation_log$train_error_mean)
                        ,mean_test_err=mean(xgbcv$evaluation_log$test_error_mean)
                    )
    
                    
}


library(cutlery)
write2clip(out_df)

params <- list(booster = "gbtree"
               , objective = "binary:logistic"
               , eval_metric = "error"
               
               , eta=0.5
    
               # , gamma=0
               # , alpha = 10
               # , lambda = 10
           
               # , max_delta_step = 10

               , max_depth=i
               , min_child_weight=0


               , subsample=0.5
               , colsample_bytree=0.5

               # , scale_pos_weight = 1
               
               )


xgbcv <- xgb.cv( params = params
                 , data = dtrain
                 , nrounds = 10
                 , nfold = 5
                 , showsd = T
                 , stratified = T
                 , print_every_n = 1
                 , early_stopping_rounds = 5
                 , maximize = F)

#XGB recognizes that many of your features are not important and didn't use them in the process of building decision trees. 
#You can force XGB to use all of them by increasing max tree depth setting, but you are overfitting the data this way.

#https://www.kaggle.com/c/santander-customer-satisfaction/discussion/20662
#https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
#https://xgboost.readthedocs.io/en/latest/parameter.html


xgbcv$best_iteration



#first default - model training
xgb1 <- xgb.train (params = params
                   , data = dtrain
                   , nrounds = 25
#                   , watchlist = list(val=dtest,train=dtrain)
                   , print_every_n = 5
#                   , early_stopping_rounds = 10
                   , maximize = F )


importance <- xgb.importance(feature_names = new_tr@Dimnames[[2]], model = xgb1)
head(importance)
# 
# Feature       Gain      Cover  Frequency
# 1:             cross_pin_employee 0.49324051 0.19943348 0.09062316
# 2:                            ltv 0.10645202 0.10745769 0.09455475
# 3:        supplier_id.default.pct 0.05644952 0.09365400 0.08177708
# 4: Current_pincode_ID.default.pct 0.04151609 0.04423804 0.05307647
# 5:               age.at.disbursal 0.02446379 0.03502194 0.06624730
# 6:   credit.history.length.months 0.02002686 0.02181067 0.04658935


####################################################
##########           Submission           ##########
####################################################

test <-DT[flag =="test",]
test$flag <- NULL
test$loan_default <- NULL

#new_test <- model.matrix(~.+0,data = test) 
new_test <-   sparse.model.matrix(~.-1,data = test) 

predicted <- predict (xgb1,newdata = new_test)
predicted
prob_pred <- ifelse(predicted > 0.35,1,0)
prop.table(table(prob_pred))
get_unique_ids <- fread(here::here("100_data_raw-input","test.csv"))

submit_kaggle <- as.data.frame(cbind(get_unique_ids$UniqueID,prob_pred))
#submit_kaggle <- as.data.frame(cbind(get_unique_ids$UniqueID,predicted))
colnames(submit_kaggle) <- c("UniqueID","loan_default")
write.csv(submit_kaggle,here(paste0("EC2_GBM_",Sys.Date(),"_4.csv")), row.names = FALSE)









