#install.packages("pacman")
library(pacman)
p_load(caret,data.table,dplyr,lubridate
       ,here,DescTools,DataExplorer,tidyverse
       ,googledrive,e1071,ggcorrplot,forcats,xgboost,DiagrammeR,ROCR)

options(scipen=999) #remove scientific notation in printing 

####### parallel on windows #######
p_load(doParallel)
cl <- makeCluster(detectCores(), type='PSOCK')
#registerDoParallel(1)
registerDoParallel(cl)

# turn parallel processing off and run sequentially again:
# registerDoSEQ()

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

#remove ID column and mobile no (one level)
DT$UniqueID <- NULL
DT$MobileNo_Avl_Flag <- NULL


########################## FLAG columns change to Factors ###########

temp <- c("Employment.Type","Aadhar_flag","PAN_flag","VoterID_flag","Driving_flag","Passport_flag")
DT[, (temp) := lapply(.SD, as.factor),.SDcols = temp]






##################### DATES  #####################

#source :https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates/1996404
#option did not work
#https://stackoverflow.com/questions/10836503/convert-difftime-time-to-years-months-and-days
#DT$age.at.disbursal <- as.POSIXct(c(difftime(DT$DisbursalDate,DT$Date.of.Birth, units = "secs")),origin = DT$Date.of.Birth)


#convert into months
DT[,avg.acct.age.months := as.integer(str_extract_all(AVERAGE.ACCT.AGE,"[0-9]+",simplify = TRUE)[,1]) * 12 +
       as.integer(str_extract_all(AVERAGE.ACCT.AGE,"[0-9]+",simplify = TRUE)[,2])]
DT[,time.since.first.loan.months := as.integer(str_extract_all(CREDIT.HISTORY.LENGTH,"[0-9]+",simplify = TRUE)[,1]) * 12 + 
       as.integer(str_extract_all(CREDIT.HISTORY.LENGTH,"[0-9]+",simplify = TRUE)[,2])]
DT$age.at.disbursal <- interval(DT$Date.of.Birth,DT$DisbursalDate) %/% months(1)
DT$age.today <- interval(DT$Date.of.Birth,Sys.Date()) %/% months(1)


DT[,`:=`(age.at.disbursal=round(age.at.disbursal/12,0)
          ,age.today=round(age.today/12,0)
          ,avg.acct.age.months=round(avg.acct.age.months/12,0)
          ,time.since.first.loan.months=round(time.since.first.loan.months/12,0))]

DT[,`:=`(age.when.first.loan = age.today - time.since.first.loan.months,
         time.left.to.repay = avg.acct.age.months - time.since.first.loan.months)]


lapply(DT[,.(age.at.disbursal
         ,age.today
         ,avg.acct.age.months
         ,time.since.first.loan.months
         ,age.when.first.loan
         ,time.left.to.repay)],
       function(x) prop.table(table(x)))


#age.today
hist(DT$age.today)
prop.table(table(cut(DT$age.today, 
                     breaks=c(quantile(DT$age.today
                                       ,probs = seq(0,1,0.25))))))
DT[,age.today := cut(DT$age.today, 
                                 breaks=c(quantile(DT$age.today
                                                   ,probs = seq(0,1,0.25)))
                                 ,ordered = TRUE)]

hist(DT$avg.acct.age.months)
group_category(data = DT, feature = "avg.acct.age.months", threshold = 0.1,update = TRUE)
DT[,avg.acct.age.months:=factor(avg.acct.age.months,ordered = TRUE)]


hist(DT$time.since.first.loan.months)
group_category(data = DT, feature = "time.since.first.loan.months", threshold = 0.2,update = TRUE)
DT[,time.since.first.loan.months:=factor(time.since.first.loan.months,ordered = TRUE)]


hist(DT$age.when.first.loan)
prop.table(table(cut(DT$age.when.first.loan, 
                     breaks=c(quantile(DT$age.when.first.loan
                                       ,probs = seq(0,1,0.25))))))
DT[,age.when.first.loan := cut(DT$age.when.first.loan, 
                     breaks=c(quantile(DT$age.when.first.loan
                                       ,probs = seq(0,1,0.25)))
                     ,ordered = TRUE)]

hist(DT$time.left.to.repay)
DT[,time.left.to.repay := factor(as.character(if_else(time.left.to.repay<0,1,0)))]


str(DT)

DT$AVERAGE.ACCT.AGE <- NULL
DT$CREDIT.HISTORY.LENGTH <- NULL
DT$Date.of.Birth <- NULL
DT$DisbursalDate <- NULL
DT$age.at.disbursal <-NULL




################### CREDIT SCORE  ###########

prop.table(table(DT$PERFORM_CNS.SCORE.DESCRIPTION))

DT[,PERFORM_CNS.SCORE.DESCRIPTION.new :=
            case_when(
                PERFORM_CNS.SCORE.DESCRIPTION == 'A-Very Low Risk' ~ 'Low',
                PERFORM_CNS.SCORE.DESCRIPTION == 'B-Very Low Risk' ~ 'Low',
                PERFORM_CNS.SCORE.DESCRIPTION == 'C-Very Low Risk' ~ 'Low',
                PERFORM_CNS.SCORE.DESCRIPTION == 'D-Very Low Risk' ~ 'Low',
                PERFORM_CNS.SCORE.DESCRIPTION == 'E-Low Risk' ~ 'Low',
                PERFORM_CNS.SCORE.DESCRIPTION == 'F-Low Risk' ~ 'Low',
                PERFORM_CNS.SCORE.DESCRIPTION == 'G-Low Risk' ~ 'Low',
                PERFORM_CNS.SCORE.DESCRIPTION == 'H-Medium Risk' ~ 'Med',
                PERFORM_CNS.SCORE.DESCRIPTION == 'I-Medium Risk' ~ 'Med',
                PERFORM_CNS.SCORE.DESCRIPTION == 'J-High Risk' ~ 'High',
                PERFORM_CNS.SCORE.DESCRIPTION == 'K-High Risk' ~ 'High',
                PERFORM_CNS.SCORE.DESCRIPTION == 'L-Very High Risk' ~ 'High',
                PERFORM_CNS.SCORE.DESCRIPTION == 'M-Very High Risk' ~ 'High',
                PERFORM_CNS.SCORE.DESCRIPTION == 'No Bureau History Available' ~ 'NoData',
                PERFORM_CNS.SCORE.DESCRIPTION == 'Not Scored: More than 50 active Accounts found' ~ 'OTHER',
                PERFORM_CNS.SCORE.DESCRIPTION == 'Not Scored: No Activity seen on the customer (Inactive)' ~ 'OTHER',
                PERFORM_CNS.SCORE.DESCRIPTION == 'Not Scored: No Updates available in last 36 months' ~ 'OTHER',
                PERFORM_CNS.SCORE.DESCRIPTION == 'Not Scored: Not Enough Info available on the customer' ~ 'OTHER',
                PERFORM_CNS.SCORE.DESCRIPTION == 'Not Scored: Only a Guarantor' ~ 'OTHER',
                PERFORM_CNS.SCORE.DESCRIPTION == 'Not Scored: Sufficient History Not Available' ~ 'OTHER'
            )]

str(DT)
prop.table(table(DT$PERFORM_CNS.SCORE.DESCRIPTION.new))
DT[,PERFORM_CNS.SCORE.DESCRIPTION.new := factor(PERFORM_CNS.SCORE.DESCRIPTION.new,
                                                levels = c("Low","Med","High","Other","NoData"),
                                                ordered = TRUE)]


DT[,PERFORM_CNS.SCORE.DESCRIPTION:=NULL]
DT[,PERFORM_CNS.SCORE:=NULL]



##################### NO OF ACCOUNTS  + NO OF INQUIRIES #####################
#Convert them to factors

temp <- grep(".ACCTS|NO.OF",names(DT),value=TRUE)
lapply(DT[,temp,with=FALSE], function(x) round(prop.table(table(x)),3))

group_category(data = DT, feature = "PRI.NO.OF.ACCTS", threshold = 0.25,update = TRUE)
group_category(data = DT, feature = "PRI.ACTIVE.ACCTS", threshold = 0.1,update = TRUE)
DT[,PRI.OVERDUE.ACCTS := as.character(if_else(PRI.OVERDUE.ACCTS >0,1,0))]

DT[,SEC.NO.OF.ACCTS := as.character(if_else(SEC.NO.OF.ACCTS >0,1,0))]
DT[,SEC.ACTIVE.ACCTS := as.character(if_else(SEC.ACTIVE.ACCTS >0,1,0))]
DT[,SEC.OVERDUE.ACCTS := as.character(if_else(SEC.OVERDUE.ACCTS >0,1,0))]

group_category(data = DT, feature = "NEW.ACCTS.IN.LAST.SIX.MONTHS", threshold = 0.04,update = TRUE)
DT[,DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS := as.character(if_else(DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS >0,1,0))]
DT[,NO.OF_INQUIRIES := as.character(if_else(NO.OF_INQUIRIES >0,1,0))]

#Convert them to ordered factors
DT[,(temp):=lapply(.SD,function(x) factor(x,ordered=TRUE)),.SDcols = temp]

# A GLM Model 
# ORDERED vs UNORDEREDFACTORS
# A glm model only on these shows a difference based on how the factors are coded
# TO bE INTERPRETED
# Source : http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/

# temp_DT <- data.frame(lapply(DT[,c(temp,"loan_default"),with=FALSE],as.factor))
# temp_model <-glm(loan_default~ .
#                  ,data = temp_DT
#                  ,family = binomial(link="logit"))
# summary(temp_model)
# 
# temp_DT <- data.frame(lapply(DT[,c(temp,"loan_default"),with=FALSE],function(x) factor(x,ordered=TRUE)))
# temp_model <-glm(loan_default~ .
#                  ,data = temp_DT
#                  ,family = binomial(link="logit"))
# summary(temp_model)



##################### the SIX Id columns  #####################
fn_id_cols <- function(DT,col) {
    
    print(col)
    print(eval(col))
    by_cols <- c(col,"loan_default")
    print(by_cols)
    
    temp <- DT[flag=="train",.(.N,disbursed_amount=as.numeric(sum(disbursed_amount))),by=by_cols][
        ,.(loan_default,
           tot_loans = sum(N),
           tot_disbursed_amount =as.numeric(sum(disbursed_amount)),
           avg_loan_amount=as.numeric(sum(disbursed_amount)/sum(N)),
           default.pct = N/sum(N)),
        by = col][loan_default == 1][order(tot_loans)]
    
    temp[,paste0(col,".default_rate") := cut(temp$default.pct, breaks=c(quantile(temp$default.pct, probs = seq(0, 1, by = 1/3))), 
                                        labels=c("Low","Med","High"),include.lowest = TRUE
                                        ,ordered = TRUE)]
    
    temp[,paste0(col,".volume") := cut(temp$tot_loans, breaks=c(quantile(temp$tot_loans, probs = seq(0, 1, by = 1/3))), 
                                  labels=c("Low","Med","High"),include.lowest = TRUE
                                  ,ordered = TRUE)]
    
    temp[,paste0(col,".avg_loan_amount") := cut(temp$avg_loan_amount, breaks=c(quantile(temp$avg_loan_amount, probs = seq(0, 1, by = 1/3))), 
                                           labels=c("Low","Med","High"),include.lowest = TRUE
                                           ,ordered = TRUE)]
    
    temp[,`:=`(loan_default=NULL,tot_loans=NULL,tot_disbursed_amount=NULL,avg_loan_amount=NULL,default.pct=NULL)]

    temp
}

####### Branch ID #################
temp <- fn_id_cols(DT,"branch_id")
nrow(DT)
DT <- merge(DT,temp,by="branch_id")
nrow(DT)



####### Supplier ID #################
temp <- fn_id_cols(DT,"supplier_id")
nrow(DT)
DT <- merge(DT,temp,by="supplier_id",all.x = TRUE)
nrow(DT)

#Impute the NA values
DT[is.na(supplier_id.default_rate),.(supplier_id.default_rate)] 
temp <- names(which(prop.table(table(DT$supplier_id.default_rate)) == max(prop.table(table(DT$supplier_id.default_rate)))))
temp
DT[is.na(supplier_id.default_rate),supplier_id.default_rate:= (temp)] 
DT[is.na(supplier_id.default_rate),.(supplier_id.default_rate)] 


DT[is.na(supplier_id.volume),.(supplier_id.volume)] 
temp <- names(which(prop.table(table(DT$supplier_id.volume)) == max(prop.table(table(DT$supplier_id.volume)))))
temp
DT[is.na(supplier_id.volume),supplier_id.volume:= (temp)] 
DT[is.na(supplier_id.volume),.(supplier_id.volume)] 


DT[is.na(supplier_id.avg_loan_amount),.(supplier_id.avg_loan_amount)] 
temp <- names(which(prop.table(table(DT$supplier_id.avg_loan_amount)) == max(prop.table(table(DT$supplier_id.avg_loan_amount)))))
temp
DT[is.na(supplier_id.avg_loan_amount),supplier_id.avg_loan_amount:= (temp)] 
DT[is.na(supplier_id.avg_loan_amount),.(supplier_id.avg_loan_amount)] 



####### Manufacturer ID #################
temp <- fn_id_cols(DT,"manufacturer_id")
nrow(DT)
DT <- merge(DT,temp,by="manufacturer_id",all.x = TRUE)
nrow(DT)

#Impute the NA values
DT[is.na(manufacturer_id.default_rate),.(manufacturer_id.default_rate)] 
temp <- names(which(prop.table(table(DT$manufacturer_id.default_rate)) == max(prop.table(table(DT$manufacturer_id.default_rate)))))
temp
DT[is.na(manufacturer_id.default_rate),manufacturer_id.default_rate:= (temp)] 
DT[is.na(manufacturer_id.default_rate),.(manufacturer_id.default_rate)] 


DT[is.na(manufacturer_id.volume),.(manufacturer_id.volume)] 
temp <- names(which(prop.table(table(DT$manufacturer_id.volume)) == max(prop.table(table(DT$manufacturer_id.volume)))))
temp
DT[is.na(manufacturer_id.volume),manufacturer_id.volume:= (temp)] 
DT[is.na(manufacturer_id.volume),.(manufacturer_id.volume)] 


DT[is.na(manufacturer_id.avg_loan_amount),.(manufacturer_id.avg_loan_amount)] 
temp <- names(which(prop.table(table(DT$manufacturer_id.avg_loan_amount)) == max(prop.table(table(DT$manufacturer_id.avg_loan_amount)))))
temp
DT[is.na(manufacturer_id.avg_loan_amount),manufacturer_id.avg_loan_amount:= (temp)] 
DT[is.na(manufacturer_id.avg_loan_amount),.(manufacturer_id.avg_loan_amount)] 



####### Employee_code_ID #################
temp <- fn_id_cols(DT,"Employee_code_ID")
nrow(DT)
DT <- merge(DT,temp,by="Employee_code_ID",all.x = TRUE)
nrow(DT)

#Impute the NA values
DT[is.na(Employee_code_ID.default_rate),.(Employee_code_ID.default_rate)] 
temp <- names(which(prop.table(table(DT$Employee_code_ID.default_rate)) == max(prop.table(table(DT$Employee_code_ID.default_rate)))))
temp
DT[is.na(Employee_code_ID.default_rate),Employee_code_ID.default_rate:= (temp)] 
DT[is.na(Employee_code_ID.default_rate),.(Employee_code_ID.default_rate)] 


DT[is.na(Employee_code_ID.volume),.(Employee_code_ID.volume)] 
temp <- names(which(prop.table(table(DT$Employee_code_ID.volume)) == max(prop.table(table(DT$Employee_code_ID.volume)))))
temp
DT[is.na(Employee_code_ID.volume),Employee_code_ID.volume:= (temp)] 
DT[is.na(Employee_code_ID.volume),.(Employee_code_ID.volume)] 


DT[is.na(Employee_code_ID.avg_loan_amount),.(Employee_code_ID.avg_loan_amount)] 
temp <- names(which(prop.table(table(DT$Employee_code_ID.avg_loan_amount)) == max(prop.table(table(DT$Employee_code_ID.avg_loan_amount)))))
temp
DT[is.na(Employee_code_ID.avg_loan_amount),Employee_code_ID.avg_loan_amount:= (temp)] 
DT[is.na(Employee_code_ID.avg_loan_amount),.(Employee_code_ID.avg_loan_amount)] 



####### State_ID  #################
temp <- fn_id_cols(DT,"State_ID")
nrow(DT)
DT <- merge(DT,temp,by="State_ID")
nrow(DT)


####### Current_pincode_ID  #################
temp <- fn_id_cols(DT,"Current_pincode_ID")
nrow(DT)
DT <- merge(DT,temp,by="Current_pincode_ID",all.x = TRUE)
nrow(DT)
str(DT)

#Impute the NA values
DT[is.na(Current_pincode_ID.default_rate),.(Current_pincode_ID.default_rate)] 
temp <- names(which(prop.table(table(DT$Current_pincode_ID.default_rate)) == max(prop.table(table(DT$Current_pincode_ID.default_rate)))))
temp
DT[is.na(Current_pincode_ID.default_rate),Current_pincode_ID.default_rate:= (temp)] 
DT[is.na(Current_pincode_ID.default_rate),.(Current_pincode_ID.default_rate)] 


DT[is.na(Current_pincode_ID.volume),.(Current_pincode_ID.volume)] 
temp <- names(which(prop.table(table(DT$Current_pincode_ID.volume)) == max(prop.table(table(DT$Current_pincode_ID.volume)))))
temp
DT[is.na(Current_pincode_ID.volume),Current_pincode_ID.volume:= (temp)] 
DT[is.na(Current_pincode_ID.volume),.(Current_pincode_ID.volume)] 


DT[is.na(Current_pincode_ID.avg_loan_amount),.(Current_pincode_ID.avg_loan_amount)] 
temp <- names(which(prop.table(table(DT$Current_pincode_ID.avg_loan_amount)) == max(prop.table(table(DT$Current_pincode_ID.avg_loan_amount)))))
temp
DT[is.na(Current_pincode_ID.avg_loan_amount),Current_pincode_ID.avg_loan_amount:= (temp)] 
DT[is.na(Current_pincode_ID.avg_loan_amount),.(Current_pincode_ID.avg_loan_amount)] 


#drop the id features
temp <- c("branch_id","supplier_id","Current_pincode_ID","State_ID","Employee_code_ID","manufacturer_id")
DT[,(temp) := NULL]



##############################################
#########   BRING BACK THE AMOUNTS ###########
##############################################

#Earlier code that was deleting it
#temp <- grep("PRI|SEC|PRIMARY|disbursed_amount|asset_cost",
#     names(DT),value = TRUE)
#DT[,(temp):=NULL]


temp <- grep("PRI|SEC|PRIMARY",names(DT),value = TRUE)
temp

#plot_histogram(DT[,(temp),with = FALSE])

DT[,PRI.CURRENT.BALANCE.bin := as.character(if_else(PRI.CURRENT.BALANCE >0,1,0))]
DT[,PRI.DISBURSED.AMOUNT.bin := as.character(if_else(PRI.DISBURSED.AMOUNT >0,1,0))]
DT[,PRI.SANCTIONED.AMOUNT.bin := as.character(if_else(PRI.SANCTIONED.AMOUNT >0,1,0))]
DT[,PRIMARY.INSTAL.AMT.bin := as.character(if_else(PRIMARY.INSTAL.AMT >0,1,0))]


DT[,SEC.CURRENT.BALANCE.bin := as.character(if_else(SEC.CURRENT.BALANCE >0,1,0))]
DT[,SEC.DISBURSED.AMOUNT.bin := as.character(if_else(SEC.DISBURSED.AMOUNT >0,1,0))]
DT[,SEC.SANCTIONED.AMOUNT.bin := as.character(if_else(SEC.SANCTIONED.AMOUNT >0,1,0))]
DT[,SEC.INSTAL.AMT.bin := as.character(if_else(SEC.INSTAL.AMT >0,1,0))]

#Convert the above bins to factors
temp <- names(which(lapply(DT,is.character)==TRUE))
DT[,(temp) := lapply(.SD, as.factor) , .SDcols = (temp)]

# #Convert ordered to unordered on some features
# temp <- names(which(lapply(DT,is.ordered)==TRUE))[1:9]
# temp <- c()
# DT[,(temp) := lapply(.SD,function(x) factor(x,ordered = FALSE)), .SDcols = temp]


str(DT)

################# Polynomials ##################
nos <- 5
temp <- data.frame(poly(DT$ltv,nos))
colnames(temp) <- paste0("ltv",seq(1,nos,1))
DT <- cbind(DT,temp)

temp <- data.frame(poly(DT$asset_cost,nos))
colnames(temp) <- paste0("asset_cost",seq(1,nos,1))
DT <- cbind(DT,temp)


temp <- data.frame(poly(DT$disbursed_amount,nos))
colnames(temp) <- paste0("disbursed_amount",seq(1,nos,1))
DT <- cbind(DT,temp)



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

str(DT)

################### Modeling Prep  #####################

#XGB gives this error 
#Error in setinfo.xgb.DMatrix(dmat, names(p), p[[1]]) : The length of labels must equal to the number of rows in the input data

#DT[is.na(DT)] <- 0
#index <- createDataPartition(y=DT[flag =="train",]$loan_default, p=1, list=FALSE) 
#train <-DT[flag =="train"][index,]

train <- DT[flag =="train"]
train$flag <- NULL

temp <- data.table(predict(dummyVars(loan_default~ .
                                      , data=train
                                      ,fullRank = TRUE)
                            , newdata=train))
str(temp)
train <- cbind(temp,loan_default = train$loan_default)
str(train)

#Prep the test
test <-DT[flag =="test",]
test$flag <- NULL
test$loan_default <- NULL


temp <- predict(dummyVars(~ .
                          ,data=test
                          ,fullRank = TRUE)
                ,newdata=test)



str(train)
####################################################
#############      split train     #################
####################################################

str(train)
index <- createDataPartition(y=train$loan_default, p=0.8
                             , list=FALSE) 
xx_train <- train[index,]
xx_test <- train[-index,]
actual_loan_defaults <- xx_test$loan_default 
xx_test$loan_default <- NULL



params <- list(booster = "gbtree"
               , objective = "binary:logistic"
               
               # ,  objective = "reg:logistic"
               , eval_metric = "auc"
               , eta=0.01
               
                      , gamma=5
               #, alpha = 20
               
               #      , lambda = 5
               # , max_delta_step = 10
               , max_depth=5
               , min_child_weight=20
               , subsample=0.5
               , colsample_bytree=0.5
               #     ,  colsample_bylevel=0.5
               , scale_pos_weight = 2
)


#first default - model training
xgb1 <- xgboost( params = params
                 , data = data.matrix(xx_train[,!c("loan_default")
                                               ,with=FALSE])
                 
                 , label = xx_train$loan_default
                 
                 , nrounds = 150
                 #         , early_stopping_rounds = 300
                 , nfold = 5
                 , showsd = T
                 , stratified = F
                 , print_every_n = 1
                 , maximize = F
                 , nthread = 4
                 ,verbose = 2
)


predicted <- predict(xgb1,newdata = as.matrix(xx_test), type ="prob") 
summary(predicted)
prob_pred <- ifelse(predicted > 0.5,1,0)
prop.table(table(prob_pred))

cf <- confusionMatrix(factor(prob_pred),factor(actual_loan_defaults))
cf$table
cf$overall
cf$byClass
cf

pred <- prediction(predicted,actual_loan_defaults)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve",
     col = "blue", lwd = 3)
perf.auc <- performance(pred, measure = "auc")
unlist(perf.auc@y.values)



####################################################
###############      MODELIING     #################
####################################################


rm(temp)
rm(DT)
rm(test)
rm(train)
rm(index)
gc()
#grid<-data.frame(eta = c(1,0.3),rowno =1)

output <- pmap(.l=as.list(grid),.f=fn_grid)
output

## Warnings in the function
## 50: In confusionMatrix.default(factor(prob_pred), factor(actual_loan_defaults)) :
## Levels are not in the same order for reference and data. Refactoring data to match.


out_df<-data.frame()
cf_other <- data.frame()
out_model <- list()
for(i in 1:nrow(grid)) {
    out_df <- rbind(out_df,
                    output[i][[1]][grep("[^(model|perf_plot|cf_other|eval_log)]"
                                        ,names(output[[1]]))])

    cf_other <- rbind(cf_other,as.data.frame(t(output[1][[1]]$cf_other)))
    
    out_model[[i]]<- output[i][[1]]$model
}

out_df
cf_other

write.csv(cbind(out_df,cf_other), file = "xgb-tuning_20190422_temp.csv",
          row.names = FALSE)



for(i in 1:14) {
    predicted <- predict(out_model[[i]],newdata = temp)
    print(summary(predicted))
    prob_pred <- ifelse(predicted > 0.5,1,0)
    print(prop.table(table(prob_pred)))
}



################ PREDICT ###############
params <- list(booster = "gbtree"
               , objective = "binary:logistic"
               
              # ,  objective = "reg:logistic"
               , eval_metric = "auc"
               , eta=0.01
         
             #       , gamma=2
               , alpha = 20
           
        #      , lambda = 5
               # , max_delta_step = 10
               , max_depth=6
               , min_child_weight=20
               , subsample=0.5
               , colsample_bytree=0.5
          #     ,  colsample_bylevel=0.5
                , scale_pos_weight = 1.2
               )


#first default - model training
xgb1 <- xgb.cv( params = params
                 , data = data.matrix(train[,!c("loan_default"),with=FALSE])

                 , label = train$loan_default

                 , nrounds = 15
                 , early_stopping_rounds = 3
                 , nfold = 5
                 , showsd = T
                 , stratified = F
                 , print_every_n = 1
                 , maximize = F
                 , nthread = 4
                 ,verbose = 2
            )

xgb1$evaluation_log$train_auc_mean



numTrees

predict(xgb1,newdata = temp, type = "prob")


predicted <- predict(xgb1,newdata = temp, type ="prob")
summary(predicted)
prob_pred <- ifelse(predicted > 0.5,1,0)
prop.table(table(prob_pred))




####################################################
##########           Submission           ##########
####################################################




get_unique_ids <- fread(here::here("100_data_raw-input","test.csv"))

#submit_kaggle <- as.data.frame(cbind(get_unique_ids$UniqueID,prob_pred))
submit_kaggle <- as.data.frame(cbind(get_unique_ids$UniqueID,predicted))
colnames(submit_kaggle) <- c("UniqueID","loan_default")
write.csv(submit_kaggle,here("120_data_output"
                             ,paste0("EC2_GBM_",Sys.Date(),"_1.csv")), row.names = FALSE)




#XGB recognizes that many of your features are not important and didn't use them in the process of building decision trees. 
#You can force XGB to use all of them by increasing max tree depth setting, but you are overfitting the data this way.

#https://www.kaggle.com/c/santander-customer-satisfaction/discussion/20662
#https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
#https://xgboost.readthedocs.io/en/latest/parameter.html

importance <- xgb.importance(feature_names = names(train)
                             , model = xgb1)
importance
xgb.plot.importance(importance)

xgb.plot.multi.trees(feature_names = names(train), 
                     model = xgb1)
# get and plot information on how important each feature is







temp <-names(which(lapply(test,is.factor)==TRUE))

dummies <- dummyVars(~., data = test[,(temp),with=FALSE])
test_factors <-  as.data.frame(predict(dummies, newdata = test[,(temp),with=FALSE]))


head(test_factors)

test[,(temp):=NULL]
test <- data.matrix(cbind(test,test_factors))


#Good explanation .. this one made me icnrease the number of trees
#https://stats.stackexchange.com/questions/204489/discussion-about-overfit-in-xgboost
#http://www.stat.columbia.edu/~jakulin/Int/
    

#Examples
#https://www.kaggle.com/aniruddhachakraborty/lasso-gbm-xgboost-top-20-0-12039-using-r
#https://www.kaggle.com/serigne/stacked-regressions-top-4-on-leaderboard
#https://www.kaggle.com/jingfengzhu/machine-learning-with-xgboost-in-r-workbook


### Increasing the scale_pos_weight to 20, increased the probabilities as below
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2668  0.6703  0.7480  0.7211  0.7961  0.8428 
# Indicating that higher weight was put on the positive observations
# Confusion Matrix and Statistics (for p>0.5)
#               Reference
# Prediction     0     1
#           0  1581    82
#           1 34791 10176

#Reducing it to 5, did not change much and also reduing the max_depth from 15 to 5 did not change anything
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2942  0.4631  0.5425  0.5393  0.6201  0.7500 
# Confusion Matrix and Statistics
#               Reference
# Prediction     0     1
#               0 15126  1752
#               1 21246  8506

Accuracy : 0.506
