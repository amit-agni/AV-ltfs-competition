install.packages("pacman")

library(pacman)
p_load(data.table,tidyverse,lubridate,here,DataExplorer,outliers,DescTools,caret,googledrive,doMC,gbm)
gc()

parallel::detectCores()




########
registerDoMC(cores = 8)



dribble <- drive_find(pattern="train.csv")
drive_download(as_dribble(dribble),overwrite = TRUE)

dribble <- drive_find(pattern="test.csv")
drive_download(as_dribble(dribble),overwrite = TRUE)



######



# DT_train <- fread(here::here("100_data_raw-input","train.csv"))
# DT_test <- fread(here::here("100_data_raw-input","test.csv"))



DT_train <- fread(here::here("train.csv"))
DT_test <- fread(here::here("test.csv"))



nrow(DT_test)
#[1] 112392

table(DT_train$loan_default)
#0      1 
#182543  50611 
#20%


DT_train$flag <- "train"
DT_test$flag <- "test"

DT_test$loan_default <- 1

DT <- rbind(DT_train,DT_test)

rm(DT_train)
rm(DT_test)


nrow(DT[flag == "test"])



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




#Explore PRI and SEC account features (SPARSE DISTRIBUTIONS)
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




#bin amounts has huge outliers but below  doesnt seem to be the right approach




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





#Create date features

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

glimpse(DT)


#cnvert some categorical columns into numeric

#Factor columns
str(DT[,names(which(lapply(DT, is.factor)==TRUE)),with=FALSE])




nrow(DT[flag == "test"])


#Score description ignore
DT[,.(count = .N,
      minscore = min(PERFORM_CNS.SCORE),
      maxscore = max(PERFORM_CNS.SCORE)), by =PERFORM_CNS.SCORE.DESCRIPTION ][order(minscore)]

unique(DT$loan_default)

#Suppliers with their total loans and default pct
temp <- DT[flag=="train",.N,by=.(supplier_id,loan_default)][
    ,.(supplier_id.tot_loans = sum(N),loan_default,supplier_id.default.pct = N/sum(N)),by = supplier_id][loan_default == 1]

temp[,loan_default:=NULL]

DT <- merge(DT,temp,by="supplier_id",all.x = TRUE)




glimpse(DT)

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







#convert to factors

#characters
temp <- names(which(lapply(DT, is.character)==TRUE))
temp
DT[, (temp) := lapply(.SD, as.factor),.SDcols = temp]


#additional columns
temp <- c("manufacturer_id","Employment.Type","Aadhar_flag","PAN_flag","VoterID_flag","Driving_flag","Passport_flag","loan_default")
DT[, (temp) := lapply(.SD, as.factor),.SDcols = temp]




#Drop features

#factor features
temp <- c("branch_id","supplier_id","Current_pincode_ID","State_ID","Employee_code_ID")

DT[,(temp) := NULL]

#date features
temp <- c("Date.of.Birth","DisbursalDate")
DT[,(temp) := NULL]


#drop PERFORM_CNS.SCORE as it captured in the description

DT$PERFORM_CNS.SCORE <- NULL


DT$PRI.SANCTIONED.AMOUNT <- NULL
DT$PRI.DISBURSED.AMOUNT <- NULL
DT$PRI.CURRENT.BALANCE <- NULL


DT$SEC.SANCTIONED.AMOUNT <- NULL
DT$SEC.DISBURSED.AMOUNT <- NULL
DT$SEC.CURRENT.BALANCE <- NULL

DT$PRIMARY.INSTAL.AMT <- NULL
DT$SEC.INSTAL.AMT <-NULL




#Box Cox transformation of numeric columns


temp <-names(which(lapply(DT,is.numeric)==TRUE))

preProcValues <- caret::preProcess(DT[,(temp),with = FALSE], method = "BoxCox")
DT_tran <- predict(preProcValues, DT[,(temp),with = FALSE])

#remove the columns from DT and cbind the transformed columns
DT[,(temp) := NULL]

DT <- cbind(DT,DT_tran)


              

####################################################

#GBM using caret

####################################################

DT[is.na(DT)] <- 0
DT$loan_default <- factor(DT$loan_default,labels = c("No","Yes"))
unique(DT$loan_default)

index <- createDataPartition(y=DT[flag =="train",]$loan_default, p=0.99, list=FALSE) 
index <- createDataPartition(y=DT[flag =="train",]$loan_default, p=1, list=FALSE) 



train <-DT[flag =="train"][index,]
test <-DT[flag =="train",][-index,]

train$flag <- NULL
test$flag <- NULL

table(train$loan_default)
table(test$loan_default)

str(test)
gc(reset=TRUE)

control <- trainControl(method="repeatedcv", number=3, repeats=1,classProbs = TRUE)
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
                     ,n.minobsinnode = 10
                     ,interaction.depth = c(50,100)
                     ,shrinkage = 0.1
            
)

gbm


model <- train(loan_default ~ ., data = test 
                 ,method = "gbm" 
                 ,trControl = control
                 ## This last option is actually one
                 ## for gbm() that passes throug
                 ,metric=metric
                ,tuneGrid = grid
               ,allowParallel = TRUE
                 ,verbose = FALSE)

model

warnings()


########## submission

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





####################################################

#RF

####################################################


control <- trainControl(method="repeatedcv", number=5, repeats=3,classProbs = TRUE)
metric <- "Kappa"
#preproc <-  c("center", "scale")
grid <- expand.grid(mtry = c(3,6,10)
                    ,ntree = c(100,500,1000))


grid
####

model <- train(loan_default ~ ., data = train 
               ,method = "parRF" 
               ,trControl = control
               ## This last option is actually one
               ## for gbm() that passes throug
               ,metric=metric
               ,tuneGrid = grid
               ,allowParallel = TRUE
                ,ntree = 50

               ,verbose = FALSE)


######### notes

Error: Stopping
In addition: Warning message:
    In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
                                There were missing values in resampled performance measures.


                            Error: Stopping
                            In addition: Warning message:
                                In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
                                                            There were missing values in resampled performance measures.
                                                        > 
                                                            
                                                            
