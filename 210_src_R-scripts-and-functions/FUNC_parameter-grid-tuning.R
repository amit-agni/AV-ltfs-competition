# TO DO 
# 1. Add Systime at every iteration / ROW NO
# CSV hedder to have the parameters being tuned 
# Use the elements from the cutpointr function instead of the ConfusionMatrix function
# return the individual performance metrics in a list


grid <- NULL
grid <- list(eta = c(0.1,0.001)
              ,max_depth = c(3,6)
              ,min_child_weight = c(10,13,16)
              ,subsample = c(0.2,0.6)
          #    ,colsample_bylevel = c(0.05,0.2)
              ,gamma=c(0,25,50)
             # ,alpha = c(0)
             # ,lambda = c(1,5)
             # ,max_delta_step = c(0)
 #             ,scale_pos_weight = c(1,1.1)
       
       )
grid <- do.call(data.table::CJ, args = grid)
grid$rowno <- seq(1,nrow(grid),1)
grid


fn_grid <- function(eta=0.3
                    ,max_depth = 6
                    ,min_child_weight=1
                    ,subsample = 1
                    ,colsample_bylevel = 1
                    ,gamma = 0
                    ,alpha = 0
                    ,lambda = 1
                    ,max_delta_step = 0
                    ,scale_pos_weight = 1
                    ,rowno = 1){
    
    gc()
    print(paste0("Iteration no : ", rowno))
    print(paste0("eta=",eta
               ," gamma=",gamma
               ," alpha =", alpha
               ," lambda = ",lambda
               ," max_delta_step = ",max_delta_step
               ," max_depth=",max_depth
               ," min_child_weight=",min_child_weight
               ," subsample=",subsample
               ," colsample_bylevel=",colsample_bylevel
               ," scale_pos_weight = ",scale_pos_weight))
               
    params <- list(booster = "gbtree"
                   , objective = "binary:logistic"
                   , eval_metric = "auc"
                   , eta=eta
                   , gamma=gamma
                   , alpha = alpha
                   , lambda = lambda
                   , max_delta_step = max_delta_step
                   , max_depth=max_depth
                   , min_child_weight=min_child_weight
                   , subsample=subsample
                   , colsample_bylevel=colsample_bylevel
                   , scale_pos_weight = scale_pos_weight)

    xgbcv <- xgb.cv( params = params
                     # , data = data.matrix(train[,!c("loan_default"),with=FALSE])
                     # , label = train$loan_default
                     
                     # , data = data.matrix(xx_train[,!c("loan_default"),with=FALSE])
                     # , label = xx_train$loan_default
                     # 
                     
                     , data = data.matrix(xx_train)
                     , label = train_labels
                     
                     
                     , nrounds = 300
                     , nfold = 5
                     , showsd = T
                     , stratified = T
                     , print_every_n = 50
                     , early_stopping_rounds = 200
                     , maximize = F
                     , nthread = 4)
    
    #Predict using these trees 
    numtrees <- min(which(xgbcv$evaluation_log$train_auc_mean==max(xgbcv$evaluation_log$train_auc_mean)))
    

    xgb <- xgboost( params = params
                     # , data = data.matrix(xx_train[,!c("loan_default"),with=FALSE])
                     # , label = xx_train$loan_default
                     # 
                    , data = data.matrix(xx_train)
                    , label = train_labels
                    
                     , nrounds = numtrees
                     , nfold = 5
                     , showsd = T
                     , stratified = T
                     , print_every_n = 1
                     , maximize = F
                     , nthread = 4)
    
    predicted <- predict(xgb,newdata = data.matrix(xx_test), type ="prob") 
    
    opt_cut <- cutpointr::cutpointr(x=predicted,
                                    class=actual_loan_defaults,
                                    #metric = roc01,
                                    direction = "<=",
                                    pos_class = 1)
    
    prob_pred <- ifelse(predicted > opt_cut$optimal_cutpoint,1,0)
    
    cf <- confusionMatrix(factor(prob_pred),factor(actual_loan_defaults))

    pred <- prediction(predicted,actual_loan_defaults)
    perf <- performance(pred, measure = "tpr", x.measure = "fpr")
    perf.auc <- performance(pred, measure = "auc")
    
    # perf_plot <- print(plot(perf, main = "ROC curve",
    #            col = "blue", lwd = 3))
    # 
    
    

    best_iteration <- xgbcv$best_iteration
    mean_train<-mean(xgbcv$evaluation_log$train_auc_mean)
    mean_test<-mean(xgbcv$evaluation_log$test_auc_mean)
    
    min_train<-min(xgbcv$evaluation_log$train_auc_mean)
    min_test<-min(xgbcv$evaluation_log$test_auc_mean)
    
    max_train<-max(xgbcv$evaluation_log$train_auc_mean)
    max_test<-max(xgbcv$evaluation_log$test_auc_mean)
    numtrees <- numtrees
    
    predicted_min <- unname(summary(predicted)[1])
    predicted_Q1 <- unname(summary(predicted)[2])
    predicted_Median <- unname(summary(predicted)[3])
    predicted_Mean <- unname(summary(predicted)[4])
    predicted_Q3 <- unname(summary(predicted)[5])
    predicted_Max <- unname(summary(predicted)[6])
    
    
    prob_pred_of_0 <- unname(prop.table(table(prob_pred))[1])
    prob_pred_of_1 <- unname(prop.table(table(prob_pred))[2])
    
    cf_Pred0_Act0 <- cf$table[1,1]
    cf_Pred0_Act1 <- cf$table[1,2]
    cf_Pred1_Act0 <- cf$table[2,1]
    cf_Pred1_Act1 <- cf$table[2,2]
    
    cf_accuracy <- unname(cf$overall[1])
    cf_kappa <- unname(cf$overall[2])
    
    perf_auc <- unlist(perf.auc@y.values)
    
    #perf_plot <- perf_plot
    
    cf_other <- cf$byClass

    
    rm(xgb)
    rm(xgbcv)
    rm(pred)
    rm(perf)

    gc()
    
    list(rowno = rowno
        ,eta=eta
        ,gamma=gamma
        ,alpha = alpha
        ,lambda = lambda
        ,max_delta_step = max_delta_step
        ,max_depth=max_depth
        ,min_child_weight=min_child_weight
        ,subsample=subsample
        ,colsample_bylevel=colsample_bylevel
        ,scale_pos_weight = scale_pos_weight

        ,   best_iteration  =    best_iteration 
        ,   mean_train =    mean_train
        ,   mean_test =    mean_test
        ,   var_mean_train_test = mean_train - mean_test
        
        , optimal_cut = opt_cut$optimal_cutpoint
        ,   min_train =    min_train
        ,   min_test =    min_test
        ,   max_train =    max_train
        ,   max_test =    max_test
        ,   numtrees  =    numtrees 
        ,   predicted_min  =    predicted_min 
        ,   predicted_Q1  =    predicted_Q1 
        ,   predicted_Median  =    predicted_Median 
        ,   predicted_Mean  =    predicted_Mean 
        ,   predicted_Q3  =    predicted_Q3 
        ,   predicted_Max  =    predicted_Max 
        ,   prob_pred_of_0  =    prob_pred_of_0 
        ,   prob_pred_of_1  =    prob_pred_of_1 
        ,   cf_Pred0_Act0  =    cf_Pred0_Act0 
        ,   cf_Pred0_Act1  =    cf_Pred0_Act1 
        ,   cf_Pred1_Act0  =    cf_Pred1_Act0 
        ,   cf_Pred1_Act1  =    cf_Pred1_Act1 
        ,   cf_accuracy  =    cf_accuracy 
        ,   cf_kappa  =    cf_kappa 
        ,   perf_auc  =    perf_auc 
        
        ,   cf_other  =    cf_other 
        
        
        #,perf_plot  =     #perf_plot 
        #,eval_log = xgbcv$evaluation_log
        #,model = xgb
        
         
         )
    
    
}



