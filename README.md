This is a quick summary for the [Analytics Vidhya LTFS
hackathon](https://datahack.analyticsvidhya.com/contest/ltfs-datascience-finhack-an-online-hackathon/)
that took place between 12-22April2019

-   My CV score was around 0.68 but the LB score did not cross 0.5.
    There were 2-3 possible causes for this :
    -   The id columns (branch id, current pin code id, etc) were all
        encoded using the target (loan\_default) column. And then
    -   Feature engineering was done by combining the Test and Train
        dataset. This possibly resulted in data leakage
    -   The range of the predicted probabilities was very narrow
        (0.4-0.6), though I was expecting atleast few to touch the
        extremes (0 or 1)

### My journey

-   Basic EDA is documented in
    [EDA-report.html](/Mac%20Backup/OneDrive/R/Competitions/AV/ltfs-loan-default_20190413/230_src_R-markdowns-and-output/EDA-report.html)
-   The FE and modeling code is
    [LTFS-Model-xgb\_20190422.R](/Mac%20Backup/OneDrive/R/Competitions/AV/ltfs-loan-default_20190413/210_src_R-scripts/LTFS-Model-xgb_20190422.R)
-   The function created for parameter grid tuning is
    [FUNC\_parameter-grid-tuning.R](/Mac%20Backup/OneDrive/R/Competitions/AV/ltfs-loan-default_20190413/210_src_R-scripts/FUNC_parameter-grid-tuning.R)
-   Modeling using caret on EC2 giving error *“In nominalTrainWorkflow(x
    = x, y = y, wts = weights, info = trainInfo, : There were missing
    values in resampled performance measures.”* even though there were
    not missing values  
-   Models tried gbm,parRF,glmnet. Was not able to install catboost in R
-   Finally settled with xgboost (without caret)

### New Learnings

-   Use of purrr::p\_map for model automation
-   Xgboost hyperparameters
    -   max\_depth : no of features to be used in each model (Higher
        =&gt; overfitting)
    -   subsample : Sampling of observations
    -   colsample\_bytree : Sampling of features
    -   scale\_pos\_weight : gives higher weightage to the positive
        target values
    -   Increase nround for slower eta
        -   Good explanation .. this one made me increase the number of
            trees.
            (<a href="https://stats.stackexchange.com/questions/204489/discussion-about-overfit-in-xgboost" class="uri">https://stats.stackexchange.com/questions/204489/discussion-about-overfit-in-xgboost</a>)
-   A simple glm model on these Ordered vs Unordered factors showed a
    difference (TO BE INTERPRETED)
    -   (<a href="http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/" class="uri">http://www.win-vector.com/blog/2012/07/modeling-trick-impact-coding-of-categorical-variables-with-many-levels/</a>)
-   Other useful links :
    -   <a href="https://www.kaggle.com/c/santander-customer-satisfaction/discussion/20662" class="uri">https://www.kaggle.com/c/santander-customer-satisfaction/discussion/20662</a>
    -   <a href="https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/" class="uri">https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/</a>
    -   <a href="http://www.stat.columbia.edu/~jakulin/Int/" class="uri">http://www.stat.columbia.edu/~jakulin/Int/</a>
-   New packages : DataExplorer, catboost, lightgbm, cutpointr
-   New Concepts : Time Series Cross validation

### Lessons / Unanswered questions / TO DO

-   Time spent should be 80% FE 20% modeling but mine was opposite
-   Need to investigate the feature importance plots. Is feature
    selection necessary in tree based models ?
-   Is collinearity a problem in tree based models ? and do feature
    selection

### Sample winning solutions (Referred)

-   <a href="https://www.kaggle.com/aniruddhachakraborty/lasso-gbm-xgboost-top-20-0-12039-using-r" class="uri">https://www.kaggle.com/aniruddhachakraborty/lasso-gbm-xgboost-top-20-0-12039-using-r</a>
-   <a href="https://www.kaggle.com/serigne/stacked-regressions-top-4-on-leaderboard" class="uri">https://www.kaggle.com/serigne/stacked-regressions-top-4-on-leaderboard</a>
-   <a href="https://www.kaggle.com/jingfengzhu/machine-learning-with-xgboost-in-r-workbook" class="uri">https://www.kaggle.com/jingfengzhu/machine-learning-with-xgboost-in-r-workbook</a>

### Other Competitors

-   The leaderboard and participants approaches
    -   [Leaderboard](https://datahack.analyticsvidhya.com/contest/ltfs-datascience-finhack-an-online-hackathon/pvt_lb)
    -   [LB
        13](https://github.com/rajat5ranjan/AV-LTFS-Data-Science-FinHack-ML-Hackathon/blob/master/Final_Submission.ipynb)
        -   Combined train/test
        -   Added PRI + SEC to create new feature
        -   Used catboost with :
            n\_estimators=3000,random\_state=1994,eval\_metric=‘AUC’,
            max\_depth=6,learning\_rate=0.029,od\_wait=50,l2\_leaf\_reg=5,
            cat\_features=categorical\_features\_indices
            ,bagging\_temperature=0.85,random\_strength=100
        -   K-Folds cross validation helps curbs overfitting
    -   [LB
        36](https://github.com/sk48880/LTFS-Data-Science-Finhack---Anlytics-Vidhya/blob/master/Loan_Default_Prediction_EDA_%26_Gradient_Boosting.ipynb)
        -   Dropped some of the outliers
        -   Feature engineering Train and Test separately
    -   [LB
        81](https://github.com/jasjotiitr/LTFS-Data-Science-FinHack/blob/master/loan%20default%20prediction-final-submit.ipynb)
        -   Group By branch id,etc same like what I did but did not use
            loan\_default column
        -   Combined Train and Test for FE
        -   Log transformed amounts
        -   Used catboost with : cat\_features= \[3,6,7,8,62\],
            max\_depth=5, reg\_lambda=3
            +[MK](https://github.com/mohsinkhn/ltfs-av)
        -   Outlier treatment
        -   word2vec

### Tuning Parameters and Model performance

-   nrounds = 400
-   early\_stopping\_rounds = 200
-   nfold = 5

-   eta = c(0.1,0.01,0.001)
-   max\_depth = c(3,5,8)
-   min\_child\_weight = c(1,5,30)
-   subsample = c(0.1,0.3)
-   colsample\_bylevel = c(0.1,0.3)
-   gamma=c(0,25,50)
-   scale\_pos\_weight = c(1,1.5)

#### Top 5 models with lowest CV train/test

``` r
DT <- data.table::fread(here("120_data_output","RESULTS_tuning-grid_MODEL-NAME_2019-04-22_1.csv"))
DT$CV_train_test <- DT$mean_train - DT$min_test

head(DT[order(CV_train_test)])
```

    ##    rowno   eta gamma alpha lambda max_delta_step max_depth
    ## 1:   214 0.001    25     0      1              0         8
    ## 2:   216 0.001    50     0      1              0         8
    ## 3:    94 0.001    25     0      1              0         5
    ## 4:    91 0.001     0     0      1              0         5
    ## 5:   120 0.001    50     0      1              0         5
    ## 6:   141 0.001    25     0      1              0         5
    ##    min_child_weight subsample colsample_bylevel scale_pos_weight
    ## 1:               30       0.3               0.3              1.5
    ## 2:               30       0.3               0.3              1.5
    ## 3:                1       0.3               0.3              1.5
    ## 4:                1       0.3               0.3              1.0
    ## 5:                5       0.3               0.3              1.5
    ## 6:               30       0.3               0.3              1.0
    ##    best_iteration mean_train mean_test min_train  min_test max_train
    ## 1:              1  0.6850600 0.6817839 0.6619460 0.6596162 0.6861950
    ## 2:              1  0.6786277 0.6770150 0.6525176 0.6529808 0.6799500
    ## 3:              1  0.6819050 0.6793368 0.6581790 0.6561172 0.6833620
    ## 4:              1  0.6851725 0.6815712 0.6606330 0.6587712 0.6867454
    ## 5:              1  0.6778537 0.6762381 0.6514624 0.6514328 0.6790132
    ## 6:              1  0.6790493 0.6769206 0.6550346 0.6519342 0.6799980
    ##     max_test numtrees predicted_min predicted_Q1 predicted_Median
    ## 1: 0.6829484      201     0.4311198    0.4446151        0.4578861
    ## 2: 0.6781886      198     0.4346813    0.4468340        0.4585922
    ## 3: 0.6807882      201     0.4317824    0.4461842        0.4585512
    ## 4: 0.6829300      201     0.4235533    0.4355415        0.4453601
    ## 5: 0.6773350      201     0.4339933    0.4466801        0.4587532
    ## 6: 0.6780348      199     0.4269824    0.4362394        0.4458210
    ##    predicted_Mean predicted_Q3 predicted_Max prob_pred_of_0 prob_pred_of_1
    ## 1:      0.4613215    0.4752065     0.5171106      0.9433198   0.0566802488
    ## 2:      0.4619014    0.4744397     0.5075462      0.9619558   0.0380441776
    ## 3:      0.4614088    0.4733239     0.5122026      0.9527128   0.0472871542
    ## 4:      0.4484250    0.4581634     0.5040518      0.9994210   0.0005790264
    ## 5:      0.4614308    0.4734140     0.5059786      0.9660519   0.0339481021
    ## 6:      0.4488624    0.4580511     0.4935407      1.0000000             NA
    ##    cf_Pred0_Act0 cf_Pred0_Act1 cf_Pred1_Act0 cf_Pred1_Act1 cf_accuracy
    ## 1:         35167          8820          1375          1268   0.7813639
    ## 2:         35619          9237           923           851   0.7821145
    ## 3:         35421          9004          1121          1084   0.7828651
    ## 4:         36535         10068             7            20   0.7839374
    ## 5:         35727          9320           815           768   0.7826507
    ## 6:         36542         10088             0             0   0.7836586
    ##       cf_kappa  perf_auc Sensitivity Specificity Pos Pred Value
    ## 1: 0.120166472 0.6876287           1           0      0.7836586
    ## 2: 0.084224377 0.6825725           1           0      0.7836586
    ## 3: 0.107058947 0.6850895           1           0      0.7836586
    ## 4: 0.002802799 0.6865077           1           0      0.7836586
    ## 5: 0.077467523 0.6816909           1           0      0.7836586
    ## 6: 0.000000000 0.6825133           1           0      0.7836586
    ##    Neg Pred Value Precision Recall        F1 Prevalence Detection Rate
    ## 1:             NA 0.7836586      1 0.8787092  0.7836586      0.7836586
    ## 2:             NA 0.7836586      1 0.8787092  0.7836586      0.7836586
    ## 3:             NA 0.7836586      1 0.8787092  0.7836586      0.7836586
    ## 4:             NA 0.7836586      1 0.8787092  0.7836586      0.7836586
    ## 5:             NA 0.7836586      1 0.8787092  0.7836586      0.7836586
    ## 6:             NA 0.7836586      1 0.8787092  0.7836586      0.7836586
    ##    Detection Prevalence Balanced Accuracy CV_train_test
    ## 1:                    1               0.5    0.02544377
    ## 2:                    1               0.5    0.02564685
    ## 3:                    1               0.5    0.02578778
    ## 4:                    1               0.5    0.02640134
    ## 5:                    1               0.5    0.02642093
    ## 6:                    1               0.5    0.02711514

#### Top 5 models with highest test set accuracy

``` r
head(DT[order(-cf_accuracy)])
```

    ##    rowno eta gamma alpha lambda max_delta_step max_depth min_child_weight
    ## 1:   451 0.1     0     0      1              0         3                1
    ## 2:   571 0.1     0     0      1              0         5               30
    ## 3:   469 0.1     0     0      1              0         3                5
    ## 4:   565 0.1     0     0      1              0         5               30
    ## 5:   637 0.1     0     0      1              0         8               30
    ## 6:   523 0.1     0     0      1              0         5                1
    ##    subsample colsample_bylevel scale_pos_weight best_iteration mean_train
    ## 1:       0.3               0.3                1              1  0.6988953
    ## 2:       0.3               0.3                1              1  0.7113628
    ## 3:       0.3               0.1                1              1  0.6954172
    ## 4:       0.3               0.1                1              1  0.7074978
    ## 5:       0.3               0.1                1              1  0.7263670
    ## 6:       0.3               0.3                1              1  0.7156289
    ##    mean_test min_train  min_test max_train  max_test numtrees
    ## 1: 0.6937859 0.6320900 0.6322526 0.7086158 0.6992004      201
    ## 2: 0.6964636 0.6567458 0.6545086 0.7237212 0.6993160      201
    ## 3: 0.6906914 0.6076228 0.6052950 0.7059192 0.6977644      201
    ## 4: 0.6947841 0.6484336 0.6468984 0.7190764 0.6983340      201
    ## 5: 0.6944399 0.6611438 0.6543598 0.7459156 0.6966736      201
    ## 6: 0.6959022 0.6613242 0.6582726 0.7325608 0.6987106      201
    ##    predicted_min predicted_Q1 predicted_Median predicted_Mean predicted_Q3
    ## 1:    0.01903785    0.1217758        0.1925910      0.2162982    0.2897061
    ## 2:    0.01597995    0.1164679        0.1901898      0.2164364    0.2938110
    ## 3:    0.01949005    0.1233833        0.1934513      0.2158602    0.2881153
    ## 4:    0.01717696    0.1189457        0.1904882      0.2160611    0.2908423
    ## 5:    0.01077596    0.1138758        0.1887312      0.2163988    0.2953431
    ## 6:    0.01480389    0.1163353        0.1892118      0.2160868    0.2920767
    ##    predicted_Max prob_pred_of_0 prob_pred_of_1 cf_Pred0_Act0 cf_Pred0_Act1
    ## 1:     0.7649041      0.9783401     0.02165988         36109          9511
    ## 2:     0.7837186      0.9728072     0.02719279         35979          9383
    ## 3:     0.7402300      0.9806777     0.01932232         36161          9568
    ## 4:     0.7690181      0.9739867     0.02601330         36004          9413
    ## 5:     0.7945608      0.9675316     0.03246837         35852          9264
    ## 6:     0.8293149      0.9702981     0.02970191         35915          9330
    ##    cf_Pred1_Act0 cf_Pred1_Act1 cf_accuracy   cf_kappa  perf_auc
    ## 1:           433           577   0.7867467 0.06725363 0.7040412
    ## 2:           563           705   0.7867038 0.07970109 0.7045100
    ## 3:           381           520   0.7866395 0.06134007 0.7033068
    ## 4:           538           675   0.7865966 0.07657237 0.7045708
    ## 5:           690           824   0.7865323 0.09070299 0.7008735
    ## 6:           627           758   0.7864679 0.08430722 0.7021126
    ##    Sensitivity Specificity Pos Pred Value Neg Pred Value Precision Recall
    ## 1:           1           0      0.7836586             NA 0.7836586      1
    ## 2:           1           0      0.7836586             NA 0.7836586      1
    ## 3:           1           0      0.7836586             NA 0.7836586      1
    ## 4:           1           0      0.7836586             NA 0.7836586      1
    ## 5:           1           0      0.7836586             NA 0.7836586      1
    ## 6:           1           0      0.7836586             NA 0.7836586      1
    ##           F1 Prevalence Detection Rate Detection Prevalence
    ## 1: 0.8787092  0.7836586      0.7836586                    1
    ## 2: 0.8787092  0.7836586      0.7836586                    1
    ## 3: 0.8787092  0.7836586      0.7836586                    1
    ## 4: 0.8787092  0.7836586      0.7836586                    1
    ## 5: 0.8787092  0.7836586      0.7836586                    1
    ## 6: 0.8787092  0.7836586      0.7836586                    1
    ##    Balanced Accuracy CV_train_test
    ## 1:               0.5    0.06664268
    ## 2:               0.5    0.05685420
    ## 3:               0.5    0.09012224
    ## 4:               0.5    0.06059940
    ## 5:               0.5    0.07200716
    ## 6:               0.5    0.05735629

#### Top 5 models with highest Kappa

``` r
head(DT[order(-cf_kappa)])
```

    ##    rowno eta gamma alpha lambda max_delta_step max_depth min_child_weight
    ## 1:   644 0.1     0     0      1              0         8               30
    ## 2:   572 0.1     0     0      1              0         5               30
    ## 3:   596 0.1     0     0      1              0         8                1
    ## 4:   638 0.1     0     0      1              0         8               30
    ## 5:   518 0.1     0     0      1              0         5                1
    ## 6:   620 0.1     0     0      1              0         8                5
    ##    subsample colsample_bylevel scale_pos_weight best_iteration mean_train
    ## 1:       0.3               0.3              1.5              1  0.7372896
    ## 2:       0.3               0.3              1.5              1  0.7122496
    ## 3:       0.3               0.3              1.5              1  0.7709391
    ## 4:       0.3               0.1              1.5              1  0.7299826
    ## 5:       0.3               0.1              1.5              1  0.7111978
    ## 6:       0.3               0.3              1.5              1  0.7584923
    ##    mean_test min_train  min_test max_train  max_test numtrees
    ## 1: 0.6943585 0.6715110 0.6625400 0.7603872 0.6967976      201
    ## 2: 0.6961589 0.6583048 0.6554180 0.7250438 0.6987276      201
    ## 3: 0.6888910 0.6659100 0.6494290 0.8157526 0.6933810      201
    ## 4: 0.6932109 0.6555174 0.6474290 0.7514320 0.6961136      201
    ## 5: 0.6937946 0.6267194 0.6256628 0.7268018 0.6972374      201
    ## 6: 0.6907991 0.6689176 0.6536412 0.7933478 0.6942116      201
    ##    predicted_min predicted_Q1 predicted_Median predicted_Mean predicted_Q3
    ## 1:   0.012764709    0.1566893        0.2540871      0.2810227    0.3835061
    ## 2:   0.023446416    0.1639742        0.2590827      0.2829283    0.3820556
    ## 3:   0.004118074    0.1492743        0.2481148      0.2768407    0.3804617
    ## 4:   0.016715249    0.1597283        0.2571273      0.2819899    0.3836946
    ## 5:   0.022951227    0.1648560        0.2609503      0.2833779    0.3822354
    ## 6:   0.011345101    0.1514390        0.2507281      0.2782859    0.3814533
    ##    predicted_Max prob_pred_of_0 prob_pred_of_1 cf_Pred0_Act0 cf_Pred0_Act1
    ## 1:     0.8731312      0.8960112     0.10398885         33954          7827
    ## 2:     0.8412538      0.9047180     0.09528201         34242          7945
    ## 3:     0.9248477      0.8953892     0.10461077         33900          7852
    ## 4:     0.8611804      0.8993995     0.10060047         34026          7913
    ## 5:     0.8516804      0.9071842     0.09281578         34288          8014
    ## 6:     0.9285222      0.8976839     0.10231611         33959          7900
    ##    cf_Pred1_Act0 cf_Pred1_Act1 cf_accuracy  cf_kappa  perf_auc Sensitivity
    ## 1:          2588          2261   0.7766459 0.1887948 0.6984738           1
    ## 2:          2300          2143   0.7802917 0.1874587 0.7037200           1
    ## 3:          2642          2236   0.7749517 0.1836876 0.6916202           1
    ## 4:          2516          2175   0.7763457 0.1819933 0.6995005           1
    ## 5:          2254          2074   0.7797984 0.1813988 0.7030068           1
    ## 6:          2583          2188   0.7751876 0.1806745 0.6949354           1
    ##    Specificity Pos Pred Value Neg Pred Value Precision Recall        F1
    ## 1:           0      0.7836586             NA 0.7836586      1 0.8787092
    ## 2:           0      0.7836586             NA 0.7836586      1 0.8787092
    ## 3:           0      0.7836586             NA 0.7836586      1 0.8787092
    ## 4:           0      0.7836586             NA 0.7836586      1 0.8787092
    ## 5:           0      0.7836586             NA 0.7836586      1 0.8787092
    ## 6:           0      0.7836586             NA 0.7836586      1 0.8787092
    ##    Prevalence Detection Rate Detection Prevalence Balanced Accuracy
    ## 1:  0.7836586      0.7836586                    1               0.5
    ## 2:  0.7836586      0.7836586                    1               0.5
    ## 3:  0.7836586      0.7836586                    1               0.5
    ## 4:  0.7836586      0.7836586                    1               0.5
    ## 5:  0.7836586      0.7836586                    1               0.5
    ## 6:  0.7836586      0.7836586                    1               0.5
    ##    CV_train_test
    ## 1:    0.07474960
    ## 2:    0.05683160
    ## 3:    0.12151011
    ## 4:    0.08255357
    ## 5:    0.08553503
    ## 6:    0.10485107
