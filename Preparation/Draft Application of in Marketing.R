# Selecting the 10% customers with the highest upward potential
# Upward potential measured by highest above
# all_res = readRDS("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Submission/Results/all_res.RData")
all_res = readRDS("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Backup/all_res_times.RData")

method_list_appl = c("intervals_BS", "intervals_EN", "intervals_BA", "intervals_QR_m", "intervals_CP_m")
a_table = data.table("a" = seq(0,1,0.01),
                     "res" = 0)
learn_table = data.table("true_CET" = all_res$gift_results)
perf_overview = data.table("Data" = rep(c("gift","el","multi","apparel"), each = 5),
                           "Method" = rep(c("BS","EN","BA","QR","CP"), 4),
                           "max_rel" = 0,
                           "max_abs" = 0,
                           "hpp" = 0,
                           "hfq" = 0,
                           "hrc" = 0,
                           "hul" = 0,
                           "hiw" = 0,
                           "huu" = 0,
                           "htp" = 0,
                           "csw" = 0,
                           #"ccw" = 0,
                           "ssq" = 0,
                           "hll" = 0,
                           "aum1" = 0,
                           "aum2" = 0,
                           "aum3" = 0,
                           "rum1" = 0,
                           "rum2" = 0,
                           "hl2" = 0
                           )

lm_overview = data.table("Data" = rep(c("gift","el","multi","apparel"), each = 5),
                         "Method" = rep(c("BS","EN","BA","QR","CP"), 4),
                         "abs" = 0,
                         "n_abs" = 0,
                         "n_rel" = 0,
                         "hpp" = 0,
                         "lm" = 0,
                         "rfr" = 0,
                         "rfc" = 0,
                         "bor" = 0,
                         "boc" = 0,
                         "xgb" = 0,
                         "clu" = 0,
                         "clu_hpp" = 0)

cor_overview = data.table("Data" = rep(c("gift","el","multi","apparel"), each = 5),
                          "Method" = rep(c("BS","EN","BA","QR","CP"), 4),
                          "upper" = 0,
                          "lower" = 0,
                          "width" = 0)

sim_overview = data.table("Data" = rep(c("gift","el","multi","apparel"), each = 5),
                          "Method" = rep(c("BS","EN","BA","QR","CP"), 4))

run = 0
app_data = list()
columns = c("Id","max_s","hpp","hpp_s","hul","hul_s","hiw","hiw_s","huu","huu_s","htp","htp_s","c2w","c2w_s","ssq","ssq_s","hll","hll_s")
delete_columns = c("CET_covered", "PTS_true", "PTS_lower", "PTS_upper", "PTS_covered", "Frequency", "Recency")

for (data_list in data_lists){
  data = data_list
  n = data_list$select
  
  frequency = data$data2[, .N, by = Id]
  recency = data$data2
  recency = recency[Date < min(Date) + data$l2 * 7]
  recency = recency[, max(Date), by = Id]
  recency$Diff = as.numeric((min(recency$V1) + data$l2 * 7) - recency$V1)
  recency = recency[,-2]
  rf_data = merge(frequency, recency, by = "Id")
  colnames(rf_data) = c("Id", "Frequency", "Recency")
  rf_data$Id = as.character(rf_data$Id)
  rf_data$Frequency = as.numeric(rf_data$Frequency)
  rf_data$Recency = as.numeric(rf_data$Recency)
  
  set.seed(1)
  train_customers = sample(unique(data$data2$Id), length(unique(data$data2$Id))/2)
  
  for (method in method_list_appl){
    run = run + 1

    application_data = all_res[[data$name]][[method]]
    application_data = merge(application_data, rf_data, by = "Id")
    application_data = application_data[order(-application_data$CET_true),]
    application_data$max_s = application_data$CET_true >= min(application_data$CET_true[1:n])
    perf_overview[run,3] = round(mean(application_data$max_s),2)
    perf_overview[run,4] = sum(application_data$max_s)
    
    cor_overview[run, 3] = cor(application_data$CET_prediction, application_data$CET_upper)
    cor_overview[run, 4] = cor(application_data$CET_prediction, application_data$CET_lower)
    cor_overview[run, 5] = cor(application_data$CET_prediction, (application_data$CET_upper - application_data$CET_lower))
    
    # Highest point predictor
    application_data = application_data[order(-application_data$CET_prediction),]
    application_data$hpp = application_data$CET_prediction
    application_data$hpp_s = application_data$CET_prediction >= min(application_data$CET_prediction[1:n])
    perf_overview[run,5] = round(mean(application_data$max_s * application_data$hpp_s) / perf_overview[run,3],4)
    
    # # Highest frequency
    # application_data = application_data[order(-application_data$Frequency),]
    # application_data$hfq = application_data$Frequency
    # application_data$hfq_s = application_data$hfq >= min(application_data$hfq[1:n])
    # perf_overview[run,6] = round(mean(application_data$max_s * application_data$hfq_s) / perf_overview[run,3],4)
    # 
    # # Highest recency
    # application_data = application_data[order(application_data$Recency),]
    # application_data$hrc = application_data$Recency
    # application_data$hrc_s = application_data$hrc <= max(application_data$hrc[1:n])
    # perf_overview[run,7] = round(mean(application_data$max_s * application_data$hrc_s) / perf_overview[run,3],4)
    
    # Highest upper limit
    application_data = application_data[order(-application_data$CET_upper),]
    application_data$hul = application_data$CET_upper
    application_data$hul_s = application_data$CET_upper >= min(application_data$CET_upper[1:n])
    perf_overview[run,8] = round(mean(application_data$max_s * application_data$hul_s) / perf_overview[run,3],4)
    
    # Highest interval width
    application_data$hiw = application_data$CET_upper - application_data$CET_lower
    application_data = application_data[order(-application_data$hiw),]
    application_data$hiw_s = application_data$hiw >= min(application_data$hiw[1:n])
    perf_overview[run,9] = round(mean(application_data$max_s * application_data$hiw_s) / perf_overview[run,3],4)
    
    # Highest upwards uncertainty
    application_data$huu = application_data$CET_upper - application_data$CET_prediction
    application_data = application_data[order(-application_data$huu),]
    application_data$huu_s = application_data$huu >= min(application_data$huu[1:n])
    perf_overview[run,10] = round(mean(application_data$max_s * application_data$huu_s) / perf_overview[run,3],4)
    
    # Highest three point estimate
    application_data$htp = application_data$CET_lower/3 + application_data$CET_upper/3 + application_data$CET_prediction/3
    application_data = application_data[order(-application_data$htp),]
    application_data$htp_s = application_data$htp >= min(application_data$htp[1:n])
    perf_overview[run,11] = round(mean(application_data$max_s * application_data$htp_s) / perf_overview[run,3],4)
    
    # Highest CET^2/width
    application_data$c2w = ((application_data$CET_prediction)^2) / (application_data$CET_upper - application_data$CET_lower)
    application_data = application_data[order(-application_data$c2w),]
    application_data$c2w_s = application_data$c2w >= min(application_data$c2w[1:n])
    perf_overview[run,12] = round(mean(application_data$max_s * application_data$c2w_s) / perf_overview[run,3],4)
    
    # Highest (CET+CET)/width
    # application_data$ccw = (application_data$CET_prediction * 2) /(application_data$CET_upper - application_data$CET_lower)
    # application_data = application_data[order(-application_data$ccw),]
    # application_data$ccw_s= application_data$ccw >= min(application_data$ccw[1:n])
    # perf_overview[run,11] = round(mean(application_data$max_s * application_data$ccw_s) / perf_overview[run,3],4)
    
    # Highest CET^2/width^2
    application_data$ssq = ((application_data$CET_prediction)^2) / sqrt(application_data$CET_upper - application_data$CET_lower)
    application_data = application_data[order(-application_data$ssq),]
    application_data$ssq_s= application_data$ssq >= min(application_data$ssq[1:n])
    perf_overview[run,13] = round(mean(application_data$max_s * application_data$ssq_s) / perf_overview[run,3],4)
    
    if (method == "intervals_QR_m"){
      application_data$hll = application_data$CET_lower_actual
    }else{
      application_data$hll = application_data$CET_lower
    }
    
    # Highest lower limit
    # being always true and therefore potentially > 1 if all lower limits are 0
    application_data = application_data[order(-application_data$hll),]
    application_data$hll_s = application_data$hll >= min(application_data$hll[1:n])
    perf_overview[run,14] = round(mean(application_data$max_s * application_data$hll_s) / perf_overview[run,3],4)
    
    # Absolute Uncertainty Metric
    application_data$aum1 = application_data$CET_prediction - 0.5 * (application_data$CET_upper - application_data$CET_lower)
    application_data = application_data[order(-application_data$aum1),]
    application_data$aum_s1= application_data$aum >= min(application_data$aum1[1:n])
    perf_overview[run,15] = round(mean(application_data$max_s * application_data$aum_s1) / perf_overview[run,3],4)
    
    # Absolute Uncertainty Metric
    application_data$aum2 = application_data$CET_prediction - 0.9 * (application_data$CET_upper - application_data$CET_lower)
    application_data = application_data[order(-application_data$aum2),]
    application_data$aum_s2 = application_data$aum2 >= min(application_data$aum2[1:n])
    perf_overview[run,16] = round(mean(application_data$max_s * application_data$aum_s2) / perf_overview[run,3],4)
    
    # Absolute Uncertainty Metric
    application_data$aum3 = application_data$CET_prediction - 0.1 * (application_data$CET_upper - application_data$CET_lower)
    application_data = application_data[order(-application_data$aum3),]
    application_data$aum_s3 = application_data$aum3 >= min(application_data$aum3[1:n])
    perf_overview[run,17] = round(mean(application_data$max_s * application_data$aum_s3) / perf_overview[run,3],4)
    
    # Relative Uncertainty Metric
    application_data$rum1 = application_data$CET_prediction * (1 - (application_data$CET_upper - application_data$CET_lower) / application_data$CET_prediction)
    application_data = application_data[order(-application_data$rum1),]
    application_data$rum_s1 = application_data$rum1 >= min(application_data$rum1[1:n])
    perf_overview[run,18] = round(mean(application_data$max_s * application_data$rum_s1) / perf_overview[run,3],4)
    
    # Relative Uncertainty Metric
    application_data$rum2 = application_data$CET_prediction / (1 - (application_data$CET_upper - application_data$CET_lower) / application_data$CET_prediction)
    application_data = application_data[order(-application_data$rum2),]
    application_data$rum_s2 = application_data$rum2 >= min(application_data$rum2[1:n])
    perf_overview[run,19] = round(mean(application_data$max_s * application_data$rum_s2) / perf_overview[run,3],4)
    
    # (H + L) / 2
    application_data$hl2 = (application_data$CET_upper + application_data$CET_lower) / 2
    application_data = application_data[order(-application_data$hl2),]
    application_data$hl2_s = application_data$hl2 >= min(application_data$hl2[1:n])
    perf_overview[run,20] = round(mean(application_data$max_s * application_data$hl2_s) / perf_overview[run,3],4)
    
    print(perf_overview)
    learn_application_data = application_data
    app_data[[run]] = application_data[order(-application_data$Frequency)]
    
    
    for (i in 17:length(application_data)){
      if (grepl("_s", colnames(application_data)[i])){
        sim_overview[run, paste0("hpp_", colnames(application_data)[i]) := round(sum(application_data$hpp_s == application_data[[colnames(application_data)[i]]]) / nrow(application_data),4) ]
      }
    }
    print(sim_overview)
    
    if (ml & !(method == "intervals_BS" & data_list$name == "multi_results")){
      
      # Machine learning
      learn_data = application_data[,-c("PTS_true", "Frequency", "Recency", "CET_covered", "PTS_covered", "PTS_lower", "PTS_upper", "PTS_prediction")]
      learn_data$hpp_s_t = with(learn_data, ifelse(max_s & hpp_s, "TT",
                                                 ifelse(max_s & !hpp_s, "TF",
                                                        ifelse(!max_s & hpp_s, "FT",
                                                               "FF"))))
      train = learn_data[Id %in% train_customers,]
      test = learn_data[!(Id %in% train_customers),]
      n_ml = nrow(test[CET_true > quantile(test$CET_true, 0.90)])
      lm_overview[run,3] = nrow(test)
      lm_overview[run,4] = n_ml
      lm_overview[run,5] = n_ml / nrow(test)
      
      # Removing/replacing NA and Inf
      # For training
      max = -sort(unique(-train$c2w))[2]
      train$c2w[is.infinite(train$c2w)] = max
      max = -sort(unique(-train$ssq))[2]
      train$ssq[is.infinite(train$ssq)] = max
      # For test
      max = -sort(unique(-test$c2w))[2]
      test$c2w[is.infinite(test$c2w)] = max
      max = -sort(unique(-test$ssq))[2]
      test$ssq[is.infinite(test$ssq)] = max

      eval_data = data.table("Id" = test$Id,
                             "true" = test$CET_true,
                             "pred" = test$CET_prediction
      )

      # if there is at any point in training or test NA, then the column will be deleted
      for (column in colnames(train)){
        if (sum(is.na(train[[column]])) | sum(is.na(test[[column]]))){
          train = train[,-..column]
          test = test[,-..column]
        }
      }

      # train: Scale between 0 and 1
      IdMax_s = train[,c("Id","max_s")]
      hpp_s_t_train = train$hpp_s_t
      train = train[,-c("Id","max_s","hpp_s_t")]
      
      for (column in colnames(train)){
        if (sum(train[[column]]) != 0){
          c_min = min(train[[column]])
          c_max = max(train[[column]])
          train[[column]] = (train[[column]] - c_min) / (c_max - c_min)
        }
      }
      train$Id = IdMax_s$Id
      train$max_s = IdMax_s$max_s

      # Remove hll_s if it has NAs
      train = train %>% select_if(~ !any(is.na(.)))
      # Add hll_s again
      if (!("hll_s" %in% names(train))){
        train$hll_s = 1
      }

      # test: Scale between 0 and 1
      IdMax_s = test[,c("Id","max_s")]
      hpp_s_t_test = test$hpp_s_t
      test = test[,-c("Id","max_s","hpp_s_t")]

      for (column in colnames(test)){
        if (sum(test[[column]]) != 0){
          c_min = min(test[[column]])
          c_max = max(test[[column]])
          test[[column]] = (test[[column]] - c_min) / (c_max - c_min)
        }
      }
      test$Id = IdMax_s$Id
      test$max_s = IdMax_s$max_s
      test = test %>% select_if(~ !any(is.na(.)))
      if (!("hll_s" %in% names(test))){
        test$hll_s = 1
      }

      # True
      eval_data = eval_data[order(-eval_data$true)]
      eval_data$true_s = eval_data$true >= min(eval_data$true[1:n_ml])

      # HPP
      eval_data = eval_data[order(-eval_data$pred)]
      eval_data$hpp_s = eval_data$pred > min(eval_data$pred[1:n_ml])
      lm_overview[run, 6] = sum(eval_data$true_s * eval_data$hpp_s)/n_ml

      # LM
      #####
      fit = lm(CET_true ~ ., data = train[,-c("Id", "max_s")])
      summary(fit)

      ml_res = data.table("Id" = test$Id,
                          "lm" = predict(fit,test[,-c("Id", "max_s")])
                          )

      eval_data = merge(x = eval_data, y = ml_res[,c("Id","lm")], by = "Id")
      eval_data = eval_data[order(-eval_data$lm),]
      eval_data$lm_s = eval_data$lm > min(eval_data$lm[1:n_ml])
      lm_overview[run, 7] = sum(eval_data$true_s * eval_data$lm_s)/n_ml

      #####
      # Random forest
      #####
      
      # Regression
      fit = rpart(CET_true ~ ., data = train[,-c("Id", "max_s")], method = "anova", control = rpart.control(cp = 0.00001))
      rfr = round(as.vector(predict(fit, test[,-c("Id", "max_s")])),4)
      Id = test$Id
      ml_res = merge(x = ml_res, y = cbind.data.frame(rfr, Id), by = "Id")
      ml_res = ml_res[order(-ml_res$rfr)]
      ml_res$rfr_s = ml_res$rfr > min(ml_res$rfr[1:n_ml])
      eval_data = merge(x = eval_data, y = ml_res[,c("Id","rfr_s")], by = "Id")
      lm_overview[run, 8] = sum(eval_data$true_s * eval_data$rfr_s)/n_ml

      # Classification
      fit = rpart(max_s ~ ., data = train[,-c("Id", "CET_true")], method = "class", control = rpart.control(cp = 0.00001))
      rfc = round(as.vector(unlist(data.table(predict(fit, test, type = "prob"))[,2])),6)
      ml_res = merge(x = ml_res, y = cbind.data.frame(rfc, Id), by = "Id")
      ml_res = ml_res[order(-ml_res$rfc)]
      ml_res$rfc_s = ml_res$rfc > min(ml_res$rfc[1:n_ml])
      eval_data = merge(x = eval_data, y = ml_res[,c("Id","rfc_s")], by = "Id")
      lm_overview[run, 9] = sum(eval_data$true_s * eval_data$rfc_s)/n_ml

      #####
      # Boosting
      #####

      # Regression
      fit = gbm(CET_true ~ ., data = train[,-c("Id", "max_s")], n.trees=10000)
      bor = round(predict(fit, test[,-c("Id", "max_s")], n_trees = 10000), 6)
      ml_res = merge(x = ml_res, y = cbind.data.frame(bor, Id), by = "Id")
      ml_res = ml_res[order(-ml_res$bor)]
      ml_res$bor_s = ml_res$bor > min(ml_res$bor[1:n_ml])
      eval_data = merge(x = eval_data, y = ml_res[,c("Id","bor_s")], by = "Id")
      lm_overview[run, 10] = sum(eval_data$true_s * eval_data$bor_s)/n_ml

      # Classification
      fit = gbm(as.numeric(max_s) ~ ., data = train[,-c("Id", "CET_true")], n.trees=100)
      boc = round(predict(fit, test[,-c("Id", "CET_true")], type = "response"), 6)
      ml_res = merge(x = ml_res, y = cbind.data.frame(boc, Id), by = "Id")
      ml_res = ml_res[order(-ml_res$boc)]
      ml_res$boc_s = ml_res$boc > min(ml_res$boc[1:n_ml])
      eval_data = merge(x = eval_data, y = ml_res[,c("Id","boc_s")], by = "Id")
      lm_overview[run, 11] = sum(eval_data$true_s * eval_data$boc_s)/n_ml

      # XGBoost
      X_train <- matrix(as.numeric(as.matrix(train[, -c("CET_true","max_s")])), ncol=length(train) - 2)
      y_train <- train$CET_true

      X_test <- matrix(as.numeric(as.matrix(test[, -c("CET_true","max_s")])), ncol=length(test) - 2)
      dtrain <- xgb.DMatrix(data = X_train, label = y_train)
      dtest <- xgb.DMatrix(data = X_test)

      params <- list(
        objective = "reg:squarederror",  # Regression for continuous target
        eta = 0.1,
        max_depth = 6
      )

      xgb_model <- xgb.train(
        params = params,
        data = dtrain,
        nrounds = 100,
        watchlist = list(train = dtrain),
        early_stopping_rounds = 10,
        print_every_n = 10
      )

      xgb = round(predict(xgb_model, dtest), 6)
      ml_res = merge(x = ml_res, y = cbind.data.frame(xgb, Id), by = "Id")
      ml_res = ml_res[order(-ml_res$xgb)]
      ml_res$xgb_s = ml_res$xgb > min(ml_res$xgb[1:n_ml])
      eval_data = merge(x = eval_data, y = ml_res[,c("Id","xgb_s")], by = "Id")
      lm_overview[run, 12] = sum(eval_data$true_s * eval_data$xgb_s)/n_ml

      #####
      # # Neural network
      # newNeuralNetwork <- cmpfun(neuralnet) # compile the function "neuralnet"
      # comp <- cmpfun(neuralnet::compute) # compile the function compute from neuralnet
      # 
      # train
      # IdMax_s = train[,c("Id","max_s")]
      # train = train[,-c("Id","max_s")]
      # train[,c("hpp_s", "hul_s", "hiw_s", "huu_s", "htp_s", "c2w_s", "ssq_s", "hll_s")] = lapply(train[,c("hpp_s", "hul_s", "hiw_s", "huu_s", "htp_s", "c2w_s", "ssq_s", "hll_s")], as.numeric)
      # for (column in colnames(train)){
      #   if (sum(train[[column]]) != 0){
      #     c_min = min(train[[column]])
      #     c_max = max(train[[column]])
      #     train[[column]] = (train[[column]] - c_min) / (c_max - c_min)
      #   }
      # }
      # train$Id = IdMax_s$Id
      # train$max_s = IdMax_s$max_s
      # train = train %>% select_if(~ !any(is.na(.)))
      # 
      # # test
      # IdMax_s = test[,c("Id","max_s")]
      # test = test[,-c("Id","max_s")]
      # test[,c("hpp_s", "hul_s", "hiw_s", "huu_s", "htp_s", "c2w_s", "ssq_s", "hll_s")] = lapply(test[,c("hpp_s", "hul_s", "hiw_s", "huu_s", "htp_s", "c2w_s", "ssq_s", "hll_s")], as.numeric)
      # for (column in colnames(test)){
      #   if (sum(test[[column]]) != 0){
      #     c_min = min(test[[column]])
      #     c_max = max(test[[column]])
      #     test[[column]] = (test[[column]] - c_min) / (c_max - c_min)
      #   }
      # }
      # test$Id = IdMax_s$Id
      # test$max_s = IdMax_s$max_s
      # test = test %>% select_if(~ !any(is.na(.)))
      # 
      # for (nodes in 1:4){
      #   for (l in 1:4){
      #     if (nodes == 1){
      #       net = newNeuralNetwork(CET_true ~ ., data = train[,-c("Id", "max_s")], hidden = l, linear.output = TRUE, stepmax=1e7)
      #       print(1)
      #       name = paste0("n",nodes,"l",l)
      #       name_s = paste0(name,"_s")
      #       nn_res = comp(net, test[,-c("Id", "max_s")])
      #       print(2)
      #       ml_res[[name]] = nn_res$net.result
      #       print(3)
      #       ml_res = ml_res[order(-ml_res[[name]])]
      #       print(4)
      #       ml_res[[name_s]] = ml_res[[name]] >= min(ml_res[[name]][1:n_ml])
      #       print(5)
      #       eval_data = merge(x = eval_data, y = ml_res[,c("Id",..name_s)], by = "Id")
      #       print(6)
      #       lm_overview[run, name] = sum(eval_data$true_s * eval_data[[name_s]])/n_ml
      #       print(7)
      #     }
      #     if (nodes == 2){
      #       net = newNeuralNetwork(CET_true ~ ., data = train[,-c("Id", "max_s")], hidden = c(l,l), linear.output = TRUE, stepmax=1e7)
      #       name = paste0("n",nodes,"l",l)
      #       name_s = paste0(name,"_s")
      #       nn_res = comp(net, test[,-c("Id", "max_s")])
      #       ml_res[[name]] = nn_res$net.result
      #       ml_res = ml_res[order(-ml_res[[name]])]
      #       ml_res[[name_s]] = ml_res[[name]] >= min(ml_res[[name]][1:n_ml])
      #       eval_data = merge(x = eval_data, y = ml_res[,c("Id",..name_s)], by = "Id")
      #       lm_overview[run, name] = sum(eval_data$true_s * eval_data[[name_s]])/n_ml
      #     }
      #     if (nodes == 3){
      #       net = newNeuralNetwork(CET_true ~ ., data = train[,-c("Id", "max_s")], hidden = c(l,l,l), linear.output = TRUE, stepmax=1e7)
      #       name = paste0("n",nodes,"l",l)
      #       name_s = paste0(name,"_s")
      #       nn_res = comp(net, test[,-c("Id", "max_s")])
      #       ml_res[[name]] = nn_res$net.result
      #       ml_res = ml_res[order(-ml_res[[name]])]
      #       ml_res[[name_s]] = ml_res[[name]] >= min(ml_res[[name]][1:n_ml])
      #       eval_data = merge(x = eval_data, y = ml_res[,c("Id",..name_s)], by = "Id")
      #       lm_overview[run, name] = sum(eval_data$true_s * eval_data[[name_s]])/n_ml
      #     }
      #     if (nodes == 4){
      #       net = newNeuralNetwork(CET_true ~ ., data = train[,-c("Id", "max_s")], hidden = c(l,l,l,l), linear.output = TRUE, stepmax=1e7)
      #       name = paste0("n",nodes,"l",l)
      #       name_s = paste0(name,"_s")
      #       nn_res = comp(net, test[,-c("Id", "max_s")])
      #       ml_res[[name]] = nn_res$net.result
      #       ml_res = ml_res[order(-ml_res[[name]])]
      #       ml_res[[name_s]] = ml_res[[name]] >= min(ml_res[[name]][1:n_ml])
      #       eval_data = merge(x = eval_data, y = ml_res[,c("Id",..name_s)], by = "Id")
      #       lm_overview[run, name] = sum(eval_data$true_s * eval_data[[name_s]])/n_ml
      #     }
      #     print(paste("run", run, "name", name, "achieved", lm_overview[run, ..name]))
      #   }
      # }
      
      # for (column in colnames(train)){
      #   if (sum(is.na(train[[column]])) | sum(is.na(test[[column]]))){
      #     train = train[,-..column]
      #     test = test[,-..column]
      #   }
      # }
      
      # train$hpp_s_t = factor(hpp_s_t_train)
      # test$hpp_s_t = factor(hpp_s_t_test)
      # fit = fda(hpp_s_t ~ CET_lower + CET_upper + CET_prediction + hul_s + hiw_s + huu_s + htp_s + c2w_s + ssq_s + hll_s + aum_s + rum_s + hl2_s, data = train[,-c("CET_true","Id","max_s","hpp","hpp_s")])
      # res = predict(fit, test[,-c("Id","max_s","hpp","hpp_s","hul")], type = "class")
      # res$actual = test$hpp_s_t
      # colnames(train)
      
      #####
      # Clustering
      print(paste("run: ", run))
      cluster_data = learn_data

      IdMax_s = cluster_data[,c("Id","max_s", "CET_true")]
      cluster_data = cluster_data[,-c("Id","max_s", "CET_true", "Frequency", "Recency", "hpp_s_t")]
      cluster_data[is.na(cluster_data)] = 0
      for (column in colnames(cluster_data)){
        if (sum(cluster_data[[column]]) != 0){
          c_min = min(cluster_data[[column]])
          c_max = max(cluster_data[[column]])
          cluster_data[[column]] = (cluster_data[[column]] - c_min) / (c_max - c_min)
        }
      }

      cluster_data = cluster_data %>% select_if(~ !any(is.na(.)))
      cluster_data = cluster_data[, !grepl("_s", names(cluster_data)), with = FALSE]

      # cluster_data$sum <- rowSums(sweep(cluster_data[,1:(length(cluster_data))], 2, weights, "*"))

      cluster_data$sum = rowSums(cluster_data)
      max_row_index = which.max(cluster_data$sum)
      vec = as.vector(t(cluster_data[max_row_index,(1:length(cluster_data))]))
      cluster_data$diff = 0

      for (i in 1:nrow(cluster_data)){
        cluster_data[i, "diff"] = sum(vec - as.vector(t(cluster_data[i,1:length(cluster_data-1)])))
      }

      cluster_data$Id = IdMax_s$Id
      cluster_data$max_s = IdMax_s$max_s
      cluster_data = cluster_data[order(cluster_data$diff)]
      cluster_data$diff_s = cluster_data$diff < max(cluster_data$diff[1:n])

      lm_overview[run, 13] = mean(cluster_data$max_s * cluster_data$diff_s) / mean(cluster_data$max_s)

      cluster_data = cluster_data[order(-cluster_data$CET_prediction)]
      cluster_data$hpp_s = cluster_data$CET_prediction > min(cluster_data$CET_prediction[1:n])
      lm_overview[run, 14] = mean(cluster_data$max_s * cluster_data$hpp_s) / mean(cluster_data$max_s)
      
      print(lm_overview)
    }
  }
}


for (i in app_data){
  n_cust = ceiling(nrow(i) * percentage)
  i$CET_true = as.numeric(i$CET_true)
  if (exists("cor_table")){
    cor_table = cor_table + cor(i[1:n_cust,c(4,12:23)])*0.25
  }else{
    cor_table = cor(i[1:n_cust,c(4,12:23)])*0.25
  }
}

cor_table
rm(cor_table)

comp_perf_overview = data.frame("Metric" = c("Better or equal", "Better", "Worse", "Mean advantage (rel)", "Sd of advantages"),
                                "hul" = 0,
                                "hiw" = 0,
                                "huu" = 0,
                                "htp" = 0,
                                "csw" = 0,
                                #"ccw" = 0,
                                "ssq" = 0)

for (i in 1:6){
  comp_perf_overview[1, (i+1)] = round(sum(perf_overview[[i+5]] >= perf_overview[[5]], na.rm = TRUE) / (nrow(perf_overview) - 1),4)
  comp_perf_overview[2, (i+1)] = round(sum(perf_overview[[i+5]] > perf_overview[[5]], na.rm = TRUE) / (nrow(perf_overview) - 1),4)
  comp_perf_overview[3, (i+1)] = round(sum(perf_overview[[i+5]] < perf_overview[[5]], na.rm = TRUE) / (nrow(perf_overview) - 1),4)
  comp_perf_overview[4, (i+1)] = round(mean((perf_overview[[i+5]] - perf_overview[[5]])/perf_overview[[5]], na.rm = TRUE),4)
  comp_perf_overview[5, (i+1)] = round(sd(perf_overview[[i+5]] - perf_overview[[5]], na.rm = TRUE),4)
}

path = paste0("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/LaTeX/perf_overview.csv")
write.csv(perf_overview, path)
path = paste0("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/LaTeX/comp_perf_overview.csv")
write.csv(comp_perf_overview, path)

