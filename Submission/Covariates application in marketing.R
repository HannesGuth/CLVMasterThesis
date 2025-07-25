# DESCRIPTION

# Basically the same as "Application in marketing" but uses the results from the individualized PIs that come from the introduced covariate

################################################################


# all_res_cov = readRDS("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Backup/all_res_cov_times.RData")
method_list_appl = c("intervals_BS", "intervals_EN", "intervals_BA", "intervals_QR_m", "intervals_CP_m")
a_table = data.table("a" = seq(0,1,0.01),
                     "res" = 0)
perf_overview_cov = data.table("Data" = rep(c("gift","el","multi","apparel"), each = 5),
                           "Method" = rep(c("BS","EN","BA","QR","CP"), 4),
                           "max_rel" = 0,
                           "max_abs" = 0,
                           "hpp" = 0,
                           # "hfq" = 0,
                           # "hrc" = 0,
                           "hub" = 0,
                           "hiw" = 0,
                           "huu" = 0,
                           "htp" = 0,
                           "csw" = 0,
                           #"ccw" = 0,
                           "ssq" = 0
                           )
run = 0
app_data = list()

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
  
  for (method in method_list_appl){
    run = run + 1

    application_data = all_res_cov[[data$name]][[method]]
    application_data = merge(application_data, rf_data, by = "Id")
    application_data = application_data[order(-application_data$CET_true),]
    application_data$max_s = application_data$CET_true >= min(application_data$CET_true[1:n])
    perf_overview_cov[run,3] = round(mean(application_data$max_s),2)
    perf_overview_cov[run,4] = sum(application_data$max_s)
    
    # Highest point predictor
    application_data = application_data[order(-application_data$CET_prediction),]
    application_data$hpp = application_data$CET_prediction
    application_data$hpp_s = application_data$CET_prediction >= min(application_data$CET_prediction[1:n])
    perf_overview_cov[run,5] = round(mean(application_data$max_s * application_data$hpp_s) / perf_overview_cov[run,3],4)
    
    # # Highest frequency
    # hfq_data = application_data[order(-application_data$Frequency),]
    # application_data$hfq = application_data$Frequency
    # perf_overview_cov[run,5] = round(sum(hfq_data$CET_true[1:n])/perf_overview_cov[run,3],2)
    # 
    # # Highest recency
    # hrc_data = application_data[order(application_data$Recency),]
    # application_data$hrc = application_data$Recency
    # perf_overview_cov[run,6] = round(sum(hrc_data$CET_true[1:n])/perf_overview_cov[run,3],2)
    
    # Highest upper limit
    application_data = application_data[order(-application_data$CET_upper),]
    application_data$hub = application_data$CET_upper
    application_data$hub_s = application_data$CET_upper >= min(application_data$CET_upper[1:n])
    perf_overview_cov[run,6] = round(mean(application_data$max_s * application_data$hub_s) / perf_overview_cov[run,3],4)
    
    # Highest interval width
    application_data$hiw = application_data$CET_upper - application_data$CET_lower
    application_data = application_data[order(-application_data$hiw),]
    application_data$hiw_s = application_data$hiw >= min(application_data$hiw[1:n])
    perf_overview_cov[run,7] = round(mean(application_data$max_s * application_data$hiw_s) / perf_overview_cov[run,3],4)
    
    # Highest upwards uncertainty
    application_data$huu = application_data$CET_upper - application_data$CET_prediction
    application_data = application_data[order(-application_data$huu),]
    application_data$huu_s= application_data$huu >= min(application_data$huu[1:n])
    perf_overview_cov[run,8] = round(mean(application_data$max_s * application_data$huu_s) / perf_overview_cov[run,3],4)
    
    # Highest three point estimate
    application_data$htp = application_data$CET_lower/3 + application_data$CET_upper/3 + application_data$CET_prediction/3
    application_data = application_data[order(-application_data$htp),]
    application_data$htp_s = application_data$htp >= min(application_data$htp[1:n])
    perf_overview_cov[run,9] = round(mean(application_data$max_s * application_data$htp_s) / perf_overview_cov[run,3],4)
    
    # Highest CET^2/width
    application_data$c2w = ((application_data$CET_prediction)^2) / (application_data$CET_upper - application_data$CET_lower)
    application_data = application_data[order(-application_data$c2w),]
    application_data$c2w_s = application_data$c2w >= min(application_data$c2w[1:n])
    perf_overview_cov[run,10] = round(mean(application_data$max_s * application_data$c2w_s) / perf_overview_cov[run,3],4)
    
    # Highest (CET+CET)/width
    # application_data$ccw = (application_data$CET_prediction * 2) /(application_data$CET_upper - application_data$CET_lower)
    # application_data = application_data[order(-application_data$ccw),]
    # application_data$ccw_s= application_data$ccw >= min(application_data$ccw[1:n])
    # perf_overview_cov[run,11] = round(mean(application_data$max_s * application_data$ccw_s) / perf_overview_cov[run,3],4)
    
    # Highest CET^2/width^2
    application_data$ssq = ((application_data$CET_prediction)^2) / sqrt(application_data$CET_upper - application_data$CET_lower)
    application_data = application_data[order(-application_data$ssq),]
    application_data$ssq_s = application_data$ssq >= min(application_data$ssq[1:n])
    perf_overview_cov[run,11] = round(mean(application_data$max_s * application_data$ssq_s) / perf_overview_cov[run,3],4)
    
    
    app_data[[run]] = application_data[order(-application_data$Frequency)]
  }
}

# for (i in app_data){
#   n_cust = ceiling(nrow(i) * percentage)
#   i$CET_true = as.numeric(i$CET_true)
#   if (exists("cor_table")){
#     cor_table = cor_table + cor(i[1:n_cust,c(4,12:23)])*0.25
#   }else{
#     cor_table = cor(i[1:n_cust,c(4,12:23)])*0.25
#   }
# }

# cor_table
# rm(cor_table)

comp_perf_overview_cov = data.frame("Metric" = c("Better or equal", "Better", "Worse", "Mean advantage (rel)", "Sd of advantages"),
                                "hub" = 0,
                                "hiw" = 0,
                                "huu" = 0,
                                "htp" = 0,
                                "csw" = 0,
                                #"ccw" = 0,
                                "ssq" = 0)

for (i in 1:6){
  comp_perf_overview_cov[1, (i+1)] = round(sum(perf_overview_cov[[i+5]] >= perf_overview_cov[[5]], na.rm = TRUE) / (nrow(perf_overview_cov)),4)
  comp_perf_overview_cov[2, (i+1)] = round(sum(perf_overview_cov[[i+5]] > perf_overview_cov[[5]], na.rm = TRUE) / (nrow(perf_overview_cov)),4)
  comp_perf_overview_cov[3, (i+1)] = round(sum(perf_overview_cov[[i+5]] < perf_overview_cov[[5]], na.rm = TRUE) / (nrow(perf_overview_cov)),4)
  comp_perf_overview_cov[4, (i+1)] = round(mean((perf_overview_cov[[i+5]] - perf_overview_cov[[5]]), na.rm = TRUE),4)
  comp_perf_overview_cov[5, (i+1)] = round(sd(perf_overview_cov[[i+5]] - perf_overview_cov[[5]], na.rm = TRUE),4)
}

print(perf_overview_cov)
print(comp_perf_overview_cov)

path_perf_overview_csv = paste0("/Results/perf_overview_cov r=", toString(target_cor), ".csv")
path_perf_overview_RData = paste0("/Results/perf_overview_cov r=", toString(target_cor), ".RData")
path_comp_perf_overview_csv = paste0("/Results/comp_perf_overview_cov r=", toString(target_cor), ".csv")
path_comp_perf_overview_RData = paste0("/Results/comp_perf_overview_cov r=", toString(target_cor), ".RData")

write.csv(perf_overview_cov, paste0(getwd(), path_perf_overview_csv))
saveRDS(perf_overview_cov, file = paste0(getwd(), path_perf_overview_RData))
write.csv(comp_perf_overview_cov, paste0(getwd(), path_comp_perf_overview_csv))
saveRDS(comp_perf_overview_cov, file = paste0(getwd(), path_comp_perf_overview_RData))

plot_selection_data = pivot_longer(perf_overview_cov, cols = hpp:ssq, names_to = "Metric", values_to = "Achieved_Number")
