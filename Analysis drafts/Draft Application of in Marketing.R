# Selecting the 10% customers with the highest upward potential
# Upward potential measured by highest above
method_list_appl = c("intervals_BS", "intervals_EN", "intervals_BA", "intervals_QR_m", "intervals_CP_m")
perf_overview = data.table("Data" = rep(c("gift","el","multi","apparel"), each = 5),
                           "Method" = rep(c("BS","EN","BA","QR","CP"), 4),
                           "max" = 0,
                           "hpp" = 0,
                           "hfq" = 0,
                           "hrc" = 0,
                           "hul" = 0,
                           "hiw" = 0,
                           "huu" = 0,
                           "hcw" = 0)
percentage = 0.2
run = 0
app_data = list()
for (data_list in data_lists){
  data = data_list
  n = ceiling(nrow(all_res[[data$name]]$intervals_EN)*percentage)
  
  frequency = data$data2[, .N, by = Id]
  recency = data$data2
  recency = recency[Date < min(Date) + data$l2 * 7]
  recency = recency[, max(Date), by = Id]
  recency$Diff = as.numeric((min(recency$V1) + data$l2 * 7) - recency$V1)
  recency = recency[,-2]
  rf_data = merge(frequency, recency, by = "Id")
  colnames(rf_data) = c("Id", "Frequency", "Recency")
  rf_data$Id = as.character(rf_data$Id)
  
  for (method in method_list_appl){
    run = run + 1
    
    application_data = all_res[[data$name]][[method]]
    application_data = merge(application_data, rf_data, by = "Id")
    max_possible = application_data[order(-application_data$CET_true),]
    perf_overview[run,3] = round(sum(max_possible$CET_true[1:n]),2)

    # Highest point predictor
    hpp_data = application_data[order(-application_data$CET_prediction),]
    application_data$hpp = application_data$CET_prediction
    perf_overview[run,4] = round(sum(hpp_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Highest frequency
    hfq_data = application_data[order(-application_data$Frequency),]
    application_data$hfq = application_data$Frequency
    perf_overview[run,5] = round(sum(hfq_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Highest recency
    hrc_data = application_data[order(application_data$Recency),]
    application_data$hrc = application_data$Recency
    perf_overview[run,6] = round(sum(hrc_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Highest upper limit
    hul_data = application_data[order(-application_data$CET_upper),]
    application_data$hul = application_data$CET_upper
    perf_overview[run,7] = round(sum(hul_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Highest interval width
    hiw_data = application_data
    hiw_data$width = hiw_data$CET_upper - hiw_data$CET_lower
    application_data$hiw = hiw_data$CET_upper - hiw_data$CET_lower
    hiw_data = hiw_data[order(-hiw_data$width)]
    perf_overview[run,8] = round(sum(hiw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Highest upwards uncertainty
    huu_data = application_data
    huu_data$udiff = huu_data$CET_upper - huu_data$CET_prediction
    application_data$huu = huu_data$CET_upper - huu_data$CET_prediction
    huu_data = huu_data[order(-huu_data$udiff)]
    perf_overview[run,9] = round(sum(huu_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Highest CET/width
    hcw_data = application_data
    hcw_data$width = hcw_data$CET_upper - hcw_data$CET_lower
    hcw_data$ratio = hcw_data$CET_prediction / hcw_data$width
    application_data$hcw = hcw_data$CET_prediction / hcw_data$width
    hcw_data = hcw_data[order(-hcw_data$ratio)]
    perf_overview[run,10] = round(sum(hcw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    app_data[[run]] = application_data[order(-application_data$Frequency)]
  }
}


