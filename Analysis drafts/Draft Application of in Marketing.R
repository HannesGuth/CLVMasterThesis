# Selecting the 10% customers with the highest upward potential
# Upward potential measured by highest above
all_res = readRDS("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Backup/all_res_times.RData")
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
                           "hcw" = 0,
                           "lcw" = 0,
                           "hfw" = 0,
                           "lfw" = 0,
                           "hrw" = 0,
                           "lrw" = 0,
                           "hfxw" = 0,
                           "lfxw" = 0,
                           "hrxw" = 0,
                           "lrxw" = 0)
percentage = 0.1
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
  rf_data$Frequency = as.numeric(rf_data$Frequency)
  rf_data$Recency = as.numeric(rf_data$Recency)
  
  for (method in method_list_appl){
    run = run + 1

    application_data = all_res[[data$name]][[method]]
    application_data = merge(application_data, rf_data, by = "Id")
    max_possible = application_data[order(-application_data$CET_true),]
    perf_overview[run,3] = round(sum(max_possible$CET_true[1:n]),2)
    width_app = (application_data$CET_upper - application_data$CET_lower)/application_data$CET_prediction
    
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
    hiw_data$width = width_app
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
    hcw_data$width = width_app
    hcw_data$ratio = hcw_data$CET_prediction / hcw_data$width
    application_data$hcw = hcw_data$CET_prediction / hcw_data$width
    hcw_data = hcw_data[order(-hcw_data$ratio)]
    perf_overview[run,10] = round(sum(hcw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Highest CET/width
    lcw_data = application_data
    lcw_data$width = width_app
    lcw_data$ratio = lcw_data$CET_prediction / lcw_data$width
    application_data$lcw = lcw_data$CET_prediction / lcw_data$width
    lcw_data = lcw_data[order(lcw_data$ratio)]
    perf_overview[run,11] = round(sum(lcw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Highest frequency/width
    hfw_data = application_data
    hfw_data$width = width_app
    hfw_data$ratio = hfw_data$Frequency / hfw_data$width
    application_data$hfw = hfw_data$Frequency / hfw_data$width
    hfw_data = hfw_data[order(-hfw_data$ratio)]
    perf_overview[run,12] = round(sum(hfw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Lowest frequency/width
    lfw_data = application_data
    lfw_data$width = width_app
    lfw_data$ratio = lfw_data$Frequency / lfw_data$width
    application_data$lfw = lfw_data$Frequency / lfw_data$width
    lfw_data = lfw_data[order(lfw_data$ratio)]
    perf_overview[run,13] = round(sum(lfw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Highest recency per width
    hrw_data = application_data
    hrw_data$width = width_app
    hrw_data$ratio = hrw_data$Recency / hrw_data$width
    application_data$hrw = hrw_data$Frequency / hrw_data$width
    hrw_data = hrw_data[order(-hrw_data$ratio)]
    perf_overview[run,14] = round(sum(hrw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Lowest recency per width
    lrw_data = application_data
    lrw_data$width = width_app
    lrw_data$ratio = lrw_data$Recency / lrw_data$width
    application_data$lrw = lrw_data$Frequency / lrw_data$width
    lrw_data = lrw_data[order(lrw_data$ratio)]
    perf_overview[run,15] = round(sum(lrw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    ###
    
    # Highest frequency * width
    hfxw_data = application_data
    hfxw_data$width = width_app
    hfxw_data$ratio = hfxw_data$Frequency * hfxw_data$width
    application_data$hfw = hfxw_data$Frequency * hfw_data$width
    hfxw_data = hfw_data[order(-hfxw_data$ratio)]
    perf_overview[run,16] = round(sum(hfxw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Lowest frequency/width
    lfxw_data = application_data
    lfxw_data$width = width_app
    lfxw_data$ratio = lfxw_data$Frequency * lfxw_data$width
    application_data$lfxw = lfxw_data$Frequency * lfxw_data$width
    lfxw_data = lfxw_data[order(lfxw_data$ratio)]
    perf_overview[run,17] = round(sum(lfxw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Highest recency per width
    hrxw_data = application_data
    hrxw_data$width = width_app
    hrxw_data$ratio = hrxw_data$Recency * hrxw_data$width
    application_data$hrxw = hrxw_data$Frequency * hrxw_data$width
    hrxw_data = hrxw_data[order(-hrxw_data$ratio)]
    perf_overview[run,18] = round(sum(hrxw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    # Lowest recency per width
    lrxw_data = application_data
    lrxw_data$width = width_app
    lrxw_data$ratio = lrxw_data$Recency * lrxw_data$width
    application_data$lrxw = lrxw_data$Frequency * lrxw_data$width
    lrxw_data = lrxw_data[order(lrxw_data$ratio)]
    perf_overview[run,19] = round(sum(lrxw_data$CET_true[1:n])/perf_overview[run,3],2)
    
    app_data[[run]] = application_data[order(-application_data$Frequency)]
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


