# DESCRIPTION

# Produce the tables that are shown in "Data"-section of the main document

################################################################

for (data_list in data_lists){
  data = data_list

  data_analysis = data.table(
    "Metric" = c("Customers", "Transactions", "Period length", 
                 "Average number of rep. purchases per customer", 
                 "Standard deviation of rep. purchases", 
                 "Zero repeaters", "First entry date", "Last entry date"),
    "Old learning" = 0,
    "Old holdout" = 0,
    "Total old" = 0,
    "New learning" = 0,
    "New holdout" = 0,
    "Total new" = 0
  )
  
  data_analysis[1,4] = length(unique(data$data1$Id))
  data_analysis[1,7] = length(unique(data$data2$Id))
  data_analysis[2,2] = nrow(data$data1[Date < (min(data$data1$Date) + data$l1 * 7)])
  data_analysis[2,3] = nrow(data$data1[(Date >= (min(data$data1$Date) + data$l1 * 7)) & (Date <= (min(data$data1$Date) + (data$l1 + data$p1) * 7))])
  data_analysis[2,4] = as.numeric(data_analysis[2,2]) + as.numeric(data_analysis[2,3])
  data_analysis[2,5] = nrow(data$data2[Date < (min(data$data2$Date) + data$l2 * 7)])
  data_analysis[2,6] = nrow(data$data2[(Date >= (min(data$data2$Date) + data$l2 * 7)) & (Date <= (min(data$data2$Date) + (data$l2 + data$p2) * 7))])
  data_analysis[2,7] = as.numeric(data_analysis[2,5]) + as.numeric(data_analysis[2,6])
  data_analysis[3,2] = data$l1
  data_analysis[3,3] = data$p1
  data_analysis[3,4] = data$l1 + data$p1
  data_analysis[3,5] = data$l2
  data_analysis[3,6] = data$p2
  data_analysis[3,7] = data$l2 + data$p2
  data_analysis[4,2] = round(data_analysis[2,2] / data_analysis[1,4],2)
  data_analysis[4,3] = round(data_analysis[2,3] / data_analysis[1,4],2)
  data_analysis[4,4] = round(data_analysis[2,4] / data_analysis[1,4],2)
  data_analysis[4,5] = round(data_analysis[2,5] / data_analysis[1,7],2)
  data_analysis[4,6] = round(data_analysis[2,6] / data_analysis[1,7],2)
  data_analysis[4,7] = round(data_analysis[2,7] / data_analysis[1,7],2)
  
  rep_purchases = data$data1[Date < min(data$data1$Date) + data$l1 * 7]
  rep_purchases = rep_purchases[, .N, by = Id]
  rep_purchases$N = rep_purchases$N - 1
  data_analysis[5,2] = round(sd(rep_purchases$N),2)
  
  rep_purchases = data$data1[Date >= min(data$data1$Date) + data$l1 * 7 & Date <= min(data$data1$Date) + (data$l1 + data$p1) * 7]
  rep_purchases = rep_purchases[, .N, by = Id]
  rep_purchases$N = rep_purchases$N - 1
  data_analysis[5,3] = round(sd(rep_purchases$N),2)
  
  rep_purchases = data$data1[Date <= min(data$data1$Date) + (data$l1 + data$p1) * 7]
  rep_purchases = rep_purchases[,.N, by = Id]
  rep_purchases$N = rep_purchases$N - 1
  data_analysis[5,4] = round(sd(rep_purchases$N),2)
  
  rep_purchases = data$data2[Date < min(data$data2$Date) + data$l2 * 7]
  rep_purchases = rep_purchases[, .N, by = Id]
  rep_purchases$N = rep_purchases$N - 1
  data_analysis[5,5] = round(sd(rep_purchases$N),2)
  
  rep_purchases = data$data2[Date >= min(data$data2$Date) + data$l2 * 7 & Date <= min(data$data2$Date) + (data$l2 + data$p2) * 7]
  rep_purchases = rep_purchases[, .N, by = Id]
  rep_purchases$N = rep_purchases$N - 1
  data_analysis[5,6] = round(sd(rep_purchases$N),2)
  
  rep_purchases = data$data2[Date <= min(data$data2$Date) + (data$l2 + data$p2) * 7]
  rep_purchases = rep_purchases[,.N, by = Id]
  rep_purchases$N = rep_purchases$N - 1
  data_analysis[5,7] = round(sd(rep_purchases$N),2)
  
  # Zero repeaters
  rep_purchases = data$data1[Date < min(data$data1$Date) + data$l1 * 7]
  rep_purchases = rep_purchases[, .N, by = Id]
  data_analysis[6,2] = nrow(rep_purchases[N == 1])
  
  rep_purchases = data$data1[Date >= min(data$data1$Date) + data$l1 * 7 & Date <= min(data$data1$Date) + (data$l1 + data$p1) * 7]
  rep_purchases = rep_purchases[, .N, by = Id]
  data_analysis[6,3] = nrow(rep_purchases[N == 1])
  
  rep_purchases = data$data1[Date <= min(data$data1$Date) + (data$l1 + data$p1) * 7]
  rep_purchases = rep_purchases[,.N, by = Id]
  data_analysis[6,4] = nrow(rep_purchases[N == 1])
  
  rep_purchases = data$data2[Date < min(data$data2$Date) + data$l2 * 7]
  rep_purchases = rep_purchases[, .N, by = Id]
  data_analysis[6,5] = nrow(rep_purchases[N == 1])
  
  rep_purchases = data$data2[Date >= min(data$data2$Date) + data$l2 * 7 & Date <= min(data$data2$Date) + (data$l2 + data$p2) * 7]
  rep_purchases = rep_purchases[, .N, by = Id]
  data_analysis[6,6] = nrow(rep_purchases[N == 1])
  
  rep_purchases = data$data2[Date <= min(data$data2$Date) + (data$l2 + data$p2) * 7]
  rep_purchases = rep_purchases[,.N, by = Id]
  data_analysis[6,7] = nrow(rep_purchases[N == 1])
  
  data_analysis = data_analysis %>% mutate_all(as.character)
  
  # Entry dates
  data_analysis[7,4] = as.character(min(data$data1[, min(Date), by = Id]$V1))
  data_analysis[8,4] = as.character(max(data$data1[, min(Date), by = Id]$V1))
  data_analysis[7,7] = as.character(min(data$data2[, min(Date), by = Id]$V1))
  data_analysis[8,7] = as.character(max(data$data2[, min(Date), by = Id]$V1))

  
  print(data$name)
  print(data_analysis)
  path = paste0(getwd(), "/Results/", data$name, "_dataset_analysis.csv")
  write.csv(data_analysis, path)
}
