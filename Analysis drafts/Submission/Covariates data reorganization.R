
datalist = c("gift", "el", "multi", "apparel")
coverage_table_cov = data.table("Data" = rep(datalist, each = 6),
                            "Method" = rep(methodlist, rep = 4),
                            "PICP" = 0,
                            "ACE" = 0,
                            "PICPW" = 0,
                            "PIARW" = 0,
                            "PIARWW" = 0,
                            "MSIS" = 0,
                            "SWR" = 0,
                            "Upper coverage" = 0,
                            "Lower coverage" = 0,
                            "Time" = 0)
extracted_columns = 0
counter = 0
for (name in names(all_res_cov)){
  for (method in methodlist){
    counter = counter + 1
    for (i in 3:12){
      coverage_table_cov[counter, i] = as.vector(unlist(all_res_cov[[name]]$CET_measures[,..method]))[i-2]
    }
  }
}
coverage_table_cov$Time_rel = coverage_table_cov$Time / (sort(coverage_table_cov$Time)[2])
# coverage_table_cov[13,3:length(coverage_table_cov)] = NA

method_order = c("BS", "EN", "BA", "QR", "CP", "CR")
coverage_table_cov = coverage_table_cov[order(factor(coverage_table_cov$Method, levels = method_order)), ]
# coverage_table_cov = coverage_table_cov[order(Method)]
data_column = coverage_table_cov$Data
coverage_table_cov$Data = coverage_table_cov$Method
coverage_table_cov$Method = data_column
names(coverage_table_cov)[1] = "Method"
names(coverage_table_cov)[2] = "Data"

coverage_table_cov_rounded = data.frame(coverage_table_cov)
for (i in 1:nrow(coverage_table_cov_rounded)){
  for (j in 3:length(coverage_table_cov_rounded)){
    coverage_table_cov_rounded[i,j] = round(coverage_table_cov_rounded[i,j],4)
  }
}

write.csv(coverage_table_cov_rounded, paste0(getwd(), "/Results/coverage_table_cov_rounded.csv"))
write.csv(coverage_table_cov, paste0(getwd(), "/Results/coverage_table_cov.csv"))
