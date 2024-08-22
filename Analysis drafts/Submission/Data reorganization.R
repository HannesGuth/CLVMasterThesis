
datalist = c("gift", "el", "multi", "apparel")
coverage_table = data.table("Data" = rep(datalist, each = 6),
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
for (name in names(all_res)){
  for (method in methodlist){
    counter = counter + 1
    for (i in 3:12){
      coverage_table[counter, i] = as.vector(unlist(all_res[[name]]$CET_measures[,..method]))[i-2]
    }
  }
}
coverage_table$Time_rel = coverage_table$Time / (sort(coverage_table$Time)[2])
coverage_table[13,3:length(coverage_table)] = NA

coverage_table = coverage_table[order(Method)]
data_column = coverage_table$Data
coverage_table$Data = coverage_table$Method
coverage_table$Method = data_column
names(coverage_table)[1] = "Method"
names(coverage_table)[2] = "Data"

coverage_table_rounded = data.frame(coverage_table)
for (i in 1:nrow(coverage_table_rounded)){
  for (j in 3:length(coverage_table_rounded)){
    coverage_table_rounded[i,j] = round(coverage_table_rounded[i,j],4)
  }
}

write.csv(coverage_table, paste0(getwd(), "/Results/coverage_table.csv"))
