# General results

all_res_old = readRDS(paste0(getwd(), "/Results - Kopie/all_res.RData"))

sum(c(all_res_old$gift_results$intervals_EN == all_res$gift_results$intervals_EN), na.rm = TRUE) + sum(is.na(is.na(c(all_res_old$gift_results$intervals_EN)) == is.na(c(all_res$gift_results$intervals_EN))))
length(c(all_res_old$gift_results$intervals_BS == all_res$gift_results$intervals_BS))

reproducibility = data.table("dataset" = rep("", (length(names(all_res)) * length(names(all_res[[names(all_res)[1]]])))),
                             "item" = "",
                             "match+NA" = "",
                             "length" = 0,
                             "match" = 0,
                             "NA" = 0)
counter = 0
repr = list()
for (dataset in names(all_res)){
  for (item in names(all_res[[dataset]])){
    counter = counter + 1
    reproducibility[counter, 1] = dataset
    reproducibility[counter, 2] = item
    reproducibility[counter, 3] = (sum(as.vector(t(all_res[[dataset]][[item]])) == as.vector(t(all_res_old[[dataset]][[item]])), na.rm = TRUE) + sum(is.na(as.vector(t(all_res[[dataset]][[item]]))) * is.na(as.vector(t(all_res_old[[dataset]][[item]]))))) == length(as.vector(t(all_res[[dataset]][[item]])))
    reproducibility[counter, 4] = length(as.vector(t(all_res[[dataset]][[item]])))
    reproducibility[counter, 5] = sum(as.vector(t(all_res[[dataset]][[item]])) == as.vector(t(all_res_old[[dataset]][[item]])), na.rm = TRUE)
    reproducibility[counter, 6] = sum(is.na(as.vector(t(all_res[[dataset]][[item]]))) * is.na(as.vector(t(all_res_old[[dataset]][[item]]))))
  }
}
repr[["overall"]] = reproducibility

problem_items = list("CET_measures", "PTS_measures", "comp_time")
for (dataset in names(all_res)){
  for (item in problem_items){
    repr[[dataset]][[item]] = all_res[[dataset]][[item]] == all_res_old[[dataset]][[item]]
    print(paste(dataset, " - ", item))
    print(all_res[[dataset]][[item]] == all_res_old[[dataset]][[item]])
    cat("\n\n")
  }
}

# Results from "Application in marketing"
perf_overview_old = readRDS(paste0(getwd(), "/Results - Kopie/perf_overview.RData"))
comp_perf_overview_old = readRDS(paste0(getwd(), "/Results - Kopie/comp_perf_overview.RData"))

repr$perf_overview = perf_overview[,3:length(perf_overview)] == perf_overview_old[,3:length(perf_overview)]
repr$comp_perf_overerview = comp_perf_overview[,2:length(comp_perf_overview)] == comp_perf_overview_old[,2:length(comp_perf_overview_old)]

saveRDS(repr, file = paste0(getwd(), "/Results/Reproducibility.RData"))