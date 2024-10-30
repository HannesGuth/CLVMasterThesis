all_res_old = readRDS("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Submission/Results - Kopie/all_res.RData")

sum(c(all_res_old$gift_results$intervals_EN == all_res$gift_results$intervals_EN), na.rm = TRUE) + sum(is.na(is.na(c(all_res_old$gift_results$intervals_EN)) == is.na(c(all_res$gift_results$intervals_EN))))
length(c(all_res_old$gift_results$intervals_BS == all_res$gift_results$intervals_BS))

reproducibility = data.table("dataset" = rep("", (length(names(all_res)) * length(names(all_res[[names(all_res)[1]]])))),
                             "item" = "",
                             "match+NA" = "",
                             "length" = 0,
                             "match" = 0,
                             "NA" = 0)
counter = 0
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

