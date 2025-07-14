library(CLVTools)
library(data.table)
library(compiler)
library(ggplot2)
library(profvis)
library(rockchalk)
library(doParallel)

s1 = c(130,180,210,230)
s2 = c(110,130,180,210)
s3 = c(0,20,40)
comparison = expand.grid(s1,s2,s3,s3)
comparison = comparison[order(comparison$Var1, comparison$Var3, comparison$Var2, comparison$Var4), ]
comparison$CET_CP_perf = 0
comparison$PTS_CP_perf = 0
comparison$CET_QR_perf = 0
comparison$PTS_QR_perf = 0
comparison$CET_quantile = 0
comparison$PTS_quantile = 0
comparison$CET_Lr = 0
comparison$CET_La = 0
comparison$CET_Ls = 0
comparison$CET_Lb = 0
comparison$CET_Ur = 0
comparison$CET_Ua = 0
comparison$CET_Us = 0
comparison$CET_Ub = 0
comparison$PTS_Lr = 0
comparison$PTS_La = 0
comparison$PTS_Ls = 0
comparison$PTS_Lb = 0
comparison$PTS_Ur = 0
comparison$PTS_Ua = 0
comparison$PTS_Us = 0
comparison$PTS_Ub = 0
comparison
compcounter = 0
end1 = 0
end2 = 0

for (training_index in 1:length(s1)){
  splitweek1 = s1[training_index]
  for (training_index_end in 1:length(s3)){
    end1 = s3[training_index_end]
    source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis Drafts/Robustness splitweek/Draft CP managerial gift 1.r")
    print(paste("Draft CP managerial gift 1 done", "-----------", "splitweek1:", splitweek1, "splitweek2:", splitweek2, "end1:", end1, "end2:", end2))
    source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis Drafts/Robustness splitweek/Draft QR managerial gift 1.r")
    print(paste("Draft QR managerial gift 1 done", "-----------", "splitweek1:", splitweek1, "splitweek2:", splitweek2, "end1:", end1, "end2:", end2))
    for (validation_index in 1:length(s2)){
      splitweek2 = s2[validation_index]
      for (validation_index_end in 1:length(s3)){
        compcounter = compcounter + 1
        end2 = s3[validation_index_end]
        source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis Drafts/Robustness splitweek/Draft CP managerial gift 2.r")
        print(paste("Draft CP managerial gift 2 done", "-----------", "splitweek1:", splitweek1, "splitweek2:", splitweek2, "end1:", end1, "end2:", end2))
        source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis Drafts/Robustness splitweek/Draft QR managerial gift 2.r")
        print(paste("Draft QR managerial gift 2 done", "-----------", "splitweek1:", splitweek1, "splitweek2:", splitweek2, "end1:", end1, "end2:", end2))
        #print(paste("splitweek1", splitweek1, "   - splitweek2", splitweek2))
        print(comparison)
        # comparison[compcounter,6] = splitweek1
        # comparison[compcounter,7] = splitweek2
        # comparison[compcounter,8] = end1
        # comparison[compcounter,9] = end2
      }
    }
  }
}

comparison_gift = comparison
write.csv(comparison_gift, "D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Robustness splitweek/comparison_gift.csv")

