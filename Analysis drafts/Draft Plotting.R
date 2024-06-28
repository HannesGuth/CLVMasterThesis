# # For check only
# 
# plot_data_old = data.table("Id" = results_boots$Id,
#                            "mod_PI_05" = results_boots$predicted.CLV.CI.5,
#                            "mod_PI_95" = results_boots$predicted.CLV.CI.95,
#                            "PB_PI_05" = intervals_PB$`PB_CLV_05%`,
#                            "PB_PI_95" = intervals_PB$`PB_CLV_95%`,
#                            # "MB_PI_05" = intervals_MB$`MB_CLV_05%`,
#                            # "MB_PI_95" = intervals_MB$`MB_CLV_95%`,
#                            "CP_PI_05" = intervals_CP$Lower,
#                            "CP_PI_95" = intervals_CP$Upper,
#                            "mod_CLV" = results_boots$predicted.CLV
#                            #"true" = new3$CLV
# )
# 
# covered_mod = sum(plot_data_old$true < plot_data_old$mod_PI_95 & plot_data_old$true > plot_data_old$mod_PI_05)
# covered_PB = sum(plot_data_old$true < plot_data_old$PB_PI_95 & plot_data_old$true > plot_data_old$PB_PI_05)
# covered_MB = sum(plot_data_old$true < plot_data_old$MB_PI_95 & plot_data_old$true > plot_data_old$MB_PI_05)
# covered_CP = sum(plot_data_old$true < plot_data_old$CP_PI_95 & plot_data_old$true > plot_data_old$CP_PI_05)
# 
# # Is any point prediction outside of any interval?
# plot_data_old$YN = (plot_data_old$mod_CLV > plot_data_old$mod_PI_95) + (plot_data_old$mod_CLV > plot_data_old$PB_PI_95) + (plot_data_old$mod_CLV > plot_data_old$MB_PI_95)
# plot_data_old$NY = (plot_data_old$mod_CLV < plot_data_old$mod_PI_05) + (plot_data_old$mod_CLV < plot_data_old$PB_PI_05) + (plot_data_old$mod_CLV < plot_data_old$MB_PI_05)
# sum(plot_data_old$YN)
# sum(plot_data_old$NY)

# Prepare data to plot

# With all methods
plot_data_CET = data.table("Id" = rep(intervals_BS$Id, each = 5),
                           "Method" = rep(c("BS","EN","CP","BA","QR"), length(unique(data2$Id))),
                           "Low" = c(rbind(intervals_BS$CET_lower, intervals_PB$CET_lower, intervals_CP_m$CET_lower, intervals_BA$CET_lower, intervals_QR_m$CET_lower)),
                           "High" = c(rbind(intervals_BS$CET_upper, intervals_PB$CET_upper, intervals_CP_m$CET_upper, intervals_BA$CET_upper, intervals_QR_m$CET_upper)),
                           "CET" = rep(intervals_BS$CET_prediction, each = 5),
                           "True" = rep(intervals_BS$CET_true, each = 5)
)

plot_data_PTS = data.table("Id" = rep(results_boots$Id, each = 3),
                           "Method" = rep(c("EN","CP","QR"), length(unique(data2$Id))),
                           "Low" = c(rbind(intervals_PB$PTS_lower, intervals_CP_m$PTS_lower, intervals_QR_m$PTS_lower)),
                           "High" = c(rbind(intervals_PB$PTS_upper, intervals_CP_m$PTS_upper, intervals_QR_m$PTS_upper)),
                           "PTS" = rep(results_boots$predicted.total.spending, each = 3)
)

# # Only M1, M2 and Bootstrap
# plot_data_CET = data.table("Id" = rep(results_boots$Id, each = 5),
#                            "Method" = rep(c("PI from CLVTools","Method 1","Conformal prediction","Bayesian","Quantile regression"), 250),
#                            "Low" = c(rbind(results_boots$CET.CI.5, intervals_PB$PB_CET_05, intervals_CP$CET_Lower, intervals_BA$Lower, intervals_QR$CET_lower)),
#                            "High" = c(rbind(results_boots$CET.CI.95, intervals_PB$PB_CET_95, intervals_CP$CET_Upper, intervals_BA$Upper, intervals_QR$CET_upper)),
#                            "True" = rep(results$actual.x, each = 5),
#                            "CET" = rep(results_boots$CET, each = 5)
# )

# Interval length plot
print(
  ggplot(plot_data_CET[CET <= 0.09531861 & CET >= 0.09531859,], aes(as.factor(Id), CET)) +
    geom_pointrange(
      aes(ymin = Low, ymax = High, color = Method),
      position = position_dodge(0.3)
    ) +
    geom_point(aes(y = True), color = "black") +
    labs(title = "90% PIs", x = "Customer", y = "CET")
)

# Ranking
names = CET_measures$Measure
#CET_measures = CET_measures[,-1]
ranking_table = data.table(t(as.matrix(CET_measures[,-1])))
colnames(ranking_table) = names
ranking_table$PICP = -ranking_table$PICP
ranking_table$ACE = abs(ranking_table$ACE)
ranking_table$`Upper coverage` = -ranking_table$`Upper coverage`
ranking_table$`Lower coverage` = -ranking_table$`Lower coverage`
ranking_table$Bias = abs(ranking_table$Bias)
ranking_table$SWR = -ranking_table$SWR
apply(ranking_table,2,rank, ties.method="average")

# Desired achieved plot
methods = c("BS","EN","CP","BA","QR")
daplot_data = data.table("Methods" = methods,
                         t(as.matrix(CET_measures[,-1])))
daplot_data$Methods = reorder(daplot_data$Methods, daplot_data$V1)

ggplot(data = daplot_data, aes(x = Methods, y = V1, label = round(V1,2))) +
  geom_point() +
  geom_texthline(yintercept = 0.9, label = "90%",
                 hjust = 0.2, color = "red4") +
  geom_text(hjust=-0.2, vjust=-0.2) +
  labs(title = "Achieved coverage",
       y = "Coverage")

# Coverage CLV
cdev_data = data.table("Id" = intervals_BS$Id,
                       "BS_covered" = ksmooth(intervals_BS$CET_true, as.numeric(intervals_BS$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "EN_covered" = ksmooth(intervals_PB$CET_true, as.numeric(intervals_PB$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "BA_covered" = ksmooth(intervals_BA$CET_true, as.numeric(intervals_BA$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "CP_covered" = ksmooth(intervals_CP_m$CET_true, as.numeric(intervals_CP_m$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "QR_covered" = ksmooth(intervals_QR_m$CET_true, as.numeric(intervals_QR_m$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "true"= intervals_BS$CET_true,
                       "true_kernel" = ksmooth(intervals_BA$CET_true, as.numeric(intervals_BA$CET_covered), kernel = "normal", bandwidth = 2)$x
                       )
ggplot(data = cdev_data, aes(x = true_kernel)) +
  geom_line(aes(y = BS_covered, color = "BS")) +
  geom_line(aes(y = EN_covered, color = "EN")) +
  geom_line(aes(y = BA_covered, color = "BA")) +
  geom_line(aes(y = CP_covered, color = "CP")) +
  geom_line(aes(y = QR_covered, color = "QR")) +
  scale_color_manual(values = c("BS" = "purple", 
                                "EN" = "yellow", 
                                "BA" = "black", 
                                "CP" = "green", 
                                "QR" = "red")) +
  labs(title = "Coverage development with CET",
       y = "Coverage",
       x = "CET")


cdev_data = cdev_data[order(cdev_data$true),]
plot(ksmooth(intervals_BA$CET_true, as.numeric(intervals_BA$CET_covered), kernel = "normal", bandwidth = 2))
plot(frollmean(cdev_data$BA_covered, 100))

# Residuals
res_data = data.table("Id" = intervals_BS$Id,
                      "BS" = ifelse(!(intervals_BS$CET_covered), (intervals_BS$CET_true < intervals_BS$CET_lower) * abs(intervals_BS$CET_true - intervals_BS$CET_lower) +
                                                                 (intervals_BS$CET_true > intervals_BS$CET_upper) * abs(intervals_BS$CET_true - intervals_BS$CET_upper), NA) / (intervals_BS$CET_upper + intervals_BS$CET_lower)/2,
                      "EN" = ifelse(!(intervals_PB$CET_covered), (intervals_PB$CET_true < intervals_PB$CET_lower) * abs(intervals_PB$CET_true - intervals_PB$CET_lower) +
                                                                 (intervals_PB$CET_true > intervals_PB$CET_upper) * abs(intervals_PB$CET_true - intervals_PB$CET_upper), NA) / (intervals_PB$CET_upper + intervals_PB$CET_lower)/2,
                      "BA" = ifelse(!(intervals_BA$CET_covered), (intervals_BA$CET_true < intervals_BA$CET_lower) * abs(intervals_BA$CET_true - intervals_BA$CET_lower) +
                                                                 (intervals_BA$CET_true > intervals_BA$CET_upper) * abs(intervals_BA$CET_true - intervals_BA$CET_upper), NA) / (intervals_BA$CET_upper + intervals_BA$CET_lower)/2,
                      "CP" = ifelse(!(intervals_CP_m$CET_covered), (intervals_CP_m$CET_true < intervals_CP_m$CET_lower) * abs(intervals_CP_m$CET_true - intervals_CP_m$CET_lower) +
                                                                   (intervals_CP_m$CET_true > intervals_CP_m$CET_upper) * abs(intervals_CP_m$CET_true - intervals_CP_m$CET_upper), NA) / (intervals_CP_m$CET_upper + intervals_CP_m$CET_lower)/2,
                      "QR" = ifelse(!(intervals_QR_m$CET_covered), (intervals_QR_m$CET_true < intervals_QR_m$CET_lower) * abs(intervals_QR_m$CET_true - intervals_QR_m$CET_lower) +
                                                                   (intervals_QR_m$CET_true > intervals_QR_m$CET_upper) * abs(intervals_QR_m$CET_true - intervals_QR_m$CET_upper), NA) / (intervals_QR_m$CET_upper + intervals_QR_m$CET_lower)/2
)

ggplot(data = res_data) +
  geom_density(aes(x = BS, color = "BS")) +
  geom_density(aes(x = EN, color = "EN")) +
  geom_density(aes(x = BA, color = "BA")) +
  geom_density(aes(x = CP, color = "CP")) +
  geom_density(aes(x = QR, color = "QR")) +
  scale_color_manual(values = c("BS" = "purple",
                                "EN" = "yellow",
                                "BA" = "black",
                                "CP" = "green",
                                "QR" = "red")) +
  xlim(0,2) +
  ylim(0,10) +
  labs(title = "Absolute scaled residuals distribution",
       x = "Scaled residual")

# Rectangles
#methods = c("BS","EN","CP","BA","QR")
#methods = c("CP","BA","QR")
methods = c("BS","EN")

rec_data1 = data.table("Id" = intervals_BS$Id,
                      "BS" = (intervals_BS$CET_true - intervals_BS$CET_lower) / (intervals_BS$CET_upper - intervals_BS$CET_lower),
                      "EN" = (intervals_PB$CET_true - intervals_PB$CET_lower) / (intervals_PB$CET_upper - intervals_PB$CET_lower),
                      "BA" = (intervals_BA$CET_true - intervals_BA$CET_lower) / (intervals_BA$CET_upper - intervals_BA$CET_lower),
                      "CP" = (intervals_CP_m$CET_true - intervals_CP_m$CET_lower) / (intervals_CP_m$CET_upper - intervals_CP_m$CET_lower),
                      "QR" = (intervals_QR_m$CET_true - intervals_QR_m$CET_lower) / (intervals_QR_m$CET_upper - intervals_QR_m$CET_lower))

# rec_data = data.table("Method" = rep(methods, 5079),
#                       "Values" = c(rec_data1$BS, rec_data1$EN, rec_data1$BA, rec_data1$CP, rec_data1$QR))

rec_data <- rec_data1[,2:3] %>%
  pivot_longer(cols = everything(), names_to = "Method", values_to = "Value")

ggplot(rec_data, aes(x = Method, y = Value)) +
  geom_rect(aes(xmin = 0.6, xmax = 2.5, ymin = 0, ymax = 1), alpha = 0.9) +
  geom_half_violin(trim = TRUE, aes(fill = Method), side = "r") +
  labs(title = "Density covered by interval",
       y = "Position with respect to PI") +
  ylim(-20,30)
  

# Width-CET
width_data = data.table("Id" = intervals_BS$Id,
                      "BS_width" = ksmooth(intervals_BS$CET_true, intervals_BS$CET_upper - intervals_BS$CET_lower, kernel = "normal", bandwidth = 2)$y,
                      "EN_width" = ksmooth(intervals_PB$CET_true, intervals_PB$CET_upper - intervals_PB$CET_lower, kernel = "normal", bandwidth = 2)$y,
                      "BA_width" = ksmooth(intervals_BA$CET_true, intervals_BA$CET_upper - intervals_BA$CET_lower, kernel = "normal", bandwidth = 2)$y,
                      "CP_width" = ksmooth(intervals_CP_m$CET_true, intervals_CP_m$CET_upper - intervals_CP_m$CET_lower, kernel = "normal", bandwidth = 2)$y,
                      "QR_width" = ksmooth(intervals_QR_m$CET_true, intervals_QR_m$CET_upper - intervals_QR_m$CET_lower, kernel = "normal", bandwidth = 2)$y,
                      "true"= intervals_BS$CET_true,
                      "true_kernel" = ksmooth(intervals_BA$CET_true, intervals_CP_m$CET_upper - intervals_CP_m$CET_lower, kernel = "normal", bandwidth = 2)$x
)

ggplot(data = width_data, aes(x = true_kernel)) +
  geom_line(aes(y = BS_width, color = "BS")) +
  geom_line(aes(y = EN_width, color = "EN")) +
  geom_line(aes(y = BA_width, color = "BA")) +
  geom_line(aes(y = CP_width, color = "CP")) +
  geom_line(aes(y = QR_width, color = "QR")) +
  scale_color_manual(values = c("BS" = "purple", 
                                "EN" = "yellow", 
                                "BA" = "black", 
                                "CP" = "green", 
                                "QR" = "red")) +
  labs(title = "Width development with CET",
       y = "Width",
       x = "CET") +
  xlim(0,20)


# #########################################
# # Not changed yet from CLV to CET and PTS
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Plotting the mspiw plot
# plot_data_mspiw = data.table("lower_BS" = results_boots$predicted.CLV.CI.5,
#                              "upper_BS" = results_boots$predicted.CLV.CI.95,
#                              # "lower_M1" = intervals_MB$`MB_CLV_05%`,
#                              # "upper_M1" = intervals_MB$`MB_CLV_95%`,
#                              "lower_M2" = intervals_PB$`PB_CLV_05%`,
#                              "upper_M2" = intervals_PB$`PB_CLV_95%`,
#                              "lower_CP" = intervals_CP$Lower,
#                              "upper_CP" = intervals_CP$Upper,
#                              "CLV" = results_boots$predicted.CLV)
# 
# plot_data_mspiw$width_BS = (plot_data_mspiw$upper_BS - plot_data_mspiw$lower_BS) / results_boots$predicted.CLV
# plot_data_mspiw$width_M1 = (plot_data_mspiw$upper_M1 - plot_data_mspiw$lower_M1) / results_boots$predicted.CLV
# plot_data_mspiw$width_M2 = (plot_data_mspiw$upper_M2 - plot_data_mspiw$lower_M2) / results_boots$predicted.CLV
# plot_data_mspiw$width_CP = (plot_data_mspiw$upper_CP - plot_data_mspiw$lower_CP) / results_boots$predicted.CLV
# 
# ggplot(plot_data_mspiw[CLV < 1000,], aes(x = CLV)) + 
#   geom_line(aes(y = width_BS, color = "blue")) +
#   geom_line(aes(y = width_M1, color = "red")) +
#   geom_line(aes(y = width_M2, color = "green")) +
#   geom_line(aes(y = width_CP, color = "black")) +
#   labs(title = "Absolute interval width/CLV for different CLV values and intervals methods", y = "Relative interval width") +
#   scale_color_identity(name = "Model",
#                        breaks = c("blue", "red", "green", "black"),
#                        labels = c("Bootstrap", "Method 1", "Method 2", "CP"),
#                        guide = "legend")
# 
# # Plotting the relative distribution plot
# 
# rel_data_CP = data.table("Id" = results_boots$Id,
#                          "prediction" = results_boots$predicted.CLV,
#                          "lower_PI" = plot_data_mspiw$lower_CP,
#                          "upper_PI" = plot_data_mspiw$upper_CP,
#                          "true" = benchmark_data$true,
#                          "position" = 0)
# 
# rel_data_M1 = data.table("Id" = results_boots$Id,
#                          "prediction" = results_boots$predicted.CLV,
#                          "lower_PI" = plot_data_mspiw$lower_M1,
#                          "upper_PI" = plot_data_mspiw$upper_M1,
#                          "true" = benchmark_data$true,
#                          "position" = 0)
# 
# rel_data_CP$position = (rel_data_CP$true - rel_data_CP$lower_PI) / (rel_data_CP$upper_PI - rel_data_CP$lower_PI)
# rel_data_M1$position = (rel_data_M1$true - rel_data_M1$lower_PI) / (rel_data_M1$upper_PI - rel_data_M1$lower_PI)
# 
# ggplot(rel_data_CP, aes(x = position)) +
#   geom_histogram(binwidth = 0.05) +
#   geom_vline(xintercept = 0) +
#   geom_vline(xintercept = 1) +
#   labs(title = "Distribution of true observations relative to their PIs",
#        x = "0: On the lower boundary, 1: On the upper boundary, outside: Outside of the PIs")
# 
# ggplot(rel_data_M1, aes(x = position)) +
#   geom_histogram(binwidth = 0.05) +
#   geom_vline(xintercept = 0) +
#   geom_vline(xintercept = 1) +
#   labs(title = "Distribution of true observations relative to their PIs",
#        x = "0: On the lower boundary, 1: On the upper boundary, outside: Outside of the PIs")
