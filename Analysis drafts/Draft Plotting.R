# For check only

plot_data_old = data.table("Id" = results_boots$Id,
                           "mod_PI_05" = results_boots$predicted.CLV.CI.5,
                           "mod_PI_95" = results_boots$predicted.CLV.CI.95,
                           "PB_PI_05" = intervals_PB$`PB_CLV_05%`,
                           "PB_PI_95" = intervals_PB$`PB_CLV_95%`,
                           # "MB_PI_05" = intervals_MB$`MB_CLV_05%`,
                           # "MB_PI_95" = intervals_MB$`MB_CLV_95%`,
                           "CP_PI_05" = intervals_CP$Lower,
                           "CP_PI_95" = intervals_CP$Upper,
                           "mod_CLV" = results_boots$predicted.CLV
                           #"true" = new3$CLV
)

covered_mod = sum(plot_data_old$true < plot_data_old$mod_PI_95 & plot_data_old$true > plot_data_old$mod_PI_05)
covered_PB = sum(plot_data_old$true < plot_data_old$PB_PI_95 & plot_data_old$true > plot_data_old$PB_PI_05)
covered_MB = sum(plot_data_old$true < plot_data_old$MB_PI_95 & plot_data_old$true > plot_data_old$MB_PI_05)
covered_CP = sum(plot_data_old$true < plot_data_old$CP_PI_95 & plot_data_old$true > plot_data_old$CP_PI_05)

# Is any point prediction outside of any interval?
plot_data_old$YN = (plot_data_old$mod_CLV > plot_data_old$mod_PI_95) + (plot_data_old$mod_CLV > plot_data_old$PB_PI_95) + (plot_data_old$mod_CLV > plot_data_old$MB_PI_95)
plot_data_old$NY = (plot_data_old$mod_CLV < plot_data_old$mod_PI_05) + (plot_data_old$mod_CLV < plot_data_old$PB_PI_05) + (plot_data_old$mod_CLV < plot_data_old$MB_PI_05)
sum(plot_data_old$YN)
sum(plot_data_old$NY)

# Prepare data to plot

# With all methods
plot_data_CET = data.table("Id" = rep(results_boots$Id, each = 5),
                           "Method" = rep(c("PI from CLVTools","Method 1","Conformal prediction","Bayesian","Quantile regression"), 250),
                           "Low" = c(rbind(results_boots$CET.CI.5, intervals_PB$PB_CET_05, intervals_CP$CET_Lower, intervals_BA$Lower, intervals_QR$CET_lower)),
                           "High" = c(rbind(results_boots$CET.CI.95, intervals_PB$PB_CET_95, intervals_CP$CET_Upper, intervals_BA$Upper, intervals_QR$CET_upper)),
                           "CET" = rep(results_boots$CET, each = 5)
)

plot_data_PTS = data.table("Id" = rep(results_boots$Id, each = 4),
                           "Method" = rep(c("PI from CLVTools","Method 1","Conformal prediction","Quantile regression"), 250),
                           "Low" = c(rbind(0, intervals_PB$PB_CET_05, intervals_CP$CET_Lower, intervals_QR$PTS_lower)),
                           "High" = c(rbind(0, intervals_PB$PB_CET_95, intervals_CP$CET_Upper, intervals_QR$PTS_upper)),
                           "CET" = rep(results_boots$CET, each = 4)
)

# Only M1, M2 and Bootstrap
plot_data_CET = data.table("Id" = rep(results_boots$Id, each = 5),
                           "Method" = rep(c("PI from CLVTools","Method 1","Conformal prediction","Bayesian","Quantile regression"), 250),
                           "Low" = c(rbind(results_boots$CET.CI.5, intervals_PB$PB_CET_05, intervals_CP$CET_Lower, intervals_BA$Lower, intervals_QR$CET_lower)),
                           "High" = c(rbind(results_boots$CET.CI.95, intervals_PB$PB_CET_95, intervals_CP$CET_Upper, intervals_BA$Upper, intervals_QR$CET_upper)),
                           "True" = rep(results$actual.x, each = 5),
                           "CET" = rep(results_boots$CET, each = 5)
)

# Plot

print(
  ggplot(plot_data_CET[CET >= 20,], aes(as.factor(Id), CET)) +
    geom_pointrange(
      aes(ymin = Low, ymax = High, color = Method),
      position = position_dodge(0.3)
    ) +
    geom_point(aes(y = True), color = "black") +
    #geom_point(aes(y = True), color = "red") +
    labs(title = "90% PIs for customers with predicted CLV in [29,32]", x = "Customer", y = "CLV") +#+
    ylim(c(-1,100))
)








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
