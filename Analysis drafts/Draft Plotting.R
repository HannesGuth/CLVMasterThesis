# For check only

plot_data_old = data.table("Id" = results_boots$Id,
                           "mod_PI_05" = results_boots$predicted.CLV.CI.5,
                           "mod_PI_95" = results_boots$predicted.CLV.CI.95,
                           "PB_PI_05" = intervals_PB$`PB_CLV_05%`,
                           "PB_PI_95" = intervals_PB$`PB_CLV_95%`,
                           "MB_PI_05" = intervals_MB$`MB_CLV_05%`,
                           "MB_PI_95" = intervals_MB$`MB_CLV_95%`,
                           "CP_PI_05" = intervals_CP$Lower,
                           "CP_PI_95" = intervals_CP$Upper,
                           "mod_CLV" = results_boots$predicted.CLV,
                           "true" = new3$CLV
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
plot_data = data.table("Id" = rep(results_boots$Id, each = 4),
                       "Method" = rep(c("PI from CLVTools","Method 1","Method 2","Conformal prediction"), n = 250),
                       "Low" = c(rbind(results_boots$predicted.CLV.CI.5, intervals_PB$`PB_CLV_05%`, intervals_MB$`MB_CLV_05%`, intervals_CP$Lower)),
                       "High" = c(rbind(results_boots$predicted.CLV.CI.95, intervals_PB$`PB_CLV_95%`, intervals_MB$`MB_CLV_95%`, intervals_CP$Upper)),
                       "CLV" = rep(results_boots$predicted.CLV, each = 4)
)

# Only M1, M2 and Bootstrap
plot_data = data.table("Id" = rep(results_boots$Id, each = 3),
                       "Method" = rep(c("PI from CLVTools","Method 1","Method 2"), n = 250),
                       "Low" = c(rbind(results_boots$predicted.CLV.CI.5, intervals_PB$`PB_CLV_05%`, intervals_MB$`MB_CLV_05%`)),
                       "High" = c(rbind(results_boots$predicted.CLV.CI.95, intervals_PB$`PB_CLV_95%`, intervals_MB$`MB_CLV_95%`)),
                       "CLV" = rep(results_boots$predicted.CLV, each = 3)
)

# Plot

ggplot(plot_data[plot_data$CLV >= 0 & plot_data$CLV <= 2,], aes(as.factor(Id), CLV)) +
  geom_pointrange(
    aes(ymin = Low, ymax = High, color = Method),
    position = position_dodge(0.3)
  ) +
  labs(title = "90% PIs for customers with predicted CLV in [29,32]", x = "Customer", y = "CLV")
