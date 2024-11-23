method_colors_all = c("BS" = "black", "EN" = "grey", "BA" = "green", "CP" = "red", "CR" = "yellow", "QR" = "blue")
method_colors_sel = c("EN" = "grey", "BA" = "green", "CP" = "red", "CR" = "yellow", "QR" = "blue")

plot_coverage_data = merge(x = coverage_table[, c("Method", "PICP", "PIARW", "Data")], y = ranking_table[, c("Method", "Rank_PICP")], by = "Method")
plot_coverage_data = plot_coverage_data[order(-Rank_PICP)]
plot_coverage_data$Method <- factor(plot_coverage_data$Method, levels = unique(plot_coverage_data$Method))

plot_width_data = data.table()

title = "PICP by Method and Data Set"
ggplot(plot_coverage_data, aes(x = Method, y = PICP*100, shape = Data)) +
  geom_point(size = 2, color = "black") +  # Set color for all points
  geom_hline(yintercept = 90, linetype = "dashed", color = "red") +  # Horizontal line at 90%
  annotate("text", x = Inf, y = 90, label = "90%", hjust = 15, vjust = -0.5, color = "red") +  # Add label for horizontal line
  labs(title = title,
       x = "Method",
       y = "PICP in %",
       shape = "Data Set") +
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "white", size = 0.5))

ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)

title = "PICP and PIARW by Method and Data Set"
ggplot(plot_coverage_data) +
  geom_point(aes(x = Method, y = PIARW*2, shape = Data), size = 2, color = "blue", position = position_nudge(0.15)) +  # Line for PIARW
  geom_point(aes(x = Method, y = PICP*100, shape = Data), size = 2, color = "black", position = position_nudge(-0.15)) +  # Set color for all points
  geom_hline(yintercept = 90, linetype = "dashed", color = "red") +  # Horizontal line at 90%
  annotate("text", x = Inf, y = 90, label = "90%", hjust = 13, vjust = -0.5, color = "red") +  # Add label for horizontal line
  scale_y_continuous(
    name = "PICP in %",
    sec.axis = sec_axis(~ ./2, name = "PIARW")  # Add a secondary axis for PIARW
  ) +
  labs(title = title,
       x = "Method",
       shape = "Data Set") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(colour = "blue"),
    legend.title = element_blank(),
    axis.text=element_text(size=12),
    axis.title=element_text(size=12),
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_line(colour = "white", size = 0.5))
ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)

### Absolute numbers radar chart

title = "Method performance radar chart"
averages_table_spider = averages_table[,1:11]
averages_table_spider$ACE = abs(averages_table_spider$ACE)
spider_data = as.data.frame(as.matrix(averages_table_spider[,2:11]))
#colnames(spider_data) = measure_list
spider_data_copy = copy(spider_data)
spider_data[1,] = spider_data_copy[2,]
spider_data[2,] = spider_data_copy[5,]
spider_data[3,] = spider_data_copy[1,]
spider_data[4,] = spider_data_copy[6,]
spider_data[5,] = spider_data_copy[3,]
spider_data[6,] = spider_data_copy[4,]
names(spider_data)[c(8,9)] = c("Upper c.","Lower c.")
rownames(spider_data) = methodlist
spider_data$ACE = 1 - spider_data$ACE
max_MSIS = max(spider_data$MSIS)
min_MSIS = min(spider_data$MSIS)
spider_data$MSIS = max_MSIS - spider_data$MSIS
max_PIARW = max(spider_data$PIARW)
min_PIARW = min(spider_data$PIARW)
spider_data$PIARW = max_PIARW - spider_data$PIARW
max_PIARWW = max(spider_data$PIARWW)
min_PIARWW = min(spider_data$PIARWW)
spider_data$PIARWW = max_PIARWW - spider_data$PIARWW
max_SWR = max(spider_data$SWR)
min_SWR = min(spider_data$SWR)
min_time = min(spider_data$Time, na.rm = TRUE)
max_time = max(spider_data$Time, na.rm = TRUE)
spider_data$Time = max_time - spider_data$Time
spider_data = rbind(c(1, 1, 1, max_PIARW-min_PIARW, max_PIARWW-min_PIARWW, max_MSIS, max_SWR, 1, 1, max_time),
                    c(0, 0, 0, 0, 0, 0, min_SWR, 0, 0, 0),
                    spider_data)
spider_data$Time = NULL
spider_colors = c("black", "grey", "green", "blue", "red", "yellow")
radarchart(spider_data, pcol = spider_colors, cglcol = "grey", vlcex = 1.8, plwd = 3)
legend(x = -2.0, y = 0.4, horiz = FALSE, legend = methodlist, bty = "n", pch=20 , col = spider_colors, text.col = "black", cex=1.5, pt.cex=3, x.intersp = 0.5, text.width = 0.1)
# save as image with 1000:...
ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)

spider_data_gg = spider_data[3:nrow(spider_data),]
spider_data_gg = cbind(Method = rownames(spider_data_gg), spider_data_gg)

for (data_list in data_lists){
  if (data_list$BS){
    print(data_list$name)
    
    # Overview
    plot_data_CET = data.table("Id" = rep(all_res[[data_list$name]]$intervals_EN$Id, each = 6),
                               "Method" = factor(rep(c("BS", "EN", "BA", "CP", "CR", "QR"), length(unique(all_res[[data_list$name]]$intervals_EN$Id))),
                                                 levels = c("BS", "EN", "BA", "CP", "CR", "QR")),
                               "Low" = c(rbind(all_res[[data_list$name]]$intervals_BS$CET_lower,
                                               all_res[[data_list$name]]$intervals_EN$CET_lower,
                                               all_res[[data_list$name]]$intervals_BA$CET_lower,
                                               all_res[[data_list$name]]$intervals_CP_m$CET_lower,
                                               all_res[[data_list$name]]$intervals_CR_m$CET_lower,
                                               all_res[[data_list$name]]$intervals_QR_m$CET_lower)),
                               "High" = c(rbind(all_res[[data_list$name]]$intervals_BS$CET_upper,
                                                all_res[[data_list$name]]$intervals_EN$CET_upper,
                                                all_res[[data_list$name]]$intervals_BA$CET_upper,
                                                all_res[[data_list$name]]$intervals_CP_m$CET_upper,
                                                all_res[[data_list$name]]$intervals_CR_m$CET_upper,
                                                all_res[[data_list$name]]$intervals_QR_m$CET_upper)),
                               "True" = rep(all_res[[data_list$name]]$intervals_EN$CET_true, each = 6),
                               "CET" = rep(all_res[[data_list$name]]$intervals_EN$CET_prediction, each = 6)
    )

    lower = quantile(plot_data_CET$CET, 0.9125)
    upper = quantile(plot_data_CET$CET, 0.9125)
    length(unique(plot_data_CET[CET >= lower & CET <= upper,]$Id))
    while (length(unique(plot_data_CET[CET >= lower & CET <= upper,]$Id)) <= 10){
      upper = upper + 0.00001
    }
    
    title = paste("90% PIs", data_list$name)
    ggplot(plot_data_CET[CET >= lower & CET <= upper,], aes(as.factor(Id), True)) +
      geom_linerange(
        aes(ymin = Low, ymax = High, color = Method),
        position = position_dodge(0.3),
        linewidth = 0.8) +
      geom_point(aes(y = True, color = "True"), position = position_dodge(0.3)) +
      labs(title = title, x = "Customer", y = "CET") +
      scale_color_manual(values = c(method_colors_all, "True" = "black")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
      theme(legend.title = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)
    
    cdev_data = data.table("Id" = all_res[[data_list$name]]$intervals_BS$Id,
                           "BS_covered" = ksmooth(all_res[[data_list$name]]$intervals_BS$CET_true, as.numeric(all_res[[data_list$name]]$intervals_BS$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                           "EN_covered" = ksmooth(all_res[[data_list$name]]$intervals_EN$CET_true, as.numeric(all_res[[data_list$name]]$intervals_EN$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                           "BA_covered" = ksmooth(all_res[[data_list$name]]$intervals_BA$CET_true, as.numeric(all_res[[data_list$name]]$intervals_BA$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                           "CP_covered" = ksmooth(all_res[[data_list$name]]$intervals_CP_m$CET_true, as.numeric(all_res[[data_list$name]]$intervals_CP_m$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                           "CR_covered" = ksmooth(all_res[[data_list$name]]$intervals_CR_m$CET_true, as.numeric(all_res[[data_list$name]]$intervals_CR_m$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                           "QR_covered" = ksmooth(all_res[[data_list$name]]$intervals_QR_m$CET_true, as.numeric(all_res[[data_list$name]]$intervals_QR_m$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                           "true" = all_res[[data_list$name]]$intervals_BS$CET_true,
                           "true_kernel" = ksmooth(all_res[[data_list$name]]$intervals_BA$CET_true, as.numeric(all_res[[data_list$name]]$intervals_BA$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$x
    )
    
    # Coverage development with CET
    title = paste("Coverage development with CET for", data_list$name)
    print(ggplot(data = cdev_data, aes(x = true_kernel)) +
            geom_line(aes(y = BS_covered*100, color = "BS")) +
            geom_line(aes(y = EN_covered*100, color = "EN")) +
            geom_line(aes(y = BA_covered*100, color = "BA")) +
            geom_line(aes(y = CP_covered*100, color = "CP")) +
            geom_line(aes(x = true_kernel + 0.05, y = CR_covered*100, color = "CR")) +
            geom_line(aes(y = QR_covered*100, color = "QR")) +
      scale_color_manual(values = method_colors_all) +
      labs(title = title,
           y = "Coverage in %",
           x = "True transactions",
           color = "Method")) +
      theme(axis.text.x = element_text(size=rel(1.7)),
            axis.text.y = element_text(size=rel(1.7)),
            axis.title.x = element_text(size=rel(1.7)),
            axis.title.y = element_text(size=rel(1.7)),
            plot.title = element_text(size=rel(1.7)),
            #legend.title = element_blank(),
            legend.position="none",
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 4.5)
    
    # Relative error
    mperf_data = data.table("Id" = all_res[[data_list$name]]$intervals_EN$Id,
                            "Pred" = all_res[[data_list$name]]$intervals_EN$CET_prediction,
                            "True" = all_res[[data_list$name]]$intervals_EN$CET_true,
                            "True_rel" = (all_res[[data_list$name]]$intervals_EN$CET_true)/max(all_res[[data_list$name]]$intervals_EN$CET_true),
                            "Diff_abs" = all_res[[data_list$name]]$intervals_EN$CET_prediction - all_res[[data_list$name]]$intervals_EN$CET_true,
                            "Diff_rel" = (all_res[[data_list$name]]$intervals_EN$CET_prediction - all_res[[data_list$name]]$intervals_EN$CET_true)/all_res[[data_list$name]]$intervals_EN$CET_prediction,
                            "Diff_rel_pos" = abs((all_res[[data_list$name]]$intervals_EN$CET_prediction - all_res[[data_list$name]]$intervals_EN$CET_true)/all_res[[data_list$name]]$intervals_EN$CET_prediction)
    )
    title = paste("Relative model error for", data_list$name)
    plot(mperf_data$True, mperf_data$Diff_rel_pos)
    ggplot(mperf_data, aes(x = True, y = Diff_rel_pos)) +
      geom_point() +
      labs(title = title,
           x = "True",
           y = "Relative error") +
      theme(axis.text.x = element_text(size=rel(1.7)),
            axis.text.y = element_text(size=rel(1.7)),
            axis.title.x = element_text(size=rel(1.7)),
            axis.title.y = element_text(size=rel(1.7)),
            plot.title = element_text(size=rel(1.7)),
            #legend.title = element_blank(),
            legend.position="none",
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)
    
    # Residuals
    res_data = data.table("Id" = all_res[[data_list$name]]$intervals_BS$Id,
                          "BS" = ifelse(!(all_res[[data_list$name]]$intervals_BS$CET_covered), (all_res[[data_list$name]]$intervals_BS$CET_true < all_res[[data_list$name]]$intervals_BS$CET_lower) * abs(all_res[[data_list$name]]$intervals_BS$CET_true - all_res[[data_list$name]]$intervals_BS$CET_lower) +
                                          (all_res[[data_list$name]]$intervals_BS$CET_true > all_res[[data_list$name]]$intervals_BS$CET_upper) * abs(all_res[[data_list$name]]$intervals_BS$CET_true - all_res[[data_list$name]]$intervals_BS$CET_upper), NA) / (all_res[[data_list$name]]$intervals_BS$CET_upper + all_res[[data_list$name]]$intervals_BS$CET_lower)/2,
                          "EN" = ifelse(!(all_res[[data_list$name]]$intervals_EN$CET_covered), (all_res[[data_list$name]]$intervals_EN$CET_true < all_res[[data_list$name]]$intervals_EN$CET_lower) * abs(all_res[[data_list$name]]$intervals_EN$CET_true - all_res[[data_list$name]]$intervals_EN$CET_lower) +
                                          (all_res[[data_list$name]]$intervals_EN$CET_true > all_res[[data_list$name]]$intervals_EN$CET_upper) * abs(all_res[[data_list$name]]$intervals_EN$CET_true - all_res[[data_list$name]]$intervals_EN$CET_upper), NA) / (all_res[[data_list$name]]$intervals_EN$CET_upper + all_res[[data_list$name]]$intervals_EN$CET_lower)/2,
                          "BA" = ifelse(!(all_res[[data_list$name]]$intervals_BA$CET_covered), (all_res[[data_list$name]]$intervals_BA$CET_true < all_res[[data_list$name]]$intervals_BA$CET_lower) * abs(all_res[[data_list$name]]$intervals_BA$CET_true - all_res[[data_list$name]]$intervals_BA$CET_lower) +
                                          (all_res[[data_list$name]]$intervals_BA$CET_true > all_res[[data_list$name]]$intervals_BA$CET_upper) * abs(all_res[[data_list$name]]$intervals_BA$CET_true - all_res[[data_list$name]]$intervals_BA$CET_upper), NA) / (all_res[[data_list$name]]$intervals_BA$CET_upper + all_res[[data_list$name]]$intervals_BA$CET_lower)/2,
                          "CP" = ifelse(!(all_res[[data_list$name]]$intervals_CP_m$CET_covered), (all_res[[data_list$name]]$intervals_CP_m$CET_true < all_res[[data_list$name]]$intervals_CP_m$CET_lower) * abs(all_res[[data_list$name]]$intervals_CP_m$CET_true - all_res[[data_list$name]]$intervals_CP_m$CET_lower) +
                                          (all_res[[data_list$name]]$intervals_CP_m$CET_true > all_res[[data_list$name]]$intervals_CP_m$CET_upper) * abs(all_res[[data_list$name]]$intervals_CP_m$CET_true - all_res[[data_list$name]]$intervals_CP_m$CET_upper), NA) / (all_res[[data_list$name]]$intervals_CP_m$CET_upper + all_res[[data_list$name]]$intervals_CP_m$CET_lower)/2,
                          "CR" = ifelse(!(all_res[[data_list$name]]$intervals_CR_m$CET_covered), (all_res[[data_list$name]]$intervals_CR_m$CET_true < all_res[[data_list$name]]$intervals_CR_m$CET_lower) * abs(all_res[[data_list$name]]$intervals_CR_m$CET_true - all_res[[data_list$name]]$intervals_CR_m$CET_lower) +
                                          (all_res[[data_list$name]]$intervals_CR_m$CET_true > all_res[[data_list$name]]$intervals_CR_m$CET_upper) * abs(all_res[[data_list$name]]$intervals_CR_m$CET_true - all_res[[data_list$name]]$intervals_CR_m$CET_upper), NA) / (all_res[[data_list$name]]$intervals_CR_m$CET_upper + all_res[[data_list$name]]$intervals_CR_m$CET_lower)/2,
                          "QR" = ifelse(!(all_res[[data_list$name]]$intervals_QR_m$CET_covered), (all_res[[data_list$name]]$intervals_QR_m$CET_true < all_res[[data_list$name]]$intervals_QR_m$CET_lower) * abs(all_res[[data_list$name]]$intervals_QR_m$CET_true - all_res[[data_list$name]]$intervals_QR_m$CET_lower) +
                                          (all_res[[data_list$name]]$intervals_QR_m$CET_true > all_res[[data_list$name]]$intervals_QR_m$CET_upper) * abs(all_res[[data_list$name]]$intervals_QR_m$CET_true - all_res[[data_list$name]]$intervals_QR_m$CET_upper), NA) / (all_res[[data_list$name]]$intervals_QR_m$CET_upper + all_res[[data_list$name]]$intervals_QR_m$CET_lower)/2
    )
    
    max_vector = sort(as.vector(unlist(res_data[,2:length(res_data)])), decreasing = TRUE)
    max_number = max(max_vector[!is.infinite(max_vector)])
    
    title = paste("Absolute scaled residuals distribution for", data_list$name)
    print(ggplot(data = res_data) +
      geom_density(aes(x = BS, color = "BS")) +
      geom_density(aes(x = EN, color = "EN")) +
      geom_density(aes(x = BA, color = "BA")) +
      geom_density(aes(x = CP, color = "CP")) +
      geom_density(aes(x = CR, color = "CR")) +
      geom_density(aes(x = QR, color = "QR")) +
      scale_color_manual(values = method_colors_all) +
      xlim(0,2) +
      ylim(0, max_number * 1.4) +
      labs(title = title,
           x = "Scaled residual",
           y = "Frequency",
           color = "Method")) +
      theme(legend.title = element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)
    
    # Rectangles
    rec_data1 = data.table("Id" = all_res[[data_list$name]]$intervals_BS$Id,
                           "BS" = (all_res[[data_list$name]]$intervals_BS$CET_true - all_res[[data_list$name]]$intervals_BS$CET_lower) / (all_res[[data_list$name]]$intervals_BS$CET_upper - all_res[[data_list$name]]$intervals_BS$CET_lower),
                           "EN" = (all_res[[data_list$name]]$intervals_EN$CET_true - all_res[[data_list$name]]$intervals_EN$CET_lower) / (all_res[[data_list$name]]$intervals_EN$CET_upper - all_res[[data_list$name]]$intervals_EN$CET_lower),
                           "BA" = (all_res[[data_list$name]]$intervals_BA$CET_true - all_res[[data_list$name]]$intervals_BA$CET_lower) / (all_res[[data_list$name]]$intervals_BA$CET_upper - all_res[[data_list$name]]$intervals_BA$CET_lower),
                           "CP" = (all_res[[data_list$name]]$intervals_CP_m$CET_true - all_res[[data_list$name]]$intervals_CP_m$CET_lower) / (all_res[[data_list$name]]$intervals_CP_m$CET_upper - all_res[[data_list$name]]$intervals_CP_m$CET_lower),
                           "CR" = (all_res[[data_list$name]]$intervals_CR_m$CET_true - all_res[[data_list$name]]$intervals_CR_m$CET_lower) / (all_res[[data_list$name]]$intervals_CR_m$CET_upper - all_res[[data_list$name]]$intervals_CR_m$CET_lower),
                           "QR" = (all_res[[data_list$name]]$intervals_QR_m$CET_true - all_res[[data_list$name]]$intervals_QR_m$CET_lower) / (all_res[[data_list$name]]$intervals_QR_m$CET_upper - all_res[[data_list$name]]$intervals_QR_m$CET_lower))
    
    # All
    methods = c("BS","EN","BA","CP","CR","QR")
    
    rec_data <- rec_data1[,2:7] %>%
      pivot_longer(cols = everything(), names_to = "Method", values_to = "Value")
    
    title = paste("Density covered by interval (all methods) for", data_list$name)
    print(ggplot(rec_data, aes(x = Method, y = Value)) +
      geom_rect(aes(xmin = 0.5, xmax = 6.5, ymin = 0, ymax = 1), alpha = 0.008) +
      geom_half_violin(trim = TRUE, aes(fill = Method), side = "r", scale = 1.5) +
      labs(title = title,
           y = "Position with respect to PI")) +
      theme(legend.title = element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)
    
    # BA, CP, CR, QR
    methods = c("BA","CP","CR","QR")
    
    rec_data <- rec_data1[,4:7] %>%
      pivot_longer(cols = everything(), names_to = "Method", values_to = "Value")
    
    title = paste("Density covered by interval (reliable methods) for", data_list$name)
    print(ggplot(rec_data, aes(x = Method, y = Value)) +
      geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 1), alpha = 0.008) +
      geom_half_violin(trim = TRUE, aes(fill = Method), side = "r", scale = 1.5) +
      labs(title = paste("Density covered by interval for", data_list$name),
           y = "Position with respect to PI") +
      ylim(-0.25,1.5)) +
      theme(legend.title = element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)
    
    # Width-CET
    width_data = data.table("Id" = all_res[[data_list$name]]$intervals_BS$Id,
                            "BS_width" = ksmooth(all_res[[data_list$name]]$intervals_BS$CET_true, (all_res[[data_list$name]]$intervals_BS$CET_upper - all_res[[data_list$name]]$intervals_BS$CET_lower)/all_res[[data_list$name]]$intervals_BS$CET_prediction, kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                            "EN_width" = ksmooth(all_res[[data_list$name]]$intervals_EN$CET_true, (all_res[[data_list$name]]$intervals_EN$CET_upper - all_res[[data_list$name]]$intervals_EN$CET_lower)/all_res[[data_list$name]]$intervals_BS$CET_prediction, kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                            "BA_width" = ksmooth(all_res[[data_list$name]]$intervals_BA$CET_true, (all_res[[data_list$name]]$intervals_BA$CET_upper - all_res[[data_list$name]]$intervals_BA$CET_lower)/all_res[[data_list$name]]$intervals_BS$CET_prediction, kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                            "CP_width" = ksmooth(all_res[[data_list$name]]$intervals_CP_m$CET_true, (all_res[[data_list$name]]$intervals_CP_m$CET_upper - all_res[[data_list$name]]$intervals_CP_m$CET_lower)/all_res[[data_list$name]]$intervals_BS$CET_prediction, kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                            "CR_width" = ksmooth(all_res[[data_list$name]]$intervals_CR_m$CET_true, (all_res[[data_list$name]]$intervals_CR_m$CET_upper - all_res[[data_list$name]]$intervals_CR_m$CET_lower)/all_res[[data_list$name]]$intervals_BS$CET_prediction, kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                            "QR_width" = ksmooth(all_res[[data_list$name]]$intervals_QR_m$CET_true, (all_res[[data_list$name]]$intervals_QR_m$CET_upper - all_res[[data_list$name]]$intervals_QR_m$CET_lower)/all_res[[data_list$name]]$intervals_BS$CET_prediction, kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                            "true"= all_res[[data_list$name]]$intervals_BS$CET_true,
                            "true_kernel" = ksmooth(all_res[[data_list$name]]$intervals_EN$CET_true, all_res[[data_list$name]]$intervals_CP_m$CET_upper - all_res[[data_list$name]]$intervals_CP_m$CET_lower, kernel = "normal", bandwidth = data_list$smoothwidth)$x
    )
    
    title = paste("Width development with CET", data_list$name)
    print(ggplot(data = width_data, aes(x = true_kernel)) +
      geom_line(aes(y = BS_width, color = "BS")) +
      geom_line(aes(y = EN_width, color = "EN")) +
      geom_line(aes(y = BA_width, color = "BA")) +
      geom_line(aes(y = CP_width, color = "CP")) +
      geom_line(aes(y = CR_width, color = "CR")) +
      geom_line(aes(y = QR_width, color = "QR")) +
      scale_color_manual(values = method_colors_all) +
      labs(title = title,
           y = "Width",
           x = "True transactions",
           color = "Method")) +
      theme(axis.text.x = element_text(size=rel(1.7)),
            axis.text.y = element_text(size=rel(1.7)),
            axis.title.x = element_text(size=rel(1.7)),
            axis.title.y = element_text(size=rel(1.7)),
            plot.title = element_text(size=rel(1.7)),
            #legend.title = element_blank(),
            legend.position="none",
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 4.5)
    
  }
  else{
    print(data_list$name)
    
    # Overview
    plot_data_CET = data.table("Id" = rep(all_res[[data_list$name]]$intervals_EN$Id, each = 5),
                               "Method" = factor(rep(c("EN", "BA", "CP", "CR", "QR"), length(unique(all_res[[data_list$name]]$intervals_EN$Id))),
                                                 levels = c("EN", "BA", "CP", "CR", "QR")),
                               "Low" = c(rbind(all_res[[data_list$name]]$intervals_EN$CET_lower,
                                               all_res[[data_list$name]]$intervals_BA$CET_lower,
                                               all_res[[data_list$name]]$intervals_CP_m$CET_lower,
                                               all_res[[data_list$name]]$intervals_CR_m$CET_lower,
                                               all_res[[data_list$name]]$intervals_QR_m$CET_lower)),
                               "High" = c(rbind(all_res[[data_list$name]]$intervals_EN$CET_upper,
                                                all_res[[data_list$name]]$intervals_BA$CET_upper,
                                                all_res[[data_list$name]]$intervals_CP_m$CET_upper,
                                                all_res[[data_list$name]]$intervals_CR_m$CET_upper,
                                                all_res[[data_list$name]]$intervals_QR_m$CET_upper)),
                               "True" = rep(all_res[[data_list$name]]$intervals_EN$CET_true, each = 5),
                               "CET" = rep(all_res[[data_list$name]]$intervals_EN$CET_prediction, each = 5)
    )
    
    lower = quantile(plot_data_CET$CET, 0.9125)
    upper = quantile(plot_data_CET$CET, 0.9125)
    length(unique(plot_data_CET[CET >= lower & CET <= upper,]$Id))
    
    while (length(unique(plot_data_CET[CET >= lower & CET <= upper,]$Id)) <= 10){
      upper = upper + 0.00001
    }
    
    title = paste("90% PIs for", data_list$name)
    ggplot(plot_data_CET[CET >= lower & CET <= upper,], aes(as.factor(Id), True)) +
      geom_linerange(
        aes(ymin = Low, ymax = High, color = Method),
        position = position_dodge(0.3),
        linewidth = 0.8) +
      geom_point(aes(y = True), color = "black") +
      labs(title = title, x = "Customer", y = "CET") +
      scale_color_manual(values = method_colors_sel) +
      scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
      theme(legend.title = element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)
    
    # Coverage development with CET
    cdev_data = data.table("Id" = all_res[[data_list$name]]$intervals_EN$Id,
                           "EN_covered" = ksmooth(all_res[[data_list$name]]$intervals_EN$CET_true, as.numeric(all_res[[data_list$name]]$intervals_EN$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                           "BA_covered" = ksmooth(all_res[[data_list$name]]$intervals_BA$CET_true, as.numeric(all_res[[data_list$name]]$intervals_BA$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                           "CP_covered" = ksmooth(all_res[[data_list$name]]$intervals_CP_m$CET_true, as.numeric(all_res[[data_list$name]]$intervals_CP_m$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                           "CR_covered" = ksmooth(all_res[[data_list$name]]$intervals_CR_m$CET_true, as.numeric(all_res[[data_list$name]]$intervals_CR_m$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                           "QR_covered" = ksmooth(all_res[[data_list$name]]$intervals_QR_m$CET_true, as.numeric(all_res[[data_list$name]]$intervals_QR_m$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                           "true"= all_res[[data_list$name]]$intervals_EN$CET_true,
                           "true_kernel" = ksmooth(all_res[[data_list$name]]$intervals_BA$CET_true, as.numeric(all_res[[data_list$name]]$intervals_BA$CET_covered), kernel = "normal", bandwidth = data_list$smoothwidth)$x
    )
    
    title = paste("Coverage development with CET for", data_list$name)
    print(ggplot(data = cdev_data, aes(x = true_kernel)) +
      geom_line(aes(y = EN_covered*100, color = "EN")) +
      geom_line(aes(y = BA_covered*100, color = "BA")) +
      geom_line(aes(y = CP_covered*100, color = "CP")) +
        geom_line(aes(x = true_kernel + 0.05, y = CR_covered*100, color = "CR")) +
      geom_line(aes(y = QR_covered*100, color = "QR")) +
      scale_color_manual(values = method_colors_sel) +
      labs(title = title,
           y = "Coverage in %",
           x = "True transactions",
           color = "Method") +
        theme(axis.text.x = element_text(size=rel(1.7)),
              axis.text.y = element_text(size=rel(1.7)),
              axis.title.x = element_text(size=rel(1.7)),
              axis.title.y = element_text(size=rel(1.7)),
              plot.title = element_text(size=rel(1.7)),
              #legend.title = element_blank(),
              legend.position="none",
              axis.text=element_text(size=12),
              axis.title=element_text(size=12),
              panel.background = element_rect(fill = "white", colour = "black"),
              panel.grid.major = element_line(colour = "white", size = 0.5))
    )

    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 4.5)
    
    # Model performance
    mperf_data = data.table("Id" = all_res[[data_list$name]]$intervals_EN$Id,
                            "Pred" = all_res[[data_list$name]]$intervals_EN$CET_prediction,
                            "True" = all_res[[data_list$name]]$intervals_EN$CET_true,
                            "True_rel" = (all_res[[data_list$name]]$intervals_EN$CET_true)/max(all_res[[data_list$name]]$intervals_EN$CET_true),
                            "Diff_abs" = all_res[[data_list$name]]$intervals_EN$CET_prediction - all_res[[data_list$name]]$intervals_EN$CET_true,
                            "Diff_rel" = (all_res[[data_list$name]]$intervals_EN$CET_prediction - all_res[[data_list$name]]$intervals_EN$CET_true)/all_res[[data_list$name]]$intervals_EN$CET_prediction,
                            "Diff_rel_pos" = abs((all_res[[data_list$name]]$intervals_EN$CET_prediction - all_res[[data_list$name]]$intervals_EN$CET_true)/all_res[[data_list$name]]$intervals_EN$CET_prediction)
    )
    title = paste("Relative model error for", data_list$name)
    plot(mperf_data$True, mperf_data$Diff_rel_pos)
    ggplot(mperf_data, aes(x = True, y = Diff_rel_pos)) +
      geom_point() +
      labs(title = title,
           x = "True",
           y = "Relative error") +
      theme(axis.text.x = element_text(size=rel(1.7)),
            axis.text.y = element_text(size=rel(1.7)),
            axis.title.x = element_text(size=rel(1.7)),
            axis.title.y = element_text(size=rel(1.7)),
            plot.title = element_text(size=rel(1.7)),
            #legend.title = element_blank(),
            legend.position="none",
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)
    
    # Residuals
    res_data = data.table("Id" = all_res[[data_list$name]]$intervals_EN$Id,
                          "EN" = ifelse(!(all_res[[data_list$name]]$intervals_EN$CET_covered), (all_res[[data_list$name]]$intervals_EN$CET_true < all_res[[data_list$name]]$intervals_EN$CET_lower) * abs(all_res[[data_list$name]]$intervals_EN$CET_true - all_res[[data_list$name]]$intervals_EN$CET_lower) +
                                          (all_res[[data_list$name]]$intervals_EN$CET_true > all_res[[data_list$name]]$intervals_EN$CET_upper) * abs(all_res[[data_list$name]]$intervals_EN$CET_true - all_res[[data_list$name]]$intervals_EN$CET_upper), NA) / (all_res[[data_list$name]]$intervals_EN$CET_upper + all_res[[data_list$name]]$intervals_EN$CET_lower)/2,
                          "BA" = ifelse(!(all_res[[data_list$name]]$intervals_BA$CET_covered), (all_res[[data_list$name]]$intervals_BA$CET_true < all_res[[data_list$name]]$intervals_BA$CET_lower) * abs(all_res[[data_list$name]]$intervals_BA$CET_true - all_res[[data_list$name]]$intervals_BA$CET_lower) +
                                          (all_res[[data_list$name]]$intervals_BA$CET_true > all_res[[data_list$name]]$intervals_BA$CET_upper) * abs(all_res[[data_list$name]]$intervals_BA$CET_true - all_res[[data_list$name]]$intervals_BA$CET_upper), NA) / (all_res[[data_list$name]]$intervals_BA$CET_upper + all_res[[data_list$name]]$intervals_BA$CET_lower)/2,
                          "CP" = ifelse(!(all_res[[data_list$name]]$intervals_CP_m$CET_covered), (all_res[[data_list$name]]$intervals_CP_m$CET_true < all_res[[data_list$name]]$intervals_CP_m$CET_lower) * abs(all_res[[data_list$name]]$intervals_CP_m$CET_true - all_res[[data_list$name]]$intervals_CP_m$CET_lower) +
                                          (all_res[[data_list$name]]$intervals_CP_m$CET_true > all_res[[data_list$name]]$intervals_CP_m$CET_upper) * abs(all_res[[data_list$name]]$intervals_CP_m$CET_true - all_res[[data_list$name]]$intervals_CP_m$CET_upper), NA) / (all_res[[data_list$name]]$intervals_CP_m$CET_upper + all_res[[data_list$name]]$intervals_CP_m$CET_lower)/2,
                          "CR" = ifelse(!(all_res[[data_list$name]]$intervals_CR_m$CET_covered), (all_res[[data_list$name]]$intervals_CR_m$CET_true < all_res[[data_list$name]]$intervals_CR_m$CET_lower) * abs(all_res[[data_list$name]]$intervals_CR_m$CET_true - all_res[[data_list$name]]$intervals_CR_m$CET_lower) +
                                          (all_res[[data_list$name]]$intervals_CR_m$CET_true > all_res[[data_list$name]]$intervals_CR_m$CET_upper) * abs(all_res[[data_list$name]]$intervals_CR_m$CET_true - all_res[[data_list$name]]$intervals_CR_m$CET_upper), NA) / (all_res[[data_list$name]]$intervals_CR_m$CET_upper + all_res[[data_list$name]]$intervals_CR_m$CET_lower)/2,
                          "QR" = ifelse(!(all_res[[data_list$name]]$intervals_QR_m$CET_covered), (all_res[[data_list$name]]$intervals_QR_m$CET_true < all_res[[data_list$name]]$intervals_QR_m$CET_lower) * abs(all_res[[data_list$name]]$intervals_QR_m$CET_true - all_res[[data_list$name]]$intervals_QR_m$CET_lower) +
                                          (all_res[[data_list$name]]$intervals_QR_m$CET_true > all_res[[data_list$name]]$intervals_QR_m$CET_upper) * abs(all_res[[data_list$name]]$intervals_QR_m$CET_true - all_res[[data_list$name]]$intervals_QR_m$CET_upper), NA) / (all_res[[data_list$name]]$intervals_QR_m$CET_upper + all_res[[data_list$name]]$intervals_QR_m$CET_lower)/2
    )
    
    max_vector = sort(as.vector(unlist(res_data[,2:length(res_data)])), decreasing = TRUE)
    max_number = max(max_vector[!is.infinite(max_vector)])
    
    title = paste("Absolute scaled residuals distribution", data_list$name)
    print(ggplot(data = res_data) +
      geom_density(aes(x = EN, color = "EN")) +
      geom_density(aes(x = BA, color = "BA")) +
      geom_density(aes(x = CP, color = "CP")) +
      geom_density(aes(x = CR, color = "CR")) +
      geom_density(aes(x = QR, color = "QR")) +
      scale_color_manual(values = c("EN" = "grey",
                                    "BA" = "green",
                                    "CP" = "red",
                                    "QR" = "blue")) +
      xlim(0,2) +
      ylim(0, max_number * 1.4) +
      labs(title = title,
           x = "Scaled residual",
           y = "Frequency",
           color = "Method")) +
      theme(legend.title = element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)
    
    # Rectangles
    rec_data1 = data.table("Id" = all_res[[data_list$name]]$intervals_EN$Id,
                           "EN" = (all_res[[data_list$name]]$intervals_EN$CET_true - all_res[[data_list$name]]$intervals_EN$CET_lower) / (all_res[[data_list$name]]$intervals_EN$CET_upper - all_res[[data_list$name]]$intervals_EN$CET_lower),
                           "BA" = (all_res[[data_list$name]]$intervals_BA$CET_true - all_res[[data_list$name]]$intervals_BA$CET_lower) / (all_res[[data_list$name]]$intervals_BA$CET_upper - all_res[[data_list$name]]$intervals_BA$CET_lower),
                           "CP" = (all_res[[data_list$name]]$intervals_CP_m$CET_true - all_res[[data_list$name]]$intervals_CP_m$CET_lower) / (all_res[[data_list$name]]$intervals_CP_m$CET_upper - all_res[[data_list$name]]$intervals_CP_m$CET_lower),
                           "CR" = (all_res[[data_list$name]]$intervals_CR_m$CET_true - all_res[[data_list$name]]$intervals_CR_m$CET_lower) / (all_res[[data_list$name]]$intervals_CR_m$CET_upper - all_res[[data_list$name]]$intervals_CR_m$CET_lower),
                           "QR" = (all_res[[data_list$name]]$intervals_QR_m$CET_true - all_res[[data_list$name]]$intervals_QR_m$CET_lower) / (all_res[[data_list$name]]$intervals_QR_m$CET_upper - all_res[[data_list$name]]$intervals_QR_m$CET_lower))
    
    # All
    methods = c("EN","BA","CP","CR","QR")
    
    rec_data <- rec_data1[,2:6] %>%
      pivot_longer(cols = everything(), names_to = "Method", values_to = "Value")
    
    title = paste("Density covered by interval (all methods) for", data_list$name)
    print(ggplot(rec_data, aes(x = Method, y = Value)) +
      geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0, ymax = 1), alpha = 0.008) +
      geom_half_violin(trim = TRUE, aes(fill = Method), side = "r", scale = 1.5) +
      labs(title = title,
           y = "Position with respect to PI")) +
      theme(legend.title = element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)
    
    # BA, CP, CR, QR
      methods = c("BA","CP","CR","QR")
      
      rec_data <- rec_data1[,3:6] %>%
        pivot_longer(cols = everything(), names_to = "Method", values_to = "Value")
      
      title = paste("Density covered by interval (reliable methods) for", data_list$name)
      print(ggplot(rec_data, aes(x = Method, y = Value)) +
        geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 1), alpha = 0.008) +
        geom_half_violin(trim = TRUE, aes(fill = Method), side = "r", scale = 1.5) +
        labs(title = paste("Density covered by interval", data_list$name),
             y = "Position with respect to PI") +
        ylim(-0.25,1.5)) +
        theme(legend.title = element_blank(),
              axis.text=element_text(size=12),
              axis.title=element_text(size=12),
              panel.background = element_rect(fill = "white", colour = "black"),
              panel.grid.major = element_line(colour = "white", size = 0.5))
      ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)
      
    # Width-CET
    width_data = data.table("Id" = all_res[[data_list$name]]$intervals_EN$Id,
                            "EN_width" = ksmooth(all_res[[data_list$name]]$intervals_EN$CET_true, (all_res[[data_list$name]]$intervals_EN$CET_upper - all_res[[data_list$name]]$intervals_EN$CET_lower)/all_res[[data_list$name]]$intervals_EN$CET_prediction, kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                            "BA_width" = ksmooth(all_res[[data_list$name]]$intervals_BA$CET_true, (all_res[[data_list$name]]$intervals_BA$CET_upper - all_res[[data_list$name]]$intervals_BA$CET_lower)/all_res[[data_list$name]]$intervals_EN$CET_prediction, kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                            "CP_width" = ksmooth(all_res[[data_list$name]]$intervals_CP_m$CET_true, (all_res[[data_list$name]]$intervals_CP_m$CET_upper - all_res[[data_list$name]]$intervals_CP_m$CET_lower)/all_res[[data_list$name]]$intervals_EN$CET_prediction, kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                            "CR_width" = ksmooth(all_res[[data_list$name]]$intervals_CR_m$CET_true, (all_res[[data_list$name]]$intervals_CR_m$CET_upper - all_res[[data_list$name]]$intervals_CR_m$CET_lower)/all_res[[data_list$name]]$intervals_EN$CET_prediction, kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                            "QR_width" = ksmooth(all_res[[data_list$name]]$intervals_QR_m$CET_true, (all_res[[data_list$name]]$intervals_QR_m$CET_upper - all_res[[data_list$name]]$intervals_QR_m$CET_lower)/all_res[[data_list$name]]$intervals_EN$CET_prediction, kernel = "normal", bandwidth = data_list$smoothwidth)$y,
                            "true"= all_res[[data_list$name]]$intervals_EN$CET_true,
                            "true_kernel" = ksmooth(all_res[[data_list$name]]$intervals_EN$CET_true, all_res[[data_list$name]]$intervals_CP_m$CET_upper - all_res[[data_list$name]]$intervals_CP_m$CET_lower, kernel = "normal", bandwidth = data_list$smoothwidth)$x
    )
    
    title = paste("Width development with CET", data_list$name)
    print(ggplot(data = width_data, aes(x = true_kernel)) +
      geom_line(aes(y = EN_width, color = "EN")) +
      geom_line(aes(y = BA_width, color = "BA")) +
      geom_line(aes(y = CP_width, color = "CP")) +
      geom_line(aes(y = CR_width, color = "CR")) +
      geom_line(aes(y = QR_width, color = "QR")) +
      scale_color_manual(values = method_colors_sel) +
      labs(title = title,
           y = "Width",
           x = "True transactions",
           color = "Method")) +
      theme(axis.text.x = element_text(size=rel(1.7)),
            axis.text.y = element_text(size=rel(1.7)),
            axis.title.x = element_text(size=rel(1.7)),
            axis.title.y = element_text(size=rel(1.7)),
            plot.title = element_text(size=rel(1.7)),
            #legend.title = element_blank(),
            legend.position="none",
            axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major = element_line(colour = "white", size = 0.5))
    ggsave(filename = file.path(plot_path = paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 4.5)
  }
}


#############


print(ggplot(data = cdev_data, aes(x = true_kernel)) +
        geom_line(aes(y = BS_covered*100, color = "BS"), linewidth = 4) +
        geom_line(aes(y = EN_covered*100, color = "EN"), linewidth = 4) +
        geom_line(aes(y = BA_covered*100, color = "BA"), linewidth = 4) +
        geom_line(aes(y = CP_covered*100, color = "CP"), linewidth = 4) +
        geom_line(aes(x = true_kernel + 0.05, y = CR_covered*100, color = "CR"), linewidth = 4) +
        geom_line(aes(y = QR_covered*100, color = "QR"), linewidth = 4) +
        scale_color_manual(values = method_colors_all, 
                           breaks = c("BS", "EN", "BA", "CP", "CR", "QR")) +
        labs(title = title,
             y = "Coverage in %",
             x = "True",
             color = "Method")) +
  theme(axis.text.x = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2)),
        axis.title.y = element_text(size=rel(2)),
        legend.title = element_blank(),
        legend.text=element_text(size=13),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(colour = "white", size = 0.5),
        legend.position="bottom") +
  guides(color = guide_legend(nrow = 1, override.aes = list(size = 10)))

         