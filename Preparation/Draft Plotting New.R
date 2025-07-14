# Achieved coverage table
##### 

plot_coverage_data = merge(x = coverage_table[, c("Method", "PICP", "PIARW", "Data")], y = ranking_table[, c("Method", "Rank_PICP")], by = "Method")
plot_coverage_data = plot_coverage_data[order(-Rank_PICP)]
plot_coverage_data$Method <- factor(plot_coverage_data$Method, levels = unique(plot_coverage_data$Method))

plot_width_data = data.table()

ggplot(plot_coverage_data, aes(x = Method, y = PICP*100, shape = Data)) +
  geom_point(size = 2, color = "black") +  # Set color for all points
  geom_hline(yintercept = 90, linetype = "dashed", color = "red") +  # Horizontal line at 90%
  annotate("text", x = Inf, y = 90, label = "90%", hjust = 15, vjust = -0.5, color = "red") +  # Add label for horizontal line
  labs(title = "PICP by Method and Data Set",
       x = "Method",
       y = "PICP in %",
       shape = "Data Set")

ggplot(plot_coverage_data) +
  geom_point(aes(x = Method, y = PIARW*2, shape = Data), size = 2, color = "blue", position = position_nudge(0.15)) +  # Line for MSPIW
  geom_point(aes(x = Method, y = PICP*100, shape = Data), size = 2, color = "black", position = position_nudge(-0.15)) +  # Set color for all points
  geom_hline(yintercept = 90, linetype = "dashed", color = "red") +  # Horizontal line at 90%
  annotate("text", x = Inf, y = 90, label = "90%", hjust = 25, vjust = -0.5, color = "red") +  # Add label for horizontal line
  scale_y_continuous(
    name = "PICP in %",
    sec.axis = sec_axis(~ ./2, name = "PIARW")  # Add a secondary axis for MSPIW
  ) +
  labs(title = "PICP and MSPIW by Method and Data Set",
       x = "Method",
       shape = "Data Set") +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(colour = "blue"))

#####
# Coverage CLV
cdev_data = data.table("Id" = all_res$gift_results$intervals_EN$Id,
                       "BS_covered" = ksmooth(all_res$gift_results$intervals_BS$CET_true, as.numeric(all_res$gift_results$intervals_BS$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "EN_covered" = ksmooth(all_res$gift_results$intervals_EN$CET_true, as.numeric(all_res$gift_results$intervals_EN$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "BA_covered" = ksmooth(all_res$gift_results$intervals_BA$CET_true, as.numeric(all_res$gift_results$intervals_BA$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "CP_covered" = ksmooth(all_res$gift_results$intervals_CP_m$CET_true, as.numeric(all_res$gift_results$intervals_CP_m$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "CR_covered" = ksmooth(all_res$gift_results$intervals_CR_m$CET_true, as.numeric(all_res$gift_results$intervals_CR_m$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "QR_covered" = ksmooth(all_res$gift_results$intervals_QR_m$CET_true, as.numeric(all_res$gift_results$intervals_QR_m$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "true"= all_res$gift_results$intervals_BS$CET_true,
                       "true_kernel" = ksmooth(all_res$gift_results$intervals_BA$CET_true, as.numeric(all_res$gift_results$intervals_BA$CET_covered), kernel = "normal", bandwidth = 2)$x
)

cdev_data = data.table("Id" = all_res$el_results$intervals_BS$Id,
                       "BS_covered" = ksmooth(all_res$el_results$intervals_BS$CET_true, as.numeric(all_res$el_results$intervals_BS$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "EN_covered" = ksmooth(all_res$el_results$intervals_EN$CET_true, as.numeric(all_res$el_results$intervals_EN$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "BA_covered" = ksmooth(all_res$el_results$intervals_BA$CET_true, as.numeric(all_res$el_results$intervals_BA$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "CP_covered" = ksmooth(all_res$el_results$intervals_CP_m$CET_true, as.numeric(all_res$el_results$intervals_CP_m$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "CR_covered" = ksmooth(all_res$el_results$intervals_CR_m$CET_true, as.numeric(all_res$el_results$intervals_CR_m$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "QR_covered" = ksmooth(all_res$el_results$intervals_QR_m$CET_true, as.numeric(all_res$el_results$intervals_QR_m$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "true"= all_res$el_results$intervals_BS$CET_true,
                       "true_kernel" = ksmooth(all_res$el_results$intervals_BA$CET_true, as.numeric(all_res$el_results$intervals_BA$CET_covered), kernel = "normal", bandwidth = 2)$x
)

cdev_data = data.table("Id" = all_res$multi_results$intervals_EN$Id,
                       #"BS_covered" = ksmooth(all_res$multi_results$intervals_BS$CET_true, as.numeric(all_res$multi_results$intervals_BS$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "EN_covered" = ksmooth(all_res$multi_results$intervals_EN$CET_true, as.numeric(all_res$multi_results$intervals_EN$CET_covered), kernel = "normal", bandwidth = 1)$y,
                       "BA_covered" = ksmooth(all_res$multi_results$intervals_BA$CET_true, as.numeric(all_res$multi_results$intervals_BA$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "CP_covered" = ksmooth(all_res$multi_results$intervals_CP_m$CET_true, as.numeric(all_res$multi_results$intervals_CP_m$CET_covered), kernel = "normal", bandwidth = 1)$y,
                       "CR_covered" = ksmooth(all_res$multi_results$intervals_CR_m$CET_true, as.numeric(all_res$multi_results$intervals_CR_m$CET_covered), kernel = "normal", bandwidth = 1)$y,
                       "QR_covered" = ksmooth(all_res$multi_results$intervals_QR_m$CET_true, as.numeric(all_res$multi_results$intervals_QR_m$CET_covered), kernel = "normal", bandwidth = 1)$y,
                       "true"= all_res$multi_results$intervals_EN$CET_true,
                       "true_kernel" = ksmooth(all_res$multi_results$intervals_EN$CET_true, as.numeric(all_res$multi_results$intervals_EN$CET_covered), kernel = "normal", bandwidth = 1)$x
)

cdev_data = data.table("Id" = all_res$apparel_results$intervals_EN$Id,
                       "BS_covered" = ksmooth(all_res$apparel_results$intervals_BS$CET_true, as.numeric(all_res$apparel_results$intervals_BS$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "EN_covered" = ksmooth(all_res$apparel_results$intervals_EN$CET_true, as.numeric(all_res$apparel_results$intervals_EN$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "BA_covered" = ksmooth(all_res$apparel_results$intervals_BA$CET_true, as.numeric(all_res$apparel_results$intervals_BA$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "CP_covered" = ksmooth(all_res$apparel_results$intervals_CP_m$CET_true, as.numeric(all_res$apparel_results$intervals_CP_m$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "CR_covered" = ksmooth(all_res$apparel_results$intervals_CR_m$CET_true, as.numeric(all_res$apparel_results$intervals_CR_m$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "QR_covered" = ksmooth(all_res$apparel_results$intervals_QR_m$CET_true, as.numeric(all_res$apparel_results$intervals_QR_m$CET_covered), kernel = "normal", bandwidth = 2)$y,
                       "true"= all_res$apparel_results$intervals_EN$CET_true,
                       "true_kernel" = ksmooth(all_res$apparel_results$intervals_EN$CET_true, as.numeric(all_res$apparel_results$intervals_EN$CET_covered), kernel = "normal", bandwidth = 2)$x
)

ggplot(data = cdev_data, aes(x = true_kernel)) +
  geom_line(aes(y = BS_covered, color = "BS")) +
  geom_line(aes(y = EN_covered, color = "EN")) +
  geom_line(aes(y = BA_covered, color = "BA")) +
  geom_line(aes(y = CP_covered, color = "CP")) +
  geom_line(aes(y = CR_covered, color = "CR")) +
  geom_line(aes(y = QR_covered, color = "QR")) +
  scale_color_manual(values = c("BS" = "purple", 
                                "EN" = "yellow", 
                                "BA" = "black", 
                                "CP" = "green", 
                                "CR" = "orange",
                                "QR" = "red")) +
  labs(title = "Coverage development with CET",
       y = "Coverage",
       x = "CET")

#####
# Residuals
res_data = data.table("Id" = all_res$gift_results$intervals_BS$Id,
                      "BS" = ifelse(!(all_res$gift_results$intervals_BS$CET_covered), (all_res$gift_results$intervals_BS$CET_true < all_res$gift_results$intervals_BS$CET_lower) * abs(all_res$gift_results$intervals_BS$CET_true - all_res$gift_results$intervals_BS$CET_lower) +
                                      (all_res$gift_results$intervals_BS$CET_true > all_res$gift_results$intervals_BS$CET_upper) * abs(all_res$gift_results$intervals_BS$CET_true - all_res$gift_results$intervals_BS$CET_upper), NA) / (all_res$gift_results$intervals_BS$CET_upper + all_res$gift_results$intervals_BS$CET_lower)/2,
                      "EN" = ifelse(!(all_res$gift_results$intervals_EN$CET_covered), (all_res$gift_results$intervals_EN$CET_true < all_res$gift_results$intervals_EN$CET_lower) * abs(all_res$gift_results$intervals_EN$CET_true - all_res$gift_results$intervals_EN$CET_lower) +
                                      (all_res$gift_results$intervals_EN$CET_true > all_res$gift_results$intervals_EN$CET_upper) * abs(all_res$gift_results$intervals_EN$CET_true - all_res$gift_results$intervals_EN$CET_upper), NA) / (all_res$gift_results$intervals_EN$CET_upper + all_res$gift_results$intervals_EN$CET_lower)/2,
                      "BA" = ifelse(!(all_res$gift_results$intervals_BA$CET_covered), (all_res$gift_results$intervals_BA$CET_true < all_res$gift_results$intervals_BA$CET_lower) * abs(all_res$gift_results$intervals_BA$CET_true - all_res$gift_results$intervals_BA$CET_lower) +
                                      (all_res$gift_results$intervals_BA$CET_true > all_res$gift_results$intervals_BA$CET_upper) * abs(all_res$gift_results$intervals_BA$CET_true - all_res$gift_results$intervals_BA$CET_upper), NA) / (all_res$gift_results$intervals_BA$CET_upper + all_res$gift_results$intervals_BA$CET_lower)/2,
                      "CP" = ifelse(!(all_res$gift_results$intervals_CP_m$CET_covered), (all_res$gift_results$intervals_CP_m$CET_true < all_res$gift_results$intervals_CP_m$CET_lower) * abs(all_res$gift_results$intervals_CP_m$CET_true - all_res$gift_results$intervals_CP_m$CET_lower) +
                                      (all_res$gift_results$intervals_CP_m$CET_true > all_res$gift_results$intervals_CP_m$CET_upper) * abs(all_res$gift_results$intervals_CP_m$CET_true - all_res$gift_results$intervals_CP_m$CET_upper), NA) / (all_res$gift_results$intervals_CP_m$CET_upper + all_res$gift_results$intervals_CP_m$CET_lower)/2,
                      "CR" = ifelse(!(all_res$gift_results$intervals_CR_m$CET_covered), (all_res$gift_results$intervals_CR_m$CET_true < all_res$gift_results$intervals_CR_m$CET_lower) * abs(all_res$gift_results$intervals_CR_m$CET_true - all_res$gift_results$intervals_CR_m$CET_lower) +
                                      (all_res$gift_results$intervals_CR_m$CET_true > all_res$gift_results$intervals_CR_m$CET_upper) * abs(all_res$gift_results$intervals_CR_m$CET_true - all_res$gift_results$intervals_CR_m$CET_upper), NA) / (all_res$gift_results$intervals_CR_m$CET_upper + all_res$gift_results$intervals_CR_m$CET_lower)/2,
                      "QR" = ifelse(!(all_res$gift_results$intervals_QR_m$CET_covered), (all_res$gift_results$intervals_QR_m$CET_true < all_res$gift_results$intervals_QR_m$CET_lower) * abs(all_res$gift_results$intervals_QR_m$CET_true - all_res$gift_results$intervals_QR_m$CET_lower) +
                                      (all_res$gift_results$intervals_QR_m$CET_true > all_res$gift_results$intervals_QR_m$CET_upper) * abs(all_res$gift_results$intervals_QR_m$CET_true - all_res$gift_results$intervals_QR_m$CET_upper), NA) / (all_res$gift_results$intervals_QR_m$CET_upper + all_res$gift_results$intervals_QR_m$CET_lower)/2
)

res_data = data.table("Id" = all_res$el_results$intervals_BS$Id,
                      "BS" = ifelse(!(all_res$el_results$intervals_BS$CET_covered), (all_res$el_results$intervals_BS$CET_true < all_res$el_results$intervals_BS$CET_lower) * abs(all_res$el_results$intervals_BS$CET_true - all_res$el_results$intervals_BS$CET_lower) +
                                      (all_res$el_results$intervals_BS$CET_true > all_res$el_results$intervals_BS$CET_upper) * abs(all_res$el_results$intervals_BS$CET_true - all_res$el_results$intervals_BS$CET_upper), NA) / (all_res$el_results$intervals_BS$CET_upper + all_res$el_results$intervals_BS$CET_lower)/2,
                      "EN" = ifelse(!(all_res$el_results$intervals_EN$CET_covered), (all_res$el_results$intervals_EN$CET_true < all_res$el_results$intervals_EN$CET_lower) * abs(all_res$el_results$intervals_EN$CET_true - all_res$el_results$intervals_EN$CET_lower) +
                                      (all_res$el_results$intervals_EN$CET_true > all_res$el_results$intervals_EN$CET_upper) * abs(all_res$el_results$intervals_EN$CET_true - all_res$el_results$intervals_EN$CET_upper), NA) / (all_res$el_results$intervals_EN$CET_upper + all_res$el_results$intervals_EN$CET_lower)/2,
                      "BA" = ifelse(!(all_res$el_results$intervals_BA$CET_covered), (all_res$el_results$intervals_BA$CET_true < all_res$el_results$intervals_BA$CET_lower) * abs(all_res$el_results$intervals_BA$CET_true - all_res$el_results$intervals_BA$CET_lower) +
                                      (all_res$el_results$intervals_BA$CET_true > all_res$el_results$intervals_BA$CET_upper) * abs(all_res$el_results$intervals_BA$CET_true - all_res$el_results$intervals_BA$CET_upper), NA) / (all_res$el_results$intervals_BA$CET_upper + all_res$el_results$intervals_BA$CET_lower)/2,
                      "CP" = ifelse(!(all_res$el_results$intervals_CP_m$CET_covered), (all_res$el_results$intervals_CP_m$CET_true < all_res$el_results$intervals_CP_m$CET_lower) * abs(all_res$el_results$intervals_CP_m$CET_true - all_res$el_results$intervals_CP_m$CET_lower) +
                                      (all_res$el_results$intervals_CP_m$CET_true > all_res$el_results$intervals_CP_m$CET_upper) * abs(all_res$el_results$intervals_CP_m$CET_true - all_res$el_results$intervals_CP_m$CET_upper), NA) / (all_res$el_results$intervals_CP_m$CET_upper + all_res$el_results$intervals_CP_m$CET_lower)/2,
                      "CR" = ifelse(!(all_res$el_results$intervals_CR_m$CET_covered), (all_res$el_results$intervals_CR_m$CET_true < all_res$el_results$intervals_CR_m$CET_lower) * abs(all_res$el_results$intervals_CR_m$CET_true - all_res$el_results$intervals_CR_m$CET_lower) +
                                      (all_res$el_results$intervals_CR_m$CET_true > all_res$el_results$intervals_CR_m$CET_upper) * abs(all_res$el_results$intervals_CR_m$CET_true - all_res$el_results$intervals_CR_m$CET_upper), NA) / (all_res$el_results$intervals_CR_m$CET_upper + all_res$el_results$intervals_CR_m$CET_lower)/2,
                      "QR" = ifelse(!(all_res$el_results$intervals_QR_m$CET_covered), (all_res$el_results$intervals_QR_m$CET_true < all_res$el_results$intervals_QR_m$CET_lower) * abs(all_res$el_results$intervals_QR_m$CET_true - all_res$el_results$intervals_QR_m$CET_lower) +
                                      (all_res$el_results$intervals_QR_m$CET_true > all_res$el_results$intervals_QR_m$CET_upper) * abs(all_res$el_results$intervals_QR_m$CET_true - all_res$el_results$intervals_QR_m$CET_upper), NA) / (all_res$el_results$intervals_QR_m$CET_upper + all_res$el_results$intervals_QR_m$CET_lower)/2
)

res_data = data.table("Id" = all_res$gift_results$intervals_EN$Id,
                      # "BS" = ifelse(!(all_res$gift_results$intervals_BS$CET_covered), (all_res$gift_results$intervals_BS$CET_true < all_res$gift_results$intervals_BS$CET_lower) * abs(all_res$gift_results$intervals_BS$CET_true - all_res$gift_results$intervals_BS$CET_lower) +
                      #                 (all_res$gift_results$intervals_BS$CET_true > all_res$gift_results$intervals_BS$CET_upper) * abs(all_res$gift_results$intervals_BS$CET_true - all_res$gift_results$intervals_BS$CET_upper), NA) / (all_res$gift_results$intervals_BS$CET_upper + all_res$gift_results$intervals_BS$CET_lower)/2,
                      "EN" = ifelse(!(all_res$gift_results$intervals_EN$CET_covered), (all_res$gift_results$intervals_EN$CET_true < all_res$gift_results$intervals_EN$CET_lower) * abs(all_res$gift_results$intervals_EN$CET_true - all_res$gift_results$intervals_EN$CET_lower) +
                                      (all_res$gift_results$intervals_EN$CET_true > all_res$gift_results$intervals_EN$CET_upper) * abs(all_res$gift_results$intervals_EN$CET_true - all_res$gift_results$intervals_EN$CET_upper), NA) / (all_res$gift_results$intervals_EN$CET_upper + all_res$gift_results$intervals_EN$CET_lower)/2,
                      "BA" = ifelse(!(all_res$gift_results$intervals_BA$CET_covered), (all_res$gift_results$intervals_BA$CET_true < all_res$gift_results$intervals_BA$CET_lower) * abs(all_res$gift_results$intervals_BA$CET_true - all_res$gift_results$intervals_BA$CET_lower) +
                                      (all_res$gift_results$intervals_BA$CET_true > all_res$gift_results$intervals_BA$CET_upper) * abs(all_res$gift_results$intervals_BA$CET_true - all_res$gift_results$intervals_BA$CET_upper), NA) / (all_res$gift_results$intervals_BA$CET_upper + all_res$gift_results$intervals_BA$CET_lower)/2,
                      "CP" = ifelse(!(all_res$gift_results$intervals_CP_m$CET_covered), (all_res$gift_results$intervals_CP_m$CET_true < all_res$gift_results$intervals_CP_m$CET_lower) * abs(all_res$gift_results$intervals_CP_m$CET_true - all_res$gift_results$intervals_CP_m$CET_lower) +
                                      (all_res$gift_results$intervals_CP_m$CET_true > all_res$gift_results$intervals_CP_m$CET_upper) * abs(all_res$gift_results$intervals_CP_m$CET_true - all_res$gift_results$intervals_CP_m$CET_upper), NA) / (all_res$gift_results$intervals_CP_m$CET_upper + all_res$gift_results$intervals_CP_m$CET_lower)/2,
                      "CR" = ifelse(!(all_res$gift_results$intervals_CR_m$CET_covered), (all_res$gift_results$intervals_CR_m$CET_true < all_res$gift_results$intervals_CR_m$CET_lower) * abs(all_res$gift_results$intervals_CR_m$CET_true - all_res$gift_results$intervals_CR_m$CET_lower) +
                                      (all_res$gift_results$intervals_CR_m$CET_true > all_res$gift_results$intervals_CR_m$CET_upper) * abs(all_res$gift_results$intervals_CR_m$CET_true - all_res$gift_results$intervals_CR_m$CET_upper), NA) / (all_res$gift_results$intervals_CR_m$CET_upper + all_res$gift_results$intervals_CR_m$CET_lower)/2,
                      "QR" = ifelse(!(all_res$gift_results$intervals_QR_m$CET_covered), (all_res$gift_results$intervals_QR_m$CET_true < all_res$gift_results$intervals_QR_m$CET_lower) * abs(all_res$gift_results$intervals_QR_m$CET_true - all_res$gift_results$intervals_QR_m$CET_lower) +
                                      (all_res$gift_results$intervals_QR_m$CET_true > all_res$gift_results$intervals_QR_m$CET_upper) * abs(all_res$gift_results$intervals_QR_m$CET_true - all_res$gift_results$intervals_QR_m$CET_upper), NA) / (all_res$gift_results$intervals_QR_m$CET_upper + all_res$gift_results$intervals_QR_m$CET_lower)/2
)

res_data = data.table("Id" = all_res$apparel_results$intervals_BS$Id,
                      "BS" = ifelse(!(all_res$apparel_results$intervals_BS$CET_covered), (all_res$apparel_results$intervals_BS$CET_true < all_res$apparel_results$intervals_BS$CET_lower) * abs(all_res$apparel_results$intervals_BS$CET_true - all_res$apparel_results$intervals_BS$CET_lower) +
                                      (all_res$apparel_results$intervals_BS$CET_true > all_res$apparel_results$intervals_BS$CET_upper) * abs(all_res$apparel_results$intervals_BS$CET_true - all_res$apparel_results$intervals_BS$CET_upper), NA) / (all_res$apparel_results$intervals_BS$CET_upper + all_res$apparel_results$intervals_BS$CET_lower)/2,
                      "EN" = ifelse(!(all_res$apparel_results$intervals_EN$CET_covered), (all_res$apparel_results$intervals_EN$CET_true < all_res$apparel_results$intervals_EN$CET_lower) * abs(all_res$apparel_results$intervals_EN$CET_true - all_res$apparel_results$intervals_EN$CET_lower) +
                                      (all_res$apparel_results$intervals_EN$CET_true > all_res$apparel_results$intervals_EN$CET_upper) * abs(all_res$apparel_results$intervals_EN$CET_true - all_res$apparel_results$intervals_EN$CET_upper), NA) / (all_res$apparel_results$intervals_EN$CET_upper + all_res$apparel_results$intervals_EN$CET_lower)/2,
                      "BA" = ifelse(!(all_res$apparel_results$intervals_BA$CET_covered), (all_res$apparel_results$intervals_BA$CET_true < all_res$apparel_results$intervals_BA$CET_lower) * abs(all_res$apparel_results$intervals_BA$CET_true - all_res$apparel_results$intervals_BA$CET_lower) +
                                      (all_res$apparel_results$intervals_BA$CET_true > all_res$apparel_results$intervals_BA$CET_upper) * abs(all_res$apparel_results$intervals_BA$CET_true - all_res$apparel_results$intervals_BA$CET_upper), NA) / (all_res$apparel_results$intervals_BA$CET_upper + all_res$apparel_results$intervals_BA$CET_lower)/2,
                      "CP" = ifelse(!(all_res$apparel_results$intervals_CP_m$CET_covered), (all_res$apparel_results$intervals_CP_m$CET_true < all_res$apparel_results$intervals_CP_m$CET_lower) * abs(all_res$apparel_results$intervals_CP_m$CET_true - all_res$apparel_results$intervals_CP_m$CET_lower) +
                                      (all_res$apparel_results$intervals_CP_m$CET_true > all_res$apparel_results$intervals_CP_m$CET_upper) * abs(all_res$apparel_results$intervals_CP_m$CET_true - all_res$apparel_results$intervals_CP_m$CET_upper), NA) / (all_res$apparel_results$intervals_CP_m$CET_upper + all_res$apparel_results$intervals_CP_m$CET_lower)/2,
                      "CR" = ifelse(!(all_res$apparel_results$intervals_CR_m$CET_covered), (all_res$apparel_results$intervals_CR_m$CET_true < all_res$apparel_results$intervals_CR_m$CET_lower) * abs(all_res$apparel_results$intervals_CR_m$CET_true - all_res$apparel_results$intervals_CR_m$CET_lower) +
                                      (all_res$apparel_results$intervals_CR_m$CET_true > all_res$apparel_results$intervals_CR_m$CET_upper) * abs(all_res$apparel_results$intervals_CR_m$CET_true - all_res$apparel_results$intervals_CR_m$CET_upper), NA) / (all_res$apparel_results$intervals_CR_m$CET_upper + all_res$apparel_results$intervals_CR_m$CET_lower)/2,
                      "QR" = ifelse(!(all_res$apparel_results$intervals_QR_m$CET_covered), (all_res$apparel_results$intervals_QR_m$CET_true < all_res$apparel_results$intervals_QR_m$CET_lower) * abs(all_res$apparel_results$intervals_QR_m$CET_true - all_res$apparel_results$intervals_QR_m$CET_lower) +
                                      (all_res$apparel_results$intervals_QR_m$CET_true > all_res$apparel_results$intervals_QR_m$CET_upper) * abs(all_res$apparel_results$intervals_QR_m$CET_true - all_res$apparel_results$intervals_QR_m$CET_upper), NA) / (all_res$apparel_results$intervals_QR_m$CET_upper + all_res$apparel_results$intervals_QR_m$CET_lower)/2
)

ggplot(data = res_data) +
  geom_density(aes(x = BS, color = "BS")) +
  geom_density(aes(x = EN, color = "EN")) +
  geom_density(aes(x = BA, color = "BA")) +
  geom_density(aes(x = CP, color = "CP")) +
  geom_density(aes(x = CR, color = "CR")) +
  geom_density(aes(x = QR, color = "QR")) +
  scale_color_manual(values = c("BS" = "purple",
                                "EN" = "yellow",
                                "BA" = "black",
                                "CP" = "green",
                                "QR" = "red")) +
  xlim(0,2) +
  ylim(0,60) +
  labs(title = "Absolute scaled residuals distribution",
       x = "Scaled residual")

#####
# Rectangles
methods = c("BS","EN","BA","CP","CR","QR")
methods = c("BA","CP","CR","QR")
methods = c("BS","EN")

rec_data1 = data.table("Id" = all_res$gift_results$intervals_BS$Id,
                       "BS" = (all_res$gift_results$intervals_BS$CET_true - all_res$gift_results$intervals_BS$CET_lower) / (all_res$gift_results$intervals_BS$CET_upper - all_res$gift_results$intervals_BS$CET_lower),
                       "EN" = (all_res$gift_results$intervals_EN$CET_true - all_res$gift_results$intervals_EN$CET_lower) / (all_res$gift_results$intervals_EN$CET_upper - all_res$gift_results$intervals_EN$CET_lower),
                       "BA" = (all_res$gift_results$intervals_BA$CET_true - all_res$gift_results$intervals_BA$CET_lower) / (all_res$gift_results$intervals_BA$CET_upper - all_res$gift_results$intervals_BA$CET_lower),
                       "CP" = (all_res$gift_results$intervals_CP_m$CET_true - all_res$gift_results$intervals_CP_m$CET_lower) / (all_res$gift_results$intervals_CP_m$CET_upper - all_res$gift_results$intervals_CP_m$CET_lower),
                       "CR" = (all_res$gift_results$intervals_CR_m$CET_true - all_res$gift_results$intervals_CR_m$CET_lower) / (all_res$gift_results$intervals_CR_m$CET_upper - all_res$gift_results$intervals_CR_m$CET_lower),
                       "QR" = (all_res$gift_results$intervals_QR_m$CET_true - all_res$gift_results$intervals_QR_m$CET_lower) / (all_res$gift_results$intervals_QR_m$CET_upper - all_res$gift_results$intervals_QR_m$CET_lower))

rec_data1 = data.table("Id" = all_res$el_results$intervals_BS$Id,
                       "BS" = (all_res$el_results$intervals_BS$CET_true - all_res$el_results$intervals_BS$CET_lower) / (all_res$el_results$intervals_BS$CET_upper - all_res$el_results$intervals_BS$CET_lower),
                       "EN" = (all_res$el_results$intervals_EN$CET_true - all_res$el_results$intervals_EN$CET_lower) / (all_res$el_results$intervals_EN$CET_upper - all_res$el_results$intervals_EN$CET_lower),
                       "BA" = (all_res$el_results$intervals_BA$CET_true - all_res$el_results$intervals_BA$CET_lower) / (all_res$el_results$intervals_BA$CET_upper - all_res$el_results$intervals_BA$CET_lower),
                       "CP" = (all_res$el_results$intervals_CP_m$CET_true - all_res$el_results$intervals_CP_m$CET_lower) / (all_res$el_results$intervals_CP_m$CET_upper - all_res$el_results$intervals_CP_m$CET_lower),
                       "CR" = (all_res$el_results$intervals_CR_m$CET_true - all_res$el_results$intervals_CR_m$CET_lower) / (all_res$el_results$intervals_CR_m$CET_upper - all_res$el_results$intervals_CR_m$CET_lower),
                       "QR" = (all_res$el_results$intervals_QR_m$CET_true - all_res$el_results$intervals_QR_m$CET_lower) / (all_res$el_results$intervals_QR_m$CET_upper - all_res$el_results$intervals_QR_m$CET_lower))

rec_data1 = data.table("Id" = all_res$multi_results$intervals_EN$Id,
                       #"BS" = (all_res$multi_results$intervals_BS$CET_true - all_res$multi_results$intervals_BS$CET_lower) / (all_res$multi_results$intervals_BS$CET_upper - all_res$multi_results$intervals_BS$CET_lower),
                       "EN" = (all_res$multi_results$intervals_EN$CET_true - all_res$multi_results$intervals_EN$CET_lower) / (all_res$multi_results$intervals_EN$CET_upper - all_res$multi_results$intervals_EN$CET_lower),
                       "BA" = (all_res$multi_results$intervals_BA$CET_true - all_res$multi_results$intervals_BA$CET_lower) / (all_res$multi_results$intervals_BA$CET_upper - all_res$multi_results$intervals_BA$CET_lower),
                       "CP" = (all_res$multi_results$intervals_CP_m$CET_true - all_res$multi_results$intervals_CP_m$CET_lower) / (all_res$multi_results$intervals_CP_m$CET_upper - all_res$multi_results$intervals_CP_m$CET_lower),
                       "CR" = (all_res$multi_results$intervals_CR_m$CET_true - all_res$multi_results$intervals_CR_m$CET_lower) / (all_res$multi_results$intervals_CR_m$CET_upper - all_res$multi_results$intervals_CR_m$CET_lower),
                       "QR" = (all_res$multi_results$intervals_QR_m$CET_true - all_res$multi_results$intervals_QR_m$CET_lower) / (all_res$multi_results$intervals_QR_m$CET_upper - all_res$multi_results$intervals_QR_m$CET_lower))

rec_data1 = data.table("Id" = all_res$apparel_results$intervals_BS$Id,
                       "BS" = (all_res$apparel_results$intervals_BS$CET_true - all_res$apparel_results$intervals_BS$CET_lower) / (all_res$apparel_results$intervals_BS$CET_upper - all_res$apparel_results$intervals_BS$CET_lower),
                       "EN" = (all_res$apparel_results$intervals_EN$CET_true - all_res$apparel_results$intervals_EN$CET_lower) / (all_res$apparel_results$intervals_EN$CET_upper - all_res$apparel_results$intervals_EN$CET_lower),
                       "BA" = (all_res$apparel_results$intervals_BA$CET_true - all_res$apparel_results$intervals_BA$CET_lower) / (all_res$apparel_results$intervals_BA$CET_upper - all_res$apparel_results$intervals_BA$CET_lower),
                       "CP" = (all_res$apparel_results$intervals_CP_m$CET_true - all_res$apparel_results$intervals_CP_m$CET_lower) / (all_res$apparel_results$intervals_CP_m$CET_upper - all_res$apparel_results$intervals_CP_m$CET_lower),
                       "CR" = (all_res$apparel_results$intervals_CR_m$CET_true - all_res$apparel_results$intervals_CR_m$CET_lower) / (all_res$apparel_results$intervals_CR_m$CET_upper - all_res$apparel_results$intervals_CR_m$CET_lower),
                       "QR" = (all_res$apparel_results$intervals_QR_m$CET_true - all_res$apparel_results$intervals_QR_m$CET_lower) / (all_res$apparel_results$intervals_QR_m$CET_upper - all_res$apparel_results$intervals_QR_m$CET_lower))
# rec_data = data.table("Method" = rep(methods, 5079),
#                       "Values" = c(rec_data1$BS, rec_data1$EN, rec_data1$BA, rec_data1$CP, rec_data1$QR))

rec_data <- rec_data1[,4:7] %>%
  pivot_longer(cols = everything(), names_to = "Method", values_to = "Value")

ggplot(rec_data, aes(x = Method, y = Value)) +
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 1), alpha = 0.008) +
  geom_half_violin(trim = TRUE, aes(fill = Method), side = "r", scale = 1.5) +
  labs(title = "Density covered by interval",
       y = "Position with respect to PI") +
  ylim(-0.25,1)

#####
# Width-CET
width_data = data.table("Id" = all_res$gift_results$intervals_BS$Id,
                        "BS_width" = ksmooth(all_res$gift_results$intervals_BS$CET_true, (all_res$gift_results$intervals_BS$CET_upper - all_res$gift_results$intervals_BS$CET_lower)/all_res$gift_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "EN_width" = ksmooth(all_res$gift_results$intervals_EN$CET_true, (all_res$gift_results$intervals_EN$CET_upper - all_res$gift_results$intervals_EN$CET_lower)/all_res$gift_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "BA_width" = ksmooth(all_res$gift_results$intervals_BA$CET_true, (all_res$gift_results$intervals_BA$CET_upper - all_res$gift_results$intervals_BA$CET_lower)/all_res$gift_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "CP_width" = ksmooth(all_res$gift_results$intervals_CP_m$CET_true, (all_res$gift_results$intervals_CP_m$CET_upper - all_res$gift_results$intervals_CP_m$CET_lower)/all_res$gift_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "CR_width" = ksmooth(all_res$gift_results$intervals_CR_m$CET_true, (all_res$gift_results$intervals_CR_m$CET_upper - all_res$gift_results$intervals_CR_m$CET_lower)/all_res$gift_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "QR_width" = ksmooth(all_res$gift_results$intervals_QR_m$CET_true, (all_res$gift_results$intervals_QR_m$CET_upper - all_res$gift_results$intervals_QR_m$CET_lower)/all_res$gift_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "true"= all_res$gift_results$intervals_BS$CET_true,
                        "true_kernel" = ksmooth(all_res$gift_results$intervals_EN$CET_true, all_res$gift_results$intervals_CP_m$CET_upper - all_res$gift_results$intervals_CP_m$CET_lower, kernel = "normal", bandwidth = 2)$x
)

width_data = data.table("Id" = all_res$el_results$intervals_BS$Id,
                        "BS_width" = ksmooth(all_res$el_results$intervals_BS$CET_true, (all_res$el_results$intervals_BS$CET_upper - all_res$el_results$intervals_BS$CET_lower)/all_res$el_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "EN_width" = ksmooth(all_res$el_results$intervals_EN$CET_true, (all_res$el_results$intervals_EN$CET_upper - all_res$el_results$intervals_EN$CET_lower)/all_res$el_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "BA_width" = ksmooth(all_res$el_results$intervals_BA$CET_true, (all_res$el_results$intervals_BA$CET_upper - all_res$el_results$intervals_BA$CET_lower)/all_res$el_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "CP_width" = ksmooth(all_res$el_results$intervals_CP_m$CET_true, (all_res$el_results$intervals_CP_m$CET_upper - all_res$el_results$intervals_CP_m$CET_lower)/all_res$el_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "CR_width" = ksmooth(all_res$el_results$intervals_CR_m$CET_true, (all_res$el_results$intervals_CR_m$CET_upper - all_res$el_results$intervals_CR_m$CET_lower)/all_res$el_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "QR_width" = ksmooth(all_res$el_results$intervals_QR_m$CET_true, (all_res$el_results$intervals_QR_m$CET_upper - all_res$el_results$intervals_QR_m$CET_lower)/all_res$el_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "true"= all_res$el_results$intervals_BS$CET_true,
                        "true_kernel" = ksmooth(all_res$el_results$intervals_EN$CET_true, all_res$el_results$intervals_CP_m$CET_upper - all_res$el_results$intervals_CP_m$CET_lower, kernel = "normal", bandwidth = 2)$x
)

width_data = data.table("Id" = all_res$multi_results$intervals_EN$Id,
                        #"BS_width" = ksmooth(all_res$multi_results$intervals_BS$CET_true, (all_res$multi_results$intervals_BS$CET_upper - all_res$multi_results$intervals_BS$CET_lower)/all_res$multi_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "EN_width" = ksmooth(all_res$multi_results$intervals_EN$CET_true, (all_res$multi_results$intervals_EN$CET_upper - all_res$multi_results$intervals_EN$CET_lower)/all_res$multi_results$intervals_EN$CET_true, kernel = "normal", bandwidth = 1)$y,
                        "BA_width" = ksmooth(all_res$multi_results$intervals_BA$CET_true, (all_res$multi_results$intervals_BA$CET_upper - all_res$multi_results$intervals_BA$CET_lower)/all_res$multi_results$intervals_EN$CET_true, kernel = "normal", bandwidth = 1)$y,
                        "CP_width" = ksmooth(all_res$multi_results$intervals_CP_m$CET_true, (all_res$multi_results$intervals_CP_m$CET_upper - all_res$multi_results$intervals_CP_m$CET_lower)/all_res$multi_results$intervals_EN$CET_true, kernel = "normal", bandwidth = 1)$y,
                        "CR_width" = ksmooth(all_res$multi_results$intervals_CR_m$CET_true, (all_res$multi_results$intervals_CR_m$CET_upper - all_res$multi_results$intervals_CR_m$CET_lower)/all_res$multi_results$intervals_EN$CET_true, kernel = "normal", bandwidth = 1)$y,
                        "QR_width" = ksmooth(all_res$multi_results$intervals_QR_m$CET_true, (all_res$multi_results$intervals_QR_m$CET_upper - all_res$multi_results$intervals_QR_m$CET_lower)/all_res$multi_results$intervals_EN$CET_true, kernel = "normal", bandwidth = 1)$y,
                        "true"= all_res$multi_results$intervals_EN$CET_true,
                        "true_kernel" = ksmooth(all_res$multi_results$intervals_EN$CET_true, all_res$multi_results$intervals_CP_m$CET_upper - all_res$multi_results$intervals_CP_m$CET_lower, kernel = "normal", bandwidth = 1)$x
)

width_data = data.table("Id" = all_res$apparel_results$intervals_BS$Id,
                        "BS_width" = ksmooth(all_res$apparel_results$intervals_BS$CET_true, (all_res$apparel_results$intervals_BS$CET_upper - all_res$apparel_results$intervals_BS$CET_lower)/all_res$apparel_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "EN_width" = ksmooth(all_res$apparel_results$intervals_EN$CET_true, (all_res$apparel_results$intervals_EN$CET_upper - all_res$apparel_results$intervals_EN$CET_lower)/all_res$apparel_results$intervals_EN$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "BA_width" = ksmooth(all_res$apparel_results$intervals_BA$CET_true, (all_res$apparel_results$intervals_BA$CET_upper - all_res$apparel_results$intervals_BA$CET_lower)/all_res$apparel_results$intervals_BS$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "CP_width" = ksmooth(all_res$apparel_results$intervals_CP_m$CET_true, (all_res$apparel_results$intervals_CP_m$CET_upper - all_res$apparel_results$intervals_CP_m$CET_lower)/all_res$apparel_results$intervals_EN$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "CR_width" = ksmooth(all_res$apparel_results$intervals_CR_m$CET_true, (all_res$apparel_results$intervals_CR_m$CET_upper - all_res$apparel_results$intervals_CR_m$CET_lower)/all_res$apparel_results$intervals_EN$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "QR_width" = ksmooth(all_res$apparel_results$intervals_QR_m$CET_true, (all_res$apparel_results$intervals_QR_m$CET_upper - all_res$apparel_results$intervals_QR_m$CET_lower)/all_res$apparel_results$intervals_EN$CET_true, kernel = "normal", bandwidth = 2)$y,
                        "true"= all_res$apparel_results$intervals_EN$CET_true,
                        "true_kernel" = ksmooth(all_res$apparel_results$intervals_EN$CET_true, all_res$apparel_results$intervals_CP_m$CET_upper - all_res$apparel_results$intervals_CP_m$CET_lower, kernel = "normal", bandwidth = 2)$x
)

ggplot(data = width_data, aes(x = true_kernel)) +
  geom_line(aes(y = BS_width, color = "BS")) +
  geom_line(aes(y = EN_width, color = "EN")) +
  geom_line(aes(y = BA_width, color = "BA")) +
  geom_line(aes(y = CP_width, color = "CP")) +
  geom_line(aes(y = CP_width, color = "CR")) +
  geom_line(aes(y = QR_width, color = "QR")) +
  scale_color_manual(values = c("BS" = "purple", 
                                "EN" = "yellow", 
                                "BA" = "black", 
                                "CP" = "green", 
                                "CR" = "orange",
                                "QR" = "red")) +
  labs(title = "Width development with CET",
       y = "Width",
       x = "CET") #+
  xlim(0,20)


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="path_to_your_dir")


# Spider chart
ranking_table_spider = averages_table

ranking_table_spider[, Rank_PICP := rank(PICP, ties.method = "min")]
ranking_table_spider[, Rank_ACE := rank(abs(-ACE), ties.method = "min")]
ranking_table_spider[, Rank_MIS := rank(-MIS, ties.method = "min")]
ranking_table_spider[, Rank_MSPIW := rank(-MSPIW, ties.method = "min")]
ranking_table_spider[, Rank_MSPIWW := rank(-MSPIWW, ties.method = "min")]
ranking_table_spider[, Rank_SWR := rank(SWR, ties.method = "min")]
ranking_table_spider[, Rank_UpperCoverage := rank(`Upper coverage`, ties.method = "min")]
ranking_table_spider[, Rank_LowerCoverage := rank(`Lower coverage`, ties.method = "min")]

ranking_table_spider = ranking_table_spider[,-(2:9)]

spider_data = as.data.frame(as.matrix(ranking_table_spider[,2:9]))
colnames(spider_data) = measure_list
rownames(spider_data) = c("BA", "BS", "CP", "CR", "EN", "QR")
spider_data = rbind(rep(6,8) , rep(1,8) , spider_data)
spider_colors = c("red", "black", "darkblue", "lightblue", "grey", "green")
radarchart(spider_data, pcol = spider_colors, cglcol = "white")
legend(x = 0.7, legend = c("BA", "BS", "CP", "CR", "EN", "QR"), bty = "n", pch=20 , col = spider_colors, text.col = "grey", cex=1.2, pt.cex=3)

### Absolute numbers

averages_table_spider = averages_table[,1:9]
averages_table_spider$ACE = abs(averages_table_spider$ACE)
spider_data = as.data.frame(as.matrix(averages_table_spider[,2:9]))
colnames(spider_data) = measure_list
names(spider_data)[c(7,8)] = c("Upper c.","Lower c.")
rownames(spider_data) = c("BA", "BS", "CP", "CR", "EN", "QR")
spider_data$ACE = 1 - spider_data$ACE
max_MIS = max(spider_data$MIS)
min_MIS = min(spider_data$MIS)
spider_data$MIS = max_MIS - spider_data$MIS
max_MSPIW = max(spider_data$MSPIW)
min_MSPIW = min(spider_data$MSPIW)
spider_data$MSPIW = max_MSPIW - spider_data$MSPIW
max_MSPIWW = max(spider_data$MSPIWW)
min_MSPIWW = min(spider_data$MSPIWW)
spider_data$MSPIWW = max_MSPIWW - spider_data$MSPIWW
max_SWR = max(spider_data$SWR)
min_SWR = min(spider_data$SWR)
spider_data = rbind(c(1, 1, max_MIS, max_MSPIW, max_MSPIWW, max_SWR, 1, 1),
                    c(0, 0, 0, 0, 0, min_SWR, 0, 0),
                    spider_data)
radarchart(spider_data, pcol = spider_colors, cglcol = "grey", vlcex = 1.2, title = "Method performance radar chart")
legend(x = 1.1, y = -0.1, legend = c("BA", "BS", "CP", "CR", "EN", "QR"), bty = "n", pch=20 , col = spider_colors, text.col = "grey", cex=1.2, pt.cex=3)
