# DESCRIPTION

# Plot the results of the stability analysis of different learning and prediction periods

################################################################

big_grid_gift = readRDS(paste0(getwd(), "/Results/big_grid_gift", ".RData"))
big_grid_el = readRDS(paste0(getwd(), "/Results/big_grid_el", ".RData"))

big_grid_gift = big_grid_gift[CP_CET != 0]

ggplot(big_grid_gift) +
  # geom_density(aes(BS_CET, color = "BS")) +
  # geom_density(aes(EN_CET, color = "EN")) +
  geom_density(aes(BA_CET, color = "BA")) +
  geom_density(aes(QR_CET, color = "QR")) +
  geom_density(aes(CP_CET, color = "CP")) +
  geom_density(aes(CR_CET, color = "CR")) +
  scale_color_manual(values = c("BA" = "green", "CP" = "purple", "CR" = "orange", "QR" = "pink")) +
  xlim(0,1)

# With all values

big_grid_gift_long = reshape2::melt(big_grid_gift[,7:10])
big_grid_el_long = reshape2::melt(big_grid_el[,7:10])

big_grid_gift_long$Dataset = "gift"
big_grid_el_long$Dataset = "el"

combined_long = rbind(big_grid_gift_long, big_grid_el_long)

title = "Performance over different periods"
ggplot(combined_long, aes(x = variable, y = value*100, fill = Dataset)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "red") +  # Horizontal line at 90%
  annotate("text", x = Inf, y = 90, label = "90%", hjust = 17.5, vjust = -0.5, color = "red") +  # Add label for horizontal line
  labs(x = "Methods", y = "Coverage in %", title = title) +
  scale_fill_manual(values = c("gift" = "green", "el" = "yellow")) +
  scale_x_discrete(labels = c("BA", "QR", "CP", "CR")) +
  coord_cartesian(xlim = c(0.7, length(unique(combined_long$variable)))) +
  theme(
    axis.title.y = element_text(color = "black"),
    legend.title = element_blank(),
    axis.text=element_text(size=12),
    axis.title=element_text(size=12),
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_line(colour = "white", size = 0.5))
ggsave(filename = file.path(paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)

# Without splitweek1 = 20
big_grid_gift = big_grid_gift[splitweek1 != 20,]

big_grid_gift_long = reshape2::melt(big_grid_gift[,7:10])
big_grid_el_long = reshape2::melt(big_grid_el[,7:10])

big_grid_gift_long$Dataset = "gift"
big_grid_el_long$Dataset = "el"

combined_long = rbind(big_grid_gift_long, big_grid_el_long)

title = "Performance over different periods (without low l1-values for gift)"
ggplot(combined_long, aes(x = variable, y = value*100, fill = Dataset)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(x = "Methods", y = "Coverage in %", title = title) +
  scale_fill_manual(values = c("gift" = "green", "el" = "yellow")) +
  scale_x_discrete(labels = c("BA", "QR", "CP", "CR")) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "red") +  # Horizontal line at 90%
  annotate("text", x = Inf, y = 90, label = "90%", hjust = 17.5, vjust = -0.5, color = "red") +  # Add label for horizontal line
  coord_cartesian(xlim = c(0.7, length(unique(combined_long$variable)))) +
  theme(
    axis.title.y = element_text(color = "black"),
    legend.title = element_blank(),
    axis.text=element_text(size=12),
    axis.title=element_text(size=12),
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_line(colour = "white", size = 0.5))
ggsave(filename = file.path(paste0(getwd(), "/Plots/", title, ".png")), width = 7, height = 3.5)

# Relation between period sum and coverage
psc_gift_data = data.table("sum" = rowSums(big_grid_gift[,1:2]),
                            "BA" = big_grid_gift$BA_CET,
                            "QR" = big_grid_gift$QR_CET,
                            "CP" = big_grid_gift$CP_CET,
                            "CR" = big_grid_gift$CR_CET)

psc_el_data = data.table("sum" = rowSums(big_grid_el[,1:2]),
                         "BA" = big_grid_el$BA_CET,
                         "QR" = big_grid_el$QR_CET,
                         "CP" = big_grid_el$CP_CET,
                         "CR" = big_grid_el$CR_CET)


############

# # gift2
# clv.gift2 <- clvdata(gift2,  
#                      date.format="ymd", 
#                      time.unit = "week",
#                      estimation.split = 20,
#                      name.id = "Id",
#                      name.date = "Date",
#                      name.price = "Price")
# est.gift2 = pnbd(clv.data = clv.gift2)
# results.gift2 = predict(est.gift2, predict.spending = TRUE)
# results.gift2 = predict(est.gift2, predict.spending = TRUE, prediction.end = 40)
# 
# # gift1
# clv.gift1 <- clvdata(gift1,  
#                      date.format="ymd", 
#                      time.unit = "week",
#                      estimation.split = 20,
#                      name.id = "Id",
#                      name.date = "Date",
#                      name.price = "Price")
# est.gift1 = pnbd(clv.data = clv.gift1)
# results.gift1 = predict(est.gift1, predict.spending = TRUE)
# results.gift1 = predict(est.gift1, predict.spending = TRUE, prediction.end = 40)
# 
# 
# #######
# 
# 
# clv.el1 <- clvdata(el1,
#                    date.format="ymd", 
#                    time.unit = "week",
#                    estimation.split = 20, # 10,20,50,80,120
#                    name.id = "Id",
#                    name.date = "Date",
#                    name.price = "Price")
# mod.el1 = pnbd(clv.data = clv.el1)
# results.el1 = predict(mod.el1, predict.spending = TRUE)
# results.el1 = predict(mod.el1, predict.spending = TRUE, prediction.end = 20)
# 
# clv.el2 <- clvdata(el2,  
#                    date.format="ymd", 
#                    time.unit = "week",
#                    estimation.split = 20, # 15,50,80,140
#                    name.id = "Id",
#                    name.date = "Date",
#                    name.price = "Price")
# mod.el2 = pnbd(clv.data = clv.el2)
# results.el2 = predict(mod.el2, predict.spending = TRUE)
# results.el2 = predict(mod.el2, predict.spending = TRUE, prediction.end = 40)
