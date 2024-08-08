big_grid_gift = readRDS("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Backup/big_grid.RData")
big_grid_el = readRDS("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Backup/big_grid_gift.RData")

big_grid_gift = big_grid[CP_CET != 0]

ggplot(big_grid_gift) +
  # geom_density(aes(BS_CET, color = "BS")) +
  # geom_density(aes(EN_CET, color = "EN")) +
  geom_density(aes(BA_CET, color = "BA")) +
  geom_density(aes(QR_CET, color = "QR")) +
  geom_density(aes(CP_CET, color = "CP")) +
  geom_density(aes(CR_CET, color = "CR")) +
  scale_color_manual(values = c("BA" = "green", "CP" = "purple", "CR" = "orange", "QR" = "pink")) +
  xlim(0,1)

big_grid_gift_long = reshape2::melt(big_grid_gift[,7:10])

ggplot(big_grid_gift_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Columns", y = "Values", title = "Boxplots of the Four Columns")



############

# gift2
clv.gift2 <- clvdata(gift2,  
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = 20,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.gift2 = pnbd(clv.data = clv.gift2)
results.gift2 = predict(est.gift2, predict.spending = TRUE)
results.gift2 = predict(est.gift2, predict.spending = TRUE, prediction.end = 40)

# gift1
clv.gift1 <- clvdata(gift1,  
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = 20,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.gift1 = pnbd(clv.data = clv.gift1)
results.gift1 = predict(est.gift1, predict.spending = TRUE)
results.gift1 = predict(est.gift1, predict.spending = TRUE, prediction.end = 40)


#######


clv.el1 <- clvdata(el1,
                   date.format="ymd", 
                   time.unit = "week",
                   estimation.split = 20, # 10,20,50,80,120
                   name.id = "Id",
                   name.date = "Date",
                   name.price = "Price")
mod.el1 = pnbd(clv.data = clv.el1)
results.el1 = predict(mod.el1, predict.spending = TRUE)
results.el1 = predict(mod.el1, predict.spending = TRUE, prediction.end = 20)

clv.el2 <- clvdata(el2,  
                   date.format="ymd", 
                   time.unit = "week",
                   estimation.split = 20, # 15,50,80,140
                   name.id = "Id",
                   name.date = "Date",
                   name.price = "Price")
mod.el2 = pnbd(clv.data = clv.el2)
results.el2 = predict(mod.el2, predict.spending = TRUE)
results.el2 = predict(mod.el2, predict.spending = TRUE, prediction.end = 40)
