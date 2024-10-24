man_data = as.data.table(app_data[1])

man_data$abs = man_data$CET_prediction - man_data$CET_lower
man_data$rel = man_data$abs / man_data$CET_prediction
man_data$hpp_s_t = 0
man_data$hpp_s_t <- with(man_data, ifelse(max_s & hpp_s, "TT",
                                          ifelse(max_s & !hpp_s, "TF",
                                                 ifelse(!max_s & hpp_s, "FT",
                                                        "FF"))))
man_data$hpp_s_t = factor(man_data$hpp_s_t)

ggplot(man_data, aes(x = CET_prediction, y = CET_true)) +
  geom_point(aes(color = hpp_s_t)) +
  scale_color_manual(values = c("TT" = "green",
                                "FF" = "yellow",
                                "TF" = "blue",
                                "FT" = "red"))

# Visualize
#####
counter = 0
meas = c("CET_lower", "CET_upper", "CET_prediction", "PTS_prediction", "hul", "hiw", "huu", "htp", "c2w", "ssq", "hll", "hl2")
#meas = c("CET_lower", "CET_upper", "CET_prediction", "rel", "abs")
for (m in meas){
  for (n in meas){
    title = paste0(m, ", ", n)
    ggplot(man_data, aes(y = man_data[[m]], x = man_data[[n]])) +
          geom_point(aes(color = hpp_s_t, shape = hpp_s_t)) +
          scale_color_manual(values = c("TT" = "green",
                                        "FF" = "yellow",
                                        "TF" = "blue",
                                        "FT" = "red")) +
          labs(x = m, y = n)
    ggsave(filename = file.path("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Plots/Manual analysis/2D", paste0(title,".png")), width = 7, height = 4.5)
    for (o in meas){
      title = paste0(m, ", ", n, ", ", o)
      png(filename = file.path("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Plots/Manual analysis/3D", paste0(title,".png")), width = 8, height = 6, units = "in", res = 300)
      par(mai = c(0.5, 0.5, 0.5, 0.5))
      s3d <- scatterplot3d(x = man_data[[m]], y = man_data[[m]], z = man_data[[o]],
                           color = c("yellow", "red", "blue", "green")[man_data$hpp_s_t],
                           cex.symbols = 1, pch = 19, angle = -30, main = title)
      # Add a legend
      legend(s3d$xyz.convert(3,0.3,-0.6),
             legend = levels(man_data$hpp_s_t), col = c("yellow", "red", "blue", "green"), pch = 19, bty = "n")
      dev.off()
      counter = counter + 1
      print(round(counter/6859, 4))
    }
  }
}

# Clustering (wrong number of observations per cluster)
#####
# Create sample data
set.seed(123)
data <- data.frame(
  var1 = man_data$CET_prediction,
  var2 = man_data$hpp,
  var3 = man_data$htp
)

# Scale the data (optional but recommended)
data_scaled <- scale(data)

# Perform hierarchical clustering
dist_matrix <- dist(data_scaled)  # Compute the distance matrix
hc <- hclust(dist_matrix, method = "ward.D2")  # Perform hierarchical clustering

# Plot the dendrogram
plot(hc, labels = FALSE, main = "Dendrogram")

# Cut the dendrogram into 3 clusters
clusters <- cutree(hc, k = 2)

fviz_cluster(list(data = data_scaled, cluster = clusters))
clu_data = data.table(data_scaled, clusters, man_data$max_s)
clu_data$clusters = ifelse(clu_data$clusters == 2, 0, 1)
mean(clu_data$clusters == clu_data$V3)

#####
cl1_dat = as.data.table(app_data[1])
cl1_dat = cl1_dat[,..meas]
cl1_dat = cl1_dat[, !grepl("_s", names(cl1_dat)), with = FALSE]