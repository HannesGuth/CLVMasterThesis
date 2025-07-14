plot_list = list()

for (l in data_lists2) {
  plot_data = l[["data2"]][, .N, by = Date]
  
  p = ggplot(plot_data, aes(x = Date, y = N)) +
    geom_line() +
    geom_vline(xintercept = (min(l$data2$Date) + as.numeric(l$l2) * 7), color = "red") +
    geom_vline(xintercept = (min(l$data2$Date) + as.numeric(l$l2) * 7 + as.numeric(l$p2) * 7), color = "red") +
    labs(title = l$name, x = "", y = "Purchases")
  
  plot_list = append(plot_list, list(p))
}

grid.arrange(grobs = plot_list, nrow = 2, ncol = 2)

for (l in data_lists2) {
  ipt = l[["data2"]][, .(Interpurchase_Time = diff(sort(Date))), by = Id]
  ipt = ipt[, mean(Interpurchase_Time), by = Id]
  print(l$name)
  print(paste("Interpurchase time:", mean(ipt$V1)))
  print(paste("customers:", length(unique(l[["data2"]]$Id))))
  print(paste("Min. Date:", min(l$data2$Date)))
  print("")
}

