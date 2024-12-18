# DESCRIPTION

# This script was only set up to produce the plots for the last page of the Appendix with a varying correlation
# Uses code snippets from other scripts

################################################################


cor_list = seq(-1, 1, by = 0.1)
counter_ = 0

for (cor in cor_list){
  all_res_cov = all_res
  cov_methods = c("intervals_BS", "intervals_EN", "intervals_BA", "intervals_QR_m", "intervals_CP_m", "intervals_CR_m")
  counter_ = counter_ + 1
  
  for (dataset in data_lists2){
    for (method in cov_methods){
      all_res_cov[[dataset$name]][[method]] = all_res_cov[[dataset$name]][[method]][, -(7:11)]
      all_res_cov[[dataset$name]][[method]][,"abs_diff"] = abs(all_res_cov[[dataset$name]][[method]][,"CET_true"] - all_res_cov[[dataset$name]][[method]][,"CET_prediction"])
      
      # Logarithmic
      # all_res_cov[[dataset$name]][[method]][,"abs_log_diff"] = log(as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"abs_diff"])))
      # all_res_cov[[dataset$name]][[method]][,"abs_log_diff"] = all_res_cov[[dataset$name]][[method]][,"abs_log_diff"] + abs(min(all_res_cov[[dataset$name]][[method]][,"abs_log_diff"]))
      # all_res_cov[[dataset$name]][[method]][,"result"] = runif(n = nrow(all_res_cov[[dataset$name]][[method]]), min = as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"abs_log_diff"]))/1.3, max = as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"abs_log_diff"]))*1.3)
      
      # Random series with correlation
      target_cor = as.numeric(unlist(cor))
      abs_diff = as.numeric(unlist(all_res_cov[[dataset$name]][[method]][, "abs_diff"]))
      set.seed(1)
      noise = rnorm(length(abs_diff))
      result = target_cor * scale(abs_diff) + sqrt(1 - target_cor^2) * scale(noise)
      result = (result - min(result) + 0.01) * 10 # *10 just to show that it does not depend on the scale being similar to the absolute prediction error
      all_res_cov[[dataset$name]][[method]][, "result"] = result
      
      # Abs diff + randomly something
      # all_res_cov[[dataset$name]][[method]][,"result"] = ifelse(rnorm(nrow(all_res_cov[[dataset$name]][[method]])) > 1.2, all_res_cov[[dataset$name]][[method]][,"abs_diff"] + (all_res_cov[[dataset$name]][[method]][,"abs_diff"] * rnorm(nrow(all_res_cov[[dataset$name]][[method]]))), all_res_cov[[dataset$name]][[method]][,"abs_diff"])
      # all_res_cov[[dataset$name]][[method]][,"result"] = all_res_cov[[dataset$name]][[method]][,"abs_diff"]
      
      # med = as.numeric(quantile(as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"result"])), 0.5))
      med = mean(as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"result"])))
      initial_length = as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"CET_upper"] - all_res_cov[[dataset$name]][[method]][,"CET_lower"]))
      fac = as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"result"])) / med
      new_length_half = initial_length * fac * 0.5
      # center = (all_res_cov[[dataset$name]][[method]][,"CET_upper"] - all_res_cov[[dataset$name]][[method]][,"CET_lower"]) / 2
      center = all_res_cov[[dataset$name]][[method]][,"CET_prediction"]
      all_res_cov[[dataset$name]][[method]][,"CET_lower"] = ((center - new_length_half) <= 0) * 0 + ((center - new_length_half) >= 0) * (center - new_length_half)
      all_res_cov[[dataset$name]][[method]][,"CET_upper"] = center + new_length_half
      all_res_cov[[dataset$name]][[method]][,"CET_covered"] = (all_res_cov[[dataset$name]][[method]][,"CET_lower"] <= all_res_cov[[dataset$name]][[method]][,"CET_true"]) &  (all_res_cov[[dataset$name]][[method]][,"CET_upper"] >= all_res_cov[[dataset$name]][[method]][,"CET_true"])
      print(paste(dataset$name, ",", method, ",", med, ",", "PIARW:", mean(as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"abs_diff"] / all_res_cov[[dataset$name]][[method]][,"CET_prediction"])))))
    }
    intervals_BS = all_res_cov[[dataset$name]][["intervals_BS"]]
    intervals_EN = all_res_cov[[dataset$name]][["intervals_EN"]]
    intervals_BA = all_res_cov[[dataset$name]][["intervals_BA"]]
    intervals_QR_m = all_res_cov[[dataset$name]][["intervals_QR_m"]]
    intervals_CP_m = all_res_cov[[dataset$name]][["intervals_CP_m"]]
    intervals_CR_m = all_res_cov[[dataset$name]][["intervals_CR_m"]]
    source(paste0(getwd(), "/Covariates benchmarking.r"))
    all_res_cov[[dataset$name]][["CET_measures"]] = CET_measures
  }
  
  all_res_cov[[dataset$name]][[method]]
  all_res[[dataset$name]][[method]]
  fac[1:5]
  cor(as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"result"])), as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"abs_diff"])))
  plot(as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"result"])), as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"abs_diff"])))
  
  source(paste0(getwd(), "/Covariates data reorganization.r"))
  source(paste0(getwd(), "/Covariates results analysis.r"))
  source(paste0(getwd(), "/Covariates application in marketing.R"))
  
  # Plotting
  plot_selection_data = pivot_longer(perf_overview_cov, cols = hpp:ssq, names_to = "Metric", values_to = "Achieved_Number")
  
  method_colors = c("BS" = "pink", "EN" = "grey", "BA" = "green", "CP" = "red", "QR" = "blue")
  title = paste0("Valuable customer selection by method and metric (r = ", cor, ")")
  ggplot(plot_selection_data, aes(x = Data, y = Achieved_Number, color = Method, shape = Metric)) +
    geom_point(position = position_dodge(width = 0.5, preserve = "total"), size = 3,
               aes(color = ifelse(Metric == "hpp", "black", Method), group = Method)) +
    scale_color_manual(values = c(method_colors, "black" = "black"),
                       breaks = c("BS", "EN", "BA", "QR", "CP"), name = "Method") +
    theme(axis.text=element_text(size=18),
          axis.title=element_text(size=18),
          plot.title = element_text(size=18),
          legend.text = element_text(size=16),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "white", size = 0.5)) +
    scale_shape_manual(values = c("hpp" = 15, "hub" = 1, "hiw" = 16, "huu" = 3, "htp" = 4, "csw" = 17, "ssq" = 6),
                       breaks = c("hpp", "hub", "hiw", "huu", "htp", "csw", "ssq")) +
    labs(title = title, x = "Data set", y = "Share of valuable customers selected", shape = "Metric") +
    scale_y_continuous(limits = c(0, 1))
  ggsave(filename = file.path(plot_path = paste0(getwd(), "/Covariates plots/Valuable customers/", counter_, ". ", title, ".png")), width = 9, height = 4.5, limitsize = FALSE)
}
