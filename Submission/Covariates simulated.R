# DESCRIPTION

# Simulates a covriate that is correlated with the prediction error
# Builds based on this covariate and the initial intervals new PIs for every customer
# Calls the other Covariates methods

################################################################


all_res_cov = all_res
cov_methods = c("intervals_BS", "intervals_EN", "intervals_BA", "intervals_QR_m", "intervals_CP_m", "intervals_CR_m")

for (dataset in data_lists2){
  for (method in cov_methods){
    all_res_cov[[dataset$name]][[method]] = all_res_cov[[dataset$name]][[method]][, -(7:11)]
    all_res_cov[[dataset$name]][[method]][,"abs_diff"] = abs(all_res_cov[[dataset$name]][[method]][,"CET_true"] - all_res_cov[[dataset$name]][[method]][,"CET_prediction"])
    
    # Logarithmic
    # all_res_cov[[dataset$name]][[method]][,"abs_log_diff"] = log(as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"abs_diff"])))
    # all_res_cov[[dataset$name]][[method]][,"abs_log_diff"] = all_res_cov[[dataset$name]][[method]][,"abs_log_diff"] + abs(min(all_res_cov[[dataset$name]][[method]][,"abs_log_diff"]))
    # all_res_cov[[dataset$name]][[method]][,"result"] = runif(n = nrow(all_res_cov[[dataset$name]][[method]]), min = as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"abs_log_diff"]))/1.3, max = as.numeric(unlist(all_res_cov[[dataset$name]][[method]][,"abs_log_diff"]))*1.3)
    
    # Random series with correlation
    target_cor = 0.6
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
source(paste0(getwd(), "/Covariates plotting.r"))
source(paste0(getwd(), "/Covariates correlations.r"))

# Comparing the actual results and the results obtained by inventing a covariate (PICP and PIARW only)
comp = data.table("Method" = coverage_table$Method,
                  "Data" = coverage_table$Data,
                  "PICP_reg" = coverage_table$PICP,
                  "PICP_cov" = coverage_table_cov$PICP,
                  "PIARW_reg" = coverage_table$PIARW,
                  "PIARW_cov" = coverage_table_cov$PIARW)
