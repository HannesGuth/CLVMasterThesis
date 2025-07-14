# DESCRIPTION

# This script is run for every data set after the methods were applied
# Define a function for each metric
# Define a function that calls all metric-functions
# Execute all metric function for the respective data set
# Save the results in CET_measures
# Works for CET and PTS

################################################################

rst = list(intervals_BS, intervals_EN, intervals_BA, intervals_QR_m, intervals_CP_m, intervals_CR_m)
measure_list = c("PICP", "ACE", "PICPW", "PIARW","PIARWW", "MSIS", "SWR", "Upper coverage", "Lower coverage", "Time")
CET_measures = data.table("Measure" = measure_list,
                      "BS" = 0,
                      "EN" = 0,
                      "BA" = 0,
                      "QR" = 0,
                      "CP" = 0,
                      "CR" = 0)

PTS_measures = data.table("Measure" = measure_list,
                          "BS" = 0,
                          "EN" = 0,
                          "BA" = 0,
                          "QR" = 0,
                          "CP" = 0,
                          "CR" = 0)

f_PICP = function(true, lower, upper){
  vec = true >= lower & true <= upper
  return(mean(vec))
}

f_ACE = function(true, lower, upper, alpha){
  return((1-alpha) - f_PICP(true, lower, upper))
}

f_UC = function(true, upper){
  return(sum(true <= upper)/length(true))
}

f_LC = function(true, lower){
  return(sum(true >= lower)/length(true))
}

f_MSIS = function(true, lower, upper, est, alpha){
  equ = (est == 0) * sort(unique(est))[2] + est
  #equ = ((upper + lower)/2 == 0) * sort(unique((upper+lower)/2))[2] + (upper+lower)/2
  return(sum((upper - lower)/equ + (2/alpha) * ((true > upper)*((true-upper)/equ) + (true < lower)*((lower-true)/equ))) / length(true)) # scaling should be done by true value or estimation, not by upper but one cannot divide by 0
}

# f_BIAS = function(true, est){
#   return(sum(true - est) / sum(true))
# }

f_PICPW = function(true, lower, upper, est){
  # equ = (est == 0) * sort(unique(est))[2] + est
  weight = true / sum(true)
  vec = (true >= lower & true <= upper) * weight
  return(sum(vec))
}

f_PIARW = function(lower, upper, est){
  equ = (est == 0) * sort(unique(est))[2] + est # should never be used because est will actually nevery be exactly 0 or negative
  return(mean((upper - lower) / equ))
}

f_PIARWW = function(lower, upper, est, true){
  equ = (est == 0) * sort(unique(est))[2] + est
  weight = true / sum(true)
  return(sum(((upper - lower)/equ)*weight))
}

f_SWR = function(true, lower, upper, est){
  return(f_PICP(true,lower,upper) / f_PIARW(lower, upper, est))
}

f_measures = function(true, lower, upper, est, alpha, comp_time){
  res = c(
         f_PICP(true, lower, upper),
         f_ACE(true, lower, upper, alpha),
         f_PICPW(true, lower, upper, est),
         f_PIARW(lower, upper, est),
         f_PIARWW(lower, upper, est, true),
         f_MSIS(true, lower, upper, est, alpha),
         f_SWR(true, lower, upper, est),
         f_UC(true, upper),
         f_LC(true, lower),
         comp_time
         )
  return(res)
}

for (i in 1:length(rst)){
  CET_measures[1:10, i+1] = f_measures(rst[[i]]$CET_true, rst[[i]]$CET_lower, rst[[i]]$CET_upper, rst[[i]]$CET_prediction, alpha, times[i])
  PTS_measures[1:10, i+1] = f_measures(rst[[i]]$PTS_true, rst[[i]]$PTS_lower, rst[[i]]$PTS_upper, rst[[i]]$PTS_prediction, alpha, times[i])
}
CET_measures
PTS_measures
