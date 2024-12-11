
numeric_columns = names(coverage_table_cov)[sapply(coverage_table_cov, is.numeric)]
coverage_table_cov[, (numeric_columns) := lapply(.SD, round, digits = 4), .SDcols = numeric_columns]
print(coverage_table_cov)

# Averages table
averages_table_cov = coverage_table_cov[, .(
  PICP = mean(PICP, na.rm = TRUE),
  ACE = mean(ACE, na.rm = TRUE),
  PICPW = mean(PICPW, na.rm = TRUE),
  PIARW = mean(PIARW, na.rm = TRUE),
  PIARWW = mean(PIARWW, na.rm = TRUE),
  MSIS = mean(MSIS, na.rm = TRUE),
  SWR = mean(SWR, na.rm = TRUE),
  `Upper coverage` = mean(`Upper coverage`, na.rm = TRUE),
  `Lower coverage` = mean(`Lower coverage`, na.rm = TRUE),
  Time = mean(Time, na.rm = TRUE)
), by = Method]


# Coefficient of variation table
cv_table_cov = coverage_table_cov[, .(
  PICP = sd(PICP, na.rm = TRUE)/mean(PICP, na.rm = TRUE),
  ACE = sd(ACE, na.rm = TRUE)/mean(ACE, na.rm = TRUE),
  PICPW = sd(PICPW, na.rm = TRUE)/mean(PICPW, na.rm = TRUE),
  PIARW = sd(PIARW, na.rm = TRUE)/mean(PIARW, na.rm = TRUE),
  PIARWW = sd(PIARWW, na.rm = TRUE)/mean(PIARWW, na.rm = TRUE),
  MSIS = sd(MSIS, na.rm = TRUE)/mean(MSIS, na.rm = TRUE),
  SWR = sd(SWR, na.rm = TRUE)/mean(SWR, na.rm = TRUE),
  `Upper coverage` = sd(`Upper coverage`, na.rm = TRUE)/mean(`Upper coverage`, na.rm = TRUE),
  `Lower coverage` = sd(`Lower coverage`, na.rm = TRUE)/mean(`Lower coverage`, na.rm = TRUE),
  Time = sd(Time, na.rm = TRUE)/mean(Time, na.rm = TRUE)
), by = Method]

# Ranking table
ranking_table_cov = copy(averages_table_cov)

ranking_table_cov[, Rank_PICP := rank(-PICP, ties.method = "min")]
ranking_table_cov[, Rank_ACE := rank(abs(ACE), ties.method = "min")]
ranking_table_cov[, Rank_PICPW := rank(-PICPW, ties.method = "min")]
ranking_table_cov[, Rank_PIARW := rank(PIARW, ties.method = "min")]
ranking_table_cov[, Rank_PIARWW := rank(PIARWW, ties.method = "min")]
ranking_table_cov[, Rank_MSIS := rank(MSIS, ties.method = "min")]
ranking_table_cov[, Rank_SWR := rank(-SWR, ties.method = "min")]
ranking_table_cov[, Rank_UpperCoverage := rank(-`Upper coverage`, ties.method = "min")]
ranking_table_cov[, Rank_LowerCoverage := rank(-`Lower coverage`, ties.method = "min")]
ranking_table_cov[, Rank_Time := rank(Time, ties.method = "min")]

ranking_table_cov = ranking_table_cov[,-(2:11)]

write.csv(averages_table_cov, paste0(getwd(), "/Results/averages_table_cov.csv"))
write.csv(ranking_table_cov, paste0(getwd(), "/Results/ranking_table_cov.csv"))
write.csv(cv_table_cov, paste0(getwd(), "/Results/cv_table_cov.csv"))
