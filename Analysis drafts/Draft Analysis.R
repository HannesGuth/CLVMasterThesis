
numeric_columns = names(coverage_table)[sapply(coverage_table, is.numeric)]
coverage_table[, (numeric_columns) := lapply(.SD, round, digits = 4), .SDcols = numeric_columns]
print(coverage_table)

# Averages table
averages_table = coverage_table[, .(
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
cv_table = coverage_table[, .(
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
ranking_table = copy(averages_table)

ranking_table[, Rank_PICP := rank(-PICP, ties.method = "min")]
ranking_table[, Rank_ACE := rank(abs(ACE), ties.method = "min")]
ranking_table[, Rank_PICPW := rank(-PICPW, ties.method = "min")]
ranking_table[, Rank_PIARW := rank(PIARW, ties.method = "min")]
ranking_table[, Rank_PIARWW := rank(PIARWW, ties.method = "min")]
ranking_table[, Rank_MSIS := rank(MSIS, ties.method = "min")]
ranking_table[, Rank_SWR := rank(-SWR, ties.method = "min")]
ranking_table[, Rank_UpperCoverage := rank(-`Upper coverage`, ties.method = "min")]
ranking_table[, Rank_LowerCoverage := rank(-`Lower coverage`, ties.method = "min")]
ranking_table[, Rank_Time := rank(Time, ties.method = "min")]

ranking_table = ranking_table[,-(2:11)]
