# DESCRIPTION

# First attempt to introduce an existing covariate that actually exists
# Not used in the work

################################################################


all_res = readRDS(paste0(getwd(), "/Results - Kopie/all_res.RData"))
path = paste0(getwd(), "/Data/durdata1_final.csv")
el_cov = fread(path)
el_cov$HOUSEHOLD_ID = as.character(el_cov$HOUSEHOLD_ID)
el_cov = merge(x = all_res$el_results$intervals_CP_m[, .(Id, CET_true, CET_prediction)],
               y = unique(el_cov[,c("HOUSEHOLD_ID", "GENDER_INDIVIDUAL", "AGE_H_HEAD")]),
               by.x = "Id", by.y = "HOUSEHOLD_ID",
               all.x = TRUE)
el_cov = el_cov[!(el_cov$GENDER_INDIVIDUAL == "")]
el_cov$diff = el_cov$CET_true - el_cov$CET_prediction
el_cov = el_cov[complete.cases(el_cov)]

index = sample(el_cov$Id, size = nrow(el_cov) * 0.3)
train = el_cov[!(Id %in% index),]
test = el_cov[(Id %in% index),]

mod = lm(el_cov$diff ~ el_cov$CET_prediction + el_cov$AGE_H_HEAD + (el_cov$GENDER_INDIVIDUAL == "F"), data = train)
summary(mod)

ggplot(el_cov, aes(x = GENDER_INDIVIDUAL, y = abs(diff))) +
  geom_point()

ggplot(el_cov, aes(x = AGE_H_HEAD, y = abs(diff))) +
  geom_point()

el_cov[, avg_diff := sapply(AGE_H_HEAD, function(age) {
  weights <- dnorm((AGE_H_HEAD - age) / bandwidth)
  sum(weights * diff) / sum(weights)
})]

el_cov[, avg_diff_abs := sapply(AGE_H_HEAD, function(age) {
  weights <- dnorm((AGE_H_HEAD - age) / bandwidth)
  sum(weights * abs(diff)) / sum(weights)
})]

ggplot(el_cov) +
  geom_point(aes(x = AGE_H_HEAD, y = avg_diff)) +
  geom_line(aes(x = AGE_H_HEAD, y = avg_diff_abs))
