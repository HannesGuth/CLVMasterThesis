# Purpose: Discount true spendings, to get the true CLV
# 3 ways:
#   - Discount everything to the first period (INcluding the first transaction) in "All periods"  (should be wrong)
#   - Discount everything to the first period (EXcluding the first transaction) in "All periods"  (should be wrong)
#   - Discount everything from the holdout period to the first period in "Only holdout period"    (should be correct)
# with 3 different discount factors
#   - (1+0.1/52)              [""]        (should be correct)
#   - (1+(log(1+0.1)/52)      ["log"]     (should be correct)
#   - (1+0.1)                 ["Direct"]  (should be wrong)
#
# -----------------------> 9 columns

# # From (1L)
library(CLVTools)
library(data.table)
splitWeek = 40
# Load data
data("apparelTrans")
clv.apparel <- clvdata(apparelTrans,  
                       date.format="ymd", 
                       time.unit = "week",
                       estimation.split = splitWeek,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")

# Estimate standard Pareto/NBD Model
est.pnbd <- pnbd(clv.data = clv.apparel, verbose = TRUE)
summary(est.pnbd)
results <- predict(est.pnbd, predict.spending = TRUE)

##### All periods

# Copy apparelTrans
allPeriods = apparelTrans
# Calculate the week difference of transactions to the first date
allPeriods$weeks = as.numeric(difftime(allPeriods$Date, as.Date("2005-01-03"), units = "weeks"))
# Divide annual discount rate by 52 [weeks] and discount it with the number of weeks
allPeriods$DiscountedPrice = allPeriods$Price/((1+0.1/52)^allPeriods$weeks)
# Divide annual discount rate by 52 [weeks] and discount it with the number of weeks (continuous rate)
allPeriods$DiscountedPriceLog = allPeriods$Price/((1+(log(1+0.1)/52))^allPeriods$weeks)
# Discount with 0.1 ^ number of weeks (continuous rate)
allPeriods$DiscountedDirect = allPeriods$Price/((1+0.1)^allPeriods$weeks)
# Aggregate the discounted prices by customer and compare them with the predicted CLV (including the first transaction)
intermediate = merge(x = results[, c("Id", "predicted.CLV", "actual.total.spending")], allPeriods[, sum(DiscountedPrice), by = Id], by = "Id", all.x = TRUE)
colnames(intermediate) = c("Id", "predicted.CLV", "actual.total.spending", "allwithfirst(1L)")
# Aggregate the discounted prices by customer and compare them with the predicted CLV (including the first transaction) (continuous rate)
intermediate = merge(x = intermediate[, c("Id", "predicted.CLV", "actual.total.spending", "allwithfirst(1L)")], allPeriods[, sum(DiscountedPriceLog), by = Id], by = "Id", all.x = TRUE)
colnames(intermediate) = c("Id", "predicted.CLV", "actual.total.spending", "allwithfirst(1L)", "allwithfirstlog(1L)")
# Aggregate the discounted prices by customer and compare them with the predicted CLV (including the first transaction) (direct)
intermediate = merge(x = intermediate[, c("Id", "predicted.CLV", "actual.total.spending","allwithfirst(1L)", "allwithfirstlog(1L)")], allPeriods[, sum(DiscountedDirect), by = Id], by = "Id", all.x = TRUE)
colnames(intermediate) = c("Id", "predicted.CLV", "actual.total.spending", "allwithfirst(1L)", "allwithfirstlog(1L)", "Direct(1L)")
# Deduct each customer's first purchase from the previous results
intermediate$allwithoutfirst = intermediate$`allwithfirst(1L)` - apparelTrans[!duplicated(apparelTrans$Id),]$Price
intermediate$allwithoutfirstlog = intermediate$`allwithfirstlog(1L)` - apparelTrans[!duplicated(apparelTrans$Id),]$Price
intermediate$allwithoutfirstDirect = intermediate$`Direct(1L)` - apparelTrans[!duplicated(apparelTrans$Id),]$Price
allPeriods = intermediate
allPeriods

##### Only holdout period

# Copy apparelTrans
intermediate = apparelTrans
# Only those transactions that occur in the holdout period
intermediate = intermediate[Date > as.Date("2005-01-03") + splitWeek * 7,]
# Calculate the week difference of transactions to the first date
intermediate$weeks = as.numeric(difftime(intermediate$Date, as.Date("2005-01-03"), units = "weeks"))
# Discount the transactions with the previously calculated weeks
intermediate$DiscountedPrice = intermediate$Price/((1+0.1/52)^intermediate$weeks)
# Discount the transactions with the previously calculated weeks (continuous rate)
intermediate$DiscountedPriceLog = intermediate$Price/((1+(log(1+0.1)/52))^intermediate$weeks)
# Discount the transactions with the previously calculated weeks (direct)
intermediate$DiscountedPriceDirect = intermediate$Price/((1+0.1)^intermediate$weeks)
# Aggregate with discounted prices
onlyHoldout = intermediate[, sum(DiscountedPrice), by = "Id"]
colnames(onlyHoldout) = c("Id", "OnlyHoldout")
# Aggregate with discounted prices (continuous rate)
onlyHoldout = merge(x = onlyHoldout[,c("Id", "OnlyHoldout")], y = intermediate[,sum(DiscountedPriceLog), by = Id], by = "Id")
colnames(onlyHoldout) = c("Id", "OnlyHoldout", "OnlyHoldoutLog")
# Aggregate with discounted prices (Direct)
onlyHoldout = merge(x = onlyHoldout[,c("Id", "OnlyHoldout", "OnlyHoldoutLog")], y = intermediate[,sum(DiscountedPriceDirect), by = Id], by = "Id")
colnames(onlyHoldout) = c("Id", "OnlyHoldout", "OnlyHoldoutLog", "OnlyHoldoutDirect")
onlyHoldout

# Merge results
comparison = merge(x = allPeriods, y = onlyHoldout, by = "Id", all.x = TRUE)
comparison[is.na(comparison)] = 0
comparison

# # With model definition
# new3 = data.table("Id" = results$Id,
#                   "actual.x" = results$actual.x,
#                   "actual.total.spending" = results$actual.total.spending,
#                   "CET" = results$CET,
#                   "DERT_old" = results$DERT,
#                   "CLV_old" = results$predicted.CLV)
# 
# new3$DERT_new = new3$actual.x / 3.612912 # results$CET / results$DERT
# new3$mean_spending = ifelse(new3$actual.x == 0, 0, new3$actual.total.spending/new3$actual.x)
# new3$CLV = new3$DERT_new * new3$mean_spending

# # Calculate DERT with the BTYD package
#   cutoffSplitweek = apparelTrans[Date <= as.Date("2005-01-03") + splitWeek * 7,]
#   new3$recent = as.numeric(difftime(cutoffSplitweek[, max(Date), by = "Id"]$V1, as.Date("2005-01-03"), units = "weeks"))
#   rptr = cutoffSplitweek[duplicated(cutoffSplitweek$Id),]
#   rptr = rptr[,.N, by = "Id"]
#   rptr = merge(x = results, y = rptr, by = "Id", all.x = TRUE)
#   rptr[is.na(rptr$N)] = 0
#   params = c(0.7858, 5.3312, 0.3606, 11.8221)
#   pnbd.DERT(params, cutoffSplitweek[,.N, by = "Id"]$N, new3$recent, 39.85714, 0.1/52, hardie = FALSE)
#   
#   dtdert = data.table("BTYD" = pnbd.DERT(params, rptr$N, new3$recent, 40, 0.1),
#                       "CLVTools" = results$DERT,
#                       "recent" = new3$recent)
#   View(dtdert)
