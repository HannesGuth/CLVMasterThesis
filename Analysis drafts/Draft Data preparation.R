# Electronics

electronics = fread("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Datasets/Electronics retailer/durdata1_final.csv")
electronics = electronics[, c("HOUSEHOLD_ID", "TRANSACTION_NBR", "TRANSACTION_TOTAL", "TRANSACTION_DATE", "QUANTITY", "UNIT_PRICE", "EXTENDED_PRICE")]
electronics$Date = as.Date(electronics$TRANSACTION_DATE, format = "%d%b%Y")
electronics$Price = electronics$QUANTITY * electronics$UNIT_PRICE
elclv = electronics[, c("HOUSEHOLD_ID", "Date", "Price")]
colnames(elclv) = c("Id", "Date", "Price")
x = elclv[, min(Date), by = "Id"]
mindate_cust = x[V1 %in% "2001-01-06"]
x = x[, .N, by = "V1"]
x[order(N, decreasing = TRUE)]
#elclv = elclv[Date %in% "2001-01-06"]
eldata = merge(x = mindate_cust, y = elclv, by = "Id", all.x = TRUE)
eldata = eldata[,-"V1"]
  
clv.electronics <- clvdata(eldata,  
                           date.format="ymd", 
                           time.unit = "week",
                           estimation.split = 20,
                           name.id = "Id",
                           name.date = "Date",
                           name.price = "Price")
elmod = pnbd(clv.data = clv.electronics, verbose = TRUE)
el.results = predict(elmod, predict.spending = TRUE)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Gifts

gift = fread("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Datasets/Gift shop/gift.csv")
gift = gift[, c("ID", "date", "amount")]
colnames(gift) = c("Id", "Date", "Price")
x = gift[, min(Date), by = "Id"]
x = x[, .N, by = "V1"]
x[order(-x$N),]
x = gift[, min(Date), by = "Id"]
giftsmall = x[V1 %in% "2001-12-17"]
giftsmall = merge(x = gift, y = giftsmall, by = "Id")
giftsmall = giftsmall[, -4]
giftsmall$Date = as.Date(giftsmall$Date, format = "%d%b%Y")
