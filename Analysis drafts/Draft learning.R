learn_application_data = application_data
learn_data = merge(x = all_res$apparel_results$intervals_BS[, c("Id", "CET_true", "PTS_true")],
      y = learn_application_data[, c(1,14:30)],
      by = "Id")
train_customers = sample(learn_data$Id, 1200)
train = learn_data[Id %in% train_customers,]
test = learn_data[!(Id %in% train_customers),]

# Linear model
fit = lm(CET_true ~ ., data = train[,-c("Id", "PTS_true", "max_s")])
summary(fit)

eval_data = data.table("Id" = test$Id,
                       "true" = test$CET_true,
                       "pred" = predict(fit, test)
                       )

n = 193 #139
eval_data = eval_data[order(-eval_data$true)]
eval_data$true_s = eval_data$true >= min(eval_data$true[1:n])
eval_data = eval_data[order(-eval_data$pred)]
eval_data$pred_s = eval_data$pred >= min(eval_data$pred[1:n])
sum(eval_data$true_s * eval_data$pred_s)/193 #139

# Logistic regression
fit = glm(max_s ~ .,
          family = binomial,
          data = train[,-c("Id", "CET_true", "PTS_true")])
summary(fit)

eval_data = data.table("Id" = test$Id,
                       "true" = test$CET_true,
                       "pred" = predict(fit, test)
)

n = 193 #139
eval_data = eval_data[order(-eval_data$true)]
eval_data$true_s = eval_data$true >= min(eval_data$true[1:n])
eval_data = eval_data[order(-eval_data$pred)]
eval_data$pred_s = eval_data$pred >= min(eval_data$pred[1:n])
sum(eval_data$true_s * eval_data$pred_s)/193 #139

# Random forest
train$max_s = as.factor(train$max_s)
fit = rpart(max_s ~ ., data = train[,-c("Id", "CET_true", "PTS_true")])
results = data.table(predict(fit, test))
colnames(results) = c("False","True")
summary(fit)

eval_data = data.table("Id" = test$Id,
                       "true" = test$CET_true,
                       "pred" = results$True
)

n = 193
eval_data = eval_data[order(-eval_data$true)]
eval_data$true_s = eval_data$true >= min(eval_data$true[1:n])
eval_data = eval_data[order(-eval_data$pred)]
eval_data$pred_s = eval_data$pred >= min(eval_data$pred[1:n])
sum(eval_data$true_s * eval_data$pred_s)/193 #139
