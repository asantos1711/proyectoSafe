library(ranger)
library(DALEX)
library(rSAFE)
library(ggplot2)

hotel_bookings <- read.csv("./data/hotel_bookings.csv", stringsAsFactors = TRUE)
# remove columns with na values
hotel_bookings <- hotel_bookings[, sapply(hotel_bookings, function(x) !any(is.na(x)))]
hotel_bookings <- hotel_bookings[, -which(colnames(hotel_bookings) %in% c("reservation_status", "reservation_status_date", "country", "company", "agent"))]
set.seed(123)
hotel_bookings <- hotel_bookings[sample(1:nrow(hotel_bookings), 20000),]


n <- nrow(hotel_bookings)
set.seed(123)
train_idx <- sample(1:n, 0.8 * n)
train <- hotel_bookings[train_idx, ] 
test <- hotel_bookings[-train_idx, ] 

x_train <- train[,-which(colnames(train) == "is_canceled")]
y_train <- train[,"is_canceled"]

x_test <- test[,-which(colnames(test) == "is_canceled")]
y_test <- test[,"is_canceled"]


### Random Forest ###
set.seed(123)
model <- ranger(is_canceled~., data = cbind(x_train, is_canceled = y_train))

pred_train <- predict(model, x_train)
mltools::auc_roc(y_train, pred_train$predictions)
# 0.853594
pred_test <- predict(model, x_test)
mltools::auc_roc(y_test, pred_test$predictions)
# 0.7471582


### Linear model ###
model_lm1 <- glm(is_canceled~., data = cbind(x_train, is_canceled = y_train), family = "binomial")

pred_train <- predict(model_lm1, x_train, type = "response")
1 - mltools::auc_roc(y_train, exp(pred_train))
# 0.60219
pred_test <- predict(model_lm1, x_test)
1 - mltools::auc_roc(y_train, exp(pred_test))
# 0.5



### SAFE ###

explain_model <- explain(model,
                         data = x_train,
                         y = y_train)
set.seed(123)
safe <- safe_extraction(explain_model, response_type = "pdp")

train_trans <- safely_transform_data(safe, x_train)
train_trans_new <- train_trans[,grepl(".*new",colnames(train_trans))]

test_trans <- safely_transform_data(safe, x_test)
test_trans_new <- test_trans[,grepl(".*new",colnames(test_trans))]


model_glm <- glm(is_canceled~., data = cbind(train_trans_new, is_canceled=y_train), family = "binomial")
pred_train <- predict(model_glm, train_trans_new)
1 - mltools::auc_roc(y_train, pred_train)
# 0.6710183
pred_test <- predict(model_glm, test_trans_new)
1 - mltools::auc_roc(y_test, pred_test)
# 0.6827459




variables = colnames(train_trans_new)
plot(safe, variable = gsub("_new", "", variables[17]))
ggsave("./figs/deposit_type.png", width = 8, height = 3)

plot(safe, variable = gsub("_new", "", variables[4]))
ggsave("./figs/arrival_date_week_number.png", width = 8, height = 3)

