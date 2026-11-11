# 최종 비교에 필요한 train/test를 원본 샘플에서 다시 구성합니다.
library(data.table)
library(lightgbm)
library(pROC)
base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))
model_dir <- file.path(base_dir, "model_outputs")
dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
model_data <- fread(file.path(model_dir, "model_sample_data.csv"), encoding = "UTF-8")
required_vars <- c("event_binary", "speed", "vol", "occ", "flow_density", "occ_speed_ratio",
                   "speed_cv", "SSD_5", "SSD_15", "hour", "mean_curvature", "sum_bearing_change",
                   "curve_vol_density", "tunnel_occ", "night_flag", "facility_flag_500m", "ic_near_500m")
stopifnot(all(required_vars %in% names(model_data)))
model_data <- model_data[, ..required_vars]
x_cols <- setdiff(required_vars, "event_binary")
model_data[, (x_cols) := lapply(.SD, function(z) {z[is.na(z) | is.infinite(z)] <- 0; z}), .SDcols = x_cols]
model_data$event_binary <- as.integer(model_data$event_binary)
set.seed(123)
idx_pos <- which(model_data$event_binary == 1)
idx_neg <- which(model_data$event_binary == 0)
train_idx <- c(sample(idx_pos, floor(length(idx_pos) * 0.7)), sample(idx_neg, floor(length(idx_neg) * 0.7)))
test_idx <- setdiff(seq_len(nrow(model_data)), train_idx)
train <- model_data[train_idx]
test <- model_data[test_idx]
y_train <- train$event_binary
y_test <- test$event_binary
scale_pos_weight <- sum(y_train == 0) / sum(y_train == 1)

traffic_vars <- c(
  "speed",
  "vol",
  "flow_density",
  "occ_speed_ratio",
  "speed_cv",
  "SSD_5",
  "SSD_15"
)

library(lightgbm)
library(pROC)
library(data.table)

# 평가 함수


eval_model <- function(prob, y_true){
  
  pred <- ifelse(prob >= 0.5, 1, 0)
  
  accuracy <- mean(pred == y_true)
  
  precision <- sum(pred == 1 & y_true == 1) /
    max(sum(pred == 1),1)
  
  recall <- sum(pred == 1 & y_true == 1) /
    max(sum(y_true == 1),1)
  
  f1 <- 2 * precision * recall /
    (precision + recall)
  
  auc_value <- as.numeric(
    auc(y_true, prob)
  )
  
  data.frame(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    AUC = auc_value
  )
}


# 1. 교통류 변수만

traffic_vars <- c(
  "speed",
  "vol",
  "occ",
  "flow_density",
  "occ_speed_ratio",
  "speed_cv",
  "SSD_5",
  "SSD_15"
)


# 2. 최종모델 변수

full_vars <- c(
  "flow_density",
  "hour",
  "mean_curvature",
  "sum_bearing_change",
  "vol",
  "speed",
  "curve_vol_density",
  "SSD_15",
  "occ_speed_ratio",
  "speed_cv",
  "SSD_5",
  "tunnel_occ",
  "night_flag",
  "facility_flag_500m",
  "ic_near_500m"
)

# 교통류 모델

x_train_traffic <- as.matrix(
  train[, ..traffic_vars]
)

x_test_traffic <- as.matrix(
  test[, ..traffic_vars]
)

dtrain_traffic <- lgb.Dataset(
  data = x_train_traffic,
  label = y_train
)

traffic_model <- lgb.train(
  params = list(
    objective = "binary",
    metric = "auc",
    num_leaves = 63,
    max_depth = 7,
    learning_rate = 0.05,
    feature_fraction = 0.8,
    bagging_fraction = 0.8,
    bagging_freq = 1,
    scale_pos_weight = scale_pos_weight,
    verbose = -1
  ),
  data = dtrain_traffic,
  nrounds = 300
)

traffic_prob <- predict(
  traffic_model,
  x_test_traffic
)

traffic_result <- eval_model(
  traffic_prob,
  y_test
)

traffic_result$model <- "Traffic Only"


# 최종모델

x_train_full <- as.matrix(
  train[, ..full_vars]
)

x_test_full <- as.matrix(
  test[, ..full_vars]
)

dtrain_full <- lgb.Dataset(
  data = x_train_full,
  label = y_train
)

full_model <- lgb.train(
  params = list(
    objective = "binary",
    metric = "auc",
    num_leaves = 63,
    max_depth = 7,
    learning_rate = 0.05,
    feature_fraction = 0.8,
    bagging_fraction = 0.8,
    bagging_freq = 1,
    scale_pos_weight = scale_pos_weight,
    verbose = -1
  ),
  data = dtrain_full,
  nrounds = 300
)

full_prob <- predict(
  full_model,
  x_test_full
)

full_result <- eval_model(
  full_prob,
  y_test
)

full_result$model <- "Traffic + Structure"


# 비교 결과


compare_result <- rbind(
  traffic_result,
  full_result
)

compare_result <- compare_result[
  ,
  c(
    "model",
    "Accuracy",
    "Precision",
    "Recall",
    "F1",
    "AUC"
  )
]

print(compare_result)

write.csv(
  compare_result,
  file.path(
    model_dir,
    "additional_variable_effect.csv"
  ),
  row.names = FALSE,
  fileEncoding = "CP949"
)
