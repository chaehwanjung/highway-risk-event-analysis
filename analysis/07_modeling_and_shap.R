# 저장소 루트에서 실행합니다. 입력·중간 데이터는 data/에 배치합니다.
base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))

# 모델링: Logistic / Decision Tree / RandomForest / XGBoost / LightGBM 비교

rm(list = ls())
gc()

library(readr)
library(dplyr)
library(data.table)
library(pROC)

if (!require(rpart)) install.packages("rpart")
if (!require(randomForest)) install.packages("randomForest")
if (!require(xgboost)) install.packages("xgboost")

library(rpart)
library(randomForest)
library(xgboost)

has_lightgbm <- require(lightgbm)

base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))
final_path <- file.path(base_dir, "data_mart_final_features_v2.csv")
model_dir <- file.path(base_dir, "model_outputs")
dir.create(model_dir, showWarnings = FALSE)

# 1. 모델링 변수

model_cols <- c(
  "event_binary",
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


# 2. 샘플링 로드
# 양성 전체 + 음성 3%

set.seed(123)

positive_data <- list()
negative_data <- list()
sample_rate_neg <- 0.03

callback_model <- function(x, pos) {
  
  x <- as.data.frame(x)
  
  pos_part <- x %>% filter(event_binary == 1)
  neg_part <- x %>% filter(event_binary == 0) %>% sample_frac(sample_rate_neg)
  
  positive_data[[length(positive_data) + 1]] <<- pos_part
  negative_data[[length(negative_data) + 1]] <<- neg_part
  
  cat(
    "처리 위치:", pos,
    "/ 양성:", sum(sapply(positive_data, nrow)),
    "/ 음성:", sum(sapply(negative_data, nrow)),
    "\n"
  )
}

read_csv_chunked(
  final_path,
  callback = DataFrameCallback$new(callback_model),
  chunk_size = 300000,
  locale = locale(encoding = "UTF-8"),
  col_types = cols_only(
    event_binary = col_integer(),
    flow_density = col_double(),
    hour = col_integer(),
    mean_curvature = col_double(),
    sum_bearing_change = col_double(),
    vol = col_double(),
    speed = col_double(),
    curve_vol_density = col_double(),
    SSD_15 = col_double(),
    occ_speed_ratio = col_double(),
    speed_cv = col_double(),
    SSD_5 = col_double(),
    tunnel_occ = col_double(),
    night_flag = col_integer(),
    facility_flag_500m = col_integer(),
    ic_near_500m = col_integer()
  )
)

model_data <- bind_rows(
  bind_rows(positive_data),
  bind_rows(negative_data)
)

rm(positive_data, negative_data)
gc()

cat("모델링 데이터 행수:", nrow(model_data), "\n")
print(table(model_data$event_binary))
print(prop.table(table(model_data$event_binary)))


# 3. 결측/무한값 처리


x_cols <- setdiff(names(model_data), "event_binary")

model_data[, x_cols] <- lapply(model_data[, x_cols], function(z) {
  z[is.na(z) | is.infinite(z)] <- 0
  z
})

model_data$event_binary <- as.factor(model_data$event_binary)


# 4. Train/Test 분할

set.seed(123)

idx_pos <- which(model_data$event_binary == "1")
idx_neg <- which(model_data$event_binary == "0")

train_pos <- sample(idx_pos, floor(length(idx_pos) * 0.7))
train_neg <- sample(idx_neg, floor(length(idx_neg) * 0.7))

train_idx <- c(train_pos, train_neg)
test_idx <- setdiff(seq_len(nrow(model_data)), train_idx)

train <- model_data[train_idx, ]
test <- model_data[test_idx, ]

x_train <- as.matrix(train[, x_cols])
x_test <- as.matrix(test[, x_cols])

y_train_num <- as.numeric(as.character(train$event_binary))
y_test_num <- as.numeric(as.character(test$event_binary))


# 5. 평가 함수

eval_model <- function(model_name, prob, y_true, threshold = 0.5) {
  
  pred <- ifelse(prob >= threshold, 1, 0)
  
  accuracy <- mean(pred == y_true)
  precision <- sum(pred == 1 & y_true == 1) / max(sum(pred == 1), 1)
  recall <- sum(pred == 1 & y_true == 1) / max(sum(y_true == 1), 1)
  f1 <- 2 * precision * recall / max(precision + recall, 1e-10)
  auc_value <- as.numeric(auc(y_true, prob))
  
  data.frame(
    model = model_name,
    threshold = threshold,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    AUC = auc_value
  )
}

results <- data.frame()


# 6. Logistic Regression

cat("\n[1] Logistic Regression\n")

logit_model <- glm(
  event_binary ~ .,
  family = binomial,
  data = train
)

logit_prob <- predict(
  logit_model,
  newdata = test,
  type = "response"
)

results <- bind_rows(
  results,
  eval_model("Logistic", logit_prob, y_test_num)
)


# 7. Decision Tree

cat("\n[2] Decision Tree\n")

tree_model <- rpart(
  event_binary ~ .,
  data = train,
  method = "class",
  control = rpart.control(
    cp = 0.001,
    maxdepth = 6,
    minsplit = 100
  )
)

tree_prob <- predict(
  tree_model,
  newdata = test,
  type = "prob"
)[, "1"]

results <- bind_rows(
  results,
  eval_model("Decision Tree", tree_prob, y_test_num)
)


# 8. RandomForest

cat("\n[3] RandomForest\n")

rf_model <- randomForest(
  event_binary ~ .,
  data = train,
  ntree = 100,
  mtry = floor(sqrt(length(x_cols))),
  importance = TRUE
)

rf_prob <- predict(
  rf_model,
  newdata = test,
  type = "prob"
)[, "1"]

results <- bind_rows(
  results,
  eval_model("RandomForest", rf_prob, y_test_num)
)

# 9. XGBoost

cat("\n[4] XGBoost\n")

dtrain <- xgb.DMatrix(data = x_train, label = y_train_num)
dtest <- xgb.DMatrix(data = x_test, label = y_test_num)

scale_pos_weight <- sum(y_train_num == 0) / sum(y_train_num == 1)

xgb_model <- xgb.train(
  params = list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 4,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    scale_pos_weight = scale_pos_weight
  ),
  data = dtrain,
  nrounds = 300,
  evals = list(train = dtrain, test = dtest),
  early_stopping_rounds = 30,
  print_every_n = 20
)

xgb_prob <- predict(xgb_model, dtest)

results <- bind_rows(
  results,
  eval_model("XGBoost", xgb_prob, y_test_num)
)

# 10. LightGBM

if (has_lightgbm) {
  
  cat("\n[5] LightGBM\n")
  
  lgb_train <- lgb.Dataset(
    data = x_train,
    label = y_train_num
  )
  
  lgb_model <- lgb.train(
    params = list(
      objective = "binary",
      metric = "auc",
      learning_rate = 0.05,
      num_leaves = 31,
      max_depth = 6,
      feature_fraction = 0.8,
      bagging_fraction = 0.8,
      bagging_freq = 1,
      scale_pos_weight = scale_pos_weight
    ),
    data = lgb_train,
    nrounds = 300,
    verbose = 1
  )
  
  lgb_prob <- predict(lgb_model, x_test)
  
  results <- bind_rows(
    results,
    eval_model("LightGBM", lgb_prob, y_test_num)
  )
  
} else {
  cat("\nLightGBM 패키지가 없어서 건너뜀\n")
}


# 11. 결과 저장


print(results)

write.csv(
  results,
  file.path(model_dir, "model_compare_results_top15.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)

# 변수 중요도 저장
xgb_importance <- xgb.importance(
  feature_names = x_cols,
  model = xgb_model
)

write.csv(
  xgb_importance,
  file.path(model_dir, "xgb_variable_importance.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)

print(xgb_importance)

if (exists("rf_model")) {
  rf_importance <- importance(rf_model)
  write.csv(
    rf_importance,
    file.path(model_dir, "rf_variable_importance.csv"),
    row.names = TRUE,
    fileEncoding = "CP949"
  )
}

cat("\n모델 비교 완료\n")
cat("결과 저장 위치:", model_dir, "\n")

results

#           model threshold  Accuracy Precision    Recall        F1       AUC
# 1      Logistic       0.5 0.8576410 0.6974102 0.4999807 0.5824191 0.8813666
# 2 Decision Tree       0.5 0.8804887 0.7857064 0.5474196 0.6452670 0.8937935
# 3  RandomForest       0.5 0.9445096 0.9014927 0.8089319 0.8527078 0.9731334
# 4       XGBoost       0.5 0.8931120 0.6772368 0.8820782 0.7662028 0.9559009
# 5      LightGBM       0.5 0.9212557 0.7519907 0.9003744 0.8195201 0.9702776

# 최종 모델링 단계
# RandomForest / LightGBM
# 5-Fold CV + 하이퍼파라미터 튜닝 + 최종 평가

rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(pROC)
library(randomForest)

if (!require(lightgbm)) install.packages("lightgbm")
library(lightgbm)

base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))
model_dir <- file.path(base_dir, "model_outputs")
dir.create(model_dir, showWarnings = FALSE)

sample_path <- file.path(model_dir, "model_sample_data.csv")


# 1. 모델링 샘플 로드

model_data <- fread(sample_path, encoding = "UTF-8")

cat("모델링 데이터 행수:", nrow(model_data), "\n")
print(table(model_data$event_binary))
print(prop.table(table(model_data$event_binary)))


# 2. XGBoost 중요도 기반 TOP15 변수 사용

selected_vars <- c(
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

use_cols <- c("event_binary", selected_vars)

model_data <- model_data[, ..use_cols]

# 결측/무한값 처리
x_cols <- setdiff(names(model_data), "event_binary")

model_data[, (x_cols) := lapply(.SD, function(z) {
  z[is.na(z) | is.infinite(z)] <- 0
  z
}), .SDcols = x_cols]

model_data$event_binary <- as.integer(model_data$event_binary)


# 3. 평가 함수

eval_model <- function(model_name, prob, y_true, threshold = 0.5) {
  
  pred <- ifelse(prob >= threshold, 1, 0)
  
  accuracy <- mean(pred == y_true)
  precision <- sum(pred == 1 & y_true == 1) / max(sum(pred == 1), 1)
  recall <- sum(pred == 1 & y_true == 1) / max(sum(y_true == 1), 1)
  f1 <- 2 * precision * recall / max(precision + recall, 1e-10)
  auc_value <- as.numeric(auc(y_true, prob))
  
  data.frame(
    model = model_name,
    threshold = threshold,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    AUC = auc_value
  )
}


# 4. Train/Test 분할


set.seed(123)

idx_pos <- which(model_data$event_binary == 1)
idx_neg <- which(model_data$event_binary == 0)

train_pos <- sample(idx_pos, floor(length(idx_pos) * 0.7))
train_neg <- sample(idx_neg, floor(length(idx_neg) * 0.7))

train_idx <- c(train_pos, train_neg)
test_idx <- setdiff(seq_len(nrow(model_data)), train_idx)

train <- model_data[train_idx]
test <- model_data[test_idx]

x_train <- as.matrix(train[, ..x_cols])
x_test <- as.matrix(test[, ..x_cols])

y_train <- train$event_binary
y_test <- test$event_binary

scale_pos_weight <- sum(y_train == 0) / sum(y_train == 1)


# 5. 5-Fold CV 함수

set.seed(123)

k <- 5

fold_id <- rep(NA, nrow(train))

pos_train_idx <- which(train$event_binary == 1)
neg_train_idx <- which(train$event_binary == 0)

fold_id[pos_train_idx] <- sample(rep(1:k, length.out = length(pos_train_idx)))
fold_id[neg_train_idx] <- sample(rep(1:k, length.out = length(neg_train_idx)))


# 6. RandomForest 5-Fold CV

rf_cv_results <- data.frame()

rf_grid <- expand.grid(
  ntree = c(100),
  mtry = c(3, 4, 5),
  maxnodes = c(20, 40)
)

for (g in 1:nrow(rf_grid)) {
  
  cat("\nRF Grid:", g, "/", nrow(rf_grid), "\n")
  
  fold_results <- data.frame()
  
  for (i in 1:k) {
    
    cat("  Fold:", i, "\n")
    
    cv_train <- train[fold_id != i]
    cv_valid <- train[fold_id == i]
    
    cv_train$event_binary <- as.factor(cv_train$event_binary)
    cv_valid_y <- cv_valid$event_binary
    
    rf_model <- randomForest(
      event_binary ~ .,
      data = cv_train,
      ntree = rf_grid$ntree[g],
      mtry = rf_grid$mtry[g],
      maxnodes = rf_grid$maxnodes[g],
      importance = TRUE
    )
    
    rf_prob <- predict(
      rf_model,
      newdata = cv_valid,
      type = "prob"
    )[, "1"]
    
    fold_results <- bind_rows(
      fold_results,
      eval_model(
        paste0("RF_mtry", rf_grid$mtry[g], "_maxnodes", rf_grid$maxnodes[g]),
        rf_prob,
        cv_valid_y
      )
    )
    
    rm(rf_model)
    gc()
  }
  
  rf_cv_results <- bind_rows(
    rf_cv_results,
    fold_results %>%
      summarise(
        model = unique(model),
        ntree = rf_grid$ntree[g],
        mtry = rf_grid$mtry[g],
        maxnodes = rf_grid$maxnodes[g],
        Accuracy = mean(Accuracy),
        Precision = mean(Precision),
        Recall = mean(Recall),
        F1 = mean(F1),
        AUC = mean(AUC)
      )
  )
}

print(rf_cv_results)

write.csv(
  rf_cv_results,
  file.path(model_dir, "rf_5fold_cv_results.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)

# 7. LightGBM 5-Fold CV

lgb_cv_results <- data.frame()

lgb_grid <- expand.grid(
  num_leaves = c(31, 63),
  max_depth = c(5, 7),
  learning_rate = c(0.05, 0.03),
  feature_fraction = c(0.8),
  bagging_fraction = c(0.8)
)

for (g in 1:nrow(lgb_grid)) {
  
  cat("\nLightGBM Grid:", g, "/", nrow(lgb_grid), "\n")
  
  fold_results <- data.frame()
  
  for (i in 1:k) {
    
    cat("  Fold:", i, "\n")
    
    cv_train <- train[fold_id != i]
    cv_valid <- train[fold_id == i]
    
    x_cv_train <- as.matrix(cv_train[, ..x_cols])
    y_cv_train <- cv_train$event_binary
    
    x_cv_valid <- as.matrix(cv_valid[, ..x_cols])
    y_cv_valid <- cv_valid$event_binary
    
    lgb_train <- lgb.Dataset(
      data = x_cv_train,
      label = y_cv_train
    )
    
    lgb_model <- lgb.train(
      params = list(
        objective = "binary",
        metric = "auc",
        num_leaves = lgb_grid$num_leaves[g],
        max_depth = lgb_grid$max_depth[g],
        learning_rate = lgb_grid$learning_rate[g],
        feature_fraction = lgb_grid$feature_fraction[g],
        bagging_fraction = lgb_grid$bagging_fraction[g],
        bagging_freq = 1,
        scale_pos_weight = scale_pos_weight,
        verbose = -1
      ),
      data = lgb_train,
      nrounds = 300
    )
    
    lgb_prob <- predict(lgb_model, x_cv_valid)
    
    fold_results <- bind_rows(
      fold_results,
      eval_model(
        paste0(
          "LGBM_leaves", lgb_grid$num_leaves[g],
          "_depth", lgb_grid$max_depth[g],
          "_lr", lgb_grid$learning_rate[g]
        ),
        lgb_prob,
        y_cv_valid
      )
    )
    
    rm(lgb_model, lgb_train)
    gc()
  }
  
  lgb_cv_results <- bind_rows(
    lgb_cv_results,
    fold_results %>%
      summarise(
        model = unique(model),
        num_leaves = lgb_grid$num_leaves[g],
        max_depth = lgb_grid$max_depth[g],
        learning_rate = lgb_grid$learning_rate[g],
        feature_fraction = lgb_grid$feature_fraction[g],
        bagging_fraction = lgb_grid$bagging_fraction[g],
        Accuracy = mean(Accuracy),
        Precision = mean(Precision),
        Recall = mean(Recall),
        F1 = mean(F1),
        AUC = mean(AUC)
      )
  )
}

print(lgb_cv_results)

write.csv(
  lgb_cv_results,
  file.path(model_dir, "lgbm_5fold_cv_results.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)


# 8. 최적 모델 선정
# 기준: F1 우선, Recall/AUC 참고

best_rf <- rf_cv_results %>%
  arrange(desc(F1), desc(AUC), desc(Recall)) %>%
  slice(1)

best_lgb <- lgb_cv_results %>%
  arrange(desc(F1), desc(AUC), desc(Recall)) %>%
  slice(1)

print(best_rf)
print(best_lgb)

write.csv(best_rf, file.path(model_dir, "best_rf_params.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(best_lgb, file.path(model_dir, "best_lgbm_params.csv"), row.names = FALSE, fileEncoding = "CP949")


# 9. 최종 RandomForest 모델 학습 및 Test 평가

cat("\n최종 RandomForest 학습\n")

train_rf <- copy(train)
train_rf$event_binary <- as.factor(train_rf$event_binary)

rf_final <- randomForest(
  event_binary ~ .,
  data = train_rf,
  ntree = best_rf$ntree,
  mtry = best_rf$mtry,
  maxnodes = best_rf$maxnodes,
  importance = TRUE
)

rf_test_prob <- predict(
  rf_final,
  newdata = test,
  type = "prob"
)[, "1"]

rf_final_result <- eval_model(
  "RandomForest_Final",
  rf_test_prob,
  y_test
)

print(rf_final_result)

rf_conf <- table(
  Actual = y_test,
  Predicted = ifelse(rf_test_prob >= 0.5, 1, 0)
)

print(rf_conf)


# 10. 최종 LightGBM 모델 학습 및 Test 평가


cat("\n최종 LightGBM 학습\n")

lgb_train_final <- lgb.Dataset(
  data = x_train,
  label = y_train
)

lgb_final <- lgb.train(
  params = list(
    objective = "binary",
    metric = "auc",
    num_leaves = best_lgb$num_leaves,
    max_depth = best_lgb$max_depth,
    learning_rate = best_lgb$learning_rate,
    feature_fraction = best_lgb$feature_fraction,
    bagging_fraction = best_lgb$bagging_fraction,
    bagging_freq = 1,
    scale_pos_weight = scale_pos_weight,
    verbose = -1
  ),
  data = lgb_train_final,
  nrounds = 300
)

lgb_test_prob <- predict(lgb_final, x_test)

lgb_final_result <- eval_model(
  "LightGBM_Final",
  lgb_test_prob,
  y_test
)

print(lgb_final_result)

lgb_conf <- table(
  Actual = y_test,
  Predicted = ifelse(lgb_test_prob >= 0.5, 1, 0)
)

print(lgb_conf)


# 11. 최종 결과 저장


final_results <- bind_rows(
  rf_final_result,
  lgb_final_result
)

write.csv(
  final_results,
  file.path(model_dir, "final_model_results.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)

write.csv(
  as.data.frame.matrix(rf_conf),
  file.path(model_dir, "rf_confusion_matrix.csv"),
  row.names = TRUE,
  fileEncoding = "CP949"
)

write.csv(
  as.data.frame.matrix(lgb_conf),
  file.path(model_dir, "lgbm_confusion_matrix.csv"),
  row.names = TRUE,
  fileEncoding = "CP949"
)


# 12. 변수 중요도 저장

rf_importance <- importance(rf_final)

write.csv(
  rf_importance,
  file.path(model_dir, "rf_final_variable_importance.csv"),
  row.names = TRUE,
  fileEncoding = "CP949"
)

lgb_importance <- lgb.importance(lgb_final)

write.csv(
  lgb_importance,
  file.path(model_dir, "lgbm_final_variable_importance.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)

print(lgb_importance)


# 13. 최종 모델 선택 문장용 출력

cat("\n최종 모델 비교 결과\n")
print(final_results)

cat("\nRandomForest 혼동행렬\n")
print(rf_conf)

cat("\nLightGBM 혼동행렬\n")
print(lgb_conf)

cat("\n완료. 저장 위치:\n")
cat(model_dir, "\n")


# LightGBM SHAP 해석 코드
# 최종 모델 해석용

rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(lightgbm)

base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))
model_dir <- file.path(base_dir, "model_outputs")

sample_path <- file.path(model_dir, "model_sample_data.csv")


# 1. 데이터 로드

model_data <- fread(sample_path, encoding = "UTF-8")

selected_vars <- c(
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

use_cols <- c("event_binary", selected_vars)

model_data <- model_data[, ..use_cols]

x_cols <- selected_vars

model_data[, (x_cols) := lapply(.SD, function(z) {
  z[is.na(z) | is.infinite(z)] <- 0
  z
}), .SDcols = x_cols]

model_data$event_binary <- as.integer(model_data$event_binary)


# 2. Train/Test 분할


set.seed(123)

idx_pos <- which(model_data$event_binary == 1)
idx_neg <- which(model_data$event_binary == 0)

train_pos <- sample(idx_pos, floor(length(idx_pos) * 0.7))
train_neg <- sample(idx_neg, floor(length(idx_neg) * 0.7))

train_idx <- c(train_pos, train_neg)
test_idx <- setdiff(seq_len(nrow(model_data)), train_idx)

train <- model_data[train_idx]
test <- model_data[test_idx]

x_train <- as.matrix(train[, ..x_cols])
y_train <- train$event_binary

x_test <- as.matrix(test[, ..x_cols])
y_test <- test$event_binary

scale_pos_weight <- sum(y_train == 0) / sum(y_train == 1)


# 3. 최종 LightGBM 재학습
# best_lgbm: num_leaves=63, max_depth=7, learning_rate=0.05


lgb_train <- lgb.Dataset(
  data = x_train,
  label = y_train
)

lgb_final <- lgb.train(
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
  data = lgb_train,
  nrounds = 300
)


# 4. SHAP 계산
# 전체 test 다 하면 무거우니 5000개만 샘플링


set.seed(123)

shap_n <- min(5000, nrow(test))
shap_idx <- sample(seq_len(nrow(test)), shap_n)

x_shap <- x_test[shap_idx, ]
y_shap <- y_test[shap_idx]

shap_values <- predict(
  lgb_final,
  x_shap,
  type = "contrib"
)

# 마지막 열은 BIAS라 제거
shap_dt <- as.data.table(shap_values)
setnames(shap_dt, c(x_cols, "BIAS"))

shap_only <- shap_dt[, ..x_cols]


# 5. SHAP 중요도: 평균 절댓값 기준

shap_importance <- data.frame(
  variable = x_cols,
  mean_abs_shap = sapply(shap_only, function(z) mean(abs(z), na.rm = TRUE))
) %>%
  arrange(desc(mean_abs_shap))

print(shap_importance)

write.csv(
  shap_importance,
  file.path(model_dir, "shap_importance.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)


# 6. SHAP 방향성 확인용 데이터 생성
# 변수값 + SHAP값 결합

x_shap_dt <- as.data.table(x_shap)
setnames(x_shap_dt, x_cols)

shap_long <- data.frame()

for (v in x_cols) {
  
  temp <- data.frame(
    variable = v,
    value = x_shap_dt[[v]],
    shap_value = shap_only[[v]],
    event_binary = y_shap
  )
  
  shap_long <- bind_rows(shap_long, temp)
}

write.csv(
  shap_long,
  file.path(model_dir, "shap_long_data.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)


# 7. 변수별 SHAP 방향성 요약
# 값이 큰 그룹과 작은 그룹의 평균 SHAP 비교


shap_direction <- data.frame()

for (v in x_cols) {
  
  value_v <- x_shap_dt[[v]]
  shap_v <- shap_only[[v]]
  
  q25 <- quantile(value_v, 0.25, na.rm = TRUE)
  q75 <- quantile(value_v, 0.75, na.rm = TRUE)
  
  low_mean_shap <- mean(shap_v[value_v <= q25], na.rm = TRUE)
  high_mean_shap <- mean(shap_v[value_v >= q75], na.rm = TRUE)
  
  direction <- ifelse(
    high_mean_shap > low_mean_shap,
    "값이 클수록 위험예측 증가",
    "값이 클수록 위험예측 감소"
  )
  
  shap_direction <- bind_rows(
    shap_direction,
    data.frame(
      variable = v,
      q25 = q25,
      q75 = q75,
      low_mean_shap = low_mean_shap,
      high_mean_shap = high_mean_shap,
      direction = direction
    )
  )
}

shap_direction <- shap_direction %>%
  left_join(shap_importance, by = "variable") %>%
  arrange(desc(mean_abs_shap))

print(shap_direction)

write.csv(
  shap_direction,
  file.path(model_dir, "shap_direction_summary.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)


# 8. 간단 시각화 저장


png(
  filename = file.path(model_dir, "shap_importance_plot.png"),
  width = 1000,
  height = 700
)

barplot(
  shap_importance$mean_abs_shap,
  names.arg = shap_importance$variable,
  las = 2,
  main = "LightGBM SHAP Variable Importance",
  ylab = "Mean Absolute SHAP Value"
)

library(ggplot2)

ggplot(
  shap_importance,
  aes(
    x = reorder(variable, mean_abs_shap),
    y = mean_abs_shap
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "SHAP Variable Importance",
    x = "Variable",
    y = "Mean |SHAP|"
  )

dev.off()

cat("\nSHAP 분석 완료\n")
cat("저장 파일:\n")
cat("1. shap_importance.csv\n")
cat("2. shap_direction_summary.csv\n")
cat("3. shap_long_data.csv\n")
cat("4. shap_importance_plot.png\n")
cat("저장 위치:", model_dir, "\n")

shap_importance
shap_direction

library(data.table)
library(lightgbm)
library(pROC)

base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))
model_dir <- file.path(base_dir, "model_outputs")

model_data <- fread(
  file.path(model_dir, "model_sample_data.csv"),
  encoding = "UTF-8"
)

selected_vars <- c(
  "flow_density", "hour", "mean_curvature", "sum_bearing_change",
  "vol", "speed", "curve_vol_density", "SSD_15",
  "occ_speed_ratio", "speed_cv", "SSD_5", "tunnel_occ",
  "night_flag", "facility_flag_500m", "ic_near_500m"
)

model_data <- model_data[, c("event_binary", selected_vars), with = FALSE]

x_cols <- selected_vars

model_data[, (x_cols) := lapply(.SD, function(z) {
  z[is.na(z) | is.infinite(z)] <- 0
  z
}), .SDcols = x_cols]

model_data$event_binary <- as.integer(model_data$event_binary)

set.seed(123)

idx_pos <- which(model_data$event_binary == 1)
idx_neg <- which(model_data$event_binary == 0)

train_pos <- sample(idx_pos, floor(length(idx_pos) * 0.7))
train_neg <- sample(idx_neg, floor(length(idx_neg) * 0.7))

train_idx <- c(train_pos, train_neg)
test_idx <- setdiff(seq_len(nrow(model_data)), train_idx)

train <- model_data[train_idx]
test <- model_data[test_idx]

x_train <- as.matrix(train[, ..x_cols])
x_test <- as.matrix(test[, ..x_cols])

y_train <- train$event_binary
y_test <- test$event_binary

scale_pos_weight <- sum(y_train == 0) / sum(y_train == 1)

lgb_train <- lgb.Dataset(
  data = x_train,
  label = y_train
)

lgb_final <- lgb.train(
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
  data = lgb_train,
  nrounds = 300
)

lgb_test_prob <- predict(lgb_final, x_test)

lgb_conf <- table(
  Actual = y_test,
  Predicted = ifelse(lgb_test_prob >= 0.5, 1, 0)
)

lgb_conf

# Confusion Matrix 그림
png(
  file.path(model_dir, "lgbm_confusion_matrix_plot.png"),
  width = 800,
  height = 600
)

fourfoldplot(
  lgb_conf,
  color = c("tomato", "skyblue"),
  conf.level = 0,
  main = "LightGBM Confusion Matrix"
)

dev.off()


# ROC Curve 그림
roc_obj <- roc(y_test, lgb_test_prob)

png(
  file.path(model_dir, "lgbm_roc_curve.png"),
  width = 800,
  height = 600
)

plot(
  roc_obj,
  print.auc = TRUE,
  main = "LightGBM ROC Curve"
)

dev.off()

# 1. 혼동행렬 화면 출력
fourfoldplot(
  lgb_conf,
  color = c("tomato", "skyblue"),
  conf.level = 0,
  main = "LightGBM Confusion Matrix"
)

# 2. ROC Curve 화면 출력
library(pROC)

roc_obj <- roc(y_test, lgb_test_prob)

plot(
  roc_obj,
  print.auc = TRUE,
  main = "LightGBM ROC Curve"
)
