# 저장소 루트에서 실행합니다. 입력·중간 데이터는 data/에 배치합니다.
base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))

library(data.table)

base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))

final_path <- file.path(
  base_dir,
  "data_mart_final_features_v2.csv"
)

file.exists(final_path)

# 가설 2
# 속도 변동성 가설

dt_speed <- fread(
  final_path,
  select = c(
    "speed_cv",
    "event_binary"
  )
)

aggregate(
  speed_cv ~ event_binary,
  dt_speed,
  mean
)

model_speed <- glm(
  event_binary ~ speed_cv,
  family = binomial,
  data = dt_speed
)

summary(model_speed)

library(data.table)

# 가설 3
# 정체·고밀도 교통류 가설

dt_cong <- fread(
  final_path,
  select = c(
    "occ_speed_ratio",
    "event_binary"
  )
)

aggregate(
  occ_speed_ratio ~ event_binary,
  dt_cong,
  mean
)

model_cong <- glm(
  event_binary ~ occ_speed_ratio,
  family = binomial,
  data = dt_cong
)

summary(model_cong)

library(data.table)


# 가설 5
# 시설물 인접 가설

dt_fac <- fread(
  final_path,
  select = c(
    "facility_flag_500m",
    "event_binary"
  )
)

tbl <- table(
  dt_fac$facility_flag_500m,
  dt_fac$event_binary
)

tbl

chisq.test(tbl)

summary(model_speed)

summary(model_cong)

chisq.test(tbl)



base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))
final_path <- file.path(base_dir, "data_mart_final_features_v2.csv")
model_dir <- file.path(base_dir, "model_outputs")
dir.create(model_dir, showWarnings = FALSE)


# 모델링용 샘플 생성 + XGBoost 변수 중요도

rm(list = ls())
gc()

library(readr)
library(dplyr)
library(data.table)

# 없으면 설치
if (!require(xgboost)) install.packages("xgboost")
if (!require(pROC)) install.packages("pROC")

library(xgboost)
library(pROC)

base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))
final_path <- file.path(base_dir, "data_mart_final_features_v2.csv")
model_dir <- file.path(base_dir, "model_outputs")
dir.create(model_dir, showWarnings = FALSE)

file.exists(final_path)


# 1. 모델 후보 변수
# 누수 변수 제외:
# crash_count, hardbraking_count, event_count, risk_score,
# crash_binary, rolling_event_30m

model_cols <- c(
  "event_binary",
  
  "speed", "vol", "occ",
  "flow_density", "occ_speed_ratio",
  
  "speed_cv", "speed_drop_ratio",
  "TSD_5", "TSD_10", "TSD_15",
  "SSD_5", "SSD_10", "SSD_15",
  "TSSD_5", "TSSD_10", "TSSD_15",
  
  "wet_road",
  "weather_risk", "rain_curve_risk",
  
  "mean_curvature", "sum_bearing_change",
  "curve_TSD", "curve_speed_risk", "curve_vol_density",
  
  "facility_flag_500m",
  "ic_near_500m", "jc_near_500m", "tg_near_500m",
  
  "has_tunnel", "has_bridge",
  "tunnel_occ", "bridge_occ",
  
  "hour", "peak_flag", "night_flag",
  "is_weekend", "is_peak_weekday"
)

# 2. 전체 로드 X
# 양성 전체 + 음성 일부 샘플링

set.seed(123)

positive_data <- list()
negative_data <- list()

sample_rate_neg <- 0.03

callback_model <- function(x, pos) {
  
  x <- as.data.frame(x)
  
  pos_part <- x %>%
    filter(event_binary == 1)
  
  neg_part <- x %>%
    filter(event_binary == 0) %>%
    sample_frac(sample_rate_neg)
  
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
    
    speed = col_double(),
    vol = col_double(),
    occ = col_double(),
    flow_density = col_double(),
    occ_speed_ratio = col_double(),
    
    speed_cv = col_double(),
    speed_drop_ratio = col_double(),
    TSD_5 = col_double(),
    TSD_10 = col_double(),
    TSD_15 = col_double(),
    SSD_5 = col_double(),
    SSD_10 = col_double(),
    SSD_15 = col_double(),
    TSSD_5 = col_double(),
    TSSD_10 = col_double(),
    TSSD_15 = col_double(),
    
    wet_road = col_integer(),
    weather_risk = col_double(),
    rain_curve_risk = col_double(),
    
    mean_curvature = col_double(),
    sum_bearing_change = col_double(),
    curve_TSD = col_double(),
    curve_speed_risk = col_double(),
    curve_vol_density = col_double(),
    
    facility_flag_500m = col_integer(),
    ic_near_500m = col_integer(),
    jc_near_500m = col_integer(),
    tg_near_500m = col_integer(),
    
    has_tunnel = col_integer(),
    has_bridge = col_integer(),
    tunnel_occ = col_double(),
    bridge_occ = col_double(),
    
    hour = col_integer(),
    peak_flag = col_integer(),
    night_flag = col_integer(),
    is_weekend = col_integer(),
    is_peak_weekday = col_integer()
  )
)

model_data <- bind_rows(
  bind_rows(positive_data),
  bind_rows(negative_data)
)

rm(positive_data, negative_data)
gc()

cat("모델링 샘플 행수:", nrow(model_data), "\n")
print(table(model_data$event_binary))
print(prop.table(table(model_data$event_binary)))

fwrite(
  model_data,
  file.path(model_dir, "model_sample_data.csv"),
  bom = TRUE,
  na = ""
)


# 3. 학습/검증 분할

set.seed(123)

pos_idx <- which(model_data$event_binary == 1)
neg_idx <- which(model_data$event_binary == 0)

train_pos <- sample(pos_idx, floor(length(pos_idx) * 0.7))
train_neg <- sample(neg_idx, floor(length(neg_idx) * 0.7))

train_idx <- c(train_pos, train_neg)
test_idx <- setdiff(seq_len(nrow(model_data)), train_idx)

train <- model_data[train_idx, ]
test <- model_data[test_idx, ]

x_cols <- setdiff(names(model_data), "event_binary")

train[, x_cols] <- lapply(train[, x_cols], function(z) {
  z[is.na(z) | is.infinite(z)] <- 0
  z
})

test[, x_cols] <- lapply(test[, x_cols], function(z) {
  z[is.na(z) | is.infinite(z)] <- 0
  z
})

x_train <- as.matrix(train[, x_cols])
y_train <- train$event_binary

x_test <- as.matrix(test[, x_cols])
y_test <- test$event_binary


# 4. XGBoost 모델


dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

scale_pos_weight <- sum(y_train == 0) / sum(y_train == 1)

params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = scale_pos_weight
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 300,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 30,
  print_every_n = 20
)

# 모델링 샘플
# 434,904행

# 정상
# 348,550

# 위험이벤트
# 86,354

# Test AUC
# 0.956684 -> 0.90 이상 = 매우 우수, 0.95 이상 = 거의 최상위권

# 5. 성능 평가

pred_prob <- predict(xgb_model, dtest)
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)

conf_mat <- table(
  Actual = y_test,
  Predicted = pred_class
)

print(conf_mat)

accuracy <- mean(pred_class == y_test)
precision <- sum(pred_class == 1 & y_test == 1) / sum(pred_class == 1)
recall <- sum(pred_class == 1 & y_test == 1) / sum(y_test == 1)
f1 <- 2 * precision * recall / (precision + recall)
auc_value <- as.numeric(auc(y_test, pred_prob))

metrics <- data.frame(
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1 = f1,
  AUC = auc_value
)

print(metrics)

write.csv(
  metrics,
  file.path(model_dir, "xgb_first_metrics.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)


# 6. 변수 중요도

importance <- xgb.importance(
  feature_names = x_cols,
  model = xgb_model
)

print(importance)

write.csv(
  importance,
  file.path(model_dir, "xgb_variable_importance.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)

top_vars <- importance$Feature[1:min(15, nrow(importance))]

selected_vars <- data.frame(
  selected_variable = top_vars
)

print(selected_vars)

write.csv(
  selected_vars,
  file.path(model_dir, "selected_variables_top15.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)