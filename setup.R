# 최초 1회 실행. 사용 환경에 맞는 버전 설치가 필요합니다.
packages <- c("readr", "readxl", "dplyr", "stringr", "lubridate", "ggplot2", "zoo",
              "sf", "tidyr", "purrr", "tidyverse", "data.table", "pROC", "rpart",
              "randomForest", "xgboost", "lightgbm")
missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing)
