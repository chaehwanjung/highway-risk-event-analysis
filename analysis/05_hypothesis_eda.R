# 저장소 루트에서 실행합니다. 입력·중간 데이터는 data/에 배치합니다.
base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))

# 가설 기반 EDA 코드
# data_mart2 로드 X
# final CSV에서 필요한 컬럼만 chunk로 읽어서 EDA
# 시간변수는 date + time으로 재생성

rm(list = ls())
gc()

library(readr)
library(dplyr)
library(lubridate)
library(stringr)

base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))

final_path <- file.path(base_dir, "data_mart_final_features_v2.csv")
eda_dir <- file.path(base_dir, "eda_outputs")

dir.create(eda_dir, showWarnings = FALSE)

append_agg <- function(old, new, group_cols) {
  bind_rows(old, new) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      total_rows = sum(total_rows, na.rm = TRUE),
      event_rows = sum(event_rows, na.rm = TRUE),
      across(starts_with("sum_"), ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

overall_total <- 0
overall_event <- 0

h1_weather <- data.frame()
h2_speed <- data.frame()
h3_congestion <- data.frame()
h4_curve <- data.frame()
h5_facility <- data.frame()
h6_structure <- data.frame()
h7_time <- data.frame()
h7_time_type <- data.frame()
h8_cz <- data.frame()

eda_callback <- function(x, pos) {
  
  x <- x %>%
    mutate(
      event_binary = as.integer(event_binary),
      wet_road = as.integer(wet_road),
      강수량.mm. = as.numeric(강수량.mm.),
      speed_cv = as.numeric(speed_cv),
      speed_drop_ratio = as.numeric(speed_drop_ratio),
      occ_speed_ratio = as.numeric(occ_speed_ratio),
      mean_curvature = as.numeric(mean_curvature),
      sum_bearing_change = as.numeric(sum_bearing_change),
      curve_TSD = as.numeric(curve_TSD),
      ic_near_500m = as.integer(ic_near_500m),
      jc_near_500m = as.integer(jc_near_500m),
      tg_near_500m = as.integer(tg_near_500m),
      has_tunnel = as.integer(has_tunnel),
      has_bridge = as.integer(has_bridge),
      
      date_col = as.character(date),
      time_col = as.character(time),
      datetime_fix = ymd_hms(paste(date_col, time_col), quiet = TRUE),
      hour_fix = hour(datetime_fix),
      wday_fix = wday(datetime_fix),
      peak_flag_fix = ifelse(hour_fix %in% c(7, 8, 9, 17, 18, 19, 20), 1L, 0L),
      night_flag_fix = ifelse(hour_fix >= 22 | hour_fix <= 6, 1L, 0L),
      is_weekend_fix = ifelse(wday_fix %in% c(1, 7), 1L, 0L)
    )
  
  overall_total <<- overall_total + nrow(x)
  overall_event <<- overall_event + sum(x$event_binary == 1, na.rm = TRUE)
  
  tmp1 <- x %>%
    mutate(weather_group = ifelse(wet_road == 1, "강수/습윤", "비강수/건조")) %>%
    group_by(weather_group) %>%
    summarise(
      total_rows = n(),
      event_rows = sum(event_binary == 1, na.rm = TRUE),
      sum_rain = sum(강수량.mm., na.rm = TRUE),
      .groups = "drop"
    )
  h1_weather <<- append_agg(h1_weather, tmp1, "weather_group")
  
  tmp2 <- x %>%
    mutate(
      speed_cv_group = cut(
        speed_cv,
        breaks = c(-Inf, 0.01, 0.02, 0.04, 0.08, Inf),
        labels = c("G1_매우낮음", "G2_낮음", "G3_보통", "G4_높음", "G5_매우높음")
      )
    ) %>%
    group_by(speed_cv_group) %>%
    summarise(
      total_rows = n(),
      event_rows = sum(event_binary == 1, na.rm = TRUE),
      sum_speed_cv = sum(speed_cv, na.rm = TRUE),
      sum_speed_drop_ratio = sum(speed_drop_ratio, na.rm = TRUE),
      .groups = "drop"
    )
  h2_speed <<- append_agg(h2_speed, tmp2, "speed_cv_group")
  
  tmp3 <- x %>%
    mutate(
      congestion_group = cut(
        occ_speed_ratio,
        breaks = c(-Inf, 0.02, 0.04, 0.06, 0.10, Inf),
        labels = c("G1_매우낮음", "G2_낮음", "G3_보통", "G4_높음", "G5_매우높음")
      )
    ) %>%
    group_by(congestion_group) %>%
    summarise(
      total_rows = n(),
      event_rows = sum(event_binary == 1, na.rm = TRUE),
      sum_occ_speed_ratio = sum(occ_speed_ratio, na.rm = TRUE),
      .groups = "drop"
    )
  h3_congestion <<- append_agg(h3_congestion, tmp3, "congestion_group")
  
  tmp4 <- x %>%
    mutate(
      curve_group = cut(
        mean_curvature,
        breaks = c(-Inf, 0.01, 0.03, 0.06, 0.10, Inf),
        labels = c("G1_매우낮음", "G2_낮음", "G3_보통", "G4_높음", "G5_매우높음")
      )
    ) %>%
    group_by(curve_group) %>%
    summarise(
      total_rows = n(),
      event_rows = sum(event_binary == 1, na.rm = TRUE),
      sum_mean_curvature = sum(mean_curvature, na.rm = TRUE),
      sum_bearing = sum(sum_bearing_change, na.rm = TRUE),
      sum_curve_TSD = sum(curve_TSD, na.rm = TRUE),
      .groups = "drop"
    )
  h4_curve <<- append_agg(h4_curve, tmp4, "curve_group")
  
  tmp5 <- x %>%
    mutate(
      facility_group = case_when(
        tg_near_500m == 1 ~ "TG_500m",
        ic_near_500m == 1 ~ "IC_500m",
        jc_near_500m == 1 ~ "JC_500m",
        TRUE ~ "시설물_비인접"
      )
    ) %>%
    group_by(facility_group) %>%
    summarise(
      total_rows = n(),
      event_rows = sum(event_binary == 1, na.rm = TRUE),
      .groups = "drop"
    )
  h5_facility <<- append_agg(h5_facility, tmp5, "facility_group")
  
  tmp6 <- x %>%
    mutate(
      structure_group = case_when(
        has_tunnel == 1 & has_bridge == 1 ~ "터널+교량",
        has_tunnel == 1 & has_bridge == 0 ~ "터널",
        has_tunnel == 0 & has_bridge == 1 ~ "교량",
        TRUE ~ "일반구간"
      )
    ) %>%
    group_by(structure_group) %>%
    summarise(
      total_rows = n(),
      event_rows = sum(event_binary == 1, na.rm = TRUE),
      .groups = "drop"
    )
  h6_structure <<- append_agg(h6_structure, tmp6, "structure_group")
  
  tmp7 <- x %>%
    group_by(hour_fix) %>%
    summarise(
      total_rows = n(),
      event_rows = sum(event_binary == 1, na.rm = TRUE),
      .groups = "drop"
    )
  h7_time <<- append_agg(h7_time, tmp7, "hour_fix")
  
  tmp8 <- x %>%
    mutate(
      time_type = case_when(
        peak_flag_fix == 1 ~ "첨두",
        night_flag_fix == 1 ~ "야간",
        is_weekend_fix == 1 ~ "주말",
        TRUE ~ "비첨두_주간"
      )
    ) %>%
    group_by(time_type) %>%
    summarise(
      total_rows = n(),
      event_rows = sum(event_binary == 1, na.rm = TRUE),
      .groups = "drop"
    )
  h7_time_type <<- append_agg(h7_time_type, tmp8, "time_type")
  
  tmp9 <- x %>%
    group_by(cz_name) %>%
    summarise(
      total_rows = n(),
      event_rows = sum(event_binary == 1, na.rm = TRUE),
      .groups = "drop"
    )
  h8_cz <<- append_agg(h8_cz, tmp9, "cz_name")
  
  cat("처리 위치:", pos, " / 누적행:", overall_total, "\n")
}

read_csv_chunked(
  final_path,
  callback = DataFrameCallback$new(eda_callback),
  chunk_size = 300000,
  locale = locale(encoding = "CP949"),
  col_types = cols_only(
    date = col_character(),
    time = col_character(),
    event_binary = col_integer(),
    wet_road = col_integer(),
    강수량.mm. = col_double(),
    speed_cv = col_double(),
    speed_drop_ratio = col_double(),
    occ_speed_ratio = col_double(),
    mean_curvature = col_double(),
    sum_bearing_change = col_double(),
    curve_TSD = col_double(),
    ic_near_500m = col_integer(),
    jc_near_500m = col_integer(),
    tg_near_500m = col_integer(),
    has_tunnel = col_integer(),
    has_bridge = col_integer(),
    cz_name = col_character()
  )
)

overall_event_rate <- tibble(
  total_rows = overall_total,
  event_rows = overall_event,
  event_rate = overall_event / overall_total
)

h1_weather <- h1_weather %>%
  mutate(event_rate = event_rows / total_rows,
         avg_rain = sum_rain / total_rows)

h2_speed <- h2_speed %>%
  mutate(event_rate = event_rows / total_rows,
         avg_speed_cv = sum_speed_cv / total_rows,
         avg_speed_drop_ratio = sum_speed_drop_ratio / total_rows)

h3_congestion <- h3_congestion %>%
  mutate(event_rate = event_rows / total_rows,
         avg_occ_speed_ratio = sum_occ_speed_ratio / total_rows)

h4_curve <- h4_curve %>%
  mutate(event_rate = event_rows / total_rows,
         avg_mean_curvature = sum_mean_curvature / total_rows,
         avg_sum_bearing_change = sum_bearing / total_rows,
         avg_curve_TSD = sum_curve_TSD / total_rows)

h5_facility <- h5_facility %>%
  mutate(event_rate = event_rows / total_rows) %>%
  arrange(desc(event_rate))

h6_structure <- h6_structure %>%
  mutate(event_rate = event_rows / total_rows) %>%
  arrange(desc(event_rate))

h7_time <- h7_time %>%
  mutate(event_rate = event_rows / total_rows) %>%
  arrange(hour_fix)

h7_time_type <- h7_time_type %>%
  mutate(event_rate = event_rows / total_rows) %>%
  arrange(desc(event_rate))

h8_cz <- h8_cz %>%
  mutate(event_rate = event_rows / total_rows) %>%
  arrange(desc(event_rate))

write.csv(overall_event_rate, file.path(eda_dir, "overall_event_rate.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h1_weather, file.path(eda_dir, "h1_weather.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h2_speed, file.path(eda_dir, "h2_speed.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h3_congestion, file.path(eda_dir, "h3_congestion.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h4_curve, file.path(eda_dir, "h4_curve.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h5_facility, file.path(eda_dir, "h5_facility.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h6_structure, file.path(eda_dir, "h6_structure.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h7_time, file.path(eda_dir, "h7_time.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h7_time_type, file.path(eda_dir, "h7_time_type.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h8_cz, file.path(eda_dir, "h8_cz.csv"), row.names = FALSE, fileEncoding = "CP949")

cat("EDA 완료\n")
cat("전체 행:", overall_total, "\n")
cat("위험이벤트:", overall_event, "\n")
cat("위험이벤트율:", overall_event / overall_total, "\n")
cat("저장 위치:", eda_dir, "\n")

list.files(eda_dir)

read.csv(file.path(eda_dir, "h1_weather.csv"), fileEncoding = "CP949")
read.csv(file.path(eda_dir, "h2_speed.csv"), fileEncoding = "CP949")
read.csv(file.path(eda_dir, "h3_congestion.csv"), fileEncoding = "CP949")
read.csv(file.path(eda_dir, "h4_curve.csv"), fileEncoding = "CP949")
read.csv(file.path(eda_dir, "h5_facility.csv"), fileEncoding = "CP949")
read.csv(file.path(eda_dir, "h6_structure.csv"), fileEncoding = "CP949")
read.csv(file.path(eda_dir, "h7_time.csv"), fileEncoding = "CP949")
read.csv(file.path(eda_dir, "h7_time_type.csv"), fileEncoding = "CP949")


# 사고차량 보조테이블 가설 기반 EDA
# R 콘솔 확인 + CSV 저장 선택 가능

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))

vehicle_path <- file.path(
  base_dir,
  "TalkFile_사고데이터 (crashcount 1-2) 최종.xlsx.xlsx"
)

eda_dir <- file.path(base_dir, "eda_outputs")
dir.create(eda_dir, showWarnings = FALSE)

# 1. 사고차량 데이터 로드 및 정리
crash_vehicle <- read_excel(
  vehicle_path,
  col_types = "text",
  na = character()
)

names(crash_vehicle) <- str_trim(names(crash_vehicle))

crash_vehicle_clean <- crash_vehicle %>%
  mutate(
    date = str_trim(date),
    time = str_trim(time),
    time_num = as.numeric(time),
    
    time_only = ifelse(
      !is.na(time_num),
      format(
        as.POSIXct(time_num * 86400, origin = "1970-01-01", tz = "UTC"),
        "%H:%M:%S"
      ),
      str_extract(time, "\\d{1,2}:\\d{2}:\\d{2}")
    ),
    
    datetime = ymd_hms(paste(date, time_only), quiet = TRUE),
    datetime_hour = floor_date(datetime, "hour"),
    hour = hour(datetime),
    
    time_group = case_when(
      hour %in% c(7, 8, 9, 17, 18, 19, 20) ~ "첨두",
      hour >= 22 | hour <= 6 ~ "야간",
      TRUE ~ "비첨두_주간"
    ),
    
    road_km = as.numeric(road_km),
    crash_count = as.numeric(crash_count),
    hardbraking_count = as.numeric(hardbraking_count),
    
    cz_name = str_trim(cz_name),
    가해차종 = str_trim(가해차종),
    피해차종 = str_trim(피해차종),
    
    가해차종 = ifelse(is.na(가해차종) | 가해차종 == "" | 가해차종 == "?", "미상", 가해차종),
    피해차종 = ifelse(is.na(피해차종) | 피해차종 == "" | 피해차종 == "?", "미상", 피해차종)
  ) %>%
  select(-any_of(c("...1", "time_num"))) %>%
  distinct()

cat("원본 행수:", nrow(crash_vehicle), "\n")
cat("중복 제거 후 행수:", nrow(crash_vehicle_clean), "\n")


# 보조가설 1.
# 가해차종별 사고 빈도는 화물/승용 중심으로 나타날 것이다.

h_aux1_attacker <- crash_vehicle_clean %>%
  count(가해차종, sort = TRUE) %>%
  mutate(
    rate = n / sum(n),
    rate_percent = round(rate * 100, 2)
  )

cat("\n[보조가설1] 가해차종별 사고 빈도\n")
print(h_aux1_attacker)

h_aux1_victim <- crash_vehicle_clean %>%
  count(피해차종, sort = TRUE) %>%
  mutate(
    rate = n / sum(n),
    rate_percent = round(rate * 100, 2)
  )

cat("\n[참고] 피해차종별 사고 빈도\n")
print(h_aux1_victim)


# 보조가설 2.
# 시간대별 사고 차종 분포는 첨두시간과 야간에서 차이가 있을 것이다.


h_aux2_time_vehicle <- crash_vehicle_clean %>%
  group_by(time_group, 가해차종) %>%
  summarise(
    crash_rows = n(),
    crash_sum = sum(crash_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(time_group) %>%
  mutate(
    rate = crash_rows / sum(crash_rows),
    rate_percent = round(rate * 100, 2)
  ) %>%
  arrange(time_group, desc(crash_rows))

cat("\n[보조가설2] 시간대별 가해차종 분포\n")
print(h_aux2_time_vehicle)

h_aux2_time_total <- crash_vehicle_clean %>%
  count(time_group, sort = TRUE) %>%
  mutate(
    rate = n / sum(n),
    rate_percent = round(rate * 100, 2)
  )

cat("\n[참고] 시간대별 전체 사고 빈도\n")
print(h_aux2_time_total)


# 보조가설 3.
# 구간별 사고 차종 분포는 특정 콘존에 집중될 것이다.

h_aux3_cz_vehicle <- crash_vehicle_clean %>%
  group_by(cz_name, 가해차종) %>%
  summarise(
    crash_rows = n(),
    crash_sum = sum(crash_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(crash_rows))

cat("\n[보조가설3] 구간별 가해차종 분포\n")
print(h_aux3_cz_vehicle)

h_aux3_cz_top10 <- crash_vehicle_clean %>%
  count(cz_name, sort = TRUE) %>%
  mutate(
    rate = n / sum(n),
    rate_percent = round(rate * 100, 2)
  ) %>%
  slice_head(n = 10)

cat("\n[보조가설3] 사고 집중 콘존 TOP 10\n")
print(h_aux3_cz_top10)


# 선택: CSV 저장


write.csv(h_aux1_attacker, file.path(eda_dir, "aux1_attacker_vehicle.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h_aux1_victim, file.path(eda_dir, "aux1_victim_vehicle.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h_aux2_time_vehicle, file.path(eda_dir, "aux2_time_vehicle.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h_aux2_time_total, file.path(eda_dir, "aux2_time_total.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h_aux3_cz_vehicle, file.path(eda_dir, "aux3_cz_vehicle.csv"), row.names = FALSE, fileEncoding = "CP949")
write.csv(h_aux3_cz_top10, file.path(eda_dir, "aux3_cz_top10.csv"), row.names = FALSE, fileEncoding = "CP949")

cat("\n보조 EDA 완료\n")