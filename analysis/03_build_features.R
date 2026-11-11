# 저장소 루트에서 실행합니다. 입력·중간 데이터는 data/에 배치합니다.
base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))

install.packages('readr')
install.packages('readxl')
install.packages('dplyr')
install.packages('stringr')
install.packages('tidyr')
install.packages('lubridate')
install.packages('purrr')
install.packages('zoo')

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(purrr)
library(zoo)
library(data.table)

# 1. 기존 1차 데이터 조인 파일 로드
base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))
data_mart_path <- file.path(base_dir, "data_mart.csv")
curve_path <- file.path(base_dir, "curvature_by_km.csv")
bridge_tunnel_path <- file.path(base_dir, "경부선 상행 교량 및 터널(최종).xlsx")

# 2. 데이터 로드
data_mart <- read.csv(data_mart_path, fileEncoding = "CP949")

curve_by_km <- read_csv(
  curve_path,
  locale = locale(encoding = "UTF-8")
)

bridge_raw <- read_excel(
  bridge_tunnel_path,
  sheet = "경부선 상행 교량"
)

tunnel_raw <- read_excel(
  bridge_tunnel_path,
  sheet = "경부선 상행 터널"
)

# 3. 기본 확인
dim(data_mart) # 11704666       25
dim(curve_by_km) # 831   5
dim(bridge_raw) # 799   6
dim(tunnel_raw) # 15  6
names(data_mart)
names(curve_by_km)
names(bridge_raw)
names(tunnel_raw)
summary(data_mart$road_km)
head(curve_by_km)
head(bridge_raw)
head(tunnel_raw)


# 4. datetime 컬럼 정리

# 병합 후 datetime.x, datetime.y가 있을 수 있어서 정리
if ("datetime.x" %in% names(data_mart)) {
  data_mart <- data_mart %>%
    rename(datetime = datetime.x)
}

if ("datetime.y" %in% names(data_mart)) {
  data_mart <- data_mart %>%
    select(-datetime.y)
}

data_mart <- data_mart %>%
  mutate(
    datetime_hour = floor_date(datetime, unit = "hour")
  )

sum(is.na(data_mart$datetime))

# 확인
class(data_mart$datetime)
class(data_mart$datetime_hour)
head(data_mart$datetime)
head(data_mart$datetime_hour)

# 5.추가 데이터 초기 EDA
summary(bridge_raw)
summary(tunnel_raw)
summary(curve_by_km)

str(bridge_raw)
str(tunnel_raw)
str(curve_by_km)

# (1)결측치 탐지
# 교량 & 터널 결측치 X, 굴곡도 결측치 mean_curvature,max_curvature,sum_bearing_change 1개씩 동시 결측 
# 선형보간 + 앞뒤값 보완 처리 검토
colSums(is.na(bridge_raw))
colSums(is.na(tunnel_raw))
colSums(is.na(curve_by_km))
sum(is.na(curve_by_km$mean_curvature) & is.na(curve_by_km$max_curvature) & is.na(curve_by_km$sum_bearing_change)) # 세 변수 동시 결측

# (2)중복치 탐지
# 중복치 모두 X
sum(duplicated(bridge_raw))
sum(duplicated(tunnel_raw))
sum(duplicated(curve_by_km))

# (3)이상치 탐지(도메인 기반 물리적으로 불가능한 값 탐지)
# 1. 교량 이상치 탐지
# 기준: 이정(km) < 0, 연장(m) <= 0
# 결과: 물리적으로 불가능한 이상치 X, 이정(km) 416.05km 초과 교량 11개 확인
# → 분석구간(양재IC 416.05km) 밖 시설물 → 이상치가 아닌 실제 시설물로 판단하여 유지
# (416.05km 초과 교량: 11개)
# 교량 이상치 x
summary(data_mart$road_km)
bridge_raw %>%
  mutate(
    `이정(km)` = as.numeric(`이정(km)`),
    `연장(m)` = as.numeric(`연장(m)`)
  ) %>%
  filter(
    `이정(km)` < 0 |
      `연장(m)` <= 0
  )

bridge_raw %>%
  mutate(`이정(km)` = as.numeric(`이정(km)`)) %>%
  filter(`이정(km)` > 416.05)

# 터널 이상치 탐지기준: 이정(km) < 0, 연장(m) <= 0, 이정(km) > 416.05는 분석구간 밖 후보로만 별도 확인(0개)
# 터널 이상치 x
tunnel_raw %>%
  mutate(
    `이정(km)` = as.numeric(`이정(km)`),
    `연장(m)` = as.numeric(`연장(m)`)
  ) %>%
  filter(
    `이정(km)` < 0 |
      `이정(km)` > 416.05 |
      `연장(m)` <= 0
  )

# 굴곡도 이상치 기준: 구간 역전: km_start >= km_end, 음수 굴곡도: 물리적으로 불가능
# 큰 굴곡도 값은 실제 급곡선 구간일 수 있으므로 제거 기준에 포함하지 않음
# 굴곡도 이상치 x
curve_by_km %>%
  mutate(
    km_start = as.numeric(km_start),
    km_end = as.numeric(km_end),
    mean_curvature = as.numeric(mean_curvature),
    max_curvature = as.numeric(max_curvature),
    sum_bearing_change = as.numeric(sum_bearing_change)
  ) %>%
  filter(
    km_start >= km_end |
      mean_curvature < 0 |
      max_curvature < 0 |
      sum_bearing_change < 0
  )

# 6.추가 데이터 전처리(결측치,중복치,이상치 처리) - 중복치,이상치는 없으므로 굴곡도 결측치만 처리 
# 공간적으로 연속된 데이터이므로 Linear Interpolation 적용
# 보간 불가능한 시작/끝 구간은 LOCF(최근 관측값 대체)로 보완

curve_clean <- curve_by_km %>%
  arrange(km_start) %>%
  mutate(
    mean_curvature = na.approx(mean_curvature, na.rm = FALSE),
    max_curvature = na.approx(max_curvature, na.rm = FALSE),
    sum_bearing_change = na.approx(sum_bearing_change, na.rm = FALSE),
    
    mean_curvature = na.locf(mean_curvature, na.rm = FALSE),
    max_curvature = na.locf(max_curvature, na.rm = FALSE),
    sum_bearing_change = na.locf(sum_bearing_change, na.rm = FALSE),
    
    mean_curvature = na.locf(mean_curvature, fromLast = TRUE),
    max_curvature = na.locf(max_curvature, fromLast = TRUE),
    sum_bearing_change = na.locf(sum_bearing_change, fromLast = TRUE)
  )

# 처리 후 확인
colSums(is.na(curve_clean))

summary(curve_clean)
# 7. 경부선 IC/JC/TG 기준 이정표 생성(시설물명 → km 주소록 생성)
gyeongbu_facility <- tribble(
  ~seq, ~name, ~type, ~km_from_busan,
  1,  "부산기점", "start", 0.00,
  2,  "구서IC", "IC", 0.60,
  3,  "영락IC", "IC", 2.00,
  4,  "부산TG", "TG", 3.00,
  5,  "노포IC", "IC", 5.08,
  6,  "노포JC", "JC", 5.08,
  7,  "양산JC", "JC", 12.84,
  8,  "양산IC", "IC", 17.94,
  9,  "통도사Hi", "IC", 30.70,
  10, "통도사IC", "IC", 32.05,
  11, "서울주JC", "JC", 34.00,
  12, "서울산IC", "IC", 36.31,
  13, "언양JC", "JC", 40.11,
  14, "활천IC", "IC", 57.30,
  15, "경주IC", "IC", 68.26,
  16, "건천IC", "IC", 78.66,
  17, "영천JC", "JC", 90.70,
  18, "영천IC", "IC", 96.39,
  19, "경산IC", "IC", 112.84,
  20, "동대구JC", "JC", 122.11,
  21, "도동JC", "JC", 126.19,
  22, "북대구IC", "IC", 133.87,
  23, "금호JC", "JC", 139.35,
  24, "칠곡JC", "JC", 143.00,
  25, "칠곡물류IC", "IC", 150.09,
  26, "왜관IC", "IC", 153.27,
  27, "남구미IC", "IC", 167.25,
  28, "구미IC", "IC", 172.32,
  29, "북구미하이패스IC", "IC", 179.28,
  30, "김천JC", "JC", 183.18,
  31, "동김천IC", "IC", 192.00,
  32, "김천IC", "IC", 200.27,
  33, "추풍령IC", "IC", 212.32,
  34, "황간IC", "IC", 222.38,
  35, "영동IC", "IC", 232.27,
  36, "금강IC", "IC", 248.31,
  37, "옥천IC", "IC", 259.71,
  38, "비룡JC", "JC", 268.38,
  39, "대전IC", "IC", 271.94,
  40, "회덕JC", "JC", 277.89,
  41, "신탄진IC", "IC", 281.97,
  42, "남청주IC", "IC", 293.05,
  43, "청주JC", "JC", 296.55,
  44, "남이JC", "JC", 300.49,
  45, "청주IC", "IC", 305.11,
  46, "옥산IC", "IC", 308.72,
  47, "옥산JC", "JC", 315.00,
  48, "목천IC", "IC", 329.91,
  49, "천안JC", "JC", 333.44,
  50, "천안IC", "IC", 340.14,
  51, "북천안IC", "IC", 348.50,
  52, "안성IC", "IC", 360.11,
  53, "안성JC", "JC", 364.94,
  54, "남사진위IC", "IC", 371.00,
  55, "오산IC", "IC", 378.21,
  56, "동탄JC", "JC", 382.10,
  57, "기흥동탄IC", "IC", 386.49,
  58, "기흥IC", "IC", 387.19,
  59, "수원신갈IC", "IC", 392.28,
  60, "신갈JC", "JC", 394.96,
  61, "서울TG", "TG", 403.00,
  62, "판교IC", "IC", 406.94,
  63, "판교JC", "JC", 408.04,
  64, "대왕판교IC", "IC", 408.32,
  65, "금토JC", "JC", 409.30,
  66, "양재IC", "IC", 416.05
) %>%
  arrange(km_from_busan)

# 8. 차로 수 함수

assign_lanes <- function(km) {
  case_when(
    km >= 403.00 & km < 406.94 ~ 5L,
    km >= 296.55 & km < 300.49 ~ 5L,
    km >= 360.11 & km < 364.94 ~ 5L,
    km >= 382.10 & km < 386.49 ~ 5L,
    km >= 392.28 & km < 394.96 ~ 5L,
    km >= 406.94 & km <= 416.05 ~ 5L,
    km >= 12.84  & km < 17.94  ~ 4L,
    km >= 112.84 & km < 133.87 ~ 4L,
    km >= 133.87 & km < 139.35 ~ 4L,
    km >= 139.35 & km < 183.18 ~ 4L,
    km >= 277.89 & km < 296.55 ~ 4L,
    km >= 329.91 & km < 333.44 ~ 4L,
    km >= 333.44 & km < 360.11 ~ 4L,
    km >= 364.94 & km < 382.10 ~ 4L,
    km >= 386.49 & km < 392.28 ~ 4L,
    km >= 394.96 & km < 403.00 ~ 4L,
    TRUE ~ 3L
  )
}

# 9. 교량/터널 구간 생성

bridge_zones <- bridge_raw %>%
  transmute(
    bridge_name = 교량명,
    b_start = `이정(km)`,
    length_m = `연장(m)`,
    b_end = b_start + length_m / 1000
  )

tunnel_zones <- tunnel_raw %>%
  transmute(
    tunnel_name = 터널명,
    t_start = `이정(km)`,
    length_m = `연장(m)`,
    t_end = t_start + length_m / 1000
  )


# 확인
nrow(bridge_zones) # bridge_zones : 799개
nrow(tunnel_zones) # tunnel_zones : 15개

# 10. cz_name별(구간이름별) km_start/km_end 계산

facility_km <- gyeongbu_facility %>%
  filter(type %in% c("IC", "JC", "TG", "start")) %>%
  select(name, km_from_busan)

cz_km <- data_mart %>%
  distinct(cz_name, cz_length) %>%
  separate(
    cz_name,
    into = c("to_fac", "from_fac"),
    sep = "-",
    remove = FALSE
  ) %>%
  left_join(
    facility_km %>% rename(km_from = km_from_busan),
    by = c("from_fac" = "name")
  ) %>%
  left_join(
    facility_km %>% rename(km_to = km_from_busan),
    by = c("to_fac" = "name")
  ) %>%
  mutate(
    km_start = pmin(km_from, km_to, na.rm = TRUE),
    km_end = pmax(km_from, km_to, na.rm = TRUE),
    km_start = ifelse(is.infinite(km_start), NA, km_start),
    km_end = ifelse(is.infinite(km_end), NA, km_end),
    km_mid = (km_start + km_end) / 2
  )

cz_km_fail <- cz_km %>%
  filter(is.na(km_start) | is.na(km_end))

cz_km_fail

# 11. cz_name별 터널/교량/차로 수 속성 생성

cz_attr <- cz_km %>%
  mutate(
    tunnel_names = map2_chr(km_start, km_end, function(ks, ke) {
      matched <- tunnel_zones$tunnel_name[
        tunnel_zones$t_start < ke & tunnel_zones$t_end > ks
      ]
      if (length(matched) == 0) "" else paste(matched, collapse = ", ")
    }),
    tunnel_count = map2_int(km_start, km_end, function(ks, ke) {
      sum(tunnel_zones$t_start < ke & tunnel_zones$t_end > ks)
    }),
    has_tunnel = ifelse(tunnel_count > 0, 1L, 0L),
    
    bridge_names = map2_chr(km_start, km_end, function(ks, ke) {
      matched <- bridge_zones$bridge_name[
        bridge_zones$b_start < ke & bridge_zones$b_end > ks
      ]
      if (length(matched) == 0) "" else paste(matched, collapse = ", ")
    }),
    bridge_count = map2_int(km_start, km_end, function(ks, ke) {
      sum(bridge_zones$b_start < ke & bridge_zones$b_end > ks)
    }),
    has_bridge = ifelse(bridge_count > 0, 1L, 0L),
    
    lanes_oneway = assign_lanes(km_mid)
  ) %>%
  select(
    cz_name, km_start, km_end, km_mid,
    has_tunnel, tunnel_count, tunnel_names,
    has_bridge, bridge_count, bridge_names,
    lanes_oneway
  )

# 확인
head(cz_attr) # 큰 콘존 : 60개
table(cz_attr$has_tunnel) # 터널 포함 구간 : 8개
table(cz_attr$has_bridge) # 교량 포함 구간 : 57개
table(cz_attr$lanes_oneway) # 차로수 : 3차로 27개, 4차로 25개, 5차로 8개

# 12. data_mart에 cz 속성 조인

data_mart2 <- data_mart %>%
  left_join(cz_attr, by = "cz_name")

# 확인
dim(data_mart)
dim(data_mart2)

colSums(is.na(data_mart2[, c(
  "km_start", "km_end", "has_tunnel", "has_bridge", "lanes_oneway"
)]))

# 13. IC/JC/TG 근접 변수 생성
# IC/JC/TG 500m 이내 행: 1,708,629개
# IC 500m 이내: 996,905개
# JC 500m 이내: 642,970개
# TG 500m 이내: 68,754개

facility_km_typed <- gyeongbu_facility %>%
  filter(type %in% c("IC", "JC", "TG")) %>%
  arrange(km_from_busan) %>%
  select(name, type, km_from_busan)

fac_km <- facility_km_typed$km_from_busan
fac_nm <- facility_km_typed$name
fac_tp <- facility_km_typed$type

get_nearest_facility <- function(x) {
  left_idx <- findInterval(x, fac_km)
  right_idx <- left_idx + 1L
  
  left_idx <- pmax(1L, pmin(left_idx, length(fac_km)))
  right_idx <- pmax(1L, pmin(right_idx, length(fac_km)))
  
  left_dist <- abs(x - fac_km[left_idx])
  right_dist <- abs(x - fac_km[right_idx])
  
  use_right <- right_dist < left_dist
  nearest_idx <- ifelse(use_right, right_idx, left_idx)
  
  tibble(
    nearest_facility_name = fac_nm[nearest_idx],
    nearest_facility_type = fac_tp[nearest_idx],
    nearest_facility_km = fac_km[nearest_idx],
    nearest_facility_dist = abs(x - fac_km[nearest_idx])
  )
}

nearest_facility <- get_nearest_facility(data_mart2$road_km)

data_mart2 <- bind_cols(data_mart2, nearest_facility) %>%
  mutate(
    facility_flag_500m = ifelse(nearest_facility_dist <= 0.5, 1L, 0L),
    ic_near_500m = ifelse(facility_flag_500m == 1 & nearest_facility_type == "IC", 1L, 0L),
    jc_near_500m = ifelse(facility_flag_500m == 1 & nearest_facility_type == "JC", 1L, 0L),
    tg_near_500m = ifelse(facility_flag_500m == 1 & nearest_facility_type == "TG", 1L, 0L)
  )

# 확인
table(data_mart2$facility_flag_500m)
table(data_mart2$ic_near_500m)
table(data_mart2$jc_near_500m)
table(data_mart2$tg_near_500m)
summary(data_mart2$nearest_facility_dist)

# 14. 굴곡도 curvature_by_km 매핑
# 굴곡도 결측치 0개
# Min     = 0.000094
# Median  = 7.762
# Mean    = 16.14
# Max     = 210
# 210도 경주터널 인근 급곡선 구간이므로 유지

curve_idx <- findInterval(data_mart2$road_km, curve_clean$km_start)
curve_idx <- pmax(1L, pmin(curve_idx, nrow(curve_clean)))

data_mart2 <- data_mart2 %>%
  mutate(
    curve_km_start = curve_clean$km_start[curve_idx],
    curve_km_end = curve_clean$km_end[curve_idx],
    mean_curvature = curve_clean$mean_curvature[curve_idx],
    max_curvature = curve_clean$max_curvature[curve_idx],
    sum_bearing_change = curve_clean$sum_bearing_change[curve_idx]
  )

colSums(is.na(data_mart2[, c(
  "curve_km_start",
  "curve_km_end",
  "mean_curvature",
  "max_curvature",
  "sum_bearing_change"
)]))

summary(data_mart2$sum_bearing_change)

# 15. 교통안전지표 포함 파생변수 생성

holiday_2022 <- as.Date(c(
  "2022-01-01",
  "2022-01-31",
  "2022-02-01",
  "2022-02-02",
  "2022-03-01",
  "2022-03-09"
))

# 1) 시간 기반 + TSD
data_mart2 <- data_mart2 %>%
  arrange(vds_id, datetime) %>%
  group_by(vds_id) %>%
  mutate(
    date_only = as.Date(datetime),
    hour = hour(datetime),
    month = month(datetime),
    dayofweek = wday(datetime, week_start = 1),  # 월=1, 일=7
    
    is_weekend = ifelse(dayofweek %in% c(6, 7), 1L, 0L),
    is_holiday = ifelse(date_only %in% holiday_2022, 1L, 0L),
    peak_flag = ifelse(hour %in% c(7, 8, 9, 17, 18, 19, 20), 1L, 0L),
    night_flag = ifelse(hour >= 22 | hour <= 6, 1L, 0L),
    is_peak_weekday = ifelse(peak_flag == 1 & is_weekend == 0 & is_holiday == 0, 1L, 0L),
    
    speed_lag1 = lag(speed, 1),
    speed_lag2 = lag(speed, 2),
    speed_lag3 = lag(speed, 3),
    
    vol_lag1 = lag(vol, 1),
    vol_lag2 = lag(vol, 2),
    vol_lag3 = lag(vol, 3),
    
    TSD_5 = speed - speed_lag1,
    TSD_10 = speed - speed_lag2,
    TSD_15 = speed - speed_lag3,
    
    abs_TSD_5 = abs(TSD_5),
    abs_TSD_10 = abs(TSD_10),
    abs_TSD_15 = abs(TSD_15),
    
    speed_drop = speed_lag1 - speed,
    
    speed_cv = rollapply(
      speed,
      width = 6,
      FUN = function(x) {
        ifelse(mean(x, na.rm = TRUE) == 0,
               NA_real_,
               sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
      },
      fill = NA,
      align = "right"
    ),
    
    occ_anomaly = occ - rollapply(
      occ,
      width = 6,
      FUN = mean,
      fill = NA,
      align = "right",
      na.rm = TRUE
    ),
    
    event_count = crash_count + hardbraking_count,
    rolling_event_30m = rollapply(
      event_count,
      width = 6,
      FUN = sum,
      fill = NA,
      align = "right",
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# 2) SSD: 같은 시간대에서 road_km 순서 기준 공간적 속도차
data_mart2 <- data_mart2 %>%
  arrange(datetime, road_km, vds_id) %>%
  group_by(datetime) %>%
  mutate(
    speed_space_lag1 = lag(speed, 1),
    speed_space_lag2 = lag(speed, 2),
    speed_space_lag3 = lag(speed, 3),
    
    SSD_5 = speed - speed_space_lag1,
    SSD_10 = speed - speed_space_lag2,
    SSD_15 = speed - speed_space_lag3,
    
    abs_SSD_5 = abs(SSD_5),
    abs_SSD_10 = abs(SSD_10),
    abs_SSD_15 = abs(SSD_15)
  ) %>%
  ungroup()

# 3) TSSD + 추가 교통류/도로구조 파생변수
data_mart2 <- data_mart2 %>%
  mutate(
    TSSD_5 = TSD_5 + SSD_5,
    TSSD_10 = TSD_10 + SSD_10,
    TSSD_15 = TSD_15 + SSD_15,
    
    abs_TSSD_5 = abs_TSD_5 + abs_SSD_5,
    abs_TSSD_10 = abs_TSD_10 + abs_SSD_10,
    abs_TSSD_15 = abs_TSD_15 + abs_SSD_15,
    
    flow_density = ifelse(speed > 0, vol / speed, NA_real_),
    lane_vol_ratio = vol / lanes_oneway,
    occ_speed_ratio = occ / (speed + 1),
    
    wet_road = ifelse(강수량.mm. > 0, 1L, 0L),
    snow_road = ifelse(적설.cm. > 0, 1L, 0L),
    
    curve_speed_risk = sum_bearing_change * (100 - speed),
    curve_vol_density = sum_bearing_change * flow_density,
    curve_TSD = sum_bearing_change * abs_TSD_5,
    curve_wet_risk = sum_bearing_change * wet_road,
    facility_speed_drop = facility_flag_500m * speed_drop,
    tunnel_occ = has_tunnel * occ,
    bridge_occ = has_bridge * occ,
    night_curve_risk = night_flag * sum_bearing_change,
    
    hardbraking_rate = ifelse(vol > 0, hardbraking_count / vol, 0),
    
    risk_score = crash_count + hardbraking_count * 0.3,
    crash_binary = ifelse(crash_count > 0, 1L, 0L),
    event_binary = ifelse(crash_count > 0 | hardbraking_count > 0, 1L, 0L)
  )

# 16. 파생변수 확인
# 결과 자체는 대체로 정상. event_binary 평균 0.007378로 위험이벤트 비율 약 0.74% 수준의 불균형 데이터 확인

# 체크 포인트
# 1. TSD/SSD/TSSD가 음수로 나오는 것은 정상
#    속도 변화량을 방향성 있게 계산했기 때문에 감소(-)와 증가(+)를 모두 표현함
#    모델에는 방향형 변수와 절댓값 변수(abs_TSD, abs_SSD, abs_TSSD)를 함께 활용 가능

# 2. speed_cv 결측치는 30분(6개 시점) 롤링 계산 과정에서
#    초기 구간 및 일부 결측 영향으로 발생한 자연스러운 결과
#    모델링 전에는 0 또는 중앙값 대체 예정

# 3. curve_TSD 최대값(26190)은 mean_curvature × abs_TSD_5 조합으로 생성된 값
#    곡률이 큰 구간에서 급격한 속도 변화가 발생한 경우 나타날 수 있음
#    스케일이 크므로 선형계열 모델에서는 표준화 적용 예정
#    트리계열(RandomForest, XGBoost, LightGBM)은 그대로 사용 가능

# 4. occ_speed_ratio 최대값 50은 매우 낮은 속도와 높은 점유율이 동시에 발생한
#    심한 정체 상황을 의미하므로 제거하지 않고 유지

# 5. hardbraking_rate, risk_score, event_binary 분포를 보면
#    위험이벤트 발생 비율이 매우 낮아 향후 모델링 시 클래스 불균형 대응 필요
#    (Recall 중심 평가, Class Weight 또는 Sampling 검토)

summary(data_mart2[, c(
  "TSD_5", "TSD_10", "TSD_15",
  "SSD_5", "SSD_10", "SSD_15",
  "TSSD_5", "TSSD_10", "TSSD_15",
  "speed_cv", "speed_drop", "flow_density",
  "lane_vol_ratio", "occ_speed_ratio",
  "hardbraking_rate", "rolling_event_30m",
  "is_weekend", "is_holiday", "peak_flag",
  "curve_TSD", "curve_wet_risk",
  "tunnel_occ", "bridge_occ",
  "risk_score", "event_binary"
)])

# 17. 최종검증
# TSD/SSD/TSSD, speed_cv 등의 결측치는 lag 및 rolling 계산 과정에서 발생한 구조적 결측치로 정상임.
# event_binary 비율은 약 0.74%(86,354건)로 매우 불균형한 데이터임.
# curve_TSD 최대값(26,190), hardbraking_rate 최대값(2), occ_speed_ratio 최대값(50)은
# 실제 위험상황 또는 극심한 정체구간에서 발생 가능한 값으로 판단하여 제거하지 않음.
# 따라서 추가 이상치 제거 없이 최종 모델링 진행.
# 결측치 확인
colSums(is.na(data_mart2[, c(
  "TSD_5", "TSD_10", "TSD_15",
  "SSD_5", "SSD_10", "SSD_15",
  "TSSD_5", "TSSD_10", "TSSD_15",
  "speed_cv", "flow_density", "rolling_event_30m"
)]))

# 목적변수 불균형 확인
table(data_mart2$event_binary)
prop.table(table(data_mart2$event_binary))

# 극단값 확인
summary(data_mart2$curve_TSD)
summary(data_mart2$hardbraking_rate)
summary(data_mart2$occ_speed_ratio)

# 18. 최종 수정

library(dplyr)
library(data.table)
library(lubridate)

data_mart2 <- data_mart2 %>%
  mutate(
    across(
      c(TSD_5, TSD_10, TSD_15,
        SSD_5, SSD_10, SSD_15,
        TSSD_5, TSSD_10, TSSD_15,
        speed_cv, rolling_event_30m),
      ~ ifelse(is.na(.), 0, .)
    ),
    flow_density = ifelse(is.na(flow_density), 0, flow_density),
    curve_TSD = ifelse(is.na(mean_curvature * abs_TSD_5), 0, mean_curvature * abs_TSD_5),
    speed_drop_ratio = ifelse(speed_lag1 > 0, (speed_lag1 - speed) / speed_lag1, 0),
    speed_drop_ratio = ifelse(is.na(speed_drop_ratio), 0, speed_drop_ratio),
    speed_drop_ratio = pmax(pmin(speed_drop_ratio, 1), -1),
    rain_curve_risk = 강수량.mm. * mean_curvature,
    weather_risk = 강수량.mm. * occ,
    congestion_curve_risk = occ_speed_ratio * mean_curvature,
    
    datetime_fix = ymd_hms(paste(as.character(date), as.character(time)), quiet = TRUE),
    hour = hour(datetime_fix),
    peak_flag = ifelse(hour %in% c(7,8,9,17,18,19,20), 1L, 0L),
    night_flag = ifelse(hour >= 22 | hour <= 6, 1L, 0L),
    is_weekend = ifelse(wday(datetime_fix) %in% c(1,7), 1L, 0L),
    is_peak_weekday = ifelse(peak_flag == 1 & is_weekend == 0, 1L, 0L)
  ) %>%
  select(-datetime_fix)

# 최종 확인
colSums(is.na(data_mart2[, c(
  "TSD_5", "TSD_10", "TSD_15",
  "SSD_5", "SSD_10", "SSD_15",
  "TSSD_5", "TSSD_10", "TSSD_15",
  "speed_cv", "flow_density", "rolling_event_30m",
  "curve_TSD", "speed_drop_ratio",
  "rain_curve_risk", "weather_risk", "congestion_curve_risk",
  "hour", "peak_flag", "night_flag", "is_weekend", "is_peak_weekday"
)]))

table(data_mart2$hour)
table(data_mart2$peak_flag)

# 19. 저장
fwrite(
  data_mart2,
  file.path(base_dir, "data_mart_final_features_v2.csv"),
  sep = ",",
  bom = TRUE,
  na = ""
)

cat("v2 저장 완료\n")
cat("행수:", nrow(data_mart2), "\n")
cat("경로:", file.path(base_dir, "data_mart_final_features_v2.csv"), "\n")

file.exists(file.path(base_dir, "data_mart_final_features_v2.csv"))

file.info(file.path(base_dir, "data_mart_final_features_v2.csv"))$size / 1024^3
