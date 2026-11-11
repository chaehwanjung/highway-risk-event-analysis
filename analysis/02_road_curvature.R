# 저장소 루트에서 실행합니다. 입력·중간 데이터는 data/에 배치합니다.
base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))

# 도로선형 굴곡도 산출 (SHP → curvature_by_km)

library(sf)
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(zoo)

# 1. SHP 읽기 및 경부선 추출
road_curve <- st_read(file.path(base_dir, "geometry"),
                      options = "ENCODING=UTF-8")

keombu <- road_curve %>% filter(key == "0010")

# 2. 좌표 추출 및 방위각 변화율 계산 (단위: 도)
coords <- st_coordinates(keombu) %>%
  as.data.frame() %>%
  mutate(
    dx          = X - lag(X, default = first(X)),
    dy          = Y - lag(Y, default = first(Y)),
    seg_dist    = sqrt(dx^2 + dy^2),
    cum_dist_m  = cumsum(seg_dist),
    cum_dist_km = cum_dist_m / 1000,
    
    bearing   = atan2(dy, dx) * (180 / pi),
    d_bearing = bearing - lag(bearing),
    d_bearing = case_when(
      d_bearing >  180 ~ d_bearing - 360,
      d_bearing < -180 ~ d_bearing + 360,
      TRUE             ~ d_bearing
    ),
    curvature = abs(d_bearing) / (seg_dist + 1e-10)   # deg/m
  )

# 3. IC 위치정보 불러오기
ic_data <- read_csv(file.path(base_dir, "고속도로IC 위치정보.csv"),
                    locale = locale(encoding = "EUC-KR"))

ic_keombu <- ic_data %>%
  filter(노선코드 == 10) %>%
  arrange(Y좌표값)

# 4. IC 좌표 → SHP coords 최근접 매핑
ic_sf <- ic_keombu %>%
  st_as_sf(coords = c("X좌표값", "Y좌표값"), crs = 4326) %>%
  st_transform(3857)

coords_sf   <- coords %>% st_as_sf(coords = c("X", "Y"), crs = 3857)
nearest_idx <- st_nearest_feature(ic_sf, coords_sf)

ic_keombu <- ic_keombu %>%
  mutate(shp_cum_km = coords$cum_dist_km[nearest_idx])

ic_clean <- ic_keombu %>%
  filter(str_starts(`IC/JC코드`, "0010")) %>%
  arrange(shp_cum_km) %>%
  select(`IC/JC코드`, `IC/JC명`, shp_cum_km)

# 5. 공식 이정 앵커링
ic_official <- tribble(
  ~name,              ~official_km,
  "구서IC",               0.60,
  "노포IC",               5.08,
  "양산JCT",             12.84,
  "양산IC",              17.94,
  "통도사IC",            32.05,
  "서울주JCT",           34.00,
  "언양JCT",             40.11,
  "경주IC",              68.26,
  "서경주IC",            78.66,
  "영천IC",              96.39,
  "경산IC",             112.84,
  "동대구JCT",          122.11,
  "북대구IC",           133.87,
  "칠곡물류IC",         150.09,
  "왜관IC",             153.27,
  "남구미IC",           167.25,
  "구미IC",             172.32,
  "동김천IC",           192.00,
  "김천IC",             200.27,
  "추풍령IC",           212.32,
  "황간IC",             222.38,
  "영동IC",             232.27,
  "금강IC",             248.31,
  "옥천IC",             259.71,
  "비룡JCT",            268.38,
  "대전IC",             271.94,
  "회덕JCT",            277.89,
  "신탄진IC",           281.97,
  "남청주IC",           293.05,
  "남이JCT",            300.49,
  "청주IC",             305.11,
  "독립기념관IC",       329.91,
  "천안JCT",            333.44,
  "천안IC",             340.14,
  "북천안IC",           348.50,
  "안성IC",             360.11,
  "안성JCT",            364.94,
  "오산IC",             378.21,
  "기흥동탄IC",         386.49,
  "기흥IC",             387.19,
  "수원신갈IC",         392.28,
  "신갈JCT",            394.96,
  "판교IC",             406.94,
  "판교JCT",            408.04,
  "대왕판교IC",         408.32,
  "양재IC",             416.05
)

ic_anchors_clean <- ic_clean %>%
  left_join(ic_official, by = c("IC/JC명" = "name")) %>%
  filter(!is.na(official_km)) %>%
  distinct(`IC/JC명`, .keep_all = TRUE) %>%
  arrange(official_km)

shp_to_ijeong <- approxfun(
  x      = ic_anchors_clean$shp_cum_km,
  y      = ic_anchors_clean$official_km,
  method = "linear",
  rule   = 2
)

coords <- coords %>%
  mutate(ijeong_km = shp_to_ijeong(cum_dist_km))

# 6. 0.5km 간격 굴곡도 집계
km_interval <- 0.5

curve_by_km <- tibble(
  km_start = seq(0, 415, by = km_interval),
  km_end   = km_start + km_interval
) %>%
  mutate(
    mean_curvature = map2_dbl(km_start, km_end, ~{
      vals <- coords %>%
        filter(ijeong_km >= .x, ijeong_km < .y, is.finite(curvature)) %>%
        pull(curvature)
      if (length(vals) == 0) NA_real_ else mean(vals)
    }),
    max_curvature = map2_dbl(km_start, km_end, ~{
      vals <- coords %>%
        filter(ijeong_km >= .x, ijeong_km < .y, is.finite(curvature)) %>%
        pull(curvature)
      if (length(vals) == 0) NA_real_ else max(vals)
    }),
    # 핵심 변수: 구간 내 누적 방향 전환량 (deg)
    sum_bearing_change = map2_dbl(km_start, km_end, ~{
      vals <- coords %>%
        filter(ijeong_km >= .x, ijeong_km < .y, is.finite(d_bearing)) %>%
        pull(d_bearing)
      if (length(vals) == 0) NA_real_ else sum(abs(vals))
    })
  ) %>%
  mutate(across(c(mean_curvature, max_curvature, sum_bearing_change),
                ~na.approx(., na.rm = FALSE)))

# 7. 검증 및 저장 
curve_by_km %>%
  summarise(across(c(mean_curvature, max_curvature, sum_bearing_change),
                   ~sum(is.na(.)), .names = "na_{.col}"))

write_csv(curve_by_km,
          file.path(base_dir, "curvature_by_km.csv"))

cat("✅ PART A 완료 | 총", nrow(curve_by_km), "개 구간 |",
    "이정 범위:", min(curve_by_km$km_start), "~",
    max(curve_by_km$km_end), "km\n")