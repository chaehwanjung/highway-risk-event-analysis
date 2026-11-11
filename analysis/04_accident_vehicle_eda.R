# 저장소 루트에서 실행합니다. 입력·중간 데이터는 data/에 배치합니다.
base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))

# 사고 차량종류 EDA용 테이블 생성

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)


# 1. 사고차량 데이터 로드

base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))

vehicle_path <- file.path(
  base_dir,
  "TalkFile_사고데이터 (crashcount 1-2) 최종.xlsx.xlsx"
)

crash_vehicle <- read_excel(
  vehicle_path,
  col_types = "text",
  na = character()
)

names(crash_vehicle) <- str_trim(names(crash_vehicle))


# 2. 자료형 / 시간 정리

crash_vehicle <- crash_vehicle %>%
  mutate(
    date = str_trim(date),
    time = str_trim(time),
    time_only = str_extract(time, "\\d{1,2}:\\d{2}:\\d{2}"),
    time_only = ifelse(
      str_count(time_only, ":") == 2 & nchar(time_only) == 7,
      paste0("0", time_only),
      time_only
    ),
    
    datetime = ymd_hms(paste(date, time_only)),
    datetime_hour = floor_date(datetime, "hour"),
    
    road_km = as.numeric(road_km),
    occ = as.numeric(occ),
    speed = as.numeric(speed),
    vol = as.numeric(vol),
    cz_length = as.numeric(cz_length),
    crash_count = as.numeric(crash_count),
    hardbraking_count = as.numeric(hardbraking_count),
    
    vds_id = str_trim(vds_id),
    cz_id = str_trim(cz_id),
    cz_name = str_trim(cz_name),
    
    가해차종 = str_trim(가해차종),
    피해차종 = str_trim(피해차종),
    
    # 빈칸, ?는 결측 삭제가 아니라 미상 범주로 처리
    가해차종 = ifelse(가해차종 == "" | 가해차종 == "?", "미상", 가해차종),
    피해차종 = ifelse(피해차종 == "" | 피해차종 == "?", "미상", 피해차종)
  )


# 3. 초기 EDA

colSums(is.na(crash_vehicle))

table(crash_vehicle$가해차종, useNA = "ifany")
table(crash_vehicle$피해차종, useNA = "ifany")

sum(duplicated(crash_vehicle))

crash_vehicle %>%
  filter(
    road_km < 0 |
      road_km > 416.05 |
      crash_count < 0 |
      hardbraking_count < 0
  )


# 자료형 변환 + 날짜/시간 정리 수정본

crash_vehicle <- crash_vehicle %>%
  mutate(
    date = str_trim(date),
    time = str_trim(time),
    
    # time이 엑셀 시간 소수값인 경우 처리
    # 예: 0.4722222 = 11:20:00
    time_num = as.numeric(time),
    
    time_only = ifelse(
      !is.na(time_num),
      format(
        as.POSIXct(time_num * 86400, origin = "1970-01-01", tz = "UTC"),
        "%H:%M:%S"
      ),
      str_extract(time, "\\d{1,2}:\\d{2}:\\d{2}")
    ),
    
    datetime = ymd_hms(paste(date, time_only)),
    datetime_hour = floor_date(datetime, "hour"),
    
    road_km = as.numeric(road_km),
    occ = as.numeric(occ),
    speed = as.numeric(speed),
    vol = as.numeric(vol),
    cz_length = as.numeric(cz_length),
    crash_count = as.numeric(crash_count),
    hardbraking_count = as.numeric(hardbraking_count),
    
    vds_id = str_trim(vds_id),
    cz_id = str_trim(cz_id),
    cz_name = str_trim(cz_name),
    
    가해차종 = str_trim(가해차종),
    피해차종 = str_trim(피해차종),
    
    # 빈칸, NA, ? 전부 미상 처리
    가해차종 = ifelse(is.na(가해차종) | 가해차종 == "" | 가해차종 == "?", "미상", 가해차종),
    피해차종 = ifelse(is.na(피해차종) | 피해차종 == "" | 피해차종 == "?", "미상", 피해차종)
  )

# 확인 -> 결측치 x
colSums(is.na(crash_vehicle))

head(crash_vehicle[, c("date", "time", "time_num", "time_only", "datetime", "datetime_hour")], 10)

table(crash_vehicle$가해차종, useNA = "ifany")
table(crash_vehicle$피해차종, useNA = "ifany")

# 중복 / 이상치 확인(이상치 앞전과 마찬가지로 물리적으로 불가능한 기준 처리)
# 중복치 64개, 이상치 x
sum(duplicated(crash_vehicle))

crash_vehicle %>%
  filter(
    road_km < 0 |
      road_km > 416.05 |
      crash_count < 0 |
      hardbraking_count < 0
  )

# 4. 전처리(중복치 처리) 및 최종 EDA용 테이블 생성: 인덱스/보조컬럼 제거 + 중복 제거
crash_vehicle_clean <- crash_vehicle %>%
  select(-any_of(c("...1", "time_num"))) %>%
  distinct()

# 처리 후 확인
colSums(is.na(crash_vehicle_clean))
sum(duplicated(crash_vehicle_clean))
dim(crash_vehicle)
dim(crash_vehicle_clean)

# 차량종류 분포 확인
table(crash_vehicle_clean$가해차종, useNA = "ifany") # 미상(78) > 승용(43) > 화물(20) > 승합(3) > 건설기계(1)
table(crash_vehicle_clean$피해차종, useNA = "ifany") # 미상(78) > 승용(52) > 화물(7) > 특수(5) > 승합(2) > 건설기계(1)

# 5. 저장
write.csv(
  crash_vehicle_clean,
  file.path(base_dir, "crash_vehicle_eda.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)

file.exists(file.path(base_dir, "crash_vehicle_eda.csv"))

library(data.table)
library(lubridate)

dt <- fread(
  file.path(base_dir, "data_mart_final_features_v2.csv"),
  encoding = "UTF-8"
)

dt[, datetime_fix :=
      ymd_hms(
        paste(
          as.character(date),
          as.character(time)
        )
      )]

dt[, hour := hour(datetime_fix)]

dt[, peak_flag :=
      ifelse(hour %in%
               c(7,8,9,17,18,19,20),1,0)]

dt[, night_flag :=
      ifelse(hour >= 22 | hour <= 6,1,0)]

dt[, is_weekend :=
      ifelse(wday(datetime_fix) %in%
               c(1,7),1,0)]

dt[, is_peak_weekday :=
      ifelse(
        peak_flag == 1 &
        is_weekend == 0,
        1,
        0
      )]

dt[, datetime_fix := NULL]