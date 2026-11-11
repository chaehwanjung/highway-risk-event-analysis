# 저장소 루트에서 실행합니다. 입력·중간 데이터는 data/에 배치합니다.
base_dir <- Sys.getenv("TRAFFIC_PROJECT_DATA_DIR", unset = file.path(getwd(), "data"))

# VDS 데이터의 cz_name은 고속도로 구간명만 제공하고 정확한 위도·경도 좌표는 제공하지 않는다.
# 따라서 본 연구에서는 각 IC/JCT 구간의 실제 위치와 인접 행정구역을 기준으로
# 가장 가까운 ASOS 관측소를 대표 기상지점으로 수동 매핑하였다.
# 또한 인접 구간 간 기상 특성이 유사한 경우 동일 관측소로 통합하여
# 공간적 연속성과 데이터 안정성을 확보하였다.
# ASOS 자료는 AWS보다 관측 품질과 데이터 안정성이 높아
# 전국 단위 고속도로 구간 분석에 적합하다고 판단하였다.


# 1. 데이터 로드
install.packages("readr")
install.packages("dplyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("zoo")

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(zoo)

vds_path <- file.path(base_dir, "(2025-1)PBL최종데이터_경부선S방향.csv")
weather_path <- file.path(base_dir, "ASOS_DATASET.csv")
vds <- read.csv(vds_path, fileEncoding = "CP949")
weather <- read.csv(weather_path, fileEncoding = "CP949")


# 2. 기본 데이터 확인
str(vds) #12890880행
str(weather) # 37453행

head(vds)
head(weather)

summary(vds)
summary(weather)

names(vds)
names(weather)

dim(vds)
dim(weather)


# 3. 날짜 형식 처리
# VDS datetime 생성
vds <- vds %>%
  mutate(
    date = as.character(date),
    datetime = ymd_hms(paste(date, time)),
    datetime_hour = floor_date(datetime, unit = "hour")
  )

# ASOS datetime 생성
weather <- weather %>%
  mutate(
    datetime = ymd_hm(일시),
    datetime_hour = floor_date(datetime, unit = "hour")
  )

# 확인
head(vds$datetime)
head(weather$datetime)

summary(vds$datetime)
summary(weather$datetime)


# 4. 분석 기간 통일
weather <- weather %>%
  filter(datetime <= ymd_hms("2022-05-01 00:00:00"))


# 5. 초기 EDA
# 결측치 확인 -> vds X, weather 기온 1개, 강수량 36043개, 습도 9개, 적설 36935개
colSums(is.na(vds))
colSums(is.na(weather))

# 기초 통계
summary(vds)
summary(weather)

# Boxplot 확인
boxplot(vds$speed, main = "speed")
boxplot(vds$vol, main = "vol")
boxplot(vds$occ, main = "occ")
boxplot(weather$기온..C., main = "temperature")
boxplot(weather$풍속.m.s., main = "wind speed")
boxplot(weather$강수량.mm., main = "rainfall")
boxplot(weather$습도..., main = "humidity")

# 음수 및 비정상값 확인 -> 이상치 확인 -> VDS 데이터 speed,volume,occ 이상치 같은행에서
# 동시에 1185781개
# ,weather 이상치 X
sum(vds$speed < 0)
sum(vds$vol < 0)
sum(vds$occ < 0)
sum(weather$기온..C. < -30, na.rm = TRUE)
sum(weather$풍속.m.s. < 0, na.rm = TRUE)
sum(weather$강수량.mm. < 0, na.rm = TRUE)
sum(weather$습도... < 0, na.rm = TRUE)
sum(weather$습도... > 100, na.rm = TRUE)

vds %>%
  summarise(
    speed_neg = sum(speed < 0),
    vol_neg = sum(vol < 0),
    occ_neg = sum(occ < 0),
    
    all_three = sum(
      speed < 0 &
        vol < 0 &
        occ < 0
    ),
    
    speed_only = sum(
      speed < 0 &
        vol >= 0 &
        occ >= 0
    ),
    
    vol_only = sum(
      speed >= 0 &
        vol < 0 &
        occ >= 0
    ),
    
    occ_only = sum(
      speed >= 0 &
        vol >= 0 &
        occ < 0
    ),
    
    mixed_case = sum(
      (speed < 0 | vol < 0 | occ < 0) &
        !(speed < 0 & vol < 0 & occ < 0)
    ),
    
    actual_removed = sum(
      speed < 0 |
        vol < 0 |
        occ < 0 |
        speed > 150 |
        occ > 100
    )
  )
# 6. 결측치 처리
weather <- weather %>%
  arrange(지점명, datetime) %>%
  group_by(지점명) %>%
  mutate(
    기온..C. = na.approx(기온..C., na.rm = FALSE),
    기온..C. = ifelse(
      is.na(기온..C.),
      median(기온..C., na.rm = TRUE),
      기온..C.
    ),
    
    강수량.mm. = ifelse(
      is.na(강수량.mm.),
      0,
      강수량.mm.
    ),
    
    적설.cm. = ifelse(
      is.na(적설.cm.),
      0,
      적설.cm.
    ),
    
    습도... = ifelse(
      is.na(습도...),
      median(습도..., na.rm = TRUE),
      습도...
    )
  ) %>%
  ungroup()

colSums(is.na(weather))


# 7. VDS 이상치 제거
# 교통류 데이터는 위험상황 자체가 anomaly일 수 있으므로
# IQR 제거 대신 물리적으로 불가능한 값만 제거

vds <- vds %>%
  filter(
    speed >= 0,
    vol >= 0,
    occ >= 0,
    speed <= 150,
    occ <= 100
  )


# 처리 결과 확인
colSums(is.na(vds))
colSums(is.na(weather))

sum(vds$speed < 0)
sum(vds$vol < 0)
sum(vds$occ < 0)
sum(vds$speed > 150)
sum(vds$occ > 100)

summary(vds)
summary(weather)


# 8. 중복치 확인
# 같은 VDS에서 같은 시각이 중복되면 문제

dup_vds <- vds %>%
  count(vds_id, datetime) %>%
  filter(n > 1)

nrow(dup_vds)
head(dup_vds)


# 같은 기상 관측지점에서 같은 시간 자료가 중복되면 문제

dup_weather <- weather %>%
  count(지점명, datetime_hour) %>%
  filter(n > 1)

nrow(dup_weather)
head(dup_weather)


# 9. ASOS 관측지점명 통일
weather <- weather %>%
  mutate(
    weather_station = trimws(지점명)
  )

unique(weather$weather_station)


# 10. 고속도로 구간별 대표 ASOS 지점 수동 매핑
# IC/JCT 기반 기상권역화

vds <- vds %>%
  mutate(
    weather_station = case_when(
      
      # 부산
      cz_name %in% c(
        "영락IC-구서IC",
        "부산TG-영락IC",
        "노포IC-부산TG",
        "노포JC-노포IC"
      ) ~ "부산",
      
      # 양산
      cz_name %in% c(
        "양산JC-노포JC",
        "양산IC-양산JC",
        "통도사Hi-양산IC",
        "서울주JC-통도사IC"
      ) ~ "양산시",
      
      # 울산
      cz_name %in% c(
        "활천IC-언양JC"
      ) ~ "울산",
      
      # 경주
      cz_name %in% c(
        "경주IC-활천IC",
        "건천IC-경주IC"
      ) ~ "경주시",
      
      # 영천
      cz_name %in% c(
        "영천JC-건천IC",
        "영천IC-영천JC",
        "경산IC-영천IC"
      ) ~ "영천",
      
      # 대구
      cz_name %in% c(
        "동대구JC-경산IC",
        "도동JC-동대구JC",
        "북대구IC-도동JC",
        "금호JC-북대구IC"
      ) ~ "대구",
      
      # 구미
      cz_name %in% c(
        "칠곡JC-금호JC",
        "칠곡물류IC-칠곡JC",
        "왜관IC-칠곡물류IC",
        "남구미IC-왜관IC",
        "구미IC-남구미IC",
        "북구미하이패스IC-구미IC",
        "김천JC-북구미하이패스IC",
        "동김천IC-김천JC",
        "김천IC-동김천IC"
      ) ~ "구미",
      
      # 추풍령
      cz_name %in% c(
        "추풍령IC-김천IC",
        "황간IC-추풍령IC",
        "영동IC-황간IC",
        "금강IC-영동IC",
        "옥천IC-금강IC"
      ) ~ "추풍령",
      
      # 대전
      cz_name %in% c(
        "비룡JC-옥천IC",
        "대전IC-비룡JC",
        "회덕JC-대전IC",
        "신탄진IC-회덕JC"
      ) ~ "대전",
      
      # 청주
      cz_name %in% c(
        "남청주IC-신탄진IC",
        "청주JC-남청주IC",
        "남이JC-청주JC",
        "청주IC-남이JC",
        "옥산IC-청주IC",
        "옥산JC-옥산IC"
      ) ~ "청주",
      
      # 천안
      cz_name %in% c(
        "목천IC-옥산JC",
        "천안JC-목천IC",
        "천안IC-천안JC",
        "북천안IC-천안IC"
      ) ~ "천안",
      
      # 수원
      cz_name %in% c(
        "안성IC-북천안IC",
        "안성JC-안성IC",
        "남사진위IC-안성JC",
        "오산IC-남사진위IC",
        "동탄JC-오산IC",
        "기흥동탄IC-동탄JC",
        "기흥IC-기흥동탄IC",
        "수원신갈IC-기흥IC",
        "신갈JC-수원신갈IC",
        "서울TG-신갈JC",
        "판교IC-서울TG",
        "판교JC-판교IC",
        "대왕판교IC-판교JC"
      ) ~ "수원",
      
      # 서울
      cz_name %in% c(
        "양재IC-금토JC"
      ) ~ "서울",
      
      TRUE ~ NA_character_
    ),
    weather_station = trimws(weather_station)
  )


# 매핑 확인
table(vds$weather_station)

sum(is.na(vds$weather_station))

unique(vds$cz_name[is.na(vds$weather_station)])


# VDS에 필요한 weather_station이 weather에 모두 있는지 확인
setdiff(
  unique(vds$weather_station),
  unique(weather$weather_station)
)


# 11. VDS + ASOS 병합
data_merged <- vds %>%
  left_join(
    weather,
    by = c("datetime_hour", "weather_station")
  )


# 12. 병합 결과 확인
dim(data_merged)

head(data_merged)

colSums(is.na(data_merged))

table(data_merged$weather_station)

unique(data_merged$cz_name)

unique(data_merged$지점명)


# 병합 실패 행 확인
data_merged %>%
  filter(is.na(지점명)) %>%
  count(weather_station, cz_name, sort = TRUE)


# 13. 1차 데이터 조인 결과 저장
write.csv(
  data_merged,
  file.path(base_dir, "data_mart.csv"),
  row.names = FALSE,
  fileEncoding = "CP949"
)

file.exists(
  file.path(base_dir, "data_mart.csv")
)

data_mart_path <- file.path(base_dir, "data_mart.csv")
data_mart <- read.csv(data_mart_path, fileEncoding = "CP949")
str(data_mart)

# <양산상북, 부산 금정 권역이었을 때 결측치 처리 방법- 참고>
# 6. 전처리(결측치,이상치 처리 후 중복치 탐색 및 처리)
# (weather 결측치 처리)
# 강수량 변수의 결측은 습도 등 기상 변수와의 관계를 고려할 때 MAR 가능성이 존재
# 도메인 기반 결측치 처리가 무엇보다 중요하지만 전체 결측 비율이 1.7% 수준으로 매우 낮고, 
# 기상 데이터 특성상 높은 습도 환경에서도 실제 강수가 발생하지 않는 경우가 존재해 이벤트성 변수 특성을 고려해 무강수 상태로 판단해 0으로 대체
# (풍향, 풍속 결측치 처리)
# 결측치가 1개이고 변동이 크기 때문에 이상치 영향 줄일 수 있는 중앙값으로 단순 대체
# (기온 결측치 대체)
# 시간에 따라 연속적으로 변화하는 시계열 데이터 특성이 강하기 때문에 결측 비율이 적은 
# 것과 상관없이 linear interpolation 수행
# 결측 비율 소수이므로 결측치 대체 기법에 대한 모델 성능 비교는 생략

