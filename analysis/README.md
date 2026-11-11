# R 분석 코드

저장소 루트를 작업 폴더로 사용하고 입력·중간 파일은 `data/` 또는 `TRAFFIC_PROJECT_DATA_DIR` 지정 위치에 놓습니다. 연구용 탐색·검증 구간이 섞여 있어 전체 파일을 한 번에 source하는 자동 파이프라인은 아닙니다.

| 단계 | 파일 | 내용 |
| --- | --- | --- |
| 01 | `01_join_traffic_weather.R` | VDS·ASOS 전처리와 시간·관측소 기준 통합 |
| 02 | `02_road_curvature.R` | 팀원이 구축한 SHP 기반 굴곡도 산출 코드 |
| 03 | `03_build_features.R` | 도로구조·시설물 결합, 시공간·교통류 파생변수 |
| 04 | `04_accident_vehicle_eda.R` | 사고 차종 보조 EDA 테이블 |
| 05 | `05_hypothesis_eda.R` | 가설별 집계와 대시보드용 CSV |
| 06 | `06_hypothesis_and_feature_selection.R` | 통계검정, 모델링 표본 생성, XGBoost 중요도 |
| 07 | `07_modeling_and_shap.R` | 5개 모델 비교, RF·LGBM 튜닝, SHAP |
| 08 | `08_compare_feature_sets.R` | 표본을 다시 불러와 교통류·통합 구성 비교 |

## 실행 조건

- 01: VDS 원본과 ASOS 원본 필요. 분석 당시 CSV는 CP949 입력을 사용했습니다.
- 02: 원본 SHP 및 IC 위치자료 필요. SHP 폴더는 `data/geometry`로 지정했습니다. 제공 폴더에 SHP는 없어 별도 준비가 필요합니다.
- 03: 01의 `data_mart.csv`, 굴곡도 CSV, 교량·터널 XLSX 필요. 메모리·디스크 사용량이 크며 마지막에 `_v2.csv`를 생성합니다.
- 04: 차종 엑셀의 시간 처리 보완 구간이 포함됩니다. 뒤쪽 시간변수 확인 코드는 별도 탐색 구간입니다.
- 05: 최종 `_v2.csv`에서 필요한 컬럼을 청크 단위로 집계합니다.
- 06: 앞부분은 전체 데이터 기반 검정, 뒷부분은 표본 생성·변수 선택입니다. `model_sample_data.csv`는 `data/model_outputs/`에 생성합니다.
- 07: `rm(list=ls())`로 시작하는 여러 독립 구간이 있습니다. 초기 비교, CV·최종 평가, SHAP, 그림 생성 구간을 구분해 실행합니다.
- 08: 기존 세션의 train/test에 의존하지 않도록 초기화 코드를 보완했습니다. 수정 후 재학습은 수행하지 않았습니다.

결과는 로컬 `data/model_outputs`·`data/eda_outputs`에 생성됩니다. 저장소 `results/`는 분석 당시 선별한 고정 결과입니다. 패키지 버전 기록이 없어 설치되는 최신 버전과 원본 API 사이의 차이가 생길 수 있습니다.
