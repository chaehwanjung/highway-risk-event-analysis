# 원본 파일과 정리 내역

- `1. 1차 데이터 조인.R` → `analysis/01_join_traffic_weather.R`
- `2. 도로선형 굴곡도 산출(curvature_by_km).R` → `analysis/02_road_curvature.R`
- `3. 2차 데이터 조인 & 피처 엔지니어링.R` → `analysis/03_build_features.R`
- `4. 사고 차량종류 EDA용 보조테이블 생성.R` → `analysis/04_accident_vehicle_eda.R`
- `5. EDA.R` → `analysis/05_hypothesis_eda.R`
- `6. 가설검정 & 변수 중요도 확인.R` → `analysis/06_hypothesis_and_feature_selection.R`
- `7. 최종 모델링.R` → `analysis/07_modeling_and_shap.R`
- `8. 교통류 변수와 예측 성능 비교.R` → `analysis/08_compare_feature_sets.R`

- 연구 기준 자료: `빅데이터 팀플 3-1 0609 최종.pdf` 33쪽, `프로젝트 요약.pdf` 28쪽
- 대시보드 이미지: 최종 발표 18쪽에서 추출
- 모델 결과: `model_outputs`의 CSV·PNG에서 선별
- 원본 발표 PDF는 1·33쪽 학번을 제거한 공개본으로 포함. 두 쪽을 래스터화해 학번이 이미지 추출로 복원되지 않도록 처리
- 학생번호가 포함된 과제·성찰일지, 수업자료·참고 논문, R 세션·복구파일, 원본·행 단위 데이터는 제외
- 개인 절대 경로를 `TRAFFIC_PROJECT_DATA_DIR` 환경변수 기반 경로로 변경
- EDA 입력 파일명을 실제 최종 데이터마트의 `_v2` 이름으로 통일
- 08 비교 코드에 샘플 로드·결측 처리·층화 70:30 분할을 추가해 이전 세션의 객체 의존성 해소. 원본에서 필요한 `occ`가 최종 15개 변수 객체에는 없어 발생하는 참조 문제를 수정
- 분석 로직·기존 저장 결과는 보존. 현재 환경에서 모델 재학습하지 않음
- 도로기하구조 데이터 구축은 팀원 담당이며, 02번 코드는 팀 제공 작업으로 구분
