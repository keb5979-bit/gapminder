# clean.R
# Gapminder 데이터 품질 점검 스크립트
# 사용법: Rscript clean.R

# ---- 0. 설정 ----------------------------------------------------------------
input_path <- file.path("data", "gapminder.csv")

cat("==============================================\n")
cat(" Gapminder 데이터 품질 점검\n")
cat("==============================================\n\n")

if (!file.exists(input_path)) {
  stop(sprintf("파일을 찾을 수 없습니다: %s", input_path))
}

# ---- 1. 데이터 로드 ---------------------------------------------------------
df <- read.csv(input_path, stringsAsFactors = FALSE, encoding = "UTF-8")

cat("[1] 기본 정보\n")
cat(sprintf("  - 행 수      : %d\n", nrow(df)))
cat(sprintf("  - 열 수      : %d\n", ncol(df)))
cat(sprintf("  - 컬럼       : %s\n\n", paste(names(df), collapse = ", ")))

# ---- 2. 컬럼별 데이터 타입 --------------------------------------------------
cat("[2] 컬럼별 데이터 타입\n")
for (col in names(df)) {
  cat(sprintf("  - %-10s : %s\n", col, class(df[[col]])))
}
cat("\n")

# ---- 3. 결측치(NA) 점검 -----------------------------------------------------
cat("[3] 결측치(NA) 개수\n")
na_counts <- sapply(df, function(x) sum(is.na(x)))
for (col in names(na_counts)) {
  cat(sprintf("  - %-10s : %d\n", col, na_counts[[col]]))
}
cat(sprintf("  => 전체 결측치 합계: %d\n\n", sum(na_counts)))

# ---- 4. 빈 문자열 점검 (문자형 컬럼) ----------------------------------------
cat("[4] 빈 문자열('') 개수 (문자형 컬럼)\n")
char_cols <- names(df)[sapply(df, is.character)]
if (length(char_cols) == 0) {
  cat("  - 문자형 컬럼 없음\n")
} else {
  for (col in char_cols) {
    cat(sprintf("  - %-10s : %d\n", col, sum(trimws(df[[col]]) == "")))
  }
}
cat("\n")

# ---- 5. 중복 행 점검 --------------------------------------------------------
cat("[5] 중복 점검\n")
cat(sprintf("  - 완전 중복 행          : %d\n", sum(duplicated(df))))
if (all(c("country", "year") %in% names(df))) {
  key_dup <- sum(duplicated(df[, c("country", "year")]))
  cat(sprintf("  - (country, year) 중복  : %d\n", key_dup))
}
cat("\n")

# ---- 6. 수치형 컬럼 요약 및 이상치 ------------------------------------------
cat("[6] 수치형 컬럼 요약 통계\n")
num_cols <- names(df)[sapply(df, is.numeric)]
for (col in num_cols) {
  v <- df[[col]]
  cat(sprintf("  [%s]\n", col))
  cat(sprintf("    min=%.3f  median=%.3f  mean=%.3f  max=%.3f\n",
              min(v, na.rm = TRUE), median(v, na.rm = TRUE),
              mean(v, na.rm = TRUE), max(v, na.rm = TRUE)))
  # 음수/0 값 점검 (인구·수명·GDP는 양수여야 함)
  n_nonpos <- sum(v <= 0, na.rm = TRUE)
  if (n_nonpos > 0) {
    cat(sprintf("    !! 0 이하 값 %d개 발견 (확인 필요)\n", n_nonpos))
  }
}
cat("\n")

# ---- 7. 도메인 규칙 점검 ----------------------------------------------------
cat("[7] 도메인 규칙 점검\n")

# 7-1. 기대수명 범위 (0~120 이내가 정상)
if ("lifeExp" %in% names(df)) {
  bad_life <- sum(df$lifeExp < 0 | df$lifeExp > 120, na.rm = TRUE)
  cat(sprintf("  - lifeExp 범위 이탈(<0 또는 >120) : %d\n", bad_life))
}

# 7-2. 연도 범위
if ("year" %in% names(df)) {
  cat(sprintf("  - year 범위                       : %d ~ %d\n",
              min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE)))
  cat(sprintf("  - 고유 연도 수                    : %d\n",
              length(unique(df$year))))
}

# 7-3. 국가/대륙 카디널리티
if ("country" %in% names(df)) {
  cat(sprintf("  - 고유 국가 수                    : %d\n",
              length(unique(df$country))))
}
if ("continent" %in% names(df)) {
  cat(sprintf("  - 고유 대륙                       : %s\n",
              paste(sort(unique(df$continent)), collapse = ", ")))
}

# 7-4. 국가별 관측치 수가 균일한지 (패널 균형 점검)
if (all(c("country", "year") %in% names(df))) {
  per_country <- table(df$country)
  cat(sprintf("  - 국가별 관측치 수 (min~max)      : %d ~ %d\n",
              min(per_country), max(per_country)))
  if (length(unique(per_country)) == 1) {
    cat("    => 모든 국가의 관측치 수 동일 (균형 패널)\n")
  } else {
    cat("    !! 국가별 관측치 수 불균형 (아래 목록 확인)\n")
    unbalanced <- per_country[per_country != as.integer(names(sort(table(per_country), decreasing = TRUE))[1])]
    print(unbalanced)
  }
}
cat("\n")

cat("==============================================\n")
cat(" 점검 완료\n")
cat("==============================================\n")
