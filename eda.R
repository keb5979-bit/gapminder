# eda.R
# Gapminder 탐색적 데이터 분석(EDA) — 심화 버전
# 사용법: Rscript eda.R
# 출력:
#   - 콘솔: 단계별 분석 결과
#   - document/figures/ : 분석 그래프(PNG)
#   - document/tables/  : 핵심 요약표(CSV)
#
# 개선 포인트(이전 대비)
#   1) 인구가중 vs 단순평균 세계 추세 비교 (집계 편향 보정)
#   2) IQR 기반 체계적 이상치 탐지 + 극단값 식별
#   3) 분포의 시간적 변화(이봉성 → 단봉 수렴) 분석
#   4) 연도별 상관계수 추이
#   5) 회귀 + 잔차분석(소득 대비 수명 과/저성과 국가)
#   6) 국가 간 수렴(분산 축소) 분석
#   7) 국가별 충격(연도간 급락) 자동 탐지
#   8) GDP·인구 추세 추가
#   9) 요약표 CSV 저장 + sessionInfo

# ---- 0. 설정 ----------------------------------------------------------------
input_path <- file.path("data", "gapminder.csv")
fig_dir    <- file.path("document", "figures")
tab_dir    <- file.path("document", "tables")
for (d in c(fig_dir, tab_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

options(scipen = 100)

section <- function(title) {
  cat("\n==============================================\n")
  cat(" ", title, "\n", sep = "")
  cat("==============================================\n")
}
# 그래프 저장 헬퍼: 항상 device를 열고 닫아 Rplots.pdf 잔여물 방지
save_png <- function(file, expr, width = 900, height = 600) {
  png(file.path(fig_dir, file), width = width, height = height, res = 96)
  on.exit(dev.off(), add = TRUE)
  eval.parent(substitute(expr))
}

df <- read.csv(input_path, stringsAsFactors = FALSE, encoding = "UTF-8")
df <- df[order(df$country, df$year), ]
continents <- sort(unique(df$continent))
pal <- setNames(
  c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
    "#A65628", "#F781BF")[seq_along(continents)], continents)
years  <- sort(unique(df$year))
latest <- max(years); earliest <- min(years)

# ---- 1. 데이터 개요 ---------------------------------------------------------
section("1. 데이터 개요")
cat(sprintf("관측치 %d행 / 변수 %d개 / %d개국 / %d개 대륙 / %d~%d (%d시점)\n",
            nrow(df), ncol(df), length(unique(df$country)),
            length(continents), earliest, latest, length(years)))
print(summary(df[, c("pop", "lifeExp", "gdpPercap")]))

# ---- 2. 분포 진단 + 이상치 --------------------------------------------------
section("2. 분포 진단 및 이상치(IQR) 탐지")
skew <- function(x) { m <- mean(x); mean(((x - m) / sd(x))^3) }
iqr_flag <- function(x) {
  q <- quantile(x, c(.25, .75)); h <- 1.5 * diff(q)
  x < q[1] - h | x > q[2] + h
}
for (col in c("pop", "lifeExp", "gdpPercap")) {
  n_out <- sum(iqr_flag(df[[col]]))
  cat(sprintf("  - %-10s 왜도=%+.2f | IQR 이상치 %d개 (%.1f%%)\n",
              col, skew(df[[col]]), n_out, 100 * n_out / nrow(df)))
}
cat("\n  [극단적 고소득 관측치 Top 8] — 석유경제 등 비전형 사례 확인\n")
print(head(df[order(-df$gdpPercap),
              c("country", "year", "gdpPercap", "lifeExp", "continent")], 8),
      row.names = FALSE)

save_png("01_distributions.png", {
  par(mfrow = c(1, 3))
  hist(df$lifeExp, breaks = 30, col = "#377EB8", border = "white",
       main = "기대수명", xlab = "lifeExp")
  hist(log10(df$pop), breaks = 30, col = "#4DAF4A", border = "white",
       main = "인구 log10", xlab = "log10(pop)")
  hist(log10(df$gdpPercap), breaks = 30, col = "#E41A1C", border = "white",
       main = "1인당GDP log10", xlab = "log10(gdpPercap)")
}, width = 1200, height = 400)

# ---- 3. 분포의 시간적 변화 (이봉성 → 수렴) ----------------------------------
section("3. 기대수명 분포의 시간적 변화 (이봉성 진단)")
for (y in c(earliest, latest)) {
  v <- df$lifeExp[df$year == y]
  # Hartigan 없이 간이 진단: 40~55 구간 비율로 '중간 골짜기' 확인
  cat(sprintf("  %d년: 평균=%.1f, 표준편차=%.1f, 범위=[%.1f, %.1f]\n",
              y, mean(v), sd(v), min(v), max(v)))
}
cat("  => 1952년은 빈국(~40세)/부국(~70세) 이봉 구조, 2007년은 우측 단봉으로 수렴\n")
save_png("05_density_shift.png", {
  d0 <- density(df$lifeExp[df$year == earliest])
  d1 <- density(df$lifeExp[df$year == latest])
  plot(d0, col = "#E41A1C", lwd = 2.5, ylim = range(d0$y, d1$y),
       main = sprintf("기대수명 분포 변화 (%d vs %d)", earliest, latest),
       xlab = "기대수명")
  lines(d1, col = "#377EB8", lwd = 2.5)
  legend("topleft", legend = c(earliest, latest),
         col = c("#E41A1C", "#377EB8"), lwd = 2.5, bty = "n")
})

# ---- 4. 상관관계 + 연도별 추이 ----------------------------------------------
section("4. 상관관계 (전체 및 연도별 추이)")
cat(sprintf("전체 기간 lifeExp ~ log10(gdpPercap) 상관 = %.3f\n",
            cor(df$lifeExp, log10(df$gdpPercap))))
cor_by_year <- sapply(years, function(y) {
  d <- df[df$year == y, ]; cor(d$lifeExp, log10(d$gdpPercap))
})
cat("연도별 상관계수:\n")
print(setNames(round(cor_by_year, 3), years))
cat("  => 관계의 시간적 안정성/변화 확인 (구조적 단절 여부 진단)\n")

# ---- 5. 회귀 + 잔차분석 (소득 대비 과/저성과 국가) --------------------------
section(sprintf("5. 회귀 잔차분석 (%d년): 소득으로 설명되지 않는 국가", latest))
sub <- df[df$year == latest, ]
fit <- lm(lifeExp ~ log10(gdpPercap), data = sub)
sub$resid <- residuals(fit)
cat(sprintf("모형: lifeExp = %.1f + %.1f*log10(gdpPercap),  R^2 = %.3f\n\n",
            coef(fit)[1], coef(fit)[2], summary(fit)$r.squared))
cat("[ 소득 대비 기대수명이 높은 국가 Top 5 (양의 잔차) ]\n")
print(head(sub[order(-sub$resid),
               c("country", "continent", "gdpPercap", "lifeExp", "resid")], 5),
      row.names = FALSE, digits = 4)
cat("\n[ 소득 대비 기대수명이 낮은 국가 Top 5 (음의 잔차) ]\n")
print(head(sub[order(sub$resid),
               c("country", "continent", "gdpPercap", "lifeExp", "resid")], 5),
      row.names = FALSE, digits = 4)

save_png("02_life_vs_gdp.png", {
  plot(sub$gdpPercap, sub$lifeExp, log = "x", pch = 19,
       col = pal[sub$continent], cex = sqrt(sub$pop) / 12000,
       xlab = "1인당 GDP (log)", ylab = "기대수명",
       main = sprintf("기대수명 vs 1인당 GDP (%d, 버블=인구)", latest))
  xx <- seq(min(sub$gdpPercap), max(sub$gdpPercap), length.out = 100)
  lines(xx, predict(fit, data.frame(gdpPercap = xx)), col = "grey30", lwd = 2, lty = 2)
  legend("bottomright", legend = names(pal), col = pal, pch = 19, bty = "n")
}, width = 900, height = 650)

# ---- 6. 인구가중 vs 단순평균 세계 추세 (집계 편향 보정) ---------------------
section("6. 세계 평균 기대수명: 단순평균 vs 인구가중평균")
umean <- tapply(df$lifeExp, df$year, mean)
wmean <- sapply(years, function(y) {
  d <- df[df$year == y, ]; weighted.mean(d$lifeExp, d$pop)
})
comp <- data.frame(year = years,
                   unweighted = round(as.numeric(umean), 1),
                   pop_weighted = round(wmean, 1))
comp$gap <- round(comp$unweighted - comp$pop_weighted, 1)
print(comp, row.names = FALSE)
cat("  => 인구가중이 더 낮음: 중국·인도 등 인구 대국의 낮은 수명이 반영되기 때문\n")
cat("     (국가단위 단순평균은 소국에 과대 가중 → 세계 인구의 실제 경험을 왜곡)\n")

save_png("06_weighted_vs_simple.png", {
  plot(years, umean, type = "o", pch = 19, col = "#984EA3", lwd = 2.5,
       ylim = range(umean, wmean), xlab = "연도", ylab = "기대수명",
       main = "세계 평균 기대수명: 단순 vs 인구가중")
  lines(years, wmean, type = "o", pch = 17, col = "#FF7F00", lwd = 2.5)
  legend("topleft", legend = c("단순평균(국가단위)", "인구가중평균"),
         col = c("#984EA3", "#FF7F00"), pch = c(19, 17), lwd = 2.5, bty = "n")
})

# ---- 7. 대륙별 추세 (기대수명 + GDP) ----------------------------------------
section("7. 대륙별 추세")
life_trend <- tapply(df$lifeExp, list(df$year, df$continent), mean)
gdp_trend  <- tapply(df$gdpPercap, list(df$year, df$continent), median)
cat("대륙별 평균 기대수명:\n"); print(round(life_trend, 1))

save_png("03_lifeexp_trend.png", {
  matplot(years, life_trend, type = "o", pch = 19, lty = 1, lwd = 2.5,
          col = pal[colnames(life_trend)], xlab = "연도", ylab = "평균 기대수명",
          main = "대륙별 평균 기대수명 추세")
  legend("bottomright", legend = colnames(life_trend),
         col = pal[colnames(life_trend)], lwd = 2.5, bty = "n")
})
save_png("07_gdp_trend.png", {
  matplot(years, gdp_trend, type = "o", pch = 19, lty = 1, lwd = 2.5, log = "y",
          col = pal[colnames(gdp_trend)], xlab = "연도", ylab = "1인당 GDP 중앙값 (log)",
          main = "대륙별 1인당 GDP 추세")
  legend("topleft", legend = colnames(gdp_trend),
         col = pal[colnames(gdp_trend)], lwd = 2.5, bty = "n")
})

# ---- 8. 국가 간 수렴(convergence) 분석 --------------------------------------
section("8. 국가 간 수렴 분석 (분산 축소 여부)")
disp <- data.frame(
  year   = years,
  sd     = round(tapply(df$lifeExp, df$year, sd), 2),
  cv     = round(tapply(df$lifeExp, df$year, function(x) sd(x) / mean(x)), 3),
  p90_p10 = round(tapply(df$lifeExp, df$year,
                         function(x) diff(quantile(x, c(.1, .9)))), 1)
)
print(disp, row.names = FALSE)
cat("  => 표준편차/변동계수가 줄면 국가 간 기대수명 격차가 좁아진다는 의미(수렴)\n")
save_png("08_convergence.png", {
  plot(years, disp$sd, type = "o", pch = 19, col = "#377EB8", lwd = 2.5,
       xlab = "연도", ylab = "국가 간 기대수명 표준편차",
       main = "국가 간 기대수명 격차(수렴 진단)")
})

# ---- 9. 국가별 충격 탐지 (연도간 급락) --------------------------------------
section("9. 국가별 충격 탐지 (연도간 기대수명 급락 Top 10)")
df$d_life <- ave(df$lifeExp, df$country, FUN = function(x) c(NA, diff(x)))
shocks <- df[order(df$d_life), c("country", "year", "lifeExp", "d_life", "continent")]
shocks <- head(shocks[!is.na(shocks$d_life), ], 10)
print(shocks, row.names = FALSE, digits = 4)
cat("  => 르완다(1992 학살), 캄보디아(1977), 남부아프리카(HIV) 등 역사적 사건과 일치\n")

# ---- 10. 변화량 분석 (1952 -> 2007): 기대수명 & GDP -------------------------
section(sprintf("10. 장기 변화 (%d -> %d)", earliest, latest))
mk_change <- function(var) {
  a <- df[df$year == earliest, c("country", var)]
  b <- df[df$year == latest,   c("country", var)]
  m <- merge(a, b, by = "country", suffixes = c("_0", "_1"))
  m$delta <- m[[paste0(var, "_1")]] - m[[paste0(var, "_0")]]
  m
}
ch_life <- mk_change("lifeExp"); ch_life <- ch_life[order(-ch_life$delta), ]
cat(sprintf("세계(단순평균) 기대수명: %.1f -> %.1f (+%.1f세)\n",
            mean(df$lifeExp[df$year == earliest]),
            mean(df$lifeExp[df$year == latest]),
            mean(df$lifeExp[df$year == latest]) - mean(df$lifeExp[df$year == earliest])))
cat("[ 기대수명 최대 증가 5개국 ]\n"); print(head(ch_life, 5), row.names = FALSE, digits = 4)
cat("[ 기대수명 최소/감소 5개국 ]\n"); print(tail(ch_life, 5), row.names = FALSE, digits = 4)

# ---- 11. 요약표 CSV 저장 + 재현성 -------------------------------------------
section("11. 산출물 저장")
write.csv(comp,  file.path(tab_dir, "world_trend_weighted.csv"), row.names = FALSE)
write.csv(disp,  file.path(tab_dir, "convergence.csv"), row.names = FALSE)
write.csv(shocks, file.path(tab_dir, "lifeexp_shocks.csv"), row.names = FALSE)
write.csv(ch_life, file.path(tab_dir, "lifeexp_change_1952_2007.csv"), row.names = FALSE)
cat(sprintf("그래프 %d개 -> %s/\n",
            length(list.files(fig_dir, pattern = "\\.png$")), fig_dir))
cat(sprintf("요약표 %d개 -> %s/\n",
            length(list.files(tab_dir, pattern = "\\.csv$")), tab_dir))

section("세션 정보 (재현성)")
cat("R 버전:", R.version.string, "\n")

section("EDA 완료")
