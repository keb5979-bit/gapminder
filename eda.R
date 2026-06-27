# eda.R
# Gapminder 탐색적 데이터 분석(EDA) 스크립트
# 사용법: Rscript eda.R
# 출력: 콘솔 요약 + document/figures/ 폴더에 PNG 그래프 저장

# ---- 0. 설정 ----------------------------------------------------------------
input_path <- file.path("data", "gapminder.csv")
fig_dir    <- file.path("document", "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

options(scipen = 100)  # 지수표기 방지

section <- function(title) {
  cat("\n==============================================\n")
  cat(" ", title, "\n", sep = "")
  cat("==============================================\n")
}

df <- read.csv(input_path, stringsAsFactors = FALSE, encoding = "UTF-8")

# 대륙별 색상 팔레트
continents <- sort(unique(df$continent))
pal <- setNames(
  c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")[seq_along(continents)],
  continents
)

# ---- 1. 데이터 개요 ---------------------------------------------------------
section("1. 데이터 개요")
cat(sprintf("관측치 %d행 / 변수 %d개\n", nrow(df), ncol(df)))
cat(sprintf("기간: %d ~ %d (%d개 시점)\n",
            min(df$year), max(df$year), length(unique(df$year))))
cat(sprintf("국가 %d개 / 대륙 %d개\n\n", length(unique(df$country)), length(continents)))
print(summary(df[, c("pop", "lifeExp", "gdpPercap")]))

# ---- 2. 변수 분포 (단변량) --------------------------------------------------
section("2. 변수 분포 (왜도 진단)")
skew <- function(x) {
  m <- mean(x); s <- sd(x); mean(((x - m) / s)^3)
}
for (col in c("pop", "lifeExp", "gdpPercap")) {
  cat(sprintf("  - %-10s 왜도(skewness) = %+.2f\n", col, skew(df[[col]])))
}
cat("  (왜도 > 0: 오른쪽 꼬리. pop/gdpPercap는 로그 변환이 유효)\n")

# 분포 히스토그램 저장
png(file.path(fig_dir, "01_distributions.png"), width = 1200, height = 400)
par(mfrow = c(1, 3))
hist(df$lifeExp, breaks = 30, col = "#377EB8", border = "white",
     main = "기대수명 분포", xlab = "lifeExp")
hist(log10(df$pop), breaks = 30, col = "#4DAF4A", border = "white",
     main = "인구 분포 (log10)", xlab = "log10(pop)")
hist(log10(df$gdpPercap), breaks = 30, col = "#E41A1C", border = "white",
     main = "1인당 GDP 분포 (log10)", xlab = "log10(gdpPercap)")
dev.off()
par(mfrow = c(1, 1))

# ---- 3. 상관관계 (다변량) ---------------------------------------------------
section("3. 상관관계")
num <- df[, c("year", "pop", "lifeExp", "gdpPercap")]
num$log_gdp <- log10(df$gdpPercap)
num$log_pop <- log10(df$pop)
cat("Pearson 상관계수 행렬:\n")
print(round(cor(num), 3))
cat(sprintf("\n  핵심: lifeExp ~ log10(gdpPercap) 상관 = %.3f (강한 양의 관계)\n",
            cor(df$lifeExp, log10(df$gdpPercap))))

# 산점도: 기대수명 vs 1인당 GDP (최신 연도)
latest <- max(df$year)
sub <- df[df$year == latest, ]
png(file.path(fig_dir, "02_life_vs_gdp.png"), width = 900, height = 650)
plot(sub$gdpPercap, sub$lifeExp, log = "x",
     pch = 19, col = pal[sub$continent],
     cex = sqrt(sub$pop) / 12000,
     xlab = "1인당 GDP (log scale)", ylab = "기대수명",
     main = sprintf("기대수명 vs 1인당 GDP (%d, 버블=인구)", latest))
legend("bottomright", legend = names(pal), col = pal, pch = 19, bty = "n")
dev.off()

# ---- 4. 시계열 추세 (대륙별 기대수명) ---------------------------------------
section("4. 대륙별 기대수명 추세")
trend <- aggregate(lifeExp ~ year + continent, data = df, FUN = mean)
trend_wide <- reshape(trend, idvar = "year", timevar = "continent",
                      direction = "wide")
names(trend_wide) <- sub("lifeExp\\.", "", names(trend_wide))
cat("대륙별 평균 기대수명 (연도별):\n")
print(round(trend_wide, 1), row.names = FALSE)

png(file.path(fig_dir, "03_lifeexp_trend.png"), width = 900, height = 600)
plot(NA, xlim = range(df$year), ylim = range(df$lifeExp),
     xlab = "연도", ylab = "평균 기대수명",
     main = "대륙별 평균 기대수명 추세 (1952-2007)")
for (cont in continents) {
  d <- trend[trend$continent == cont, ]
  lines(d$year, d$lifeExp, col = pal[cont], lwd = 2.5)
  points(d$year, d$lifeExp, col = pal[cont], pch = 19)
}
legend("bottomright", legend = names(pal), col = pal, lwd = 2.5, bty = "n")
dev.off()

# ---- 5. 대륙별 비교 (최신 연도) ---------------------------------------------
section(sprintf("5. 대륙별 비교 (%d년)", latest))
by_cont <- aggregate(cbind(lifeExp, gdpPercap, pop) ~ continent,
                     data = sub, FUN = median)
by_cont$n_countries <- as.integer(table(sub$continent)[by_cont$continent])
by_cont <- by_cont[order(-by_cont$lifeExp), ]
cat("대륙별 중앙값 (기대수명 내림차순):\n")
print(data.frame(
  continent  = by_cont$continent,
  n          = by_cont$n_countries,
  lifeExp    = round(by_cont$lifeExp, 1),
  gdpPercap  = round(by_cont$gdpPercap, 0),
  pop_median = format(round(by_cont$pop, 0), big.mark = ",")
), row.names = FALSE)

png(file.path(fig_dir, "04_continent_boxplot.png"), width = 900, height = 600)
boxplot(lifeExp ~ continent, data = sub, col = pal[levels(factor(sub$continent))],
        ylab = "기대수명", xlab = "대륙",
        main = sprintf("대륙별 기대수명 분포 (%d)", latest))
dev.off()

# ---- 6. 순위: Top/Bottom 국가 (최신 연도) -----------------------------------
section(sprintf("6. 기대수명 Top/Bottom 5개국 (%d년)", latest))
ord <- sub[order(-sub$lifeExp), c("country", "continent", "lifeExp", "gdpPercap")]
cat("[ Top 5 ]\n")
print(head(ord, 5), row.names = FALSE)
cat("\n[ Bottom 5 ]\n")
print(tail(ord, 5), row.names = FALSE)

# ---- 7. 변화량 분석 (1952 -> 2007) ------------------------------------------
section("7. 기대수명 변화량 (1952 -> 2007)")
y0 <- min(df$year); y1 <- max(df$year)
w0 <- df[df$year == y0, c("country", "lifeExp")]
w1 <- df[df$year == y1, c("country", "lifeExp")]
chg <- merge(w0, w1, by = "country", suffixes = c("_1952", "_2007"))
chg$delta <- chg$lifeExp_2007 - chg$lifeExp_1952
chg <- chg[order(-chg$delta), ]
cat(sprintf("전 세계 평균 기대수명: %.1f세(%d) -> %.1f세(%d), +%.1f세\n\n",
            mean(w0$lifeExp), y0, mean(w1$lifeExp), y1,
            mean(w1$lifeExp) - mean(w0$lifeExp)))
cat("[ 가장 많이 증가한 5개국 ]\n")
print(head(chg[, c("country", "lifeExp_1952", "lifeExp_2007", "delta")], 5),
      row.names = FALSE)
cat("\n[ 가장 적게 증가/감소한 5개국 ]\n")
print(tail(chg[, c("country", "lifeExp_1952", "lifeExp_2007", "delta")], 5),
      row.names = FALSE)

# ---- 완료 -------------------------------------------------------------------
section("EDA 완료")
cat(sprintf("그래프 %d개가 저장되었습니다 -> %s/\n",
            length(list.files(fig_dir, pattern = "\\.png$")), fig_dir))
cat(paste0("  - ", list.files(fig_dir, pattern = "\\.png$"), collapse = "\n"), "\n")
