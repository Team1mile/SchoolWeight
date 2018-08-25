# 必要なライブラリの宣言
library(moments)
library(SuppDists)

# 初期定義
new_year <- NULL
new_age <- NULL
new_sex <- NULL
new_weight <- NULL
new_actCount <- NULL
new_theoCount <- NULL
new_diffCount <- NULL

# 体重明細データ読み込み
hoken_data <- read.csv(file = "Hoken_data.csv", header = TRUE, sep = ",")

# 年度、性別、年齢別の平均・標準偏差データの読み込み
hoken_meansd <- read.csv(file="Hoken_meansd.csv", header = TRUE, sep = ",")

# どの年度、性別、年齢でデータ統計処理を試すかの定義
current_year <- 2016
current_sex <- "Male"
current_age <- 5

# 該当の年度、性別、年齢のデータのみを抽出
hoken_data_year <- hoken_data[hoken_data$Year==current_year,]
hoken_data_year_age <- hoken_data_year[hoken_data_year$Age==current_age,]
hoken_data_year_age_sex <- hoken_data_year_age[hoken_data_year_age$Sex==current_sex,]

# 体重別の度数データから一件別のList型データに展開(R関数が使いやすいように)
i <- 1
x <- NULL
while (i <= nrow(hoken_data_year_age_sex)){
  x <- c(x, rep(hoken_data_year_age_sex[i,]$Weight, hoken_data_year_age_sex[i,]$Count * 10))
  i <- i + 1
}

# 尖度・歪度を求める
cur_kur <- kurtosis(x)
cur_skw <- skewness(x)

# CSVから読み込んだ平均・標準偏差データから該当の年度・性別・年齢のデータを抽出
hoken_meansd_year <- hoken_meansd[hoken_meansd$Year==current_year,]
hoken_meansd_year_age <- hoken_meansd_year[hoken_meansd_year$Age==current_age,]
hoken_meansd_year_age_sex <- hoken_meansd_year_age[hoken_meansd_year_age$Sex==current_sex,]
cur_sd <- hoken_meansd_year_age_sex$SD
cur_var <- cur_sd * cur_sd
cur_mean <- hoken_meansd_year_age_sex$Mean


# ジョンソンSU分布への適用結果を補完するための準備
i <- 10
y <- NULL
sum_y <- 0

# ジョンソンSU分布への適用を試みる
parms <- JohnsonFit(x)
while (i <= 125){
  # ジョンソンSU分布を適用した時の度数理論値を計算
  # (累積分布関数のi+0.5の値からi-0.5の値を引いて出た値を採用)
  tmp_count <- pJohnson(i+0.5, parms) - pJohnson(i-0.5, parms)
  tmp_count <- round(tmp_count * sum(hoken_data_year_age_sex$Count) * 10)
  
  # ジョンソンSU分布適用時のヒストグラムを作成するためのList型データ作成
  if (tmp_count > 0){
    y <- c(y, rep(i, tmp_count))
    sum_y <- sum_y + tmp_count
  }
  
  # 度数の理論値を補完するためのデータフレームを作るためのList型データ準備
  new_year <- c(new_year, c(current_year))
  new_age <- c(new_age, c(current_age))
  new_sex <- c(new_sex, c(current_sex))
  new_weight <- c(new_weight, c(i))
  new_theoCount <- c(new_theoCount, c(tmp_count))
  
  hoken_data_year_age_sex_weight <- hoken_data_year_age_sex[hoken_data_year_age_sex$Weight==i,]
  if (is.na(hoken_data_year_age_sex_weight[1,]$Count)) {
    origCount <- 0
  }
  else {
    origCount <- hoken_data_year_age_sex_weight[1,]$Count * 10
  }
  new_actCount <- c(new_actCount, c(origCount))

    
  if (nrow(hoken_data_year_age_sex_weight) > 0){
    new_diffCount = c(new_diffCount, c(abs(origCount - tmp_count)))
  }
  else {
    new_diffCount = c(new_diffCount, c(origCount))
  }

  i <- i + 1
}

sum_x <- sum(hoken_data_year_age_sex$Count) * 10

# 計算した理論値及びそれと実測値の差をデータフレームに保管
hoken_theo_data <- data.frame(Year = new_year, Age = new_age, Sex = new_sex, Weight = new_weight, Act_count = new_actCount, Theo_count = new_theoCount, Diff_count = new_diffCount)
