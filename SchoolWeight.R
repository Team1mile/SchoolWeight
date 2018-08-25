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

new_m_year <- NULL
new_m_age <- NULL
new_m_sex <- NULL
new_m_mean <- NULL
new_m_sd <- NULL
new_m_m3 <- NULL
new_m_m4 <- NULL

# 体重明細データ読み込み
hoken_data <- read.csv(file = "Hoken_data.csv", header = TRUE, sep = ",")

# 年度、性別、年齢別の平均・標準偏差データの読み込み
hoken_meansd <- read.csv(file="Hoken_meansd.csv", header = TRUE, sep = ",")

# どの年度、性別、年齢でデータ統計処理を試すかの定義
current_sex <- "Female"

# どの年度から処理を始めるかを定義してループを回す
current_year <- 1983
while (current_year <= 2016){

  # どの年齢から処理を始めるかを定義してループを回す
  current_age <- 5

  while (current_age <= 17){
    # どの性別の番号から処理を始めるかを定義してループを回す
    # 0だとMale, 1だとFemale, それ以外はNone
    current_snum <- 0

    while (current_snum <= 1) {

      # 0->"Male", 1->"Female"に置き換え
      if (current_snum == 0) {
        current_sex <- "Male"
      } else {
        if (current_snum == 1){
          current_sex <- "Female"
        } else {
          current_sex <- "None"
        }
      }

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

      # 平均、標準偏差、歪度、尖度のファイル出力用に保管
      new_m_year <- c(new_m_year, c(current_year))
      new_m_age <- c(new_m_age, c(current_age))
      new_m_sex <- c(new_m_sex, c(current_sex))
      new_m_mean <- c(new_m_mean, c(cur_mean))
      new_m_sd <- c(new_m_sd, c(cur_sd))
      new_m_m3 <- c(new_m_m3, c(cur_skw))
      new_m_m4 <- c(new_m_m4, c(cur_kur))
      
      # ジョンソンSU分布への適用結果を補完するための準備
      i <- 11
      y <- NULL
      sum_y <- 0
      
      # ジョンソンSU分布への適用を試みる
      parms <- JohnsonFit(x)
      while (i <= 125){
        # ジョンソンSU分布を適用した時の度数理論値を計算
        # (累積分布関数のi+0.5の値からi-0.5の値を引いて出た値を採用)
        low_val <- try(pJohnson(i-0.5, parms))
        if (class(low_val) == "try-error"){
          if (i-0.5 <= cur_mean){
            low_val <- 0
          } else {
            low_val <- 1
          }
        } else {
          if (is.nan(low_val)){
            low_val <- 0
          }
        }
        high_val <- try(pJohnson(i+0.5, parms))
        if (class(high_val) == "try-error"){
          if (i+0.5 <= cur_mean){
            high_val <- 0
          } else {
            high_val <- 1
          }
        } else {
          if (is.nan(high_val)){
            high_val <- 0
          }
        }
        tmp_count <- high_val - low_val
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
        new_theoCount <- c(new_theoCount, c(tmp_count*0.1))
        
        hoken_data_year_age_sex_weight <- hoken_data_year_age_sex[hoken_data_year_age_sex$Weight==i,]
        if (is.na(hoken_data_year_age_sex_weight[1,]$Count)) {
          origCount <- 0
        }
        else {
          origCount <- hoken_data_year_age_sex_weight[1,]$Count * 10
        }
        new_actCount <- c(new_actCount, c(origCount*0.1))      
          
        if (nrow(hoken_data_year_age_sex_weight) > 0){
          new_diffCount = c(new_diffCount, c(abs(origCount - tmp_count)*0.1))
        }
        else {
          new_diffCount = c(new_diffCount, c(origCount*0.1))
        }
      
        i <- i + 1
      }
      
      sum_x <- sum(hoken_data_year_age_sex$Count) * 10

      # 性別番号を１進める
      current_snum <- current_snum + 1
    }

    # 年齢を１進める
    current_age <- current_age + 1
  }

  # 年度を１進める
  current_year <- current_year + 1
}

# 計算した理論値及びそれと実測値の差をデータフレームに保管し、ファイルに出力
hoken_theo_data <- data.frame(Year = new_year, Age = new_age, Sex = new_sex, Weight = new_weight, Act_count = new_actCount, Theo_count = new_theoCount, Diff_count = new_diffCount)
write.csv(hoken_theo_data, "Hoken_theo_diff.csv", quote=F)

# 平均、標準偏差、歪度、尖度のデータもファイル出力
hoken_moment_data <- data.frame(Year = new_m_year, Age = new_m_age, Sex = new_m_sex, Mean = new_m_mean, SD = new_m_sd, M3 = new_m_m3, M4 = new_m_m4)
write.csv(hoken_moment_data, "Hoken_moment_data.csv", quote=F)