
########################################################
# CART 決策樹實作教學
# 主題：分類（Classification）與迴歸（Regression）
#
# 使用資料：
# 1. Iris：預測鳶尾花的品種
# 2. Boston Housing：預測房價
#
# 操作方法：
# 選取一個步驟的程式碼，按 Ctrl + Enter 執行。
########################################################


########################################################
# STEP 1：準備分析環境
########################################################

# 檢查 R 目前會去哪裡尋找套件
.libPaths()

# 老師原本指定的安裝路徑
# 因為每台電腦的 R 版本與安裝位置可能不同，
# 所以先保持註解，不要執行。
# .libPaths("C:/Program Files/R/R-4.4.1/library")


# 安裝套件只需做一次
# 如果之前已經成功安裝，就不需要重複執行。
#
# install.packages(c(
#   "rpart", "rpart.plot", "caTools",
#   "ggplot2", "GGally", "gridExtra", "mlbench"
# ))


# library()：載入已經安裝的套件
# 每次重新啟動 R 時，都需要重新載入。

library(rpart)       # 建立決策樹模型
library(rpart.plot)  # 把決策樹畫出來
library(caTools)     # 切分訓練集與測試集
library(ggplot2)     # 製作統計圖
library(GGally)      # 繪製多變數散佈圖
library(gridExtra)   # 把多張圖排在一起
library(mlbench)     # 提供 Boston Housing 資料


########################################################
# STEP 2：認識 Iris 鳶尾花資料
########################################################

# Iris 是 R 內建的資料集
# 有 150 朵花、4 個數值特徵、3 種品種。

data("iris")

# dim()：查看資料有幾列、幾欄
# 預期結果：150 5
dim(iris)

# summary()：查看每個欄位的基本統計
# 包含最小值、中位數、平均值、最大值等
summary(iris)

# str()：查看資料結構與欄位型態
str(iris)

# head()：查看前 10 筆資料
head(iris, n = 10)


########################################################
# STEP 3：將 Iris 資料視覺化
########################################################

# boxplot()：繪製盒鬚圖
# iris[, -5] 表示保留第 1～4 欄
# 排除第 5 欄的品種名稱 Species

boxplot(
  iris[, -5],
  col = c("red", "blue", "green", "yellow"),
  main = "Boxplot of Iris Data"
)

# ggpairs()：比較四個數值特徵之間的關係
# 顏色表示不同品種
# 可以觀察哪些特徵能區分不同品種

ggpairs(
  iris,
  columns = 1:4,
  aes(color = Species, alpha = 0.7)
) +
  theme_bw() +
  theme(legend.position = "right")


# 接下來製作四張盒鬚圖
# 比較三種鳶尾花的各項身體特徵

# p1：花萼長度
p1 <- ggplot(
  iris,
  aes(x = Species, y = Sepal.Length)
) +
  geom_boxplot(aes(fill = Species)) +
  labs(title = "Sepal Length") +
  theme_bw()

# p2：花萼寬度
p2 <- ggplot(
  iris,
  aes(x = Species, y = Sepal.Width)
) +
  geom_boxplot(aes(fill = Species)) +
  labs(title = "Sepal Width") +
  theme_bw()

# p3：花瓣長度
p3 <- ggplot(
  iris,
  aes(x = Species, y = Petal.Length)
) +
  geom_boxplot(aes(fill = Species)) +
  labs(title = "Petal Length") +
  theme_bw()

# p4：花瓣寬度
p4 <- ggplot(
  iris,
  aes(x = Species, y = Petal.Width)
) +
  geom_boxplot(aes(fill = Species)) +
  labs(title = "Petal Width") +
  theme_bw()


# windows() 會另外開啟 Windows 繪圖視窗
# 目前先註解，改用 RStudio 內建 Plots
# windows()

# 把四張圖排列成 2 × 2
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


########################################################
# STEP 4：建立 Iris 分類決策樹
########################################################

# 這是第一個真正的機器學習模型！
#
# 目標：
# 用花萼、花瓣的尺寸預測花的品種。
#
# Species ~ .
# Species：我們想預測的答案（Y）
# .：使用其他所有欄位作為特徵（X）
#
# rpart()：訓練決策樹
# <-：將訓練完成的模型存入 fit.iris

fit.iris <- rpart(
  Species ~ .,
  data = iris,
  method = "class"
)

# 查看模型的判斷規則
print(fit.iris)

# 畫出決策樹
# extra = 101：顯示節點的分類資訊與比例
rpart.plot(fit.iris, extra = 101)

# 完成後：
# Environment 應該會出現 fit.iris
# Plots 應該會出現一張決策樹。


########################################################
# STEP 5：Boston Housing 房價資料分析
########################################################

# 接下來不再預測「類別」，而是預測「數字」。
#
# Iris：預測花是哪個品種 → 分類問題
# Boston：預測房價是多少 → 迴歸問題


# 載入 Boston Housing 資料
data("BostonHousing", package = "mlbench")

# 查看資料規模
# 預期結果：506 筆、14 欄
dim(BostonHousing)

# 查看統計摘要
summary(BostonHousing)

# 查看欄位結構
str(BostonHousing)

# 查看前 10 筆
head(BostonHousing, n = 10)


# medv：房價的中位數
# 這次要預測的目標 Y
#
# 其他欄位：
# 用來預測房價的特徵 X


# 固定亂數種子，方便重現相同結果
set.seed(123)

# 將資料分成兩組：
# 80%：訓練集，讓模型學習
# 20%：測試集，檢查模型表現
#
# 使用 medv 作為切分依據
split <- sample.split(
  BostonHousing$medv,
  SplitRatio = 0.8
)

# TRUE 的資料分配到訓練集
train_data <- subset(BostonHousing, split == TRUE)

# FALSE 的資料分配到測試集
test_data <- subset(BostonHousing, split == FALSE)

# 查看兩組資料各有幾筆
dim(train_data)
dim(test_data)


########################################################
# STEP 6A：使用線性迴歸預測房價
########################################################

# 先建立一個線性迴歸模型
# 作為後面決策樹的比較基準。
#
# medv ~ .：用其他所有欄位預測房價
# gaussian：假設使用常態誤差模型
# identity：預測值直接對應房價尺度

lm.fit <- glm(
  medv ~ .,
  family = gaussian(link = "identity"),
  data = train_data
)

# 查看模型摘要
summary(lm.fit)

# 使用測試集預測房價
pred.lm.fit <- predict(
  lm.fit,
  newdata = test_data
)

# MSE：均方誤差
# 計算預測值與實際值差距的平方，再取平均
#
# 越小表示預測誤差越小
mse.lm.fit <- mean(
  (pred.lm.fit - test_data$medv)^2
)

# 顯示 MSE
mse.lm.fit

# 將預測值和實際值放在同一張表
data.frame(
  pred.medv = pred.lm.fit,
  true.medv = test_data$medv
)

# 畫出預測房價與實際房價的散佈圖
plot(
  pred.lm.fit,
  test_data$medv,
  xlim = c(0, 50),
  ylim = c(0, 50),
  xlab = "Predicted Values",
  ylab = "Observed Values",
  main = "Linear Regression"
)

# 加上一條紅色虛線
# 若預測完全正確，資料點就會落在這條線上
abline(a = 0, b = 1, col = "red", lty = 2)


########################################################
# STEP 6B：使用決策樹預測房價
########################################################

# 建立迴歸決策樹
#
# 與 Iris 最大的差別：
# Iris 的 Y 是品種（類別）
# Boston 的 Y 是房價（連續數值）
#
# method = "anova"：
# 讓 rpart 使用迴歸樹，而非分類樹

tree.fit <- rpart(
  medv ~ .,
  method = "anova",
  data = train_data
)

# 畫出房價預測決策樹
rpart.plot(tree.fit, extra = 101)

# 查看哪些特徵對房價預測最重要
tree.fit$variable.importance


# 使用測試集預測房價
pred.tree.fit <- predict(
  tree.fit,
  newdata = test_data
)

# 計算決策樹的 MSE
mse.tree.fit <- mean(
  (pred.tree.fit - test_data$medv)^2
)

# 查看 MSE
mse.tree.fit

# 將預測值與實際房價放在一起
data.frame(
  pred.tree.fit = pred.tree.fit,
  true.medv = test_data$medv
)

# 繪製預測值與實際值的散佈圖
plot(
  pred.tree.fit,
  test_data$medv,
  xlim = c(0, 50),
  ylim = c(0, 50),
  xlab = "Predicted Values",
  ylab = "Observed Values",
  main = "Regression Tree"
)

# 紅色虛線代表完全準確的預測
abline(a = 0, b = 1, col = "red", lty = 2)


########################################################
# STEP 6C：比較兩種房價預測模型
# 這是額外補充的教學步驟
########################################################

# 將兩個模型的 MSE 放在同一張表
comparison <- data.frame(
  Model = c("Linear Regression", "Decision Tree"),
  MSE = c(mse.lm.fit, mse.tree.fit)
)

# 查看比較結果
print(comparison)

# 在這一次測試資料中：
# 哪個模型的 MSE 比較小？
# 哪個模型的預測比較接近實際房價？
#
# 注意：
# 一次資料切分的勝負，不代表某模型永遠比較好。
########################################################
