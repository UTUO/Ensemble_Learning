
# STEP 1：讀取老師提供的資料
# 第一欄是 ID（原始資料沒有 ID 欄名）
ball <- read.table("ball.txt", header = TRUE)

# STEP 2：確認資料結構
dim(ball)
names(ball)
head(ball)

# STEP 3：刪除 ID、Name、Salary
# select = -c(...) 表示排除指定欄位
ball_clean <- subset(
  ball,
  select = -c(Id, Name, Salary)
)

# STEP 4：確認整理結果
dim(ball_clean)
names(ball_clean)
head(ball_clean)
