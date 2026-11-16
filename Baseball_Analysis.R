
##################################################
# Baseball Data Analysis
# 棒球球員資料分析
##################################################

# STEP 1：讀取資料
# ball.txt 必須放在目前 RStudio 專案資料夾
ball <- read.table("ball.txt", header = TRUE)

# STEP 2：探索原始資料
dim(ball)       # 檢查資料筆數與欄位數
names(ball)     # 查看所有欄位名稱
head(ball)      # 查看前 6 筆資料
str(ball)       # 查看資料型態
summary(ball)   # 查看統計摘要

# STEP 3：移除老師指定的三個欄位
# Id：球員編號
# Name：球員姓名
# Salary：原始薪資
ball_clean <- subset(
  ball,
  select = -c(Id, Name, Salary)
)

# STEP 4：檢查整理結果
dim(ball_clean)      # 預期 263 筆、23 欄
names(ball_clean)
head(ball_clean)

# STEP 5：檢查缺失值
colSums(is.na(ball_clean))

# STEP 6：確認目標變數
# Logsalary 是薪資的自然對數
# 老師要求移除 Salary，但保留 Logsalary
summary(ball_clean$Logsalary)
hist(
  ball_clean$Logsalary,
  main = "Distribution of Log Salary",
  xlab = "Log Salary",
  col = "lightblue"
)
