setwd("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋/raw data")
activity <- readRDS("CL101F_CL108F.rds")

colSums(is.na(activity))
library(stringr)
activity$c0116 <- str_replace(activity$c0116, pattern = " ", replacement = "")
activity[activity == ""] <- NA

activity_building <- subset(activity, !is.na(c0116))
colSums(is.na(activity_building))
table(activity_building$c0808)

# 計算時數
library(lubridate)
activity_building$hour <- int_length(interval(ymd_hms(activity_building$c0111),ymd_hms(activity_building$c0112)))/60/60
plot(activity_building$hour)

# 先輸出給家揚
setwd("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋")
library(openxlsx)
write.xlsx(activity_building, file = "activity_v0.xlsx")  

# 24小時內算短期活動（有開始和結束填反的要處理）
activity_building_short <- subset(activity_building, activity_building$hour<=24)
plot(activity_building_short$hour)

# 長期活動
activity_building_long <- subset(activity_building, activity_building$hour>24)
table(activity_building_long$hour)
boxplot(activity_building_long$hour)
summary(activity_building_long$hour)

test <- subset(activity_building_long, activity_building_long$hour>720)
table(test$hour)



# 日期時間留年份
# library(tidyr)
# activity_building$c0111 = substr(activity_building$c0111,1,7)
# activity_building$c0112 = substr(activity_building$c0112,1,7)
# activity_building <- separate(activity_building, c0111, c("c0111y", "c0111m"), "-")
# activity_building <- separate(activity_building, c0112, c("c0112y", "c0112m"), "-")

# 一個月內算短期
# activity_building_short <- subset(activity_building, activity_building$c0111==activity_building$c0112)
# activity_building_long <- subset(activity_building, activity_building$c0111!=activity_building$c0112)

# 2018-2021
activity_building_1821 <- subset(activity_building, activity_building$c0111!="2022")

# 2022
activity_building_22 <- subset(activity_building, !is.na(c0116))


