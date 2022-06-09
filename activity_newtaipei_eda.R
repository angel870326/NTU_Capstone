setwd("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋/社區活動")
library(openxlsx)
# activity_nt <- read.xlsx("Activity_NewTaipei.xlsx")

# 資料清理
# library(stringr)
# activity_nt$c0103 <- str_replace(activity_nt$c0103, pattern = " ", replacement = "")
# activity_nt[activity_nt == ""] <- NA
# table(activity_nt$c0103)
# activity_nt <- subset(activity_nt, !is.na(c0103))
# colSums(is.na(activity_nt))
# write.xlsx(activity_nt, file="activity_nt.xlsx")

activity_nt <- read.xlsx("activity_nt.xlsx")


# EDA
par(family="STKaiti")

# c0102(活動大類)
table(activity_nt$c0102)
barplot(table(activity_nt$c0102),main="活動大類")

# c0103(活動小類)
table(activity_nt$c0103)
barplot(table(activity_nt$c0103),main="活動小類")

## 用活動大類去切開來看
c0102_2 <- subset(activity_nt, activity_nt$c0102=="2") 
c0102_A <- subset(activity_nt, activity_nt$c0102=="A") 
c0102_C <- subset(activity_nt, activity_nt$c0102=="C") 
c0102_E <- subset(activity_nt, activity_nt$c0102=="E") 
c0102_H <- subset(activity_nt, activity_nt$c0102=="H") 
c0102_X <- subset(activity_nt, activity_nt$c0102=="X") 

barplot(table(c0102_2$c0103),main="活動大類2-活動小類")
I20029 <- subset(activity_nt, activity_nt$c0103=="20029") #社區達人競賽
I20025 <- subset(activity_nt, activity_nt$c0103=="20025") #中秋節線上活動

barplot(table(c0102_A$c0103),main="活動大類A-活動小類")
a0016 <- subset(activity_nt, activity_nt$c0103=="A0016") #社區專屬繪畫比賽
a0001 <- subset(activity_nt, activity_nt$c0103=="A0001") #節慶活動

barplot(table(c0102_C$c0103),main="活動大類C-活動小類") 
c0002 <- subset(activity_nt, activity_nt$c0103=="C0002") #中元普渡
c0001 <- subset(activity_nt, activity_nt$c0103=="C0001") #住戶大會

barplot(table(c0102_E$c0103),main="活動大類E-活動小類")
e0005 <- subset(activity_nt, activity_nt$c0103=="E0005") #信義捕手
g0021 <- subset(activity_nt, activity_nt$c0103=="G0021") #防疫

barplot(table(c0102_H$c0103),main="活動大類H-活動小類")
h0004 <- subset(activity_nt, activity_nt$c0103=="H0004") #母親節康乃馨申請
h0002 <- subset(activity_nt, activity_nt$c0103=="H0002") #很多

barplot(table(c0102_X$c0103),main="活動大類X-活動小類") 
x0010 <- subset(activity_nt, activity_nt$c0103=="X0010") #傳遞愛馨 共創幸福


# c0104(表單狀態)
table(activity_nt$c0104)
barplot(table(activity_nt$c0104), names.arg=c("待審核","進行中","已結案","待請款","資料輸入中"), main="表單狀態")
check_c0104_A <- subset(activity_nt, activity_nt$c0104=="A")


# 開始時間(c0111)分年份
library(tidyr)
activity_nt$c0111 = substr(activity_nt$c0111,1,7)
activity_nt <- separate(activity_nt, c0111, c("c0111y", "c0111m"), "-")
nt_2018 <- subset(activity_nt, activity_nt$c0111y==2018)
nt_2019 <- subset(activity_nt, activity_nt$c0111y==2019)
nt_2020 <- subset(activity_nt, activity_nt$c0111y==2020)
nt_2021 <- subset(activity_nt, activity_nt$c0111y==2021)
nt_2022 <- subset(activity_nt, activity_nt$c0111y==2022)

colSums(is.na(nt_2018))
colSums(is.na(nt_2019))
colSums(is.na(nt_2020))
colSums(is.na(nt_2021))
colSums(is.na(nt_2022))

table(activity_nt$c0111y)
barplot(table(activity_nt$c0111y), main="活動年份")


# 活動時長(c0111-c0112)
table(activity_nt$hour)
summary(activity_nt$hour)

hour_range <- activity_nt$hour
hour_range[hour_range<=2] <- 2
hour_range[2<hour_range & hour_range<25] <- 24
hour_range[25<=hour_range & hour_range<169] <- 168
hour_range[169<=hour_range & hour_range<337] <- 336
hour_range[337<=hour_range & hour_range<505] <- 504
hour_range[505<=hour_range & hour_range<721] <- 720 #一個月
hour_range[721<=hour_range & hour_range<2161] <- 2160 #三個月
hour_range[2161<=hour_range] <- 2161 #三個月以上
table(hour_range)
# hour_range <- str_replace_all(hour_range, c("^2$"="2小時", "24"="一天", "168"="一週", "336"="兩週", "504"="三週", "720"="一個月", "2160"="一至三個月", "2161"="三個月以上"))
activity_nt <- cbind(activity_nt, hour_range)

table(activity_nt$hour_range)
barplot(table(activity_nt$hour_range),names.arg=c("2小時", "一天", "一週", "兩週", "三週", "一個月", "一至三個月", "三個月以上"), main="活動時長")

# c0113
table(activity_nt$c0113)
barplot(table(activity_nt$c0113),names.arg=c("單店", "多店", "區"), main="舉辦單位")

# c0115
table(activity_nt$c0115)

# 刪除c0808有空值的obs
activity_nt_nona <- subset(activity_nt, !is.na(c0808))
table(activity_nt_nona$c0808)
summary(activity_nt_nona$c0808)

c0808 <- activity_nt_nona$c0808
c0808[c0808>13] <- 14
barplot(table(c0808),names.arg=c("1","2","3","4","5","6","7","8","9","10","11","12","13","大於13"), main="參加人數")


# 分區域
table(activity_nt$Area)
barplot(table(activity_nt$Area),main="各區域舉辦活動數")

# 對照分群用資料集中的新北市大樓數
building_newtaipei <- readRDS("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋/雙北分群用資料集/building_newtaipei.rds")
barplot(table(building_newtaipei$area),main="新北市各區大樓數")




activity_nt_na <- subset(activity_nt, is.na(c0808))
table(activity_nt_na$c0102)
table(activity_nt$c0102)

table(activity_nt_na$c0103)
table(activity_nt$c0103)

write.xlsx(activity_nt_na, file="activity_nt_na.xlsx")

