setwd("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋")
building <- readRDS("最終資料集/final_data_v1.2.rds")

# 標準型
table(building$c0180)
building_cluster <- subset(building, building$c0180=="1")

# 電梯大樓、華廈
table(building_cluster$c0102_new)
building_cluster <- subset(building_cluster, building_cluster$c0102_new=="大樓(電梯)"|building_cluster$c0102_new=="華廈")
table(building_cluster$c0102_new)

# 雙北
table(building_cluster$city)
building_cluster <- subset(building_cluster, building_cluster$city=="臺北市"|building_cluster$city=="新北市")

# 加上區域
library(stringr)
building_cluster$area <- building_cluster$c0146
building_cluster$area <- str_replace_all(building_cluster$area, c("100"="中正區", "103"="大同區", "104"="中山區", "105"="松山區", "106"="大安區", "108"="萬華區", "110"="信義區", "111"="士林區", "112"="北投區", "114"="內湖區", "115"="南港區", "116"="文山區"))
building_cluster$area <- str_replace_all(building_cluster$area, c("207"="萬里區", "208"="金山區", "220"="板橋區", "221"="汐止區", "222"="深坑區", "223"="石碇區", "224"="瑞芳區", "226"="平溪區", "227"="雙溪區", "228"="貢寮區", "231"="新店區", "232"="坪林區", "233"="烏來區", "234"="永和區", "235"="中和區", "236"="土城區", "237"="三峽區", "238"="樹林區", "239"="鶯歌區", "241"="三重區", "242"="新莊區", "243"="泰山區", "244"="林口區", "247"="蘆洲區", "248"="五股區", "248"="新莊區", "249"="八里區", "251"="淡水區", "252"="三芝區", "253"="石門區"))
table(building_cluster$area)

# 分群用資料集
building_cluster <- subset(building_cluster, select=-c(c0103,c0104,c0105,c0107,c0110,c0111,c0112,c0113,c0115,c0116,c0117,c0119,c0120,c0128,c0139,c0146,c0178,c0180,c0181,c0182,c0184,c0185,c0119_Category,c0120_Category,c0104_new,c0107_new,c0178_new,c0102,c0102_new,c0124_new,c0125_new))
colSums(is.na(building_cluster))

# 刪除數值變數的NA
building_cluster1 <- subset(building_cluster, !is.na(c0118)&!is.na(c0158)&!is.na(age))
colSums(is.na(building_cluster1))

# 純文字分群
building_tm <- subset(building_cluster1, select=-c(c0106,c0118,c0121,c0122,c0123,c0124,c0125,c0158,c0179,age,c0106_new))
colSums(is.na(building_tm))

# 零公設比
table(building_tm$c0118_new)
building_tm$c0118_new[building_tm$c0118_new == "零公設比"] <- NA
colSums(is.na(building_tm))

# 改typo
table(building_tm$age_Category)
building_tm$age_Category[building_tm$age_Category == "二十年至二十五年屋齡"] <- "二十ㄧ年至二十五年屋齡"

# 看所有有na的觀察值
building_tm_nona <- building_tm[complete.cases(building_tm), ]
colSums(is.na(building_tm_nona))
building_tm_na <- building_tm[!complete.cases(building_tm), ] # 4503

# 分雙北
building_taipei <- subset(building_tm, building_tm$city=="臺北市")
building_taipei <- subset(building_taipei, select=-city)
colSums(is.na(building_taipei))
building_newtaipei <- subset(building_tm, building_tm$city=="新北市")
building_newtaipei <- subset(building_newtaipei, select=-city)
colSums(is.na(building_newtaipei))

# 臺北市輸出
setwd("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋/雙北分群用資料集")
library(openxlsx)
write.xlsx(building_taipei, file = "building_taipei.xlsx")
saveRDS(building_taipei, file = "building_taipei.rds")
write.csv(building_taipei, file = "building_taipei.csv", row.names=F)  

# 新北市輸出
write.xlsx(building_newtaipei, file = "building_newtaipei.xlsx")
saveRDS(building_newtaipei, file = "building_newtaipei.rds")
write.csv(building_newtaipei, file = "building_newtaipei.csv", row.names=F)  

# check 檔案
setwd("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋/雙北分群用資料集")
building_taipei <- readRDS("building_taipei.rds")
colSums(is.na(building_taipei))

building_newtaipei <- readRDS("building_newtaipei.rds")
colSums(is.na(building_newtaipei))
