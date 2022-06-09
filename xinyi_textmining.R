setwd("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋")
building <- readRDS("最終資料集/final_data_v1.2.rds")

# 標準型
table(building$c0180)
building_cluster <- subset(building, building$c0180=="1")

# 電梯大樓、華廈
table(building_cluster$c0102_new)
building_cluster <- subset(building_cluster, building_cluster$c0102_new=="大樓(電梯)"|building_cluster$c0102_new=="華廈")
table(building_cluster$c0102_new)

# 六都+新竹
table(building_cluster$city)
building_cluster <- subset(building_cluster, building_cluster$city!="基隆市"&building_cluster$city!="彰化縣")
table(building_cluster$city)

# 分群用資料集
building_cluster <- subset(building_cluster, select=-c(c0103,c0104,c0105,c0107,c0110,c0111,c0112,c0113,c0115,c0116,c0117,c0119,c0120,c0128,c0139,c0146,c0178,c0180,c0181,c0182,c0184,c0185,c0119_Category,c0120_Category,c0104_new,c0107_new,c0178_new,c0102,c0102_new,c0124_new,c0125_new))
colSums(is.na(building_cluster))

# 刪除數值變數的NA
building_cluster1 <- subset(building_cluster, !is.na(c0118)&!is.na(c0158)&!is.na(age))
colSums(is.na(building_cluster1))

# 輸出
library(openxlsx)
write.xlsx(building_cluster1, file = "building_cluster.xlsx")  
saveRDS(building_cluster1, file = "building_cluster.rds")
write.csv(building_cluster1, file = "building_cluster.csv", row.names=F)  

# 純文字分群
building_cluster1 <- readRDS("分群用資料檔/building_cluster.rds")
building_tm <- subset(building_cluster1, select=-c(c0106,c0118,c0121,c0122,c0123,c0124,c0125,c0158,c0179,age))
colSums(is.na(building_tm))

# 輸出
library(openxlsx)
write.xlsx(building_tm, file = "building_tm.xlsx")  
saveRDS(building_tm, file = "building_tm.rds")
write.csv(building_tm, file = "building_tm.csv", row.names=F)  


# check
setwd("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋/分群用資料檔")
building_tm <- readRDS("building_tm.rds")
colSums(is.na(building_tm))
# 鋼筋混凝土
table(building_tm$c0106_new)
# 近一年實價登錄
table(building_tm$c0179_new)
# 分類
table(building_tm$c0162)

# test
building_tm_test <- readRDS("building_tm.rds")
building_tm_test<- building_tm_test[1:20,]
write.csv(building_tm_test, file = "building_tm_test.csv", row.names=F) 



# 分雙北
setwd("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋/分群用資料檔")
building_cluster <- readRDS("building_cluster.rds")
building_cluster_taipei <- subset(building_cluster, building_cluster$city=="臺北市")
building_cluster_newtaipei <- subset(building_cluster, building_cluster$city=="新北市")

# 臺北市輸出
library(openxlsx)
write.xlsx(building_cluster_taipei, file = "building_cluster_taipei.xlsx")
saveRDS(building_cluster_taipei, file = "building_cluster_taipei.rds")
write.csv(building_cluster_taipei, file = "building_cluster_taipei.csv", row.names=F)  

# 新北市輸出
library(openxlsx)
write.xlsx(building_cluster_newtaipei, file = "building_cluster_newtaipei.xlsx")
saveRDS(building_cluster_newtaipei, file = "building_cluster_newtaipei.rds")
write.csv(building_cluster_newtaipei, file = "building_cluster_newtaipei.csv", row.names=F)  



