setwd("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋")
building <- readRDS("CL001F.rds")
library(readxl)
building_new <- read_excel("data_new.xlsx")
c0101 <- building$c0101
building <- cbind(c0101, building_new)

# 空值設為NA
building[building == ""] <- NA
building[building == " "] <- NA
building[building == "　"] <- NA
building[building == "-"] <- NA
building[building == "?"] <- NA
building[building == "."] <- NA
building[building == "不知|不詳"] <- NA
colSums(is.na(building))

#-------------------------------------- c0146 縣市代號（郵遞區號） --------------------------------------#
library(stringr)
city <- building$c0146
city <- str_replace_all(city, c("100"="臺北市", "103"="臺北市", "104"="臺北市", "105"="臺北市", "106"="臺北市", "108"="臺北市", "110"="臺北市", "111"="臺北市", "112"="臺北市", "114"="臺北市", "115"="臺北市", "116"="臺北市", "200"="基隆市", "201"="基隆市", "202"="基隆市", "203"="基隆市", "204"="基隆市", "205"="基隆市", "206"="基隆市", "207"="新北市", "208"="新北市", "209"="連江縣", "210"="連江縣", "211"="連江縣", "212"="連江縣", "220"="新北市", "221"="新北市", "222"="新北市", "223"="新北市", "224"="新北市", "226"="新北市", "227"="新北市", "228"="新北市", "231"="新北市", "232"="新北市", "233"="新北市", "234"="新北市", "235"="新北市", "236"="新北市", "237"="新北市", "238"="新北市", "239"="新北市", "241"="新北市", "242"="新北市", "243"="新北市", "244"="新北市", "247"="新北市", "248"="新北市", "249"="新北市", "251"="新北市", "252"="新北市", "253"="新北市", "260"="宜蘭縣", "261"="宜蘭縣", "262"="宜蘭縣", "263"="宜蘭縣", "264"="宜蘭縣", "265"="宜蘭縣", "266"="宜蘭縣", "267"="宜蘭縣", "268"="宜蘭縣", "269"="宜蘭縣", "270"="宜蘭縣", "272"="宜蘭縣", "290"="宜蘭縣", "300"="新竹市", "302"="新竹縣", "303"="新竹縣", "304"="新竹縣", "305"="新竹縣", "306"="新竹縣", "307"="新竹縣", "308"="新竹縣", "310"="新竹縣", "311"="新竹縣", "312"="新竹縣", "313"="新竹縣", "314"="新竹縣", "315"="新竹縣", "320"="桃園市", "324"="桃園市", "325"="桃園市", "326"="桃園市", "327"="桃園市", "328"="桃園市", "330"="桃園市", "333"="桃園市", "334"="桃園市", "335"="桃園市", "336"="桃園市", "337"="桃園市", "338"="桃園市", "350"="苗栗縣", "351"="苗栗縣", "352"="苗栗縣", "353"="苗栗縣", "354"="苗栗縣", "356"="苗栗縣", "357"="苗栗縣", "358"="苗栗縣", "360"="苗栗縣", "361"="苗栗縣", "362"="苗栗縣", "363"="苗栗縣", "364"="苗栗縣", "365"="苗栗縣", "366"="苗栗縣", "367"="苗栗縣", "368"="苗栗縣", "369"="苗栗縣", "400"="臺中市", "401"="臺中市", "402"="臺中市", "403"="臺中市", "404"="臺中市", "406"="臺中市", "407"="臺中市", "408"="臺中市", "411"="臺中市", "412"="臺中市", "413"="臺中市", "414"="臺中市", "420"="臺中市", "421"="臺中市", "422"="臺中市", "423"="臺中市", "424"="臺中市", "426"="臺中市", "427"="臺中市", "428"="臺中市", "429"="臺中市", "432"="臺中市", "433"="臺中市", "434"="臺中市", "435"="臺中市", "436"="臺中市", "437"="臺中市", "438"="臺中市", "439"="臺中市", "500"="彰化縣", "502"="彰化縣", "503"="彰化縣", "504"="彰化縣", "505"="彰化縣", "506"="彰化縣", "507"="彰化縣", "508"="彰化縣", "509"="彰化縣", "510"="彰化縣", "511"="彰化縣", "512"="彰化縣", "513"="彰化縣", "514"="彰化縣", "515"="彰化縣", "516"="彰化縣", "520"="彰化縣", "521"="彰化縣", "522"="彰化縣", "523"="彰化縣", "524"="彰化縣", "525"="彰化縣", "526"="彰化縣", "527"="彰化縣", "528"="彰化縣", "530"="彰化縣", "540"="南投縣", "541"="南投縣", "542"="南投縣", "544"="南投縣", "545"="南投縣", "546"="南投縣", "551"="南投縣", "552"="南投縣", "553"="南投縣", "555"="南投縣", "556"="南投縣", "557"="南投縣", "558"="南投縣", "600"="嘉義市", "602"="嘉義縣", "603"="嘉義縣", "604"="嘉義縣", "605"="嘉義縣", "606"="嘉義縣", "607"="嘉義縣", "608"="嘉義縣", "611"="嘉義縣", "612"="嘉義縣", "613"="嘉義縣", "614"="嘉義縣", "615"="嘉義縣", "616"="嘉義縣", "621"="嘉義縣", "622"="嘉義縣", "623"="嘉義縣", "624"="嘉義縣", "625"="嘉義縣"))

city <- str_replace_all(city, c("630"="雲林縣", "631"="雲林縣", "632"="雲林縣", "633"="雲林縣", "634"="雲林縣", "635"="雲林縣", "636"="雲林縣", "637"="雲林縣", "638"="雲林縣", "640"="雲林縣", "643"="雲林縣", "646"="雲林縣", "647"="雲林縣", "648"="雲林縣", "649"="雲林縣", "651"="雲林縣", "652"="雲林縣", "653"="雲林縣", "654"="雲林縣", "655"="雲林縣", "700"="臺南市", "701"="臺南市", "702"="臺南市", "704"="臺南市", "708"="臺南市", "709"="臺南市", "710"="臺南市", "711"="臺南市", "712"="臺南市", "713"="臺南市", "714"="臺南市", "715"="臺南市", "716"="臺南市", "717"="臺南市", "718"="臺南市", "719"="臺南市", "720"="臺南市", "721"="臺南市", "722"="臺南市", "723"="臺南市", "724"="臺南市", "725"="臺南市", "726"="臺南市", "727"="臺南市", "730"="臺南市", "731"="臺南市", "732"="臺南市", "733"="臺南市", "734"="臺南市", "735"="臺南市", "736"="臺南市", "737"="臺南市", "741"="臺南市", "742"="臺南市", "743"="臺南市", "744"="臺南市", "745"="臺南市", "800"="高雄市", "801"="高雄市", "802"="高雄市", "803"="高雄市", "804"="高雄市", "805"="高雄市", "806"="高雄市", "807"="高雄市", "811"="高雄市", "812"="高雄市", "813"="高雄市", "814"="高雄市", "815"="高雄市", "817"="南海島", "819"="南海島", "820"="高雄市", "821"="高雄市", "822"="高雄市", "823"="高雄市", "824"="高雄市", "825"="高雄市", "826"="高雄市", "827"="高雄市", "828"="高雄市", "829"="高雄市", "830"="高雄市", "831"="高雄市", "832"="高雄市", "833"="高雄市", "840"="高雄市", "842"="高雄市", "843"="高雄市", "844"="高雄市", "845"="高雄市", "846"="高雄市", "847"="高雄市", "848"="高雄市", "849"="高雄市", "851"="高雄市", "852"="高雄市", "880"="澎湖縣", "881"="澎湖縣", "882"="澎湖縣", "883"="澎湖縣", "884"="澎湖縣", "885"="澎湖縣", "890"="金門縣", "891"="金門縣", "892"="金門縣", "893"="金門縣", "894"="金門縣", "896"="金門縣", "900"="屏東縣", "901"="屏東縣", "902"="屏東縣", "903"="屏東縣", "904"="屏東縣", "905"="屏東縣", "906"="屏東縣", "907"="屏東縣", "908"="屏東縣", "909"="屏東縣", "911"="屏東縣", "912"="屏東縣", "913"="屏東縣", "920"="屏東縣", "921"="屏東縣", "922"="屏東縣", "923"="屏東縣", "924"="屏東縣", "925"="屏東縣", "926"="屏東縣", "927"="屏東縣", "928"="屏東縣", "929"="屏東縣", "931"="屏東縣", "932"="屏東縣", "940"="屏東縣", "941"="屏東縣", "942"="屏東縣", "943"="屏東縣", "944"="屏東縣", "945"="屏東縣", "946"="屏東縣", "947"="屏東縣", "950"="臺東縣", "951"="臺東縣", "952"="臺東縣", "953"="臺東縣", "954"="臺東縣", "955"="臺東縣", "956"="臺東縣", "957"="臺東縣", "958"="臺東縣", "959"="臺東縣", "961"="臺東縣", "962"="臺東縣", "963"="臺東縣", "964"="臺東縣", "965"="臺東縣", "966"="臺東縣", "970"="花蓮縣", "971"="花蓮縣", "972"="花蓮縣", "973"="花蓮縣", "974"="花蓮縣", "975"="花蓮縣", "976"="花蓮縣", "977"="花蓮縣", "978"="花蓮縣", "979"="花蓮縣", "981"="花蓮縣", "982"="花蓮縣", "983"="花蓮縣"))
building <- cbind(building, city)

# 縣市
summary(building$city)
par(family="STKaiti")
plot(building$city, main = "各縣市大樓數")

# 臺北市
taipei <- subset(building, building$city=="臺北市")
taipei_area <- taipei$c0146
taipei_area <- str_replace_all(taipei_area, c("100"="中正區", "103"="大同區", "104"="中山區", "105"="松山區", "106"="大安區", "108"="萬華區", "110"="信義區", "111"="士林區", "112"="北投區", "114"="內湖區", "115"="南港區", "116"="文山區"))
taipei <- cbind(taipei, taipei_area)
plot(taipei$taipei_area, main = "臺北市各區大樓數")

# 新北市
newtaipei <- subset(building, building$city=="新北市")
newtaipei_area <- newtaipei$c0146
newtaipei_area <- str_replace_all(newtaipei_area, c("207"="萬里區", "208"="金山區", "220"="板橋區", "221"="汐止區", "222"="深坑區", "223"="石碇區", "224"="瑞芳區", "226"="平溪區", "227"="雙溪區", "228"="貢寮區", "231"="新店區", "232"="坪林區", "233"="烏來區", "234"="永和區", "235"="中和區", "236"="土城區", "237"="三峽區", "238"="樹林區", "239"="鶯歌區", "241"="三重區", "242"="新莊區", "243"="泰山區", "244"="林口區", "247"="蘆洲區", "248"="五股區", "248"="新莊區", "249"="八里區", "251"="淡水區", "252"="三芝區", "253"="石門區"))
newtaipei <- cbind(newtaipei, newtaipei_area)
plot(newtaipei$newtaipei_area, main = "新北市各區大樓數")


#-------------------------------------- c0162 分類：重點/一般 --------------------------------------#
building$c0162 <- str_replace(building$c0162, pattern = "1", replacement = "重點")
building$c0162 <- str_replace(building$c0162, pattern = "0", replacement = "一般")
barplot(table(building$c0162), main="大樓分類")


#-------------------------------------- c0129 警衛形式 --------------------------------------#
# 24小時
building$c0129[grep("24|２４|全天|全日|全時段|００００|ALL|三人輪班,一人8小時|3班|３班",building$c0129)] <- "24小時"

# 日間管理（還滿多是星期日沒有的）
building$c0129[grep("日|白|早|^周一|^AM|^週一|^一~|^星期一|至晚上|禮拜一|一般|一到五|到晚上|半天|^8-|^0.....1....|^0.....2....|0....1...|0....2...|^1.小時|０...～....|０.：..～..：..|...0~....0|...0～....0|0...0～....0|.~..|0.~..|12HR|１２小時|^到|到.點|到..點|2班|２班|兩班|二班|AM.....PM|晚上7點下班|1班",building$c0129)] <- "日間管理"

# 無管理
building$c0129[grep("無|沒有|未|NO|待|自我|自行|自主|ＮＯ|N|no|互助|相助|里鄰戶助|自宅|一個人|-|監視器",building$c0129)] <- "無管理"
building$c0129 <- str_replace(building$c0129, pattern = "警察", replacement = "警察")
building$c0129[grep("警察",building$c0129)] <- "警察"

# 夜間管理
building$c0129 <- str_replace(building$c0129, pattern = "夜間", replacement = "夜間管理")
building$c0129[grep("夜|^pm|１８００～０８００",building$c0129)] <- "夜間管理"

# 有管理
building$c0129[grep("保全|警衛|物業|主委|社區阿伯|總幹事|許大哥|自聘|物管|維新|台灣國際|盛世新航|清潔人員|私|管理員|京城|隆富建設|鄰里巡守|東京都|飯店式管理|管理室|第下一樓|守望亭|別墅型|3位住戶輪流管理|管委會|站哨",building$c0129)] <- "有管理"
building$c0129[grep("車",building$c0129)] <- "車道管控"
building$c0129[grep("磁|門卡",building$c0129)] <- "磁卡管控"

# 無意義
building$c0129[grep("^洽|^依|元|備註|年|坪|3436038|2300|1200|不一",building$c0129)] <- ""

# 剩下的分類
building$c0129[grep("其他",building$c0129)] <- ""
building$c0129[grep("^0$|警察",building$c0129)] <- "無管理"
building$c0129[grep("^全$",building$c0129)] <- "24小時"
building$c0129[grep("^1$|^2$|^12$|車道管控|磁卡管控|^有$",building$c0129)] <- "有管理"
building$c0129 <- str_replace(building$c0129, pattern = "有管理", replacement = "其他管理形式")

building[building == ""] <- NA

# table和圖
c0129 <- building$c0129
t0129 <- table(c0129)
t0129
barplot(t0129, main = "大樓管理形式")


#-------------------------------------- c0114 朝向 --------------------------------------#
building$c0114 <- str_replace(building$c0114, pattern = "座", replacement = "坐")
building$c0114 <- str_replace(building$c0114, pattern = "^坐.朝", replacement = "")
building$c0114 <- str_replace(building$c0114, pattern = "^大樓朝|^大樓面|^大樓門面|^面|^大門朝|^社區朝|方$", replacement = "")
building$c0114 <- str_replace_all(building$c0114, pattern = "0|1|2|20|50|朝向|物件:|  |南港|商城|雙|單|N/A|X|^有$", replacement = "")
building$c0114[grep("未|^待|調查中|不足|不知",building$c0114)] <- ""
building$c0114 <- str_replace(building$c0114, pattern = "^朝", replacement = "")
building$c0114 <- str_replace_all(building$c0114, pattern = ", |、朝|、|或|/|-|，|及|和|跟", replacement = ",")
building$c0114 <- str_replace_all(building$c0114, pattern = "\\.|\\&", replacement = ",")
building$c0114 <- str_replace_all(building$c0114, pattern = "\\?|^,$|\\*|\\'|`", replacement = "")
building$c0114[grep("^不|.有|全方位|皆|東南西北|東,西南北|東西南北|東西西北|東西南|南北,東西",building$c0114)] <- "多方位"
building$c0114 <- str_replace(building$c0114, pattern = "無|^A|其他|坐向|向", replacement = "")

# 北開頭
building$c0114 <- str_replace(building$c0114, pattern = "北北東|東北B北", replacement = "北,東北")
building$c0114 <- str_replace(building$c0114, pattern = "北朝西", replacement = "北,西")
building$c0114 <- str_replace(building$c0114, pattern = "北東|東,北|東朝北", replacement = "北,東")
building$c0114 <- str_replace(building$c0114, pattern = "北東西|東,西,北", replacement = "北,東,西")
building$c0114 <- str_replace(building$c0114, pattern = "北南", replacement = "北,南")
building$c0114 <- str_replace(building$c0114, pattern = "北南西|^北,南西", replacement = "北,南,西")
building$c0114 <- str_replace(building$c0114, pattern = "北偏東", replacement = "東北")

# 東開頭
building$c0114 <- str_replace(building$c0114, pattern = "^東,北,南$|^東,南,北$", replacement = "北,南,東")
building$c0114 <- str_replace(building$c0114, pattern = "東,西,南", replacement = "東,南,西")
building$c0114 <- str_replace(building$c0114, pattern = "東北朝西南", replacement = "東北,西南")
building$c0114 <- str_replace(building$c0114, pattern = "^東北東$", replacement = "東,東北")
building$c0114 <- str_replace(building$c0114, pattern = "^東北西$", replacement = "西,東北")
building$c0114 <- str_replace(building$c0114, pattern = "東朝南", replacement = "東,南")
building$c0114 <- str_replace(building$c0114, pattern = "東南,東北|東南東北", replacement = "東北,東南")
building$c0114 <- str_replace(building$c0114, pattern = "東西", replacement = "東,西")

# 南開頭
building$c0114 <- str_replace(building$c0114, pattern = "^南,北,東$|^南北,東$", replacement = "北,南,東")
building$c0114 <- str_replace(building$c0114, pattern = "^南,北,東北$", replacement = "北,南,東北")
building$c0114 <- str_replace(building$c0114, pattern = "^南,北,西$|南北西$", replacement = "北,南,西")
building$c0114 <- str_replace(building$c0114, pattern = "^南,東,西$|^南,西,東$|^南東,西$", replacement = "東,南,西")
building$c0114 <- str_replace(building$c0114, pattern = "南,北|南北", replacement = "北,南")
building$c0114 <- str_replace(building$c0114, pattern = "^南,東$|南東", replacement = "東,南")
building$c0114 <- str_replace(building$c0114, pattern = "^南,東北$", replacement = "東北,南")

# 西開頭
building$c0114 <- str_replace(building$c0114, pattern = "^西,$|^坐東北,西$", replacement = "西")
building$c0114 <- str_replace(building$c0114, pattern = "^西,北$", replacement = "北,西")
building$c0114 <- str_replace(building$c0114, pattern = "^西,北,東$|^西東北$", replacement = "北,東,西")
building$c0114 <- str_replace(building$c0114, pattern = "^西,北,南$|^西,南,北$|^西,南北$|^西南北$", replacement = "北,南,西")
building$c0114 <- str_replace(building$c0114, pattern = "^西,北,西北$", replacement = "北,西,西北")
building$c0114 <- str_replace(building$c0114, pattern = "^西,東$", replacement = "東,西")
building$c0114 <- str_replace(building$c0114, pattern = "^西,南$", replacement = "南,西")
building$c0114 <- str_replace(building$c0114, pattern = "^西北,東北$", replacement = "東北,西北")
building$c0114 <- str_replace(building$c0114, pattern = "^西北,東南$", replacement = "東南,西北")
building$c0114 <- str_replace(building$c0114, pattern = "^西南,東北$", replacement = "東北,西南")
building$c0114 <- str_replace(building$c0114, pattern = "^西北,東$", replacement = "東,西北")
building$c0114 <- str_replace(building$c0114, pattern = "^西北,南$", replacement = "南,西北")
building$c0114 <- str_replace(building$c0114, pattern = "^西北,西北$", replacement = "西北")
building$c0114 <- str_replace(building$c0114, pattern = "^西東南$", replacement = "東,南,西")
building$c0114 <- str_replace(building$c0114, pattern = "^西西北$", replacement = "西,西北")
building$c0114 <- str_replace(building$c0114, pattern = "^西西南$", replacement = "西,西南")

# 最終
building$c0114 <- str_replace(building$c0114, pattern = " ", replacement = "")
building[building == ""] <- NA

c0114 <- building$c0114
t0114 <- table(c0114)
t0114

# 圖
c0114_edit <- c0114
c0114_edit <- str_replace(c0114_edit, pattern = "多方位|^北,東,南$|^北,東,西$|^北,南,東$|^北,南,東北$|^北,南,西$|^北,西,西北$|^東,南,東南$|^東,南,西$", replacement = "其他")
c0114_edit <- str_replace(c0114_edit, pattern = "^北,東$|^北,東北$|^北,東南$|^北,西$|^北,西北$|^北,西南$|^東,東北$|^東,東南$|^東,南$|^東,西北$|^東,西南$|^東北,東$|^東北,東南$|^東北,南$|^東北,西北$|^東南,西南$|^南,東南$|^南,西$|^南,西北$|^南,西南$|^西,東北$|^西,東南$|^西,西北$|^西,西南$|^西北,西南$", replacement = "其他")
t0114_edit <- table(c0114_edit)
t0114_edit
barplot(t0114_edit, main = "大樓朝向")


#-------------------------------------- 多變數 --------------------------------------#
# 縣市 v.s. 分類
table(building$city,building$c0162)
barplot(table(building$c0162,building$city), legend.text = c("一般", "重點"), beside=TRUE, main = "各縣市大樓分類")

## 臺北市
table(taipei$taipei_area,taipei$c0162)
barplot(table(taipei$c0162,taipei$taipei_area), beside=TRUE, main = "臺北市大樓分類", legend.text=c("一般", "重點"), ylim=c(0,1000))

## 新北市
table(newtaipei$newtaipei_area,newtaipei$c0162)
barplot(table(newtaipei$c0162,newtaipei$newtaipei_area), beside=TRUE, main = "新北市大樓分類", legend.text=c("一般", "重點"), ylim=c(0,500))

# 縣市 v.s. 大樓管理形式
table(building$city,building$c0129)
barplot(table(building$c0129,building$city), legend.text = c("24小時", "其他", "日間", "無", "夜間"), beside=TRUE, main = "各縣市大樓管理形式")
barplot(table(building$c0129,building$city), legend.text = c("24小時", "其他", "日間", "無", "夜間"), main = "各縣市大樓管理形式")

## 臺北市
taipei <- subset(building, building$city=="臺北市")
taipei_area <- taipei$c0146
taipei_area <- str_replace_all(taipei_area, c("100"="中正區", "103"="大同區", "104"="中山區", "105"="松山區", "106"="大安區", "108"="萬華區", "110"="信義區", "111"="士林區", "112"="北投區", "114"="內湖區", "115"="南港區", "116"="文山區"))
taipei <- cbind(taipei, taipei_area)
table(taipei$taipei_area,taipei$c0129)
barplot(table(taipei$c0129,taipei$taipei_area), main = "臺北市大樓管理形式", legend.text=c("24小時", "其他", "日間", "無", "夜間"), ylim=c(0,1800))

## 新北市
newtaipei <- subset(building, building$city=="新北市")
newtaipei_area <- newtaipei$c0146
newtaipei_area <- str_replace_all(newtaipei_area, c("207"="萬里區", "208"="金山區", "220"="板橋區", "221"="汐止區", "222"="深坑區", "223"="石碇區", "224"="瑞芳區", "226"="平溪區", "227"="雙溪區", "228"="貢寮區", "231"="新店區", "232"="坪林區", "233"="烏來區", "234"="永和區", "235"="中和區", "236"="土城區", "237"="三峽區", "238"="樹林區", "239"="鶯歌區", "241"="三重區", "242"="新莊區", "243"="泰山區", "244"="林口區", "247"="蘆洲區", "248"="五股區", "248"="新莊區", "249"="八里區", "251"="淡水區", "252"="三芝區", "253"="石門區"))
newtaipei <- cbind(newtaipei, newtaipei_area)
table(newtaipei$newtaipei_area,newtaipei$c0129)
barplot(table(newtaipei$c0129,newtaipei$newtaipei_area), main = "新北市大樓管理形式")

# 縣市 v.s. 朝向
c0114_edit2 <- str_replace(c0114_edit, pattern = "^東北,西南$|^東南,西北$|^北,南$|^東,西$", replacement = "其他")

table(building$city,c0114_edit2)
barplot(table(c0114_edit2,building$city), main = "各縣市大樓朝向")
write.csv(table(building$city,c0114_edit2),file="city_c0114.csv",row.names=FALSE)

# 最終版資料集
library(openxlsx)
write.xlsx(building, file = "final_data.xlsx")  
saveRDS(building, file = "final_data.rds")
write.csv(building, file = "final_data.csv")  




# 縣市 v.s. 屋齡
setwd("/Users/angelwang/Desktop/台大/碩一下/1_商業智慧與數據分析企業實作/信義房屋")
building <- readRDS("final_data.rds")
par(family="STKaiti")
boxplot(building$age~building$city, cex=0.5, xlab = "縣市", ylab = "屋齡", main = "各縣市屋齡")

city_age <- na.omit(cbind.data.frame(building$age,building$city))
colnames(city_age) <- c("age", "city")
library(dplyr)
city_age %>% 
  group_by(city) %>%
  summarize(Minimum = min(age),
            quant25 = quantile(age, probs = 0.25),
            quant50 = quantile(age, probs = 0.5),
            quant75 = quantile(age, probs = 0.75),
            Maximum = max(age))

newtaipei <- subset(city_age, city_age$city=="新北市")
barplot(table(newtaipei$city, newtaipei$age))




# final data v1.1
library(readxl)
building1 <- read_excel("final_data_v1.1.xlsx")
building1[building1 == "NA"] <- NA
building1[building1 == "NaN"] <- NA
building1[building1 == "na"] <- NA

colSums(is.na(building1))
# building_c0118_new <- subset(building, is.na(c0118_new))
# building_c0118_miss <- subset(building_c0118_new, !is.na(c0118))
# building_c0118_miss$c0118_new <- "大公設比"
# table(building_c0118_miss$c0118_new)
building1$c0118_new <- ifelse(is.na(building1$c0118_new) & !is.na(building1$c0118),"大公設比", building1$c0118_new)
colSums(is.na(building1))

# 最終版資料集 v1.2
library(openxlsx)
write.xlsx(building1, file = "final_data_v1.2.xlsx")  
saveRDS(building1, file = "final_data_v1.2.rds")
write.csv(building1, file = "final_data_v1.2.csv")  


