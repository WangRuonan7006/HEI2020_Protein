
## 一. 数据预处理 ####
## 1. 膳食变量HEI2020转换处理 ####
rm(list = ls())
setwd("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/2020 年健康饮食指数（HEI2020）")
library(haven)
library(devtools) # Load devtools
install_github("jamesjiadazhan/dietaryindex")
library(dplyr)
library(haven)
library(readr)
library(dietaryindex)
# 导入2011-2012调查周期所需的数据
DEMO_PATH_12 = read_xpt(file ='DEMO_G-2011-2012.XPT')#人口数据
FPED_PATH_1_12 = read_sas('fped_dr1tot_1112.sas7bdat')#第一天数据
NUTRIENT_PATH_1_12 = read_xpt(file ='3 饮食面试 - 第一天总营养素摄入量 2011-2012 DR1TOT_G.XPT')#第一天数据
FPED_PATH_2_12 = read_sas('fped_dr2tot_1112.sas7bdat')#第二天数据
NUTRIENT_PATH_2_12 = read_xpt(file ='4 饮食访谈 - 总营养素摄入量，第二天 2011-2012 DR2TOT_G.XPT')#第二天数据

# 导入2013-2014调查周期所需的数据
DEMO_PATH_34 = read_xpt(file ='DEMO_H-2013-2014.XPT')#人口数据
FPED_PATH_1_34 = read_sas('fped_dr1tot_1314.sas7bdat')#第一天数据
NUTRIENT_PATH_1_34 = read_xpt(file ='3 饮食面试 - 第一天总营养素摄入量  2013-2014 DR1TOT_H.XPT')#第一天数据
FPED_PATH_2_34 = read_sas('fped_dr2tot_1314.sas7bdat')#第二天数据
NUTRIENT_PATH_2_34 = read_xpt(file ='4 饮食访谈 - 总营养素摄入量，第二天 2013-2014 DR2TOT_H.XPT')#第二天数据

# 导入2015-2016调查周期所需的数据
DEMO_PATH_56 = read_xpt(file ='DEMO_I-2015-2016.XPT')#人口数据
FPED_PATH_1_56 = read_sas('fped_dr1tot_1516.sas7bdat')#第一天数据
NUTRIENT_PATH_1_56 = read_xpt(file ='3 饮食面试 - 第一天总营养素摄入量 2015-2016 DR1TOT_I.XPT')#第一天数据
FPED_PATH_2_56 = read_sas('fped_dr2tot_1516.sas7bdat')#第二天数据
NUTRIENT_PATH_2_56 = read_xpt(file ='4 饮食访谈 - 总营养素摄入量，第二天 2015-2016 DR2TOT_I.XPT')#第二天数据

data("NHANES_20172018")
dt_1718<-HEI2020_NHANES_FPED(FPED_PATH = NHANES_20172018$FPED, NUTRIENT_PATH = NHANES_20172018$NUTRIENT,
                         DEMO_PATH = NHANES_20172018$DEMO, 
                         FPED_PATH2 = NHANES_20172018$FPED2, NUTRIENT_PATH2 = NHANES_20172018$NUTRIENT2)

dt_1112<-HEI2020_NHANES_FPED(FPED_PATH = FPED_PATH_1_12, NUTRIENT_PATH = NUTRIENT_PATH_1_12,
                         DEMO_PATH = DEMO_PATH_12, 
                         FPED_PATH2 = FPED_PATH_2_12, NUTRIENT_PATH2 = NUTRIENT_PATH_2_12)

dt_1314<-HEI2020_NHANES_FPED(FPED_PATH = FPED_PATH_1_34, NUTRIENT_PATH = NUTRIENT_PATH_1_34,
                             DEMO_PATH = DEMO_PATH_34, 
                             FPED_PATH2 = FPED_PATH_2_34, NUTRIENT_PATH2 = NUTRIENT_PATH_2_34)

dt_1516<-HEI2020_NHANES_FPED(FPED_PATH = FPED_PATH_1_56, NUTRIENT_PATH = NUTRIENT_PATH_1_56,
                             DEMO_PATH = DEMO_PATH_56, 
                             FPED_PATH2 = FPED_PATH_2_56, NUTRIENT_PATH2 = NUTRIENT_PATH_2_56)


dt_1112$survey_year <-"7"       # "2011~2012"
dt_1314$survey_year <-"8"       # "2013~2014"
dt_1516$survey_year <-"9"       # "2015~2016"
dt_1718$survey_year <-"10"      # "2017~2018"

# 假设你的数据集存储在一个列表中
df_list <- list(dt_1112, dt_1314, dt_1516, dt_1718)

# 获取每个数据集的列名
colnames_list <- lapply(df_list, colnames)

# 找出所有数据集中共有的列名
common_colnames <- Reduce(intersect, colnames_list)

# 根据共有列名提取每个数据集的对应列
df_list_common_cols <- lapply(df_list, function(df) df[, common_colnames, drop = FALSE])

# 将所有数据集纵向合并
df_combined <- do.call(rbind, df_list_common_cols)

# 保存数据，后续可直接使用
write_xlsx(df_combined, "E:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/正式分析-2024.05.06/HEI2020.xlsx")

## 2. 数据合并  ####
rm(list = ls())
setwd("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data")
# install.packages("Hmisc")
# install.packages("Hmisc")
library(readxl)
library(randomForest)
library(dplyr)
library(Hmisc)
library(foreign)
library(tidyverse)
library(writexl)
library(readr)
library(broom)
library(xtable)
library(autoReg)
library(ggstatsplot)
library(ggplot2)
library(gtsummary)  # 用于tbl_regression 函数
library(tidyr) # drop_na 函数，用于快速去掉 NA
library(survey)
library(haven)
library(openxlsx)
library(tableone)

#### *一. 数据合并-2024.05.01 
# 以下引用的Body数据为2024.04.10重新生成的BMI数据。可放心使用
rm(list = ls())
#### 1. 一_人口统计数据 
population <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/一_人口统计数据_2011-2018(39156+43).xlsx", col_names = T) 
table(population$survey_year)
population <- population[, c("SEQN","survey_year",
                             "RIAGENDR","RIDAGEYR","RIDRETH3","DMDCITZN","DMDEDUC3",
                             "DMDEDUC2","DMDMARTL","DMDHHSIZ","INDHHIN2","INDFMPIR",
                             "WTINT2YR","WTMEC2YR","SDMVPSU", "SDMVSTRA")]
population$PIR.factor <- ifelse(population$INDFMPIR < 1.3, 1, 0)

population <- population %>%
  mutate(age.factor = case_when(
    RIDAGEYR >= 0 & RIDAGEYR <= 5 ~ "0~5岁儿童群体",
    RIDAGEYR > 5 & RIDAGEYR <= 19 ~ "6~19岁儿童青少年群体",
    RIDAGEYR > 19 & RIDAGEYR <= 59 ~ "20~59岁青壮年群体",
    RIDAGEYR > 59 & RIDAGEYR <= 80 ~ "60岁及以上老年群体",
    TRUE ~ "缺失"))  # 如果所有条件都不满足，设置为 NA 或其他默认值
table(population$age.factor)

population <- population %>%
  mutate(age.factor_10 = case_when(
    RIDAGEYR < 20 ~ "0~19岁儿童青少年群体",
    RIDAGEYR >= 20 & RIDAGEYR <= 30 ~ "20~30岁成年群体",
    RIDAGEYR > 30 & RIDAGEYR <= 40 ~ "31~40岁成年群体",
    RIDAGEYR > 40 & RIDAGEYR <= 50 ~ "41~50岁成年群体",
    RIDAGEYR > 50 & RIDAGEYR <= 60 ~ "51~60岁成年群体",
    RIDAGEYR > 60 & RIDAGEYR <= 70 ~ "61~70岁成年群体",
    RIDAGEYR > 70 & RIDAGEYR <= 80 ~ "71~80岁成年群体",
    TRUE ~ "缺失"))  # 如果所有条件都不满足，设置为 NA 或其他默认值
table(population$age.factor_10)

# 对数据集中的每一列进行操作
for(col in names(population)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(population[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}

#### 2.1. 三_检查_5身体测量 
Body_measure <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/2011-2018_7-10/三_检查_5身体测量_2011-2018(37399+20).xlsx", col_names = T) 
table(Body_measure$survey_year)
Body_measure <- Body_measure[, c("SEQN","BMXWT","BMXRECUM","BMXHT","BMXWAIST")]
# 对数据集中的每一列进行操作
for(col in names(Body_measure)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(Body_measure[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}


#### 2.2. 人口统计+身体测量-合并后BMI合并版本 
BMI <- read_excel("D:/BaiduSyncdisk/WHO生长标准文件/NHANES-橙橙-2024.02.28/收到后修改-2024.02.28晚22.50/重新生成数据-2024.04.10/merged_data-((1999-2018)94843+25)-人口统计+身体测量-合并后BMI合并版本.xlsx", col_names = T) 
table(BMI$survey_year) 
BMI <- BMI[BMI$survey_year > 6 | BMI$survey_year == 10, ]  
table(BMI$survey_year) 
BMI <- rename(BMI, BMI.factor = BMI_levels)     # 新变量名 = 旧变量名
BMI <- BMI[, c("SEQN","RIDEXAGM","BMXBMI","BMI.factor")]
# 对数据集中的每一列进行操作
for(col in names(BMI)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(BMI[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}


#### 3. 二_饮食_3第一天总营养素摄入量 
# Diet <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/二_饮食_3第一天总营养素摄入量_2003-2018_77007+154_英文lable.xlsx", col_names = T)
Diet <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/2011-2018_7-10/二_饮食_3第一天总营养素摄入量_2011-2018(37399+164)-2024.04.20.xlsx", col_names = T)
table(Diet$survey_year) 
Diet <- Diet[, -which(names(Diet) == "survey_year")]
Diet <- Diet[, c("SEQN","WTDRD1",
                 "DR1TKCAL","DR1TPROT","DR1TCARB","DR1TSUGR","DR1TFIBE",
                 "DR1TTFAT","DR1TSFAT","DR1TMFAT","DR1TPFAT","DR1TCHOL",
                 "DR1TRET","DR1TVARA","DR1TACAR","DR1TBCAR", "DR1TCRYP",
                 "DR1TLYCO","DR1TLZ","DR1TVB1","DR1TVB2","DR1TNIAC",
                 "DR1TVB6","DR1TFOLA","DR1TFA","DR1TFF","DR1TFDFE",
                 "DR1TVB12","DR1TVC","DR1TVK","DR1TCALC", "DR1TPHOS",
                 "DR1TMAGN","DR1TIRON","DR1TZINC","DR1TCOPP","DR1TSODI",
                 "DR1TPOTA","DR1TSELE","DR1TCAFF","DR1TTHEO","DR1TALCO",
                 "DR1TMOIS","DR1TS040","DR1TS060","DR1TS100", "DR1TS120",
                 "DR1TS140","DR1TS160","DR1TS180","DR1TM181","DR1TP183",
                 "DR1TP184","DR1TP204","DR1TP205","DR1TP226","DR1_300")]

HEI2020 <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/正式分析-2024.05.06/HEI2020.xlsx", col_names = T)
names(HEI2020)
HEI2020 <- HEI2020[, -which(names(HEI2020) == "survey_year")]
HEI2020 <- HEI2020[, -which(names(HEI2020) == "RIDAGEYR")]

# 对数据集中的每一列进行操作
for(col in names(Diet)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(Diet[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}
# 或者使用complete.cases()函数
Diet2 <- Diet[complete.cases(Diet$DR1TKCAL), ]
# 对数据集中的每一列进行操作
for(col in names(Diet2)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(Diet2[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}


#### 4. 五_问卷_2饮酒 
Alcohol <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/2011-2018_7-10/五_问卷_2饮酒_2011-2018(22807+7).xlsx", col_names = T) 
table(Alcohol$ALQ101)   # ALQ101 - 每 1 年至少喝过 12 杯酒精饮料？  1-是  2-不  9-不知道
Alcohol <- Alcohol[, c("SEQN","ALQ101")]
# 对数据集中的每一列进行操作
for(col in names(Alcohol)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(Alcohol[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}
Alcohol2 <- Alcohol[complete.cases(Alcohol$ALQ101), ]


#### 5. 五_问卷_14垂髫 
Early_Childhood <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/2011-2018_7-10/五_问卷_14垂髫_2011-2018(14029+11).xlsx", col_names = T) 
# 对数据集中的每一列进行操作
for(col in names(Early_Childhood)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(Early_Childhood[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}
Early_Childhood <- Early_Childhood[, -which(names(Early_Childhood) == "survey_year")]


#### 6. 五_问卷_24心理健康-抑郁症+评分分类版本 
Depression <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/2011-2018_7-10/五_问卷_24心理健康-抑郁症+评分分类版本_2011-2018(22807+15).xlsx", col_names = T) 
names(Depression)
Depression <- Depression[, c("SEQN","Depression_score","Depression_category","Depression_category2")]
# 对数据集中的每一列进行操作
for(col in names(Depression)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(Depression[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}
table(Depression$Depression_category) 
table(Depression$Depression_category2) 


#### 7. 四_实验室_1白蛋白和肌酐-尿液 
Creatinine <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/2011-2018_7-10/四_实验室_1白蛋白和肌酐-尿液_2011-2018(32656+7).xlsx", col_names = T) 
Creatinine <- Creatinine[, -which(names(Creatinine) == "survey_year")]
# 对数据集中的每一列进行操作
for(col in names(Creatinine)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(Creatinine[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}
names(Creatinine)
Creatinine2 <- Creatinine[complete.cases(Creatinine$URXUMA), ]    # 去除Creatinine$URXUMA变量的NA值


#### 8. 三_检查_7双能 X 射线吸收测定法-全身 
Fat <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/2011-2018_7-10/三_检查_7双能 X 射线吸收测定法-全身_2011-2018(22619+94).xlsx", col_names = T) 
Fat <- Fat[, -which(names(Fat) == "survey_year")]
Fat <- Fat[, c("SEQN","DXDTOFAT","DXDTOLE","DXDTOTOT","DXDTOPF")]
# 对数据集中的每一列进行操作
for(col in names(Fat)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(Fat[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}
Fat2 <- Fat[complete.cases(Fat$DXDTOFAT), ]    # 去除Fat$DXDTOFAT变量的NA值


#### 9. 四_实验室_62标准生化概况 
Biochemical <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/2011-2018_7-10/四_实验室_62标准生化概况_2011-2018(26673+39).xlsx", col_names = T) 
Biochemical <- Biochemical[, c("SEQN",
                               "LBXSBU","LBDSBUSI","LBXSCK","LBXSGB","LBDSGBSI",
                               "LBXSCA","LBDSCASI","LBXSTP","LBDSTPSI","LBXSTR",
                               "LBDSTRSI","LBXSUA","LBDSUASI")]
# 对数据集中的每一列进行操作
for(col in names(Biochemical)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(Biochemical[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))} 


#### 10. 五_问卷_4血压和胆固醇/三五_检查+问卷_4+4血压 
BP <- read_excel("分析可用-BP_score-((2011-2018)28479+29).xlsx", col_names = T) 
BP <- BP[, c("SEQN","SBP_mean","DBP_mean","BP_score","BPQ020","BPQ080")]  
# 对数据集中的每一列进行操作
for(col in names(BP)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(BP[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}


#### 11. 四_实验室_12+14血脂 
CHOL <- read_excel("分析可用-CHOL-((2011-2018)28606+12).xlsx", col_names = T) 
CHOL <- CHOL[, c("SEQN","LBXTC","LBDHDD","CHOL_score")] 
# 对数据集中的每一列进行操作
for(col in names(CHOL)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(CHOL[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}


#### 12. 四五_实验室+问卷_11+26+54血糖 
DM <- read_excel("分析可用-DM-((2011-2018)25173+13).xlsx", col_names = T) 
DM <- DM[, c("SEQN","LBXGLU","LBXIN","DM_score")]
# 对数据集中的每一列进行操作
for(col in names(DM)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(DM[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}


#### 13. 五_问卷_28体力活动 
PA <- read_excel("分析可用-PA_origin+评分版-((2011-2018)26701+33).xlsx", col_names = T) 
PA <- PA[, c("SEQN","PA_score")]  
# 对数据集中的每一列进行操作
for(col in names(PA)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(PA[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}


#### 14. 五_问卷_35睡眠障碍 
Sleep <- read_excel("分析可用-sleep-((2011-2018)25019+6).xlsx", col_names = T) 
Sleep <- Sleep[, c("SEQN","sleep_score")] 
# 对数据集中的每一列进行操作
for(col in names(Sleep)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(Sleep[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}

#### 15. 五_问卷_36吸烟-香烟使用 
Cigarette <- read_excel("分析可用-cigarettee_score-((2011-2018)23473+28).xlsx", col_names = T) 
Cigarette <- Cigarette[, c("SEQN","cigarette_score")]  
# 对数据集中的每一列进行操作
for(col in names(Cigarette)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(Cigarette[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}


#### 合并数据 
bmi <- left_join(Body_measure, BMI, by = "SEQN")  
df1 <- left_join(population, bmi, by = "SEQN")  
df2 <- left_join(df1, Diet2, by = "SEQN") 
df2 <- left_join(df2, HEI2020, by = "SEQN") 
df3 <- left_join(df2, BP, by = "SEQN") 
df4 <- left_join(df3, CHOL, by = "SEQN") 
df5 <- left_join(df4, DM, by = "SEQN") 
df6 <- left_join(df5, PA, by = "SEQN") 

for(col in names(df14)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(df14[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}

# "E:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-代码/2024.05.01_R-数据合并.R"
# write_xlsx(df6, "E:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/2024.05.21_R-数据合并-HEI-df6_39156+108.xlsx")


####  *变量筛选
####  *数据预处理-2024.05.01 
rm(list = ls())
df <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/2024.05.21_R-数据合并-HEI-df6_39156+108.xlsx", col_names = T) 

table(df$BMI.factor)

df <- df %>%
  mutate(DR1TPROT2 = BMXWT*2.2,
         DR1TPROT3 = BMXWT*0.8) # 生成一个新变量DR1TPROT2：假设这个人健身的话，蛋白质摄入上限为2.2g/kg/天

df <- df %>%
  mutate(Protein_intake.factor = case_when(
    DR1TPROT < DR1TPROT3 ~ 0,            # 蛋白质摄入量过低
    DR1TPROT > DR1TPROT3 & DR1TPROT < DR1TPROT2 ~ 1,         # 蛋白质摄入量适中
    DR1TPROT > DR1TPROT2 ~ 2,            # 蛋白质摄入量过高
    TRUE ~ 9999 ))  # 如果所有条件都不满足，设置为 NA 或其他默认值

df <- df %>%
  mutate(BMI_logistic = case_when(
    BMI.factor == "肥胖" ~ "1",
    TRUE ~ "0"))

df <- df %>%
  mutate(WTDRD1_adjust = WTDRD1*1/4)     
dim(df)  

for(col in names(df)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(df[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}

# ### 对变量排序
df <- df[, c("SEQN","survey_year",
             "RIAGENDR","RIDAGEYR","RIDRETH3",
             "RIDEXAGM","WTINT2YR","WTMEC2YR","SDMVPSU", "SDMVSTRA",
             "BMXBMI","BMXWT","WTDRD1","WTDRD1_adjust","age.factor",
             "BMI.factor","age.factor_10","BMI_logistic",
             "Protein_intake.factor","DR1TPROT2","DR1TPROT3",
             "DR1TKCAL","DR1TPROT","DR1TCARB","DR1TSUGR","DR1TFIBE",
             "DR1TTFAT","DR1TSFAT","DR1TMFAT","DR1TPFAT","DR1TCHOL",
             "DR1TRET","DR1TVARA","DR1TACAR","DR1TBCAR", "DR1TCRYP",
             "DR1TLYCO","DR1TLZ","DR1TVB1","DR1TVB2","DR1TNIAC",
             "DR1TVB6","DR1TFOLA","DR1TFA","DR1TFF","DR1TFDFE",
             "DR1TVB12","DR1TVC","DR1TVK","DR1TCALC", "DR1TPHOS",
             "DR1TMAGN","DR1TIRON","DR1TZINC","DR1TCOPP","DR1TSODI",
             "DR1TPOTA","DR1TSELE","DR1TCAFF","DR1TTHEO","DR1TALCO",
             "DR1TMOIS","DR1TS040","DR1TS060","DR1TS100", "DR1TS120",
             "DR1TS140","DR1TS160","DR1TS180","DR1TM181","DR1TP183",
             "DR1TP184","DR1TP204","DR1TP205","DR1TP226",
             "BP_score","PA_score","DM_score","CHOL_score",
             
             "HEI2020_ALL","HEI2020_TOTALFRT","HEI2020_FRT","HEI2020_VEG",
             "HEI2020_GREENNBEAN","HEI2020_TOTALPRO","HEI2020_SEAPLANTPRO",
             "HEI2020_WHOLEGRAIN",  "HEI2020_DAIRY","HEI2020_FATTYACID",
             "HEI2020_REFINEDGRAIN","HEI2020_SODIUM","HEI2020_ADDEDSUGAR",  
             "HEI2020_SATFAT")]

population <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/一_人口统计数据_2011-2018(39156+43).xlsx", col_names = T) 
table(population$survey_year)
population <- population[, c("SEQN","RIDEXPRG")]


cancer <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/五_问卷_23医疗条件_2011-2018(37606+39).xlsx", col_names = T) 
cancer <- cancer[cancer$survey_year > 6 | cancer$survey_year == 10, ]  
table(cancer$survey_year)
cancer <- cancer[, c("SEQN","MCQ220","MCQ370A","MCQ370B","MCQ370C","MCQ370D")]
cancer <- cancer[, c("SEQN","MCQ220")]

# RIDEXPRG - Pregnancy status at exam
# RIDEXPRG - 检查时的怀孕状况

# MCQ220 - Ever told you had cancer or malignancy
# MCQ220 - 曾经告诉过您患有癌症或恶性肿瘤

# MCQ370A - Are you now controlling or losing weight
# MCQ370A - 您现在正在控制体重还是正在减肥

# MCQ370B - Are you now increasing exercise
# MCQ370B - 您现在增加锻炼吗

# MCQ370C - Are you now reducing salt in diet
# MCQ370C - 您现在正在减少饮食中的盐吗

# MCQ370D - Are you now reducing fat in diet
# MCQ370D - 您现在正在减少饮食中的脂肪吗

df <- left_join(df, population, by = "SEQN") 
df <- left_join(df, cancer, by = "SEQN") 

for(col in names(df)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(df[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}

table(df$RIDEXPRG)
table(df$MCQ220)

df <- df %>%
  mutate(RIDEXPRG = case_when(
    RIDEXPRG ==1 ~ 1,            # 处于怀孕状态
    TRUE ~ 0 ))

df <- df %>%
  mutate(MCQ220 = case_when(
    MCQ220  == 1 ~ 1,            # 曾被医生或其他健康专业人士告知患有任何类型的癌症或恶性肿瘤 
    TRUE ~ 0))

# 去除不含BMI的样本
df <- df[complete.cases(df$BMI.factor), ]

# 去除不含饮食信息的样本
df <- df[complete.cases(df$DR1TKCAL), ]

# 去除不含HEI2020_SATFAT的样本
df <- df[complete.cases(df$HEI2020_SATFAT), ] 

table(df$RIDEXPRG)
table(df$MCQ220)

df_1 <- df[df$RIDEXPRG == 0, ]  
table(df_1$RIDEXPRG)

table(df_1$MCQ220)
df_1.2 <- df_1[df_1$MCQ220 == 0, ]  

# 不去除意图减肥人群-23237-实际生成版本
for(col in names(df_1.2)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(df_1.2[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}

population <- read_excel("E:/0-分析数据合集/1-NHANES数据库挖掘-2023.12.25/数据-2023.12.25/Output file-2024.02.15/一_人口统计数据_2011-2018(39156+43).xlsx", col_names = T) 
table(population$survey_year)
population$PIR.factor <- ifelse(population$INDFMPIR < 1.3, 1, 0)
population <- population[, c("SEQN","INDFMPIR","PIR.factor")]

df_1.2 <- left_join(df_1.2, population, by = "SEQN")
table(df_1.2$BMI.factor)

df_1.7 <- df_1.2[complete.cases(df_1.2$INDFMPIR), ] 

for(col in names(df_1.7)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(df_1.7[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}

df_1.7 <- df_1.7 %>%
  dplyr::select(-RIDEXPRG,-MCQ220,
                -DM_score,-CHOL_score,-BP_score,-PA_score)

table(df_1.7$BMI.factor)
df_1.7 <- df_1.7[df_1.7$BMI.factor != "重度消瘦", ]    # 去除8个重度消瘦

# 将经过上述步骤生成的数据文件另存为excel表格文件
# write_xlsx(df_1.7, "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/正式分析-2024.05.06/1-2024.05.21_R-正式数据-(不去除意图减肥人群）23237+91.xlsx")

## 计算研究人群的加权后总人数 
df <- df_1.7
# 对一些人口统计学变量进行修改
# 定义数据预处理函数
preprocess_data <- function(df) {
  df <- df %>%
    mutate(RIAGENDR = recode(RIAGENDR, 
                             `1` = "Male", 
                             `2` = "Female"))
  df$RIAGENDR <- factor(df$RIAGENDR, 
                        levels = c("Male","Female"))
  df <- df %>%
    mutate( age.factor = case_when(
      RIDAGEYR >= 0 & RIDAGEYR <= 5 ~ "0-5 years",
      RIDAGEYR > 5 & RIDAGEYR <= 19 ~ "6-19 years",
      RIDAGEYR > 19 & RIDAGEYR <= 59 ~ "20-59 years",
      RIDAGEYR > 59 & RIDAGEYR <= 80 ~ "60+ years")) 
  df$age.factor <- factor(df$age.factor, 
                          levels = c("0-5 years",
                                     "6-19 years",
                                     "20-59 years",
                                     "60+ years"))
  df <- df %>%
    mutate( RIDRETH3 = case_when(
      RIDRETH3 == 1 ~ "Mexican American",
      RIDRETH3 == 2 ~ "Other Hispanic",
      RIDRETH3 == 3 ~ "Non-Hispanic White",
      RIDRETH3 == 4 ~ "Non-Hispanic Black",
      RIDRETH3 == 6 ~ "Non-Hispanic Asian",
      RIDRETH3 == 7 ~ "Other/Multi-Racial")) 
  df$RIDRETH3 <- factor(df$RIDRETH3, 
                        levels = c("Mexican American",
                                   "Non-Hispanic White",
                                   "Non-Hispanic Black",
                                   "Non-Hispanic Asian",
                                   "Other Hispanic",
                                   "Other/Multi-Racial"))
  df <- df %>%
    mutate(BMI.factor = recode(BMI.factor, 
                               `偏瘦` = "消瘦"))
  df <- df %>%
    mutate(BMI.factor = recode(BMI.factor, 
                               `肥胖` = "Obesity", 
                               `超重` = "Overweight", 
                               `正常` = "Normal Weight", 
                               `消瘦` = "Underweight"))
  df$BMI.factor <- factor(df$BMI.factor, 
                          levels = c("Underweight",
                                     "Normal Weight",
                                     "Overweight",
                                     "Obesity"))
  df <- df %>%
    mutate( NHANES_cycle = case_when(
      survey_year == 7 ~ "2011-2012",
      survey_year == 8 ~ "2013-2014",
      survey_year == 9 ~ "2015-2016",
      survey_year == 10 ~ "2017-2018")) 
  df$NHANES_cycle <- factor(df$NHANES_cycle, 
                            levels = c("2011-2012",
                                       "2013-2014",
                                       "2015-2016",
                                       "2017-2018"))
  df <- df %>%
    mutate( PIR.factor2 = case_when(
      INDFMPIR <= 1.30 ~ "≤ 1.30",
      INDFMPIR > 1.30 & INDFMPIR <= 3.50 ~ "1.31-3.50",
      INDFMPIR > 3.50 ~ "> 3.50")) 
  df$PIR.factor2 <- factor(df$PIR.factor2, 
                           levels = c("≤ 1.30",
                                      "1.31-3.50",
                                      "> 3.50"))
  df <- df %>%
    mutate( BMI.logistic2 = case_when(
      BMI.factor == "Obesity" ~ 1,
      BMI.factor == "Overweight" ~ 1,
      BMI.factor == "Normal Weight" ~ 0,
      BMI.factor == "Underweight" ~ 0)) 
  
  df <- df %>%
    mutate( PROT = DR1TPROT/BMXWT) 
  
  varsToFactor <- c("survey_year","RIAGENDR","RIDRETH3",
                    "age.factor","BMI.factor","NHANES_cycle",
                    "age.factor_10","BMI_logistic","BMI.logistic2",
                    "PIR.factor","PIR.factor2","Protein_intake.factor")
  df[varsToFactor] <- lapply(df[varsToFactor], factor) 
  df$survey_year <- factor(df$survey_year)
  df$NHANES_cycle <- factor(df$NHANES_cycle)
  df$RIAGENDR <- factor(df$RIAGENDR)
  df$RIDRETH3 <- factor(df$RIDRETH3)
  df$age.factor <- factor(df$age.factor)
  df$BMI.factor <- factor(df$BMI.factor)
  df$BMI_logistic <- factor(df$BMI_logistic)
  df$BMI.logistic2 <- factor(df$BMI.logistic2)
  df$PIR.factor <- factor(df$PIR.factor)
  df$PIR.factor2 <- factor(df$PIR.factor2)
  df$Protein_intake.factor <- factor(df$Protein_intake.factor)
  
  return(df)
}

df <- preprocess_data(df)

# 创建复杂设计
NHANES_design_df <- svydesign(
  data = df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust") # 可以加上 survey.lonely.psu = "adjust" 避免1个PSU报错

summary(NHANES_design_df)

# 计算数据集的加权总人数
total_weighted_population <- df %>%
  summarise(total_weighted_population = sum(WTDRD1_adjust, na.rm = TRUE))
total_weighted_population

# 假设你的计算结果存储在 total_weighted_population 变量中
total_weighted_population <- sprintf("%.2f", total_weighted_population)
total_weighted_population

# 怎么用R计算data数据集中根据BMI.logistic2进行分组的加权人数
# 假设你的数据框叫做 df，BMI.logistic2 是一个分类变量
library(plyr)
# 按照 BMI.logistic2 分组并计算加权人数
grouped_data <- ddply(df, .(BMI.logistic2), summarise,
                      weighted_count = sum(WTDRD1_adjust, na.rm = TRUE))
# 打印结果
print(grouped_data)




#  3. Table 1 人口特征基本描述 ####
rm(list = ls())
setwd("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存")
library(readxl)
library(randomForest)
library(dplyr)
library(Hmisc)
library(foreign)
library(tidyverse)
library(writexl)
library(readr)
library(broom)
library(xtable)
library(autoReg)
library(ggstatsplot)
library(ggplot2)
library(gtsummary)  # 用于tbl_regression 函数
library(tidyr) # drop_na 函数，用于快速去掉 NA
library(survey)
library(haven)
library(openxlsx)
library(tableone)
library(MatchIt)
library(rms)
library(reshape2)
library(plotRCS)
library(rrtable)
library(quantreg)
library(gWQS)
library(ggplot2)
library(epiDisplay)
library(officer)
library(flextable)
library(nortest)

df <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/正式分析-2024.05.06/1-2024.05.21_R-正式数据-(不去除意图减肥人群）23237+91.xlsx",col_names = T)

# 查看样本变量缺失情况
for(col in names(df)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(df[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}

# 对一些人口统计学变量进行修改
df <- df %>%
  mutate(RIAGENDR = recode(RIAGENDR, 
                           `1` = "Male", 
                           `2` = "Female"))
df$RIAGENDR <- factor(df$RIAGENDR, 
                      levels = c("Male","Female"))
df <- df %>%
  mutate( age.factor = case_when(
    RIDAGEYR >= 0 & RIDAGEYR <= 5 ~ "0-5 years",
    RIDAGEYR > 5 & RIDAGEYR <= 19 ~ "6-19 years",
    RIDAGEYR > 19 & RIDAGEYR <= 59 ~ "20-59 years",
    RIDAGEYR > 59 & RIDAGEYR <= 80 ~ "60+ years")) 
df$age.factor <- factor(df$age.factor, 
                        levels = c("0-5 years",
                                   "6-19 years",
                                   "20-59 years",
                                   "60+ years"))
df <- df %>%
  mutate( RIDRETH3 = case_when(
    RIDRETH3 == 1 ~ "Mexican American",
    RIDRETH3 == 2 ~ "Other Hispanic",
    RIDRETH3 == 3 ~ "Non-Hispanic White",
    RIDRETH3 == 4 ~ "Non-Hispanic Black",
    RIDRETH3 == 6 ~ "Non-Hispanic Asian",
    RIDRETH3 == 7 ~ "Other/Multi-Racial")) 
df$RIDRETH3 <- factor(df$RIDRETH3, 
                      levels = c("Mexican American",
                                 "Non-Hispanic White",
                                 "Non-Hispanic Black",
                                 "Non-Hispanic Asian",
                                 "Other Hispanic",
                                 "Other/Multi-Racial"))
df <- df %>%
  mutate(BMI.factor = recode(BMI.factor, 
                             `偏瘦` = "消瘦"))
df <- df %>%
  mutate(BMI.factor = recode(BMI.factor, 
                             `肥胖` = "Obesity", 
                             `超重` = "Overweight", 
                             `正常` = "Normal Weight", 
                             `消瘦` = "Underweight"))
df$BMI.factor <- factor(df$BMI.factor, 
                        levels = c("Underweight",
                                   "Normal Weight",
                                   "Overweight",
                                   "Obesity"))
table(df$BMI.factor)
df <- df %>%
  mutate( NHANES_cycle = case_when(
    survey_year == 7 ~ "2011-2012",
    survey_year == 8 ~ "2013-2014",
    survey_year == 9 ~ "2015-2016",
    survey_year == 10 ~ "2017-2018")) 
df$NHANES_cycle <- factor(df$NHANES_cycle, 
                          levels = c("2011-2012",
                                     "2013-2014",
                                     "2015-2016",
                                     "2017-2018"))
df <- df %>%
  mutate( PIR.factor2 = case_when(
    INDFMPIR <= 1.30 ~ "≤ 1.30",
    INDFMPIR > 1.30 & INDFMPIR <= 3.50 ~ "1.31-3.50",
    INDFMPIR > 3.50 ~ "> 3.50")) 
df$PIR.factor2 <- factor(df$PIR.factor2, 
                         levels = c("≤ 1.30",
                                    "1.31-3.50",
                                    "> 3.50"))


df <- df %>%
  mutate( BMI.logistic2 = case_when(
    BMI.factor == "Obesity" ~ 1,
    BMI.factor == "Overweight" ~ 1,
    BMI.factor == "Normal Weight" ~ 0,
    BMI.factor == "Underweight" ~ 0)) 

df <- df %>%
  mutate( PROT = DR1TPROT/BMXWT) 

# 计算超重率和肥胖率
data <- df
data <- df[df$RIDAGEYR < 20, ]   # 0-19岁的儿童青少年
data <- df[df$RIDAGEYR >= 20, ]  # 20+岁的成人 
table(data$BMI.factor)

## 1.数据预处理——分类变量因子化
varsToFactor <- c("survey_year","RIAGENDR","RIDRETH3",
                  "age.factor","BMI.factor","NHANES_cycle",
                  "age.factor_10","BMI_logistic","BMI.logistic2",
                  "PIR.factor","PIR.factor2","Protein_intake.factor")
df[varsToFactor] <- lapply(df[varsToFactor], factor) 
df$survey_year <- factor(df$survey_year)
df$NHANES_cycle <- factor(df$NHANES_cycle)
df$RIAGENDR <- factor(df$RIAGENDR)
df$RIDRETH3 <- factor(df$RIDRETH3)
df$age.factor <- factor(df$age.factor)
df$BMI.factor <- factor(df$BMI.factor)
df$BMI_logistic <- factor(df$BMI_logistic)
df$BMI.logistic2 <- factor(df$BMI.logistic2)
df$PIR.factor <- factor(df$PIR.factor)
df$PIR.factor2 <- factor(df$PIR.factor2)
df$Protein_intake.factor <- factor(df$Protein_intake.factor)

NHANES_design1 <- svydesign(
  data = df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust") # 可以加上 survey.lonely.psu = "adjust" 避免1个PSU报错

summary(NHANES_design1)

# *1.绘制Table 1
# BMI.logistic2(0=消瘦/正常，1=超重/肥胖）-复杂抽样下的 Table1
table1 <- tbl_svysummary(NHANES_design1, by = BMI.logistic2, missing = 'no',
                         include = c(RIDAGEYR,BMXBMI,NHANES_cycle,RIAGENDR,age.factor,RIDRETH3,PIR.factor2))%>%    
  add_p() 
print(table1)
# 将table1转换为数据框
table1_df <- as.data.frame(table1)
# 然后创建flextable对象
ft <- flextable(table1_df)
# 接下来的步骤与之前相同
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
# print(doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/正文表格输出保存-2024.05.14/HEI2020-2024.05.21/table1.docx")

#### 1.1 批量加权差异性分析-Table1-保留两位小数
#分析协变量定义
vars<-c("RIDAGEYR", "BMXBMI","NHANES_cycle","RIAGENDR","age.factor",
        "RIDRETH3","PIR.factor2")
#分类变量定义为fvars
fvars<-c("NHANES_cycle","RIAGENDR","age.factor","RIDRETH3","PIR.factor2")
# BMI.logistic2(0=消瘦/正常，1=超重/肥胖）
Svytab1.3<- svyCreateTableOne(vars = vars,
                              strata = "BMI.logistic2", data =NHANES_design1,
                              factorVars =fvars)
print(Svytab1.3,showAllLevels = T)

# 4. 绘制HEI2020-相关性热图 ####
names(df)
hotplot <-data[,76:89]
#1 corrplot包，corrplot.mixed 相关系数;
pacman::p_load(corrplot)
cor.matrix <- cor(hotplot, method = "spearman")  
#绘制热力图
corrplot.mixed(cor.matrix, number.cex = 0.8)  

corrplot.mixed(cor.matrix, number.cex = 0.8, tl.pos = "lt")  # 在左侧和顶部添加变量标签
corrplot.mixed(cor.matrix, number.cex = 0.8, tl.pos = "lt", tl.srt = 45)  # 使用tl.srt参数来设置文本的旋转角度
corrplot.mixed(cor.matrix, number.cex = 0.8, tl.pos = "lt", tl.srt = 45, tl.col = "black")  # 将左侧和顶部的标签转换为黑色

colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", 
                   "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))


# 创建颜色渐变
color_palette <- colorRampPalette(c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0","#FFFFFF", "#FDDBC7"
                                    , "#F4A582", "#D6604D",'#B2182B',"#67001F"))

# & 相关性热图 应用颜色渐变到corrplot.mixed函数 - 正式适用版本-2024.05.22
corrplot.mixed(cor.matrix,upper = "number",lower = "ellipse",
               number.cex = 0.8, tl.pos = "lt", tl.srt = 45, tl.col = "black",
               lower.col = color_palette(300), upper.col = color_palette(300))

# 5. 绘制table2 ####
# 修改tbl_svysummary()或tbl_summary()函数，指定统计类型为'continuous2'
library(gtsummary)
# 3.BMI.logistic2  1-超重/肥胖
table2 <- tbl_svysummary(
  data = NHANES_design1,
  by = BMI.logistic2,
  type = list(all_continuous() ~ "continuous2"),
  statistic = all_continuous() ~ c(
    "{mean} ({sd}) / {median} ({p25}, {p75})"
  ),
  digits = all_continuous() ~ c(2, 2, 2, 2, 2),  # 设置小数点位数
  include = c("HEI2020_ALL","HEI2020_TOTALFRT","HEI2020_FRT",
              "HEI2020_VEG","HEI2020_GREENNBEAN","HEI2020_TOTALPRO",
              "HEI2020_SEAPLANTPRO","HEI2020_WHOLEGRAIN","HEI2020_DAIRY",        
              "HEI2020_FATTYACID","HEI2020_REFINEDGRAIN",
              "HEI2020_SODIUM","HEI2020_ADDEDSUGAR","HEI2020_SATFAT")
) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))  # 添加P值
print(table2)

##保存表格
# 将table1转换为数据框
table1_df <- as.data.frame(table2)
# 然后创建flextable对象
table2.ft <- flextable(table1_df)
# 接下来的步骤与之前相同
table2.doc <- read_docx()
table2.doc <- body_add_flextable(table2.doc, value = table2.ft)
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.1-HEI2020的基本描述.docx")

table2 <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.1-HEI2020的基本描述.xlsx",sheet="Sheet1",col_names = T)  
table2 <- table2 %>%
  separate("**0**, N = 86,329,529", into = c("0-Mean (SD)", "0-Median (IQR)"), sep = " / ") %>%
  separate("**1**, N = 140,502,036", into = c("1-Mean (SD)", "1-Median (IQR)"), sep = " / ")

# 将table1转换为数据框
table1_df <- as.data.frame(table2)
# 然后创建flextable对象
table2.ft <- flextable(table1_df)
# 接下来的步骤与之前相同
table2.doc <- read_docx()
table2.doc <- body_add_flextable(table2.doc, value = table2.ft)
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.2-HEI2020的基本描述.docx")

# Table2 
#### 分人群基本描述 
data <- df
data <- df[df$RIAGENDR=="Male", ]
data <- df[df$RIAGENDR=="Female", ]

table(df$age.factor)
data <- df[df$age.factor=="0-5 years", ]
data <- df[df$age.factor=="6-19 years", ]
data <- df[df$age.factor=="20-59 years", ]
data <- df[df$age.factor=="60+ years", ]
## 数据预处理——分类变量因子化
varsToFactor <- c("survey_year","RIAGENDR","RIDRETH3",
                  "age.factor","BMI.factor","NHANES_cycle",
                  "age.factor_10","BMI_logistic","BMI.logistic2",
                  "PIR.factor","PIR.factor2","Protein_intake.factor")
data[varsToFactor] <- lapply(data[varsToFactor], factor) 
data$survey_year <- factor(data$survey_year)
data$NHANES_cycle <- factor(data$NHANES_cycle)
data$RIAGENDR <- factor(data$RIAGENDR)
data$RIDRETH3 <- factor(data$RIDRETH3)
data$age.factor <- factor(data$age.factor)
data$BMI.factor <- factor(data$BMI.factor)
data$BMI_logistic <- factor(data$BMI_logistic)
data$BMI.logistic2 <- factor(data$BMI.logistic2)
data$PIR.factor <- factor(data$PIR.factor)
data$PIR.factor2 <- factor(data$PIR.factor2)
data$Protein_intake.factor <- factor(data$Protein_intake.factor)

NHANES_design2 <- svydesign(
  data = data, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust") # 可以加上 survey.lonely.psu = "adjust" 避免1个PSU报错

summary(NHANES_design2)

#### table2
library(gtsummary)

# 识别只有一个PSU的分层
strata_with_single_psu <- c(134,135,140,141,143,147,148)

# 从数据集中删除这些分层
data.2 <- data[!data$SDMVSTRA %in% strata_with_single_psu, ]

# 更新调查设计
NHANES_design2 <- svydesign(
  data = data.2, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTINT2YR,
  survey.lonely.psu = "adjust") # 可以加上 survey.lonely.psu = "adjust" 避免1个PSU报错

summary(NHANES_design2)


# 6. 雷达图绘图数据整理 ####
table2 <- tbl_svysummary(
  data = NHANES_design2,
  by = BMI.logistic2,
  type = list(all_continuous() ~ "continuous2"),
  statistic = all_continuous() ~ c(
    "{median}"
  ),
  digits = all_continuous() ~ c(4, 4, 4, 4, 4),  # 设置小数点位数
  include = c("HEI2020_ALL","HEI2020_TOTALFRT","HEI2020_FRT",
              "HEI2020_VEG","HEI2020_GREENNBEAN","HEI2020_TOTALPRO",
              "HEI2020_SEAPLANTPRO","HEI2020_WHOLEGRAIN","HEI2020_DAIRY",        
              "HEI2020_FATTYACID","HEI2020_REFINEDGRAIN",
              "HEI2020_SODIUM","HEI2020_ADDEDSUGAR","HEI2020_SATFAT")
) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))  # 添加P值
print(table2)
#保存表格
# 将table1转换为数据框
table1_df <- as.data.frame(table2)
# 然后创建flextable对象
table2.ft <- flextable(table1_df)
# 接下来的步骤与之前相同
table2.doc <- read_docx()
table2.doc <- body_add_flextable(table2.doc, value = table2.ft)
# 雷达图-绘图数据文件保存
print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.3-2024.05.27.docx")

# 3.BMI.logistic2
table2 <- tbl_svysummary(
  data = NHANES_design2,
  by = BMI.logistic2,
  type = list(all_continuous() ~ "continuous2"),
  statistic = all_continuous() ~ c(
    "{mean} ({sd}) / {median} ({p25}, {p75})"
  ),
  digits = all_continuous() ~ c(2, 2, 2, 2, 2),  # 设置小数点位数
  include = c("HEI2020_ALL","HEI2020_TOTALFRT","HEI2020_FRT",
              "HEI2020_VEG","HEI2020_GREENNBEAN","HEI2020_TOTALPRO",
              "HEI2020_SEAPLANTPRO","HEI2020_WHOLEGRAIN","HEI2020_DAIRY",        
              "HEI2020_FATTYACID","HEI2020_REFINEDGRAIN",
              "HEI2020_SODIUM","HEI2020_ADDEDSUGAR","HEI2020_SATFAT")
) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))  # 添加P值
print(table2)

##保存表格
# 将table1转换为数据框
table1_df <- as.data.frame(table2)
# 然后创建flextable对象
table2.ft <- flextable(table1_df)
# 接下来的步骤与之前相同
table2.doc <- read_docx()
table2.doc <- body_add_flextable(table2.doc, value = table2.ft)
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.1-HEI2020的基本描述.docx")
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.1_0-5岁-HEI2020的基本描述.docx")
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.1_6-19岁-HEI2020的基本描述.docx")
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.1_20-59岁-HEI2020的基本描述.docx")
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.1_60+岁-HEI2020的基本描述.docx")

# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.1_male-HEI2020的基本描述.docx")
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.1_female-HEI2020的基本描述.docx")
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.1_survey_year-HEI2020的基本描述.docx")

table2 <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.1_female-HEI2020的基本描述.xlsx",sheet="Sheet1",col_names = T)
# 新的变量名叫做var1和var2
table2 <- table2 %>%
  separate("**0**, N = 46,373,593", into = c("0-Mean (SD)", "0-Median (IQR)"), sep = " / ") %>%
  separate("**1**, N = 68,940,045", into = c("1-Mean (SD)", "1-Median (IQR)"), sep = " / ")

# 将table1转换为数据框
table1_df <- as.data.frame(table2)
# 然后创建flextable对象
table2.ft <- flextable(table1_df)
# 接下来的步骤与之前相同
table2.doc <- read_docx()
table2.doc <- body_add_flextable(table2.doc, value = table2.ft)
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.2_0-5岁-HEI2020的基本描述.docx")
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.2_6-19岁-HEI2020的基本描述.docx")
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.2_20-59岁-HEI2020的基本描述.docx")
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.2_60+岁-HEI2020的基本描述.docx")

# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.2_male-HEI2020的基本描述.docx")
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.2_female-HEI2020的基本描述.docx")
# print(table2.doc, target = "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存/table2.2_survey_year-HEI2020的基本描述.docx")



# 7. 绘制雷达图  ####
rm(list = ls())
setwd("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/雷达图绘制数据-2024.05.24")
library(ggradar)
# 下方为载入代码，R包需提前安装
library(ggplot2) 
library(dplyr) 
library(tidyr) 
library(scales)
library(fmsb)
library(readxl)
# install.packages("cowplot")
library(cowplot)

# 1.全人群
radar <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/雷达图绘制数据-2024.05.24/雷达图HEI2020-全人群.xlsx", 
                    sheet="全人群",col_names = T) 
radar <- as.data.frame(radar)
# 将第一列设置为行名
rownames(radar) <- radar[,1]
# 删除现在已经成为行名的第一列
radar <- radar[,-1]

#设定每个定量变量的最大值和最小值
radar <- rbind(rep(10,1) , rep(0,1) , radar)

#每个分组的颜色向量
colors_border=c( rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9))
colors_in=c(rgb(0.8,0.2,0.5,0.4),rgb(0.2,0.5,0.5,0.4))


# 图例在上方
par(mar=c(0,0,5,3)) # 您可以调整这些值以为标签提供更多空间
radarchart( radar , axistype=1,
            pcol=colors_border, 
            pfcol=colors_in, 
            plwd=2 , plty=1,
            cglcol="grey", cglty=1, 
            axislabcol="grey", 
            caxislabels=seq(0,10,2.5), 
            cglwd=0.8,
            vlcex=1.5)
# 添加图例
legend(x=-0.75, y=1.35, 
       legend=rownames(radar[-c(1,2),]),
       bty = "n", 
       col=colors_in, 
       pch=20, cex=1, 
       pt.cex=2, 
       horiz=TRUE)

# 2.全人群-男性 
rm(list = ls())
radar <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/雷达图绘制数据-2024.05.24/雷达图HEI2020-全人群.xlsx", 
                    sheet="全人群-男",col_names = T) 
radar <- as.data.frame(radar)
# 将第一列设置为行名
rownames(radar) <- radar[,1]
# 删除现在已经成为行名的第一列
radar <- radar[,-1]

#设定每个定量变量的最大值和最小值
radar <- rbind(rep(10,1) , rep(0,1) , radar)

#每个分组的颜色向量
colors_border=c( rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9))
colors_in=c(rgb(0.8,0.2,0.5,0.4),rgb(0.2,0.5,0.5,0.4))

par(mar=c(0,0,5,3)) # 您可以调整这些值以为标签提供更多空间
radarchart( radar , axistype=1,
            pcol=colors_border, 
            pfcol=colors_in, 
            plwd=2 , plty=1,
            cglcol="grey", cglty=1, 
            axislabcol="grey", 
            caxislabels=seq(0,10,2.5), 
            cglwd=0.8,
            vlcex=1.5)

# 3.全人群-女性
rm(list = ls())
radar <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/雷达图绘制数据-2024.05.24/雷达图HEI2020-全人群.xlsx", 
                    sheet="全人群-女",col_names = T) 
radar <- as.data.frame(radar)
# 将第一列设置为行名
rownames(radar) <- radar[,1]
# 删除现在已经成为行名的第一列
radar <- radar[,-1]

#设定每个定量变量的最大值和最小值
radar <- rbind(rep(10,1) , rep(0,1) , radar)

#每个分组的颜色向量
colors_border=c( rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9))
colors_in=c(rgb(0.8,0.2,0.5,0.4),rgb(0.2,0.5,0.5,0.4))

par(mar=c(0,0,5,3)) # 您可以调整这些值以为标签提供更多空间
radarchart( radar , axistype=1,
            pcol=colors_border, 
            pfcol=colors_in, 
            plwd=2 , plty=1,
            cglcol="grey", cglty=1, 
            axislabcol="grey", 
            caxislabels=seq(0,10,2.5), 
            cglwd=0.8,
            vlcex=1.5)

# 4. 0-5岁
rm(list = ls())
radar <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/雷达图绘制数据-2024.05.24/雷达图HEI2020-全人群.xlsx", 
                    sheet="0-5岁",col_names = T) 
radar <- as.data.frame(radar)
# 将第一列设置为行名
rownames(radar) <- radar[,1]
# 删除现在已经成为行名的第一列
radar <- radar[,-1]

#设定每个定量变量的最大值和最小值
radar <- rbind(rep(10,1) , rep(0,1) , radar)

#每个分组的颜色向量
colors_border=c( rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9))
colors_in=c(rgb(0.8,0.2,0.5,0.4),rgb(0.2,0.5,0.5,0.4))

par(mar=c(0,0,5,3)) # 您可以调整这些值以为标签提供更多空间
radarchart( radar , axistype=1,
            pcol=colors_border, 
            pfcol=colors_in, 
            plwd=2 , plty=1,
            cglcol="grey", cglty=1, 
            axislabcol="grey", 
            caxislabels=seq(0,10,2.5), 
            cglwd=0.8,
            vlcex=1.5)

# 5. 6-19岁 
rm(list = ls())
radar <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/雷达图绘制数据-2024.05.24/雷达图HEI2020-全人群.xlsx", 
                    sheet="6-19岁",col_names = T) 
radar <- as.data.frame(radar)
# 将第一列设置为行名
rownames(radar) <- radar[,1]
# 删除现在已经成为行名的第一列
radar <- radar[,-1]

#设定每个定量变量的最大值和最小值
radar <- rbind(rep(10,1) , rep(0,1) , radar)

#每个分组的颜色向量
colors_border=c( rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9))
colors_in=c(rgb(0.8,0.2,0.5,0.4),rgb(0.2,0.5,0.5,0.4))

par(mar=c(0,0,5,3)) # 您可以调整这些值以为标签提供更多空间
radarchart( radar , axistype=1,
            pcol=colors_border, 
            pfcol=colors_in, 
            plwd=2 , plty=1,
            cglcol="grey", cglty=1, 
            axislabcol="grey", 
            caxislabels=seq(0,10,2.5), 
            cglwd=0.8,
            vlcex=1.5)

# 6. 20-59岁 
rm(list = ls())
radar <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/雷达图绘制数据-2024.05.24/雷达图HEI2020-全人群.xlsx", 
                    sheet="20-59岁",col_names = T) 
radar <- as.data.frame(radar)
# 将第一列设置为行名
rownames(radar) <- radar[,1]
# 删除现在已经成为行名的第一列
radar <- radar[,-1]

#设定每个定量变量的最大值和最小值
radar <- rbind(rep(10,1) , rep(0,1) , radar)

#每个分组的颜色向量
colors_border=c( rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9))
colors_in=c(rgb(0.8,0.2,0.5,0.4),rgb(0.2,0.5,0.5,0.4))

par(mar=c(0,0,5,3)) # 您可以调整这些值以为标签提供更多空间
radarchart( radar , axistype=1,
            pcol=colors_border, 
            pfcol=colors_in, 
            plwd=2 , plty=1,
            cglcol="grey", cglty=1, 
            axislabcol="grey", 
            caxislabels=seq(0,10,2.5), 
            cglwd=0.8,
            vlcex=1.5)

# 7. 60+岁 
rm(list = ls())
radar <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/雷达图绘制数据-2024.05.24/雷达图HEI2020-全人群.xlsx", 
                    sheet="60+",col_names = T) 
radar <- as.data.frame(radar)
# 将第一列设置为行名
rownames(radar) <- radar[,1]
# 删除现在已经成为行名的第一列
radar <- radar[,-1]

#设定每个定量变量的最大值和最小值
radar <- rbind(rep(10,1) , rep(0,1) , radar)

#每个分组的颜色向量
colors_border=c( rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9))
colors_in=c(rgb(0.8,0.2,0.5,0.4),rgb(0.2,0.5,0.5,0.4))

par(mar=c(0,0,5,3)) # 您可以调整这些值以为标签提供更多空间
radarchart( radar , axistype=1,
            pcol=colors_border, 
            pfcol=colors_in, 
            plwd=2 , plty=1,
            cglcol="grey", cglty=1, 
            axislabcol="grey", 
            caxislabels=seq(0,10,2.5), 
            cglwd=0.8,
            vlcex=1.5)


# 8. Logistic回归分析  ####
rm(list = ls())
setwd("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存")
library(readxl)
library(randomForest)
library(dplyr)
library(Hmisc)
library(foreign)
library(tidyverse)
library(writexl)
library(readr)
library(broom)
library(xtable)
library(autoReg)
library(ggstatsplot)
library(ggplot2)
library(gtsummary)  # 用于tbl_regression 函数
library(tidyr) # drop_na 函数，用于快速去掉 NA
library(survey)
library(haven)
library(openxlsx)
library(tableone)
library(MatchIt)
library(rms)
library(reshape2)
library(plotRCS)
library(rrtable)
library(quantreg)
library(gWQS)
library(ggplot2)
library(epiDisplay)
library(officer)
library(flextable)
library(nortest)
library(autoReg)

options(scipen = 999) 
###########  *一、 数据变量预处理-2024.05.16
df <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/正式分析-2024.05.06/1-2024.05.21_R-正式数据-(不去除意图减肥人群）23237+91.xlsx",col_names = T)

# 对一些人口统计学变量进行修改
# 定义数据预处理函数
preprocess_data <- function(df) {
  df <- df %>%
    mutate(RIAGENDR = recode(RIAGENDR, 
                             `1` = "Male", 
                             `2` = "Female"))
  df$RIAGENDR <- factor(df$RIAGENDR, 
                        levels = c("Male","Female"))
  df <- df %>%
    mutate( age.factor = case_when(
      RIDAGEYR >= 0 & RIDAGEYR <= 5 ~ "0-5 years",
      RIDAGEYR > 5 & RIDAGEYR <= 19 ~ "6-19 years",
      RIDAGEYR > 19 & RIDAGEYR <= 59 ~ "20-59 years",
      RIDAGEYR > 59 & RIDAGEYR <= 80 ~ "60+ years")) 
  df$age.factor <- factor(df$age.factor, 
                          levels = c("0-5 years",
                                     "6-19 years",
                                     "20-59 years",
                                     "60+ years"))
  df <- df %>%
    mutate( RIDRETH3 = case_when(
      RIDRETH3 == 1 ~ "Mexican American",
      RIDRETH3 == 2 ~ "Other Hispanic",
      RIDRETH3 == 3 ~ "Non-Hispanic White",
      RIDRETH3 == 4 ~ "Non-Hispanic Black",
      RIDRETH3 == 6 ~ "Non-Hispanic Asian",
      RIDRETH3 == 7 ~ "Other/Multi-Racial")) 
  df$RIDRETH3 <- factor(df$RIDRETH3, 
                        levels = c("Mexican American",
                                   "Non-Hispanic White",
                                   "Non-Hispanic Black",
                                   "Non-Hispanic Asian",
                                   "Other Hispanic",
                                   "Other/Multi-Racial"))
  df <- df %>%
    mutate(BMI.factor = recode(BMI.factor, 
                               `偏瘦` = "消瘦"))
  df <- df %>%
    mutate(BMI.factor = recode(BMI.factor, 
                               `肥胖` = "Obesity", 
                               `超重` = "Overweight", 
                               `正常` = "Normal Weight", 
                               `消瘦` = "Underweight"))
  df$BMI.factor <- factor(df$BMI.factor, 
                          levels = c("Underweight",
                                     "Normal Weight",
                                     "Overweight",
                                     "Obesity"))
  df <- df %>%
    mutate( NHANES_cycle = case_when(
      survey_year == 7 ~ "2011-2012",
      survey_year == 8 ~ "2013-2014",
      survey_year == 9 ~ "2015-2016",
      survey_year == 10 ~ "2017-2018")) 
  df$NHANES_cycle <- factor(df$NHANES_cycle, 
                            levels = c("2011-2012",
                                       "2013-2014",
                                       "2015-2016",
                                       "2017-2018"))
  df <- df %>%
    mutate( PIR.factor2 = case_when(
      INDFMPIR <= 1.30 ~ "≤ 1.30",
      INDFMPIR > 1.30 & INDFMPIR <= 3.50 ~ "1.31-3.50",
      INDFMPIR > 3.50 ~ "> 3.50")) 
  df$PIR.factor2 <- factor(df$PIR.factor2, 
                           levels = c("≤ 1.30",
                                      "1.31-3.50",
                                      "> 3.50"))
  df <- df %>%
    mutate( BMI.logistic2 = case_when(
      BMI.factor == "Obesity" ~ 1,
      BMI.factor == "Overweight" ~ 1,
      BMI.factor == "Normal Weight" ~ 0,
      BMI.factor == "Underweight" ~ 0)) 
  
  df <- df %>%
    mutate( PROT = DR1TPROT/BMXWT) 
  
  varsToFactor <- c("survey_year","RIAGENDR","RIDRETH3",
                    "age.factor","BMI.factor","NHANES_cycle",
                    "age.factor_10","BMI_logistic","BMI.logistic2",
                    "PIR.factor","PIR.factor2","Protein_intake.factor")
  df[varsToFactor] <- lapply(df[varsToFactor], factor) 
  df$survey_year <- factor(df$survey_year)
  df$NHANES_cycle <- factor(df$NHANES_cycle)
  df$RIAGENDR <- factor(df$RIAGENDR)
  df$RIDRETH3 <- factor(df$RIDRETH3)
  df$age.factor <- factor(df$age.factor)
  df$BMI.factor <- factor(df$BMI.factor)
  df$BMI_logistic <- factor(df$BMI_logistic)
  df$BMI.logistic2 <- factor(df$BMI.logistic2)
  df$PIR.factor <- factor(df$PIR.factor)
  df$PIR.factor2 <- factor(df$PIR.factor2)
  df$Protein_intake.factor <- factor(df$Protein_intake.factor)
  
  return(df)
}

df <- preprocess_data(df)

#### *三、logistic回归-2024.05.15
library(autoReg)
# 分人群基本描述 
data <- df
data <- df[df$RIAGENDR=="Male", ]
data <- df[df$RIAGENDR=="Female", ]

data <- df[df$age.factor=="0-5 years", ]
data <- df[df$age.factor=="6-19 years", ]
data <- df[df$age.factor=="20-59 years", ]
data <- df[df$age.factor=="60+ years", ]

## 数据预处理——分类变量因子化
{varsToFactor <- c("survey_year","RIAGENDR","RIDRETH3",
                   "age.factor","BMI.factor","NHANES_cycle",
                   "age.factor_10","BMI_logistic","BMI.logistic2",
                   "PIR.factor","PIR.factor2","Protein_intake.factor")
  data[varsToFactor] <- lapply(data[varsToFactor], factor) 
  data$survey_year <- factor(data$survey_year)
  data$NHANES_cycle <- factor(data$NHANES_cycle)
  data$RIAGENDR <- factor(data$RIAGENDR)
  data$RIDRETH3 <- factor(data$RIDRETH3)
  data$age.factor <- factor(data$age.factor)
  data$BMI.factor <- factor(data$BMI.factor)
  data$BMI_logistic <- factor(data$BMI_logistic)
  data$BMI.logistic2 <- factor(data$BMI.logistic2)
  data$PIR.factor <- factor(data$PIR.factor)
  data$PIR.factor2 <- factor(data$PIR.factor2)
  data$Protein_intake.factor <- factor(data$Protein_intake.factor)
}

NHANES_design_data <- svydesign(
  data = data, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust") 

summary(NHANES_design_data)

##单因素、多因素、逐步回归分析
logfit<-svyglm(BMI.logistic2~ RIAGENDR+ RIDAGEYR + RIDRETH3 +
                 PIR.factor2 + NHANES_cycle + age.factor+
                 HEI2020_ALL,
               data = data,
               design=NHANES_design_data,
               family = "quasibinomial")
logfit
logreg<-autoReg(logfit,uni=TRUE,threshold=1)#单因素分析,uni进行单因素分析，threshold纳入条件，final=T逐步回归
logreg
logtable1<-myft(logreg)
print(logtable1)

logfit<-svyglm(BMI.logistic2~RIAGENDR + RIDAGEYR + RIDRETH3 +
                 PIR.factor2 + NHANES_cycle + age.factor +
                 HEI2020_TOTALFRT + HEI2020_FRT + HEI2020_VEG +
                 HEI2020_GREENNBEAN + HEI2020_TOTALPRO + HEI2020_SEAPLANTPRO +
                 HEI2020_WHOLEGRAIN + HEI2020_DAIRY + HEI2020_FATTYACID +
                 HEI2020_REFINEDGRAIN + HEI2020_SODIUM + HEI2020_ADDEDSUGAR + 
                 HEI2020_SATFAT,
               data = data,
               design=NHANES_design_data,
               family = "quasibinomial")
logfit
logreg<-autoReg(logfit,uni=TRUE,threshold=1)#单因素分析,uni进行单因素分析，threshold纳入条件，final=T逐步回归
logreg
logtable1<-myft(logreg)
print(logtable1)
# family = "binomial"或者"quasibinomial"
# table2docx(logtable1,title="logreg",target="logreg-全人群")
# table2docx(logtable1,title="logreg",target="logreg-0-5岁")
# table2docx(logtable1,title="logreg",target="logreg-6-19岁")
# table2docx(logtable1,title="logreg",target="logreg-20-59岁")
# table2docx(logtable1,title="logreg",target="logreg-60+岁")
# table2docx(logtable1,title="logreg",target="logreg-male")
# table2docx(logtable1,title="logreg",target="logreg-female")


# 9. RCS-限制性立方样条分析  ####
rm(list = ls())
setwd("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存")
library(readxl)
library(randomForest)
library(dplyr)
library(Hmisc)
library(foreign)
library(tidyverse)
library(writexl)
library(readr)
library(broom)
library(xtable)
library(autoReg)
library(ggstatsplot)
library(ggplot2)
library(gtsummary)  # 用于tbl_regression 函数
library(tidyr) # drop_na 函数，用于快速去掉 NA
library(survey)
library(haven)
library(openxlsx)
library(tableone)
library(MatchIt)
library(rms)
library(reshape2)
library(plotRCS)
library(rrtable)
library(quantreg)
library(gWQS)
library(ggplot2)
library(epiDisplay)
library(officer)
library(flextable)
library(ggrcs)
library(autoReg)
library(gridExtra)

###########  *一、 数据变量预处理-2024.05.16 
df <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/正式分析-2024.05.06/1-2024.05.21_R-正式数据-(不去除意图减肥人群）23237+91.xlsx",col_names = T)

# 对一些人口统计学变量进行修改 
# 定义数据预处理函数
preprocess_data <- function(df) {
  df <- df %>%
    mutate(RIAGENDR = recode(RIAGENDR, 
                             `1` = "Male", 
                             `2` = "Female"))
  df$RIAGENDR <- factor(df$RIAGENDR, 
                        levels = c("Male","Female"))
  df <- df %>%
    mutate( age.factor = case_when(
      RIDAGEYR >= 0 & RIDAGEYR <= 5 ~ "0-5 years",
      RIDAGEYR > 5 & RIDAGEYR <= 19 ~ "6-19 years",
      RIDAGEYR > 19 & RIDAGEYR <= 59 ~ "20-59 years",
      RIDAGEYR > 59 & RIDAGEYR <= 80 ~ "60+ years")) 
  df$age.factor <- factor(df$age.factor, 
                          levels = c("0-5 years",
                                     "6-19 years",
                                     "20-59 years",
                                     "60+ years"))
  df <- df %>%
    mutate( RIDRETH3 = case_when(
      RIDRETH3 == 1 ~ "Mexican American",
      RIDRETH3 == 2 ~ "Other Hispanic",
      RIDRETH3 == 3 ~ "Non-Hispanic White",
      RIDRETH3 == 4 ~ "Non-Hispanic Black",
      RIDRETH3 == 6 ~ "Non-Hispanic Asian",
      RIDRETH3 == 7 ~ "Other/Multi-Racial")) 
  df$RIDRETH3 <- factor(df$RIDRETH3, 
                        levels = c("Mexican American",
                                   "Non-Hispanic White",
                                   "Non-Hispanic Black",
                                   "Non-Hispanic Asian",
                                   "Other Hispanic",
                                   "Other/Multi-Racial"))
  df <- df %>%
    mutate(BMI.factor = recode(BMI.factor, 
                               `偏瘦` = "消瘦"))
  df <- df %>%
    mutate(BMI.factor = recode(BMI.factor, 
                               `肥胖` = "Obesity", 
                               `超重` = "Overweight", 
                               `正常` = "Normal Weight", 
                               `消瘦` = "Underweight"))
  df$BMI.factor <- factor(df$BMI.factor, 
                          levels = c("Underweight",
                                     "Normal Weight",
                                     "Overweight",
                                     "Obesity"))
  df <- df %>%
    mutate( NHANES_cycle = case_when(
      survey_year == 7 ~ "2011-2012",
      survey_year == 8 ~ "2013-2014",
      survey_year == 9 ~ "2015-2016",
      survey_year == 10 ~ "2017-2018")) 
  df$NHANES_cycle <- factor(df$NHANES_cycle, 
                            levels = c("2011-2012",
                                       "2013-2014",
                                       "2015-2016",
                                       "2017-2018"))
  df <- df %>%
    mutate( PIR.factor2 = case_when(
      INDFMPIR <= 1.30 ~ "≤ 1.30",
      INDFMPIR > 1.30 & INDFMPIR <= 3.50 ~ "1.31-3.50",
      INDFMPIR > 3.50 ~ "> 3.50")) 
  df$PIR.factor2 <- factor(df$PIR.factor2, 
                           levels = c("≤ 1.30",
                                      "1.31-3.50",
                                      "> 3.50"))
  df <- df %>%
    mutate( BMI.logistic2 = case_when(
      BMI.factor == "Obesity" ~ 1,
      BMI.factor == "Overweight" ~ 1,
      BMI.factor == "Normal Weight" ~ 0,
      BMI.factor == "Underweight" ~ 0)) 
  
  df <- df %>%
    mutate( PROT = DR1TPROT/BMXWT) 
  
  varsToFactor <- c("survey_year","RIAGENDR","RIDRETH3",
                    "age.factor","BMI.factor","NHANES_cycle",
                    "age.factor_10","BMI_logistic","BMI.logistic2",
                    "PIR.factor","PIR.factor2","Protein_intake.factor")
  df[varsToFactor] <- lapply(df[varsToFactor], factor) 
  df$survey_year <- factor(df$survey_year)
  df$NHANES_cycle <- factor(df$NHANES_cycle)
  df$RIAGENDR <- factor(df$RIAGENDR)
  df$RIDRETH3 <- factor(df$RIDRETH3)
  df$age.factor <- factor(df$age.factor)
  df$BMI.factor <- factor(df$BMI.factor)
  df$BMI_logistic <- factor(df$BMI_logistic)
  df$BMI.logistic2 <- factor(df$BMI.logistic2)
  df$PIR.factor <- factor(df$PIR.factor)
  df$PIR.factor2 <- factor(df$PIR.factor2)
  df$Protein_intake.factor <- factor(df$Protein_intake.factor)
  
  return(df)
}
df <- preprocess_data(df)

## 数据预处理——分类变量因子化
{varsToFactor <- c("survey_year","RIAGENDR","RIDRETH3",
                   "age.factor","BMI.factor","NHANES_cycle",
                   "age.factor_10","BMI_logistic","BMI.logistic2",
                   "PIR.factor","PIR.factor2","Protein_intake.factor")
  df[varsToFactor] <- lapply(df[varsToFactor], factor) 
  df$survey_year <- factor(df$survey_year)
  df$NHANES_cycle <- factor(df$NHANES_cycle)
  df$RIAGENDR <- factor(df$RIAGENDR)
  df$RIDRETH3 <- factor(df$RIDRETH3)
  df$age.factor <- factor(df$age.factor)
  df$BMI.factor <- factor(df$BMI.factor)
  df$BMI_logistic <- factor(df$BMI_logistic)
  df$BMI.logistic2 <- factor(df$BMI.logistic2)
  df$PIR.factor <- factor(df$PIR.factor)
  df$PIR.factor2 <- factor(df$PIR.factor2)
  df$Protein_intake.factor <- factor(df$Protein_intake.factor)
}

#### *四、RCS-2024.06.11 
# 分人群基本描述 
data <- df
data <- df[df$RIAGENDR=="Male", ]
data <- df[df$RIAGENDR=="Female", ]

table(df$age.factor)
table(df$RIAGENDR)
data <- df[df$age.factor=="0-5 years", ]
data <- df[df$age.factor=="6-19 years", ]
data <- df[df$age.factor=="20-59 years", ]
data <- df[df$age.factor=="60+ years", ]

# 1.3 生成复杂抽样 NHANES_design
NHANES_design1 <- svydesign(
  data = data, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust") # 可以加上 survey.lonely.psu = "adjust" 避免1个PSU报错

summary(NHANES_design1)

df1 <- NHANES_design1$variables # 从 svydesign 中提取 df
dd <- rms::datadist(df1)
options(datadist = "dd")

df2 <- data.frame(df1)
ddist <- datadist(df2) #
options(datadist="ddist") #  defines data dist. to rms

fit1 <- rms::lrm(BMXBMI~rcs(HEI2020_TOTALPRO,4)+RIAGENDR+RIDAGEYR+RIDRETH3,
                 data = df2,x = T,y = T)

fit2 <- rms::lrm(BMXBMI~rcs(HEI2020_SEAPLANTPRO,4)+RIAGENDR+RIDAGEYR+RIDRETH3,
                 data = df2,x = T,y = T)

fit3 <- rms::lrm(BMXBMI~rcs(DR1TPROT,4)+RIAGENDR+RIDAGEYR+RIDRETH3,
                 data = df2,x = T,y = T)


# 性别亚组
fit1 <- rms::lrm(BMXBMI~rcs(HEI2020_TOTALPRO,4)+RIDAGEYR+RIDRETH3,
                 data = df2,x = T,y = T)

fit2 <- rms::lrm(BMXBMI~rcs(HEI2020_SEAPLANTPRO,4)+RIDAGEYR+RIDRETH3,
                 data = df2,x = T,y = T)

fit3 <- rms::lrm(BMXBMI~rcs(DR1TPROT,4)+RIDAGEYR+RIDRETH3,
                 data = df2,x = T,y = T)

# fit1 # 模型系数 
anova(fit1)# 非线性的检验

# fit2 # 模型系数 
anova(fit2)# 非线性的检验

# fit3 # 模型系数 
anova(fit3)# 非线性的检验

HEI2020_TOTALPRO <- fit1$data$HEI2020_TOTALPRO
OR.3_TOTALPRO <- rms::Predict(fit1, HEI2020_TOTALPRO, type="predictions", fun = exp, ref.zero = T) # 取中位数为 OR = 1

HEI2020_SEAPLANTPRO <- fit2$data$HEI2020_SEAPLANTPRO
OR.3_SEAPLANTPRO <- rms::Predict(fit2, HEI2020_SEAPLANTPRO, type="predictions", fun = exp, ref.zero = T) # 取中位数为 OR = 1

DR1TPROT <- fit3$data$DR1TPROT
OR.3_PROT <- rms::Predict(fit3, DR1TPROT, type="predictions", fun = exp, ref.zero = T) # 取中位数为 OR = 1

ggplot(OR.3_TOTALPRO) # ref.zero = T，取中位数为 OR = 1 
ggplot(OR.3_SEAPLANTPRO) # ref.zero = T，取中位数为 OR = 1
ggplot(OR.3_PROT) # ref.zero = T，取中位数为 OR = 1

ggplot()+
  geom_line(data=OR.3_TOTALPRO, aes(HEI2020_TOTALPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR.3_TOTALPRO, 
              aes(HEI2020_TOTALPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="#0070b9")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  # geom_vline(xintercept=197.33,size=0.5,color = '#d40e8c')+ #查表HR=1对应的HEI2020_TOTALPRO
  geom_vline(xintercept=4.39,size=0.5,color = '#d40e8c')+ #查表HR=1对应的HEI2020_TOTALPRO
  labs( x="HEI2020_TOTALPRO", y="OR (95%CI)") 


# 暂定绘图方案 
# 1-全人群 
library(gridExtra)
p1<-ggrcs(data=data,fit=fit3,x="DR1TPROT",histbinwidth=10)+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_vline(xintercept=67.5,linetype=2,size=0.5,color = '#0070b9')+ #查表HR=1对应的HEI2020_TOTALPRO
  labs( x="Protein", y="OR (95%CI)")+
  geom_text(aes(x=80, y=-0.5, label=paste0(
    "67.5")),color = 'gray36', hjust=0)+
  theme(plot.title = element_blank())  # 隐藏标题
p1

p2<-ggplot()+
  geom_line(data=OR.3_TOTALPRO, aes(HEI2020_TOTALPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR.3_TOTALPRO, 
              aes(HEI2020_TOTALPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="#0070b9")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_line(data=OR.3_SEAPLANTPRO, aes(HEI2020_SEAPLANTPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="palegreen4")+
  geom_ribbon(data=OR.3_SEAPLANTPRO, 
              aes(HEI2020_SEAPLANTPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="palegreen4")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  labs( x="TOTALPRO or SEAPLANTPRO", y="OR (95%CI)")+
  geom_vline(xintercept=4.38,linetype=2,size=0.5,color = 'blue')+
  geom_vline(xintercept=2.50,linetype=2,size=0.5,color = 'springgreen4')+
  geom_text(aes(x=4.45, y=0.6, label=paste0(
    "4.38")),color = 'blue', hjust=0,size=4)+
  geom_text(aes(x=2.57, y=0.6, label=paste0(
    "2.50")),color = 'springgreen4', hjust=0,size=4)+
  geom_text(aes(x=0.55, y=1.125, label=paste0(
    "SEAPLANTPRO :",
    "\nP for overall <0.0001", # your model pvalue
    "\nP for non-linear =0.0047")),color = 'palegreen4', hjust=0,size=4)+
  geom_text(aes(x=2.55, y=1.125, label=paste0(
    "TOTALPRO :",
    "\nP for overall <0.0001", # your model pvalue
    "\nP for non-linear <0.0001")),color = '#0070b9', hjust=0,size=4)+
  geom_text(aes(x=0, y=1.3, label=paste0(
    "All Participants")),color = 'black', hjust=0,size=5,
    fontface = 'bold' )+ # 加粗
  geom_rect(aes(xmin = -0.1, xmax = 1.63, ymin = 1.26, ymax = 1.34),
            fill = "gray65", alpha = 0.3)# 添加黑色方框

p2
# 使用grid.arrange函数将两个图形组合在一起
grid.arrange(p2, p1, ncol = 1)

# 2-全人群-男性 
library(gridExtra)
p1<-ggrcs(data=data,fit=fit3,x="DR1TPROT",histbinwidth=10)+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_vline(xintercept=76.0,linetype=2,size=0.5,color = '#0070b9')+ #查表HR=1对应的HEI2020_TOTALPRO
  labs( x="Protein", y="OR (95%CI)")+
  geom_text(aes(x=85, y=-0.2, label=paste0(
    "76.0")),color = 'gray36', hjust=0)+
  theme(plot.title = element_blank())  # 隐藏标题
p1

p2<-ggplot()+
  geom_line(data=OR.3_TOTALPRO, aes(HEI2020_TOTALPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR.3_TOTALPRO, 
              aes(HEI2020_TOTALPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="#0070b9")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_line(data=OR.3_SEAPLANTPRO, aes(HEI2020_SEAPLANTPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="palegreen4")+
  geom_ribbon(data=OR.3_SEAPLANTPRO, 
              aes(HEI2020_SEAPLANTPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="palegreen4")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  labs( x="TOTALPRO or SEAPLANTPRO", y="OR (95%CI)")+
  geom_vline(xintercept=4.46,linetype=2,size=0.5,color = 'blue')+
  geom_vline(xintercept=2.50,linetype=2,size=0.5,color = 'springgreen4')+
  geom_text(aes(x=4.55, y=0.5, label=paste0(
    "4.46")),color = 'blue', hjust=0,size=4)+
  geom_text(aes(x=2.59, y=0.5, label=paste0(
    "2.50")),color = 'springgreen4', hjust=0,size=4)+
  geom_text(aes(x=0.65, y=1.18, label=paste0(
    "SEAPLANTPRO :",
    "\nP for overall <0.0001", # your model pvalue
    "\nP for non-linear =0.0636")),color = 'palegreen4', hjust=0,size=4)+
  geom_text(aes(x=2.6, y=1.18, label=paste0(
    "TOTALPRO :",
    "\nP for overall <0.0001", # your model pvalue
    "\nP for non-linear <0.0001")),color = '#0070b9', hjust=0,size=4)+
  geom_text(aes(x=0, y=1.35, label=paste0(
    "Male")),color = 'black', hjust=0,size=5,
    fontface = 'bold' )+ # 加粗
  geom_rect(aes(xmin = -0.1, xmax = 0.6, ymin = 1.29, ymax = 1.40),
            fill = "gray65", alpha = 0.3)# 添加黑色方框
p2
# 使用grid.arrange函数将两个图形组合在一起
grid.arrange(p2, p1, ncol = 1)



# 3-全人群-女性 
library(gridExtra)
p1<-ggrcs(data=data,fit=fit3,x="DR1TPROT",histbinwidth=10)+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_vline(xintercept=60.5,linetype=2,size=0.5,color = '#0070b9')+ #查表HR=1对应的HEI2020_TOTALPRO
  labs( x="Protein", y="OR (95%CI)")+
  geom_text(aes(x=70, y=-0.2, label=paste0(
    "60.5")),color = 'gray36', hjust=0)+
  theme(plot.title = element_blank())  # 隐藏标题
p1

p2<-ggplot()+
  geom_line(data=OR.3_TOTALPRO, aes(HEI2020_TOTALPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR.3_TOTALPRO, 
              aes(HEI2020_TOTALPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="#0070b9")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_line(data=OR.3_SEAPLANTPRO, aes(HEI2020_SEAPLANTPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="palegreen4")+
  geom_ribbon(data=OR.3_SEAPLANTPRO, 
              aes(HEI2020_SEAPLANTPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="palegreen4")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  labs( x="TOTALPRO or SEAPLANTPRO", y="OR (95%CI)")+
  geom_vline(xintercept=4.28,linetype=2,size=0.5,color = 'blue')+
  geom_vline(xintercept=2.50,linetype=2,size=0.5,color = 'springgreen4')+
  geom_text(aes(x=4.33, y=0.7, label=paste0(
    "4.28")),color = 'blue', hjust=0,size=4)+
  geom_text(aes(x=2.55, y=0.7, label=paste0(
    "2.50")),color = 'springgreen4', hjust=0,size=4)+
  geom_text(aes(x=0.7, y=1.125, label=paste0(
    "SEAPLANTPRO :",
    "\nP for overall <0.0001", # your model pvalue
    "\nP for non-linear =0.1069")),color = 'palegreen4', hjust=0,size=4)+
  geom_text(aes(x=2.53, y=1.125, label=paste0(
    "TOTALPRO :",
    "\nP for overall <0.0001", # your model pvalue
    "\nP for non-linear =0.0634")),color = '#0070b9', hjust=0,size=4)+
  geom_text(aes(x=0, y=1.3, label=paste0(
    "Female")),color = 'black', hjust=0,size=5,
    fontface = 'bold' )+ # 加粗
  geom_rect(aes(xmin = -0.1, xmax = 0.85, ymin = 1.25, ymax = 1.34),
            fill = "gray65", alpha = 0.3)# 添加黑色方框
p2
# 使用grid.arrange函数将两个图形组合在一起
grid.arrange(p2, p1, ncol = 1)


# 4-0-5岁 
library(gridExtra)
p1<-ggrcs(data=data,fit=fit3,x="DR1TPROT",histbinwidth=10,px=80,py=1.3)+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_vline(xintercept=48,linetype=2,size=0.5,color = '#0070b9')+ #查表HR=1对应的HEI2020_TOTALPRO
  labs( x="Protein", y="OR (95%CI)")+
  geom_text(aes(x=53, y=-0.05, label=paste0(
    "48")),color = 'gray36', hjust=0)+
  theme(plot.title = element_blank())  # 隐藏标题
p1

p2<-ggplot()+
  geom_line(data=OR.3_TOTALPRO, aes(HEI2020_TOTALPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR.3_TOTALPRO, 
              aes(HEI2020_TOTALPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="#0070b9")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_line(data=OR.3_SEAPLANTPRO, aes(HEI2020_SEAPLANTPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="palegreen4")+
  geom_ribbon(data=OR.3_SEAPLANTPRO, 
              aes(HEI2020_SEAPLANTPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="palegreen4")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  labs( x="TOTALPRO or SEAPLANTPRO", y="OR (95%CI)")+
  geom_vline(xintercept=3.68,linetype=2,size=0.5,color = 'blue')+
  geom_vline(xintercept=1.54,linetype=2,size=0.5,color = 'springgreen4')+
  geom_text(aes(x=3.78, y=0.75, label=paste0(
    "3.68")),color = 'blue', hjust=0,size=4)+
  geom_text(aes(x=1.64, y=0.75, label=paste0(
    "1.54")),color = 'springgreen4', hjust=0,size=4)+
  geom_text(aes(x=0.55, y=1.2, label=paste0(
    "SEAPLANTPRO :",
    "\nP for overall =0.0378", # your model pvalue
    "\nP for non-linear =0.4781")),color = 'palegreen4', hjust=0,size=4)+
  geom_text(aes(x=2.55, y=1.2, label=paste0(
    "TOTALPRO :",
    "\nP for overall =0.8762", # your model pvalue
    "\nP for non-linear =0.7408")),color = '#0070b9', hjust=0,size=4)+
  geom_text(aes(x=0.52, y=1.45, label=paste0(
    "0-5 years")),color = 'black', hjust=0,size=5,
    fontface = 'bold' )+ # 加粗
  geom_rect(aes(xmin = 0.49, xmax = 1.51, ymin = 1.38, ymax = 1.5),
            fill = "gray65", alpha = 0.3)# 添加黑色方框
p2
# 使用grid.arrange函数将两个图形组合在一起
grid.arrange(p2, p1, ncol = 1)


# 5-6-19岁 
library(gridExtra)
p1<-ggrcs(data=data,fit=fit3,x="DR1TPROT",histbinwidth=10)+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_vline(xintercept=64.9,linetype=2,size=0.5,color = '#0070b9')+ #查表HR=1对应的HEI2020_TOTALPRO
  labs( x="Protein", y="OR (95%CI)")+
  geom_text(aes(x=75, y=-0.1, label=paste0(
    "64.9")),color = 'gray36', hjust=0)+
  theme(plot.title = element_blank())  # 隐藏标题
p1

p2<-ggplot()+
  geom_line(data=OR.3_TOTALPRO, aes(HEI2020_TOTALPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR.3_TOTALPRO, 
              aes(HEI2020_TOTALPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="#0070b9")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_line(data=OR.3_SEAPLANTPRO, aes(HEI2020_SEAPLANTPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="palegreen4")+
  geom_ribbon(data=OR.3_SEAPLANTPRO, 
              aes(HEI2020_SEAPLANTPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="palegreen4")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  labs( x="TOTALPRO or SEAPLANTPRO", y="OR (95%CI)")+
  geom_vline(xintercept=3.96,linetype=2,size=0.5,color = 'blue')+
  geom_vline(xintercept=1.36,linetype=2,size=0.5,color = 'springgreen4')+
  geom_text(aes(x=4.06, y=0.7, label=paste0(
    "3.96")),color = 'blue', hjust=0,size=4)+
  geom_text(aes(x=1.46, y=0.7, label=paste0(
    "1.36")),color = 'springgreen4', hjust=0,size=4)+
  geom_text(aes(x=0.55, y=1.125, label=paste0(
    "SEAPLANTPRO :",
    "\nP for overall =0.0037", # your model pvalue
    "\nP for non-linear =0.5792")),color = 'palegreen4', hjust=0,size=4)+
  geom_text(aes(x=2.55, y=1.125, label=paste0(
    "TOTALPRO :",
    "\nP for overall 0.0013", # your model pvalue
    "\nP for non-linear =0.0717")),color = '#0070b9', hjust=0,size=4)+
  geom_text(aes(x=0, y=1.32, label=paste0(
    "6-19 years")),color = 'black', hjust=0,size=5,
    fontface = 'bold' )+ # 加粗
  geom_rect(aes(xmin = -0.1, xmax = 1.17, ymin = 1.27, ymax = 1.36),
            fill = "gray65", alpha = 0.3)# 添加黑色方框
p2
# 使用grid.arrange函数将两个图形组合在一起
grid.arrange(p2, p1, ncol = 1)



# 6-20-59岁 
library(gridExtra)
p1<-ggrcs(data=data,fit=fit3,x="DR1TPROT",histbinwidth=10,px=180,py=1.3)+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_vline(xintercept=78.5,linetype=2,size=0.5,color = '#0070b9')+ #查表HR=1对应的HEI2020_TOTALPRO
  labs( x="Protein", y="OR (95%CI)")+
  geom_text(aes(x=88, y=-0.1, label=paste0(
    "78.5")),color = 'gray36', hjust=0)+
  theme(plot.title = element_blank())  # 隐藏标题
p1

p2<-ggplot()+
  geom_line(data=OR.3_TOTALPRO, aes(HEI2020_TOTALPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR.3_TOTALPRO, 
              aes(HEI2020_TOTALPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="#0070b9")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_line(data=OR.3_SEAPLANTPRO, aes(HEI2020_SEAPLANTPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="palegreen4")+
  geom_ribbon(data=OR.3_SEAPLANTPRO, 
              aes(HEI2020_SEAPLANTPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="palegreen4")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  labs( x="TOTALPRO or SEAPLANTPRO", y="OR (95%CI)")+
  geom_vline(xintercept=4.74,linetype=2,size=0.5,color = 'blue')+
  geom_vline(xintercept=2.50,linetype=2,size=0.5,color = 'springgreen4')+
  geom_text(aes(x=4.79, y=0.6, label=paste0(
    "4.74")),color = 'blue', hjust=0,size=4)+
  geom_text(aes(x=2.53, y=0.6, label=paste0(
    "2.50")),color = 'springgreen4', hjust=0,size=4)+
  geom_text(aes(x=0.55, y=1.125, label=paste0(
    "SEAPLANTPRO :",
    "\nP for overall <0.0001", # your model pvalue
    "\nP for non-linear =0.4307")),color = 'palegreen4', hjust=0,size=4)+
  geom_text(aes(x=2.8, y=1.125, label=paste0(
    "TOTALPRO :",
    "\nP for overall =0.0189", # your model pvalue
    "\nP for non-linear =0.7569")),color = '#0070b9', hjust=0,size=4)+
  geom_text(aes(x=0.5, y=1.3, label=paste0(
    "20-59 years")),color = 'black', hjust=0,size=5,
    fontface = 'bold' )+ # 加粗
  geom_rect(aes(xmin = 0.48, xmax = 1.72, ymin = 1.25, ymax = 1.34),
            fill = "gray65", alpha = 0.3)# 添加黑色方框
p2
# 使用grid.arrange函数将两个图形组合在一起
grid.arrange(p2, p1, ncol = 1)



# 7-60+岁 
library(gridExtra)
p1<-ggrcs(data=data,fit=fit3,x="DR1TPROT",histbinwidth=10)+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_vline(xintercept=66.5,linetype=2,size=0.5,color = '#0070b9')+ #查表HR=1对应的HEI2020_TOTALPRO
  labs( x="Protein", y="OR (95%CI)")+
  geom_text(aes(x=75, y=-0.05, label=paste0(
    "66.5")),color = 'gray36', hjust=0)+
  theme(plot.title = element_blank())  # 隐藏标题
p1

p2<-ggplot()+
  geom_line(data=OR.3_TOTALPRO, aes(HEI2020_TOTALPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="#0070b9")+
  geom_ribbon(data=OR.3_TOTALPRO, 
              aes(HEI2020_TOTALPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="#0070b9")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  geom_line(data=OR.3_SEAPLANTPRO, aes(HEI2020_SEAPLANTPRO,yhat),
            linetype="solid",size=0.5,alpha = 0.7,colour="palegreen4")+
  geom_ribbon(data=OR.3_SEAPLANTPRO, 
              aes(HEI2020_SEAPLANTPRO,ymin = lower, ymax = upper),
              alpha = 0.1,fill="palegreen4")+
  theme_classic()+
  geom_hline(yintercept=1,linetype=2,size=0.5)+
  labs( x="TOTALPRO or SEAPLANTPRO", y="OR (95%CI)")+
  geom_vline(xintercept=4.78,linetype=2,size=0.5,color = 'blue')+
  geom_vline(xintercept=2.48,linetype=2,size=0.5,color = 'springgreen4')+
  geom_text(aes(x=4.85, y=0.75, label=paste0(
    "4.78")),color = 'blue', hjust=0,size=4)+
  geom_text(aes(x=2.55, y=0.75, label=paste0(
    "2.48")),color = 'springgreen4', hjust=0,size=4)+
  geom_text(aes(x=0.6, y=1.2, label=paste0(
    "SEAPLANTPRO :",
    "\nP for overall =0.0062", # your model pvalue
    "\nP for non-linear =0.0665")),color = 'palegreen4', hjust=0,size=4)+
  geom_text(aes(x=2.8, y=1.2, label=paste0(
    "TOTALPRO :",
    "\nP for overall =0.0595", # your model pvalue
    "\nP for non-linear =0.2070")),color = '#0070b9', hjust=0,size=4)+
  geom_text(aes(x=0.6, y=1.45, label=paste0(
    "60+ years")),color = 'black', hjust=0,size=5,
    fontface = 'bold' )+ # 加粗
  geom_rect(aes(xmin = 0.55, xmax = 1.64, ymin = 1.39, ymax = 1.50),
            fill = "gray65", alpha = 0.3)# 添加黑色方框
p2
# 使用grid.arrange函数将两个图形组合在一起
grid.arrange(p2, p1, ncol = 1)



# 10. Cox比例风险回归+KM生存曲线  ####
rm(list = ls())
setwd("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存")
# install.packages("Hmisc")
library(readxl)
library(randomForest)
library(dplyr)
library(Hmisc)
library(foreign)
library(tidyverse)
library(writexl)
library(readr)
library(broom)
library(xtable)
library(autoReg)
library(ggstatsplot)
library(ggplot2)
library(gtsummary)  # 用于tbl_regression 函数
library(tidyr) # drop_na 函数，用于快速去掉 NA
library(survey)
library(haven)
library(openxlsx)
library(tableone)
library(MatchIt)
library(rms)
library(reshape2)
library(plotRCS)
library(rrtable)
library(quantreg)
library(gWQS)
library(ggplot2)
library(epiDisplay)
library(officer)
library(flextable)
library(nortest)
library(autoReg)
library(plyr)

###########  *一、 数据变量预处理-2024.05.16  
df1 <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/正式分析-2024.05.06/1-2024.05.21_R-正式数据-(不去除意图减肥人群）23237+91.xlsx",col_names = T)
Death <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/死亡数据库-2024.02.18/Mortality_1999-2018.xlsx",col_names = T)
# 转变为大写的列名，和 demo 进行拼接
colnames(Death) <- toupper(colnames(Death))

df <- merge(Death, df1, by = "SEQN") 


for(col in names(df)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(df[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}

# 对一些人口统计学变量进行修改
# 定义数据预处理函数
preprocess_data <- function(df) {
  df <- df %>%
    mutate(RIAGENDR = recode(RIAGENDR, 
                             `1` = "Male", 
                             `2` = "Female"))
  df$RIAGENDR <- factor(df$RIAGENDR, 
                        levels = c("Male","Female"))
  df <- df %>%
    mutate( age.factor = case_when(
      RIDAGEYR >= 0 & RIDAGEYR <= 5 ~ "0-5 years",
      RIDAGEYR > 5 & RIDAGEYR <= 19 ~ "6-19 years",
      RIDAGEYR > 19 & RIDAGEYR <= 59 ~ "20-59 years",
      RIDAGEYR > 59 & RIDAGEYR <= 80 ~ "60+ years")) 
  df$age.factor <- factor(df$age.factor, 
                          levels = c("0-5 years",
                                     "6-19 years",
                                     "20-59 years",
                                     "60+ years"))
  df <- df %>%
    mutate( RIDRETH3 = case_when(
      RIDRETH3 == 1 ~ "Mexican American",
      RIDRETH3 == 2 ~ "Other Hispanic",
      RIDRETH3 == 3 ~ "Non-Hispanic White",
      RIDRETH3 == 4 ~ "Non-Hispanic Black",
      RIDRETH3 == 6 ~ "Non-Hispanic Asian",
      RIDRETH3 == 7 ~ "Other/Multi-Racial")) 
  df$RIDRETH3 <- factor(df$RIDRETH3, 
                        levels = c("Mexican American",
                                   "Non-Hispanic White",
                                   "Non-Hispanic Black",
                                   "Non-Hispanic Asian",
                                   "Other Hispanic",
                                   "Other/Multi-Racial"))
  df <- df %>%
    mutate(BMI.factor = recode(BMI.factor, 
                               `偏瘦` = "消瘦"))
  df <- df %>%
    mutate(BMI.factor = recode(BMI.factor, 
                               `肥胖` = "Obesity", 
                               `超重` = "Overweight", 
                               `正常` = "Normal Weight", 
                               `消瘦` = "Underweight"))
  df$BMI.factor <- factor(df$BMI.factor, 
                          levels = c("Underweight",
                                     "Normal Weight",
                                     "Overweight",
                                     "Obesity"))
  df <- df %>%
    mutate( NHANES_cycle = case_when(
      survey_year == 7 ~ "2011-2012",
      survey_year == 8 ~ "2013-2014",
      survey_year == 9 ~ "2015-2016",
      survey_year == 10 ~ "2017-2018")) 
  df$NHANES_cycle <- factor(df$NHANES_cycle, 
                            levels = c("2011-2012",
                                       "2013-2014",
                                       "2015-2016",
                                       "2017-2018"))
  df <- df %>%
    mutate( PIR.factor2 = case_when(
      INDFMPIR <= 1.30 ~ "≤ 1.30",
      INDFMPIR > 1.30 & INDFMPIR <= 3.50 ~ "1.31-3.50",
      INDFMPIR > 3.50 ~ "> 3.50")) 
  df$PIR.factor2 <- factor(df$PIR.factor2, 
                           levels = c("≤ 1.30",
                                      "1.31-3.50",
                                      "> 3.50"))
  df <- df %>%
    mutate( BMI.logistic2 = case_when(
      BMI.factor == "Obesity" ~ 1,
      BMI.factor == "Overweight" ~ 1,
      BMI.factor == "Normal Weight" ~ 0,
      BMI.factor == "Underweight" ~ 0)) 
  
  df <- df %>%
    mutate( PROT = DR1TPROT/BMXWT) 
  
  varsToFactor <- c("survey_year","RIAGENDR","RIDRETH3",
                    "age.factor","BMI.factor","NHANES_cycle",
                    "age.factor_10","BMI_logistic","BMI.logistic2",
                    "PIR.factor","PIR.factor2","Protein_intake.factor")
  df[varsToFactor] <- lapply(df[varsToFactor], factor) 
  df$survey_year <- factor(df$survey_year)
  df$NHANES_cycle <- factor(df$NHANES_cycle)
  df$RIAGENDR <- factor(df$RIAGENDR)
  df$RIDRETH3 <- factor(df$RIDRETH3)
  df$age.factor <- factor(df$age.factor)
  df$BMI.factor <- factor(df$BMI.factor)
  df$BMI_logistic <- factor(df$BMI_logistic)
  df$BMI.logistic2 <- factor(df$BMI.logistic2)
  df$PIR.factor <- factor(df$PIR.factor)
  df$PIR.factor2 <- factor(df$PIR.factor2)
  df$Protein_intake.factor <- factor(df$Protein_intake.factor)
  
  return(df)
}

df <- preprocess_data(df)

###########  *二、生存分析-KM生存曲线-正式绘图版本  
# 1.提取DR1TPROT的加权四分位数 
library(survminer)
Quartile_DR1TPROT <- quantile(df1$DR1TPROT, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
Quartile_DR1TPROT

df <- df %>%
  mutate( Quartile_DR1TPROT = case_when(
    DR1TPROT >= 0 & DR1TPROT < 47.85 ~ "DR1TPROT_Q1",
    DR1TPROT >= 47.85 & DR1TPROT < 67.44 ~ "DR1TPROT_Q2",
    DR1TPROT >= 67.44 & DR1TPROT < 94.32 ~ "DR1TPROT_Q3",
    DR1TPROT >= 94.32 ~ "DR1TPROT_Q4")) 
df$Quartile_DR1TPROT <- factor(df$Quartile_DR1TPROT, 
                               levels = c("DR1TPROT_Q1",
                                          "DR1TPROT_Q2",
                                          "DR1TPROT_Q3",
                                          "DR1TPROT_Q4"))
table(df$Quartile_DR1TPROT)

NHANES_design_df <- svydesign(
  data = df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust") # 可以加上 survey.lonely.psu = "adjust" 避免1个PSU报错

summary(NHANES_design_df)
df1 <- NHANES_design_df$variables # 从 svydesign 中提取 df



fit <- survfit(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_DR1TPROT, data = df1)
fit
summary(fit)

output <- data.frame(time = fit$time,
                     n.risk = fit$n.risk,
                     n.event = fit$n.event,
                     n.censor = fit$n.censor,
                     surv = fit$surv,
                     upper = fit$upper,
                     lower = fit$lower)
# 可视化结果
ggsurvplot(fit,
           conf.int = TRUE,
           conf.int.style="ribbon",# 设置置信区间的类型:置信区间风格：你可以选择使用“带状”（ribbon）或“阶梯状”（step）的置信区间。带状置信区间更常见，但阶梯状也是一种选择
           conf.int.alpha =0.25, #指定置信区间填充颜色的透明度(0-1)
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c('green3', "#E7B800", "#2E9FDF","purple", '#d40e8c'),
           xlim = c(0, 120),
           ylim = c(0.85, 1),
           legend = c(0.3, 0.3),   # 指定图例位置
           legend.labs = c("ALL (0≤DR1TPROT≤869.49)",
                           "DR1TPROT_Q1 (0≤DR1TPROT<47.85)",
                           "DR1TPROT_Q2 (47.85≤DR1TPROT<67.44)",
                           "DR1TPROT_Q3 (67.44≤DR1TPROT<94.32)",
                           "DR1TPROT_Q4 (DR1TPROT≥94.32)"),   # 自定义图例标签
           legend.title = "Quartile_DR1TPROT",  # 设置图例标题
           legend.title.size = c(16, "italic"),  # 设置图例标题的字体大小
           font.legend = c(12, "italic"),  # 设置图例标签的字体大小 
           font.main = c(16, "bold"),  # 设置主标题的字体大小和粗细
           font.x = c(14, "italic"),  # 设置 x 轴标签的字体大小和样式
           font.y = c(14, "italic"),  # 设置 y 轴标签的字体大小和粗细
           font.caption = c(12, "normal"),  # 设置图表标题的字体大小和样式 
           font.tickslab = 12,   # 刻度标签 
           risk.table = TRUE,#添加风险表
           add.all = TRUE,  # 添加总患者的生存曲线
           pval = TRUE,#打开P值
           pval.size=5, # p值字体大小，默认为5。
           pval.coord=c(0.2,0.95), # p值坐标=c(x,y)
           pval.method=TRUE,#打开log.rank
           pval.method.size=5, #log.rank字体大小
           pval.method.coord=c(0.3,0.95),#log.rank的坐标
           risk.table.y.text = FALSE#在风险表图例中的文本注释中显示条形而不是名称
           # risk.table.title="The number of people at different time nodes",
)

# 比较生存曲线的对数秩检验-图中的 p value 的含义
surv_diff <- survdiff(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_DR1TPROT, data = df1)
surv_diff

# 2.提取HEI2020_SEAPLANTPRO的加权四分位数 
library(survminer)
Quartile_seaplantpro <- quantile(df1$HEI2020_SEAPLANTPRO, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
Quartile_seaplantpro

df <- df %>%
  mutate( Quartile_SEAPLANTPRO = case_when(
    HEI2020_SEAPLANTPRO >= 0 & HEI2020_SEAPLANTPRO < 0.08 ~ "SEAPLANTPRO_Q1",
    HEI2020_SEAPLANTPRO >= 0.08 & HEI2020_SEAPLANTPRO < 2.50 ~ "SEAPLANTPRO_Q2",
    HEI2020_SEAPLANTPRO >= 2.50 & HEI2020_SEAPLANTPRO < 3.13 ~ "SEAPLANTPRO_Q3",
    HEI2020_SEAPLANTPRO >= 3.13 & HEI2020_SEAPLANTPRO <= 5 ~ "SEAPLANTPRO_Q4")) 
df$Quartile_SEAPLANTPRO <- factor(df$Quartile_SEAPLANTPRO, 
                                  levels = c("SEAPLANTPRO_Q1",
                                             "SEAPLANTPRO_Q2",
                                             "SEAPLANTPRO_Q3",
                                             "SEAPLANTPRO_Q4"))
table(df$Quartile_SEAPLANTPRO)

NHANES_design_df <- svydesign(
  data = df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust") # 可以加上 survey.lonely.psu = "adjust" 避免1个PSU报错

summary(NHANES_design_df)
df1 <- NHANES_design_df$variables # 从 svydesign 中提取 df



fit <- survfit(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_SEAPLANTPRO, data = df1)
fit
summary(fit)

output <- data.frame(time = fit$time,
                     n.risk = fit$n.risk,
                     n.event = fit$n.event,
                     n.censor = fit$n.censor,
                     surv = fit$surv,
                     upper = fit$upper,
                     lower = fit$lower)
# 可视化结果
ggsurvplot(fit,
           conf.int = TRUE,
           conf.int.style="ribbon",# 设置置信区间的类型:置信区间风格：你可以选择使用“带状”（ribbon）或“阶梯状”（step）的置信区间。带状置信区间更常见，但阶梯状也是一种选择
           conf.int.alpha =0.25, #指定置信区间填充颜色的透明度(0-1)
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c('green3', "#E7B800", "#2E9FDF","purple", '#d40e8c'),
           xlim = c(0, 120),
           ylim = c(0.85, 1),
           legend = c(0.3, 0.3),   # 指定图例位置
           legend.labs = c("ALL (0≤SEAPLANTPRO≤5)",
                           "SEAPLANTPRO_Q1 (0≤SEAPLANTPRO<0.08)",
                           "SEAPLANTPRO_Q2 (0.08≤SEAPLANTPRO<2.50)",
                           "SEAPLANTPRO_Q3 (2.50≤SEAPLANTPRO<3.13)",
                           "SEAPLANTPRO_Q4 (3.13≤SEAPLANTPRO≤5)"),   # 自定义图例标签
           legend.title = "Quartile_SEAPLANTPRO",  # 设置图例标题
           legend.title.size = c(16, "italic"),  # 设置图例标题的字体大小
           font.legend = c(12, "italic"),  # 设置图例标签的字体大小 
           font.main = c(16, "bold"),  # 设置主标题的字体大小和粗细
           font.x = c(14, "italic"),  # 设置 x 轴标签的字体大小和样式
           font.y = c(14, "italic"),  # 设置 y 轴标签的字体大小和粗细
           font.caption = c(12, "normal"),  # 设置图表标题的字体大小和样式 
           font.tickslab = 12,   # 刻度标签 
           risk.table = TRUE,#添加风险表
           add.all = TRUE,  # 添加总患者的生存曲线
           pval = TRUE,#打开P值
           pval.size=5, # p值字体大小，默认为5。
           pval.coord=c(0.2,0.95), # p值坐标=c(x,y)
           pval.method=TRUE,#打开log.rank
           pval.method.size=5, #log.rank字体大小
           pval.method.coord=c(0.3,0.95),#log.rank的坐标
           risk.table.y.text = FALSE#在风险表图例中的文本注释中显示条形而不是名称
           # risk.table.title="The number of people at different time nodes",
)

# 比较生存曲线的对数秩检验-图中的 p value 的含义
surv_diff <- survdiff(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_SEAPLANTPRO, data = df1)
surv_diff


# 3.提取HEI2020_TOTALPRO的加权四分位数 
Quartile_TOTALPRO <- quantile(df1$HEI2020_TOTALPRO, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
Quartile_TOTALPRO

df <- df %>%
  mutate( Quartile_TOTALPRO = case_when(
    HEI2020_TOTALPRO >= 0 & HEI2020_TOTALPRO < 3.37 ~ "TOTALPRO_Q1",
    HEI2020_TOTALPRO >= 3.37 & HEI2020_TOTALPRO < 4.19 ~ "TOTALPRO_Q2",
    HEI2020_TOTALPRO >= 4.19 & HEI2020_TOTALPRO < 4.59 ~ "TOTALPRO_Q3",
    HEI2020_TOTALPRO >= 4.59 & HEI2020_TOTALPRO <= 5.0 ~ "TOTALPRO_Q4")) 
df$Quartile_TOTALPRO <- factor(df$Quartile_TOTALPRO, 
                               levels = c("TOTALPRO_Q1",
                                          "TOTALPRO_Q2",
                                          "TOTALPRO_Q3",
                                          "TOTALPRO_Q4"))

df <- df %>%
  mutate( Quartile_TOTALPRO = case_when(
    HEI2020_TOTALPRO >= 0 & HEI2020_TOTALPRO < 3.37 ~ "TOTALPRO_Q1",
    HEI2020_TOTALPRO >= 3.37 & HEI2020_TOTALPRO < 4.38 ~ "TOTALPRO_Q2",
    HEI2020_TOTALPRO >= 4.38 & HEI2020_TOTALPRO <= 5.0 ~ "TOTALPRO_Q3")) 
df$Quartile_TOTALPRO <- factor(df$Quartile_TOTALPRO, 
                               levels = c("TOTALPRO_Q1",
                                          "TOTALPRO_Q2",
                                          "TOTALPRO_Q3"))

table(df$Quartile_TOTALPRO)


NHANES_design_df <- svydesign(
  data = df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust") # 可以加上 survey.lonely.psu = "adjust" 避免1个PSU报错

summary(NHANES_design_df)
df1 <- NHANES_design_df$variables # 从 svydesign 中提取 df


fit <- survfit(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_TOTALPRO, data = df1)
fit

output <- data.frame(time = fit$time,
                     n.risk = fit$n.risk,
                     n.event = fit$n.event,
                     n.censor = fit$n.censor,
                     surv = fit$surv,
                     upper = fit$upper,
                     lower = fit$lower)
# 可视化结果
ggsurvplot(fit,
           conf.int = TRUE,
           conf.int.style="ribbon",# 设置置信区间的类型:置信区间风格：你可以选择使用“带状”（ribbon）或“阶梯状”（step）的置信区间。带状置信区间更常见，但阶梯状也是一种选择
           conf.int.alpha =0.25, #指定置信区间填充颜色的透明度(0-1)
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c('green3',"#E7B800", "#2E9FDF","purple", '#d40e8c'),
           xlim = c(0, 120),
           ylim = c(0.85, 1),
           legend = c(0.28, 0.3),   # 指定图例位置
           legend.labs = c("ALL (0≤TOTALPRO≤5)",
                           "TOTALPRO_Q1 (0≤TOTALPRO<3.37)",
                           "TOTALPRO_Q2 (3.37≤TOTALPRO<4.19)",
                           "TOTALPRO_Q3 (4.19≤TOTALPRO<4.59)",
                           "TOTALPRO_Q4 (4.59≤TOTALPRO≤5)"),   # 自定义图例标签
           legend.title = "Quartile_TOTALPRO",  # 设置图例标题
           legend.title.size = c(16, "italic"),  # 设置图例标题的字体大小
           font.legend = c(12, "italic"),  # 设置图例标签的字体大小 
           font.main = c(16, "bold"),  # 设置主标题的字体大小和粗细
           font.x = c(14, "italic"),  # 设置 x 轴标签的字体大小和样式
           font.y = c(14, "italic"),  # 设置 y 轴标签的字体大小和粗细
           font.caption = c(12, "normal"),  # 设置图表标题的字体大小和样式 
           font.tickslab = 12,   # 刻度标签 
           risk.table = TRUE,#添加风险表
           add.all = TRUE,  # 添加总患者的生存曲线
           pval = TRUE,#打开P值
           pval.size=5, # p值字体大小，默认为5。
           pval.coord=c(0.2,0.95), # p值坐标=c(x,y)
           pval.method=TRUE,#打开log.rank
           pval.method.size=5, #log.rank字体大小
           pval.method.coord=c(0.3,0.95),#log.rank的坐标
           risk.table.y.text = FALSE#在风险表图例中的文本注释中显示条形而不是名称
           # risk.table.title="The number of people at different time nodes",
)


ggsurvplot(fit,
           conf.int = TRUE,
           conf.int.style="ribbon",# 设置置信区间的类型:置信区间风格：你可以选择使用“带状”（ribbon）或“阶梯状”（step）的置信区间。带状置信区间更常见，但阶梯状也是一种选择
           conf.int.alpha =0.25, #指定置信区间填充颜色的透明度(0-1)
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c('green3',"#E7B800", "#2E9FDF", '#d40e8c'),
           xlim = c(0, 120),
           ylim = c(0.85, 1),
           legend = c(0.28, 0.25),   # 指定图例位置
           legend.labs = c("ALL (0≤TOTALPRO≤5)",
                           "TOTALPRO_Q1 (0≤TOTALPRO<3.37)",
                           "TOTALPRO_Q2 (3.37≤TOTALPRO<4.38)",
                           "TOTALPRO_Q3 (4.38≤TOTALPRO≤5)"),   # 自定义图例标签
           legend.title = "Quartile_TOTALPRO",  # 设置图例标题
           legend.title.size = c(16, "italic"),  # 设置图例标题的字体大小
           font.legend = c(12, "italic"),  # 设置图例标签的字体大小 
           font.main = c(16, "bold"),  # 设置主标题的字体大小和粗细
           font.x = c(14, "italic"),  # 设置 x 轴标签的字体大小和样式
           font.y = c(14, "italic"),  # 设置 y 轴标签的字体大小和粗细
           font.caption = c(12, "normal"),  # 设置图表标题的字体大小和样式 
           font.tickslab = 12,   # 刻度标签 
           risk.table = TRUE,#添加风险表
           add.all = TRUE,  # 添加总患者的生存曲线
           pval = TRUE,#打开P值
           pval.size=5, # p值字体大小，默认为5。
           pval.coord=c(0.2,0.95), # p值坐标=c(x,y)
           pval.method=TRUE,#打开log.rank
           pval.method.size=5, #log.rank字体大小
           pval.method.coord=c(0.3,0.95),#log.rank的坐标
           risk.table.y.text = FALSE#在风险表图例中的文本注释中显示条形而不是名称
           # risk.table.title="The number of people at different time nodes",
)

# 比较生存曲线的对数秩检验-图中的 p value 的含义
surv_diff <- survdiff(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_TOTALPRO, data = df1)
surv_diff

###########  *三、 复杂抽样设计的Cox回归分析 
df <- df %>%
  mutate( Quartile_TOTALPRO_Q4 = case_when(
    HEI2020_TOTALPRO >= 0 & HEI2020_TOTALPRO < 3.37 ~ "TOTALPRO_Q1",
    HEI2020_TOTALPRO >= 3.37 & HEI2020_TOTALPRO < 4.19 ~ "TOTALPRO_Q2",
    HEI2020_TOTALPRO >= 4.19 & HEI2020_TOTALPRO < 4.59 ~ "TOTALPRO_Q3",
    HEI2020_TOTALPRO >= 4.59 & HEI2020_TOTALPRO <= 5.0 ~ "TOTALPRO_Q4")) 
df$Quartile_TOTALPRO_Q4 <- factor(df$Quartile_TOTALPRO_Q4, 
                                  levels = c("TOTALPRO_Q1",
                                             "TOTALPRO_Q2",
                                             "TOTALPRO_Q3",
                                             "TOTALPRO_Q4"))

df <- df %>%
  mutate( Quartile_TOTALPRO_Q3 = case_when(
    HEI2020_TOTALPRO >= 0 & HEI2020_TOTALPRO < 3.37 ~ "TOTALPRO_Q1",
    HEI2020_TOTALPRO >= 3.37 & HEI2020_TOTALPRO < 4.38 ~ "TOTALPRO_Q2",
    HEI2020_TOTALPRO >= 4.38 & HEI2020_TOTALPRO <= 5.0 ~ "TOTALPRO_Q3")) 
df$Quartile_TOTALPRO_Q3 <- factor(df$Quartile_TOTALPRO_Q3, 
                                  levels = c("TOTALPRO_Q1",
                                             "TOTALPRO_Q2",
                                             "TOTALPRO_Q3"))

df <- df %>%
  mutate( Quartile_SEAPLANTPRO_Q4 = case_when(
    HEI2020_SEAPLANTPRO >= 0 & HEI2020_SEAPLANTPRO < 0.08 ~ "SEAPLANTPRO_Q1",
    HEI2020_SEAPLANTPRO >= 0.08 & HEI2020_SEAPLANTPRO < 2.50 ~ "SEAPLANTPRO_Q2",
    HEI2020_SEAPLANTPRO >= 2.50 & HEI2020_SEAPLANTPRO < 3.13 ~ "SEAPLANTPRO_Q3",
    HEI2020_SEAPLANTPRO >= 3.13 & HEI2020_SEAPLANTPRO <= 5 ~ "SEAPLANTPRO_Q4")) 
df$Quartile_SEAPLANTPRO_Q4 <- factor(df$Quartile_SEAPLANTPRO_Q4, 
                                     levels = c("SEAPLANTPRO_Q1",
                                                "SEAPLANTPRO_Q2",
                                                "SEAPLANTPRO_Q3",
                                                "SEAPLANTPRO_Q4"))

NHANES_design_df <- svydesign(
  data = df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust") # 可以加上 survey.lonely.psu = "adjust" 避免1个PSU报错

summary(NHANES_design_df)


#####  复杂抽样的 Cox 回归模型 
#单因素Cox
m1_TOTALPRO_Origianl <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_DR1TPROT, 
                                 design = NHANES_design_df)    #单因素

m1_TOTALPRO_Q3 <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_TOTALPRO_Q3, 
                           design = NHANES_design_df)    #单因素

m1_TOTALPRO_Q4 <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_TOTALPRO_Q4, 
                           design = NHANES_design_df)    #单因素

m1_SEAPLANTPRO_Q4 <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_SEAPLANTPRO_Q4, 
                              design = NHANES_design_df)    #单因素

##### 2.5 使用 gtsummary 优化输出 
tbl_regression(m1_TOTALPRO_Origianl)
tbl_regression(m1_TOTALPRO_Q3)
tbl_regression(m1_TOTALPRO_Q4)
tbl_regression(m1_SEAPLANTPRO_Q4)

#多因素Cox：对性别、种族、年龄、贫困比进行调整
m0 <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_DR1TPROT + 
                 RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2, 
               design = NHANES_design_df)    #多因素

m1.1 <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_TOTALPRO_Q3 + 
                   RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2, 
                 design = NHANES_design_df)    #多因素

m1.2 <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_TOTALPRO_Q4 + 
                   RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2, 
                 design = NHANES_design_df)    #多因素

m1.3 <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_SEAPLANTPRO_Q4 + 
                   RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2, 
                 design = NHANES_design_df)    #多因素

m2.1 <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_TOTALPRO_Q4 + Quartile_SEAPLANTPRO_Q4, 
                 design = NHANES_design_df)    #未调整

m3.1 <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_TOTALPRO_Q3 + Quartile_SEAPLANTPRO_Q4, 
                 design = NHANES_design_df)    #未调整

m2.2 <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_TOTALPRO_Q4 + Quartile_SEAPLANTPRO_Q4 + 
                   RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2, 
                 design = NHANES_design_df)    #多因素

m3.2 <- svycoxph(Surv(PERMTH_INT, MORTSTAT) ~ Quartile_TOTALPRO_Q3 + Quartile_SEAPLANTPRO_Q4 + 
                   RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2, 
                 design = NHANES_design_df)    #多因素


summary(m1)

##### 2.5 使用 gtsummary 优化输出 
tbl_regression(m0)
tbl_regression(m1.1)
tbl_regression(m1.2)
tbl_regression(m1.3)
tbl_regression(m2.1)
tbl_regression(m3.1)
tbl_regression(m2.2)
tbl_regression(m3.2)

###########  *四、 Cox回归-森林图绘制-论文中版本  
rm(list = ls())
library(grid)
library(forestploter)
library(ggplot2)

dt<-read.csv("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-代码/全人群HEI2020-2024.05.21/Cox_forest_蛋白质-2024.07.26-3.csv",sep=',',header=TRUE)

# 先把数据格式整理一下，先让它缩进一格
dt$Subgroup <- ifelse(is.na(dt$est), 
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# 生成一个变量se，它在绘图的时候表示正方形的大小
dt$se <- (dt$hi - dt$est)/1.96

# 生成一个绘图区间，等下用来绘图
# 增加两个空地方来画图
dt$`Unadjusted`<- paste(rep(" ", 20), collapse = " ")
dt$`Adjusted`<- paste(rep(" ", 20), collapse = " ")

# 生成HR和可信区间
dt$`           Unadjusted\nlog(HR) (95% CI)` <- ifelse(is.na(dt$se), "",
                                                       sprintf("%.2f (%.2f to %.2f)",
                                                               dt$est, dt$low, dt$hi))#sprintF返回字符和可变量组合

# 生成HR和可信区间
dt$`           Adjusted\nlog(HR) (95% CI)` <- ifelse(is.na(dt$se), "",
                                                     sprintf("%.2f (%.2f to %.2f)",
                                                             dt$est2, dt$low2, dt$hi2))#sprintF返回字符和可变量组合

p <- forest(dt[,c(1,11,13,5,12,14,9)],
            est = list(dt$est,dt$est2),       # 效应值
            lower = list(dt$low,dt$low2),     # 可信区间下限
            upper = list(dt$hi,dt$hi2),      # 可信区间上限
            sizes = dt$se,      # 黑框的大小
            ci_column = c(2,5),      # 在那一列画森林图，要选空的那一列
            ref_line = 0,
            arrow_lab = c("log(HR)<0", "log(HR)>0"),
            xlim = c(-1, 0.3),     # 增加 xlim 的上限
            ticks_at = c(-1.0,-0.5,-0.25,0,0.15,0.3)) # 添加更多的刻度以适应新的 xlim
p

# orest_theme函数可以对森林图图片的细节进行调整，我们可以在forest_theme函数上设定预定的森林图样式模块，到时直接绘图就好了
tm <- forest_theme(base_size = 10,  #文本的大小
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,   #可信区间点的形状
                   ci_col = "#762a83",    #CI的颜色
                   ci_fill = "#762a83",     #ci颜色填充
                   ci_alpha = 0.8,        #ci透明度
                   ci_lty = 1,            #CI的线型
                   ci_lwd = 1.5,          #CI的线宽
                   ci_Theight = 0.2, # Set an T end at the end of CI  ci的高度，默认是NULL
                   # Reference line width/type/color   参考线默认的参数，中间的竖的虚线
                   refline_lwd = 1,       #中间的竖的虚线
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color  垂直线宽/类型/颜色   可以添加一条额外的垂直线，如果没有就不显示
                   vertline_lwd = 1,              #可以添加一条额外的垂直线，如果没有就不显示
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders   更改填充和边框的摘要颜色
                   summary_fill = "yellow",       #汇总部分大菱形的颜色
                   summary_col = "#4575b4",
                   # Footnote font size/face/color  脚注字体大小/字体/颜色
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "red",
                   # legend
                   legend_name = "Group",   #设置标题名字
                   legend_value = c("All participants"," "))   #设置分组名字


# 设定好模块后直接绘图就可以了
pt <- forest(dt[,c(1,11,13,5,12,14,9)],
             est = list(dt$est,dt$est2),       # 效应值
             lower = list(dt$low,dt$low2),     # 可信区间下限
             upper = list(dt$hi,dt$hi2),      # 可信区间上限
             sizes = 0.4,
             ci_column = c(2,5),
             ref_line = 0,
             arrow_lab = c("log(HR)<0", "log(HR)>0"),
             xlim = c(-1, 0.3),     # 增加 xlim 的上限
             ticks_at = c(-1.0,-0.5,-0.25,0,0.15,0.3), # 添加更多的刻度以适应新的 xlim
             theme = tm)

plot(pt)

# 我们还可以对图片细节进行修改，比如我们想把第三行变成红色
g <- edit_plot(pt, row = c(1,5,10,15,25), 
               gp = gpar(col = "royalblue3", fontface = "bold"))
g

# 把2, 5, 10, 13, 17, 20行的文本变成粗体
g <- edit_plot(g,
               row = c(1,5,10,15,16,20,25,26,31),
               gp = gpar(fontface = "bold"))

g <- edit_plot(g,
               row = c(16,20,26,31),
               gp = gpar(col = "hotpink4", fontface = "bold"))
g

# 把第五行的背景改成绿色
g <- edit_plot(g, row = c(1,5,10),
               which = "background",
               gp = gpar(fill = "lightblue1"))

g <- edit_plot(g, row = c(15,25),
               which = "background",
               gp = gpar(fill = "palegreen2"))
g


## Cox回归-森林图-DR1PROT 
rm(list = ls())
library(grid)
library(forestploter)
library(ggplot2)

dt<-read.csv("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-代码/全人群HEI2020-2024.05.21/Cox_forest_蛋白质原始数据DR1PROT-2024.06.20.csv",sep=',',header=TRUE)

# 先把数据格式整理一下，先让它缩进一格
dt$Subgroup <- ifelse(is.na(dt$est), 
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# 生成一个变量se，它在绘图的时候表示正方形的大小
dt$se <- (dt$hi - dt$est)/1.96

# 生成一个绘图区间，等下用来绘图
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# 生成HR和可信区间
dt$`log(HR) (95% CI)` <- ifelse(is.na(dt$se), "",
                                sprintf("%.2f (%.2f to %.2f)",
                                        dt$est, dt$low, dt$hi))#sprintF返回字符和可变量组合
# 添加p值
dt$`p value` <- paste(c(" ","0.700","0.008","<0.001"," ",
                        "0.700","0.130","0.091"))

p <- forest(dt[,c(1:1,6:8)],
            est = dt$est,       # 效应值
            lower = dt$low,     # 可信区间下限
            upper = dt$hi,      # 可信区间上限
            sizes = dt$se,      # 黑框的大小
            ci_column = 2,      # 在那一列画森林图，要选空的那一列
            ref_line = 0,
            arrow_lab = c("log(HR)<0", "log(HR)>0"),
            xlim = c(-1, 0.4),     # 增加 xlim 的上限
            ticks_at = c(-1.0,-0.5,-0.25,0,0.2,0.4)) # 添加更多的刻度以适应新的 xlim
p

# orest_theme函数可以对森林图图片的细节进行调整，我们可以在forest_theme函数上设定预定的森林图样式模块，到时直接绘图就好了
tm <- forest_theme(base_size = 10,  #文本的大小
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,   #可信区间点的形状
                   ci_col = "#762a83",    #CI的颜色
                   ci_fill = "#762a83",     #ci颜色填充
                   ci_alpha = 0.8,        #ci透明度
                   ci_lty = 1,            #CI的线型
                   ci_lwd = 1.5,          #CI的线宽
                   ci_Theight = 0.2, # Set an T end at the end of CI  ci的高度，默认是NULL
                   # Reference line width/type/color   参考线默认的参数，中间的竖的虚线
                   refline_lwd = 1,       #中间的竖的虚线
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color  垂直线宽/类型/颜色   可以添加一条额外的垂直线，如果没有就不显示
                   vertline_lwd = 1,              #可以添加一条额外的垂直线，如果没有就不显示
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders   更改填充和边框的摘要颜色
                   summary_fill = "yellow",       #汇总部分大菱形的颜色
                   summary_col = "#4575b4",
                   # Footnote font size/face/color  脚注字体大小/字体/颜色
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "red",
                   # legend
                   legend_name = "Group",   #设置标题名字
                   legend_value = c("All participants"," "))   #设置分组名字


# 设定好模块后直接绘图就可以了
pt <- forest(dt[,c(1:1, 6:8)],
             est = dt$est,
             lower = dt$low, 
             upper = dt$hi,
             sizes = 0.4,
             ci_column = 2,
             ref_line = 0,
             arrow_lab = c("log(HR)<0", "log(HR)>0"),
             xlim = c(-1, 0.4),     # 增加 xlim 的上限
             ticks_at = c(-1.0,-0.5,-0.25,0,0.2,0.4), # 添加更多的刻度以适应新的 xlim
             theme = tm)

plot(pt)

# 我们还可以对图片细节进行修改，比如我们想把第三行变成红色
g <- edit_plot(pt, row = c(3,4), 
               gp = gpar(col = "royalblue3", fontface = "bold"))
g

# 把2, 5, 10, 13, 17, 20行的文本变成粗体
g <- edit_plot(g,
               row = c(1,5),
               gp = gpar(fontface = "bold"))
g

# 把第五行的背景改成绿色
g <- edit_plot(g, row = c(1),
               which = "background",
               gp = gpar(fill = "lightblue1"))

g <- edit_plot(g, row = c(5),
               which = "background",
               gp = gpar(fill = "palegreen2"))
g



# 散点图 
# 使用 ggplot2 绘制加权散点图
library(ggplot2)

# 创建散点图
ggplot(df1, aes(x = HEI2020_SEAPLANTPRO, y = BMXBMI)) +
  geom_point(aes(size = WTDRD1_adjust), alpha = 0.6) +  # 加权散点图
  labs(x = "HEI2020_SEAPLANTPRO", y = "BMXBMI", title = "Weighted Scatter Plot") +
  theme_minimal()



ggplot(df1, aes(x = HEI2020_SEAPLANTPRO, y = BMXBMI)) +
  geom_point(aes(size = WTDRD1_adjust), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # 添加拟合曲线
  labs(x = "HEI2020_SEAPLANTPRO", y = "BMXBMI", title = "Weighted Scatter Plot with Regression Line") +
  theme_minimal()


ggplot(df1, aes(x = HEI2020_TOTALPRO, y = BMXBMI)) +
  geom_point(aes(size = WTDRD1_adjust), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # 添加拟合曲线
  labs(x = "HEI2020_SEAPLANTPRO", y = "BMXBMI", title = "Weighted Scatter Plot with Regression Line") +
  theme_minimal()



ggplot(df1, aes(x = HEI2020_TOTALPRO, y = BMXBMI)) +
  geom_point(aes(size = WTDRD1_adjust), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "HEI2020_SEAPLANTPRO", y = "BMXBMI", title = "Weighted Scatter Plot with Regression Line") +
  theme_minimal() +
  facet_wrap(~ age.factor, ncol = 2)  # 根据 age.factor 分组



ggplot(df1, aes(x = HEI2020_SEAPLANTPRO, y = BMXBMI)) +
  geom_point(aes(size = WTDRD1_adjust), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "HEI2020_SEAPLANTPRO", y = "BMXBMI", title = "Weighted Scatter Plot with Regression Line") +
  theme_minimal() +
  facet_wrap(~ age.factor, ncol = 2)  # 根据 age.factor 分组


# 创建加权散点图并添加拟合线
library(ggplot2)
library(ggpmisc)

ggplot(df1, aes(x = HEI2020_TOTALPRO, y = BMXBMI)) +
  geom_point(aes(size = WTDRD1_adjust), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_poly_eq(
    formula = y ~ x,
    parse = TRUE,
    size = 5,
    label.x = 0.05,
    label.y = 0.95
  ) +
  labs(x = "HEI2020_SEAPLANTPRO", y = "BMXBMI", title = "Weighted Scatter Plot with Regression Line") +
  theme_minimal()


# 创建加权散点图并添加拟合线
library(ggplot2)
library(ggpmisc)

ggplot(df1, aes(x = HEI2020_TOTALPRO, y = BMXBMI)) +
  geom_point(aes(size = WTDRD1_adjust), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_poly_eq(
    formula = y ~ x,
    parse = TRUE,
    size = 5,
    label.x = 0.05,
    label.y = 0.95
  ) +
  labs(x = "HEI2020_SEAPLANTPRO", y = "BMXBMI", title = "Weighted Scatter Plot with Regression Line") +
  theme_minimal()


# 创建加权线性回归模型
model <- lm(BMXBMI ~ RIAGENDR + RIDAGEYR + RIDRETH3 +
              PIR.factor2 + NHANES_cycle + age.factor + HEI2020_SEAPLANTPRO+HEI2020_TOTALPRO,
            data = df1, weights = WTDRD1_adjust)

# 查看模型摘要
summary(model)


# 创建线性回归模型
linear_model <- lm(BMXBMI ~ RIAGENDR + RIDAGEYR + RIDRETH3 +
                     PIR.factor2 + NHANES_cycle + age.factor + HEI2020_SEAPLANTPRO + HEI2020_TOTALPRO,
                   data = df1, weights = WTDRD1_adjust)

# 查看模型摘要
summary(linear_model)

# 根据 age.factor 分组
library(ggplot2)
ggplot(df1, aes(x = HEI2020_SEAPLANTPRO, y = BMXBMI)) +
  geom_point(aes(size = WTDRD1_adjust), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "HEI2020_SEAPLANTPRO", y = "BMXBMI", title = "Weighted Scatter Plot with Regression Line") +
  theme_minimal() +
  facet_wrap(~ age.factor, ncol = 2)


# 11. 中介分析  ####
rm(list = ls())
setwd("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/文档及结果保存-2024.05.14/文档/结果保存")
library(readxl)
library(randomForest)
library(dplyr)
library(Hmisc)
library(foreign)
library(tidyverse)
library(writexl)
library(readr)
library(broom)
library(xtable)
library(autoReg)
library(ggstatsplot)
library(ggplot2)
library(gtsummary)  # 用于tbl_regression 函数
library(tidyr) # drop_na 函数，用于快速去掉 NA
library(survey)
library(haven)
library(openxlsx)
library(tableone)
library(MatchIt)
library(rms)
library(reshape2)
library(plotRCS)
library(rrtable)
library(quantreg)
library(gWQS)
library(ggplot2)
library(epiDisplay)
library(officer)
library(flextable)
library(nortest)
library(autoReg)
library(plyr)

###########  *一、 数据变量预处理-2024.05.16  
df1 <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/正式分析-2024.05.06/1-2024.05.21_R-正式数据-(不去除意图减肥人群）23237+91.xlsx",col_names = T)
Death <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/死亡数据库-2024.02.18/Mortality_1999-2018.xlsx",col_names = T)
# 转变为大写的列名，和 demo 进行拼接
colnames(Death) <- toupper(colnames(Death))
table(Death$SURVEY_YEAR)

df <- merge(Death, df1, by = "SEQN") 
table(df$SURVEY_YEAR)

for(col in names(df)) {
  # 计算每一列中的缺失值数量
  missing_values <- sum(is.na(df[[col]]))
  # 打印结果
  print(paste("变量", col, "的缺失样本量为：", missing_values))}

# 对一些人口统计学变量进行修改 
# 定义数据预处理函数
preprocess_data <- function(df) {
  df <- df %>%
    mutate(RIAGENDR = recode(RIAGENDR, 
                             `1` = "Male", 
                             `2` = "Female"))
  df$RIAGENDR <- factor(df$RIAGENDR, 
                        levels = c("Male","Female"))
  df <- df %>%
    mutate( age.factor = case_when(
      RIDAGEYR >= 0 & RIDAGEYR <= 5 ~ "0-5 years",
      RIDAGEYR > 5 & RIDAGEYR <= 19 ~ "6-19 years",
      RIDAGEYR > 19 & RIDAGEYR <= 59 ~ "20-59 years",
      RIDAGEYR > 59 & RIDAGEYR <= 80 ~ "60+ years")) 
  df$age.factor <- factor(df$age.factor, 
                          levels = c("0-5 years",
                                     "6-19 years",
                                     "20-59 years",
                                     "60+ years"))
  df <- df %>%
    mutate( RIDRETH3 = case_when(
      RIDRETH3 == 1 ~ "Mexican American",
      RIDRETH3 == 2 ~ "Other Hispanic",
      RIDRETH3 == 3 ~ "Non-Hispanic White",
      RIDRETH3 == 4 ~ "Non-Hispanic Black",
      RIDRETH3 == 6 ~ "Non-Hispanic Asian",
      RIDRETH3 == 7 ~ "Other/Multi-Racial")) 
  df$RIDRETH3 <- factor(df$RIDRETH3, 
                        levels = c("Mexican American",
                                   "Non-Hispanic White",
                                   "Non-Hispanic Black",
                                   "Non-Hispanic Asian",
                                   "Other Hispanic",
                                   "Other/Multi-Racial"))
  df <- df %>%
    mutate(BMI.factor = recode(BMI.factor, 
                               `偏瘦` = "消瘦"))
  df <- df %>%
    mutate(BMI.factor = recode(BMI.factor, 
                               `肥胖` = "Obesity", 
                               `超重` = "Overweight", 
                               `正常` = "Normal Weight", 
                               `消瘦` = "Underweight"))
  df$BMI.factor <- factor(df$BMI.factor, 
                          levels = c("Underweight",
                                     "Normal Weight",
                                     "Overweight",
                                     "Obesity"))
  df <- df %>%
    mutate( NHANES_cycle = case_when(
      survey_year == 7 ~ "2011-2012",
      survey_year == 8 ~ "2013-2014",
      survey_year == 9 ~ "2015-2016",
      survey_year == 10 ~ "2017-2018")) 
  df$NHANES_cycle <- factor(df$NHANES_cycle, 
                            levels = c("2011-2012",
                                       "2013-2014",
                                       "2015-2016",
                                       "2017-2018"))
  df <- df %>%
    mutate( PIR.factor2 = case_when(
      INDFMPIR <= 1.30 ~ "≤ 1.30",
      INDFMPIR > 1.30 & INDFMPIR <= 3.50 ~ "1.31-3.50",
      INDFMPIR > 3.50 ~ "> 3.50")) 
  df$PIR.factor2 <- factor(df$PIR.factor2, 
                           levels = c("≤ 1.30",
                                      "1.31-3.50",
                                      "> 3.50"))
  df <- df %>%
    mutate( BMI.logistic2 = case_when(
      BMI.factor == "Obesity" ~ 1,
      BMI.factor == "Overweight" ~ 1,
      BMI.factor == "Normal Weight" ~ 0,
      BMI.factor == "Underweight" ~ 0)) 
  
  df <- df %>%
    mutate( PROT = DR1TPROT/BMXWT) 
  
  varsToFactor <- c("survey_year","RIAGENDR","RIDRETH3",
                    "age.factor","BMI.factor","NHANES_cycle",
                    "age.factor_10","BMI_logistic","BMI.logistic2",
                    "PIR.factor","PIR.factor2","Protein_intake.factor")
  df[varsToFactor] <- lapply(df[varsToFactor], factor) 
  df$survey_year <- factor(df$survey_year)
  df$NHANES_cycle <- factor(df$NHANES_cycle)
  df$RIAGENDR <- factor(df$RIAGENDR)
  df$RIDRETH3 <- factor(df$RIDRETH3)
  df$age.factor <- factor(df$age.factor)
  df$BMI.factor <- factor(df$BMI.factor)
  df$BMI_logistic <- factor(df$BMI_logistic)
  df$BMI.logistic2 <- factor(df$BMI.logistic2)
  df$PIR.factor <- factor(df$PIR.factor)
  df$PIR.factor2 <- factor(df$PIR.factor2)
  df$Protein_intake.factor <- factor(df$Protein_intake.factor)
  
  return(df)
}

df <- preprocess_data(df)

# 复杂抽样设计的Cox回归分析 
df <- df %>%
  mutate( Quartile_TOTALPRO_Q4 = case_when(
    HEI2020_TOTALPRO >= 0 & HEI2020_TOTALPRO < 3.37 ~ "TOTALPRO_Q1",
    HEI2020_TOTALPRO >= 3.37 & HEI2020_TOTALPRO < 4.19 ~ "TOTALPRO_Q2",
    HEI2020_TOTALPRO >= 4.19 & HEI2020_TOTALPRO < 4.59 ~ "TOTALPRO_Q3",
    HEI2020_TOTALPRO >= 4.59 & HEI2020_TOTALPRO <= 5.0 ~ "TOTALPRO_Q4")) 
df$Quartile_TOTALPRO_Q4 <- factor(df$Quartile_TOTALPRO_Q4, 
                                  levels = c("TOTALPRO_Q1",
                                             "TOTALPRO_Q2",
                                             "TOTALPRO_Q3",
                                             "TOTALPRO_Q4"))

df <- df %>%
  mutate( Quartile_TOTALPRO_Q3 = case_when(
    HEI2020_TOTALPRO >= 0 & HEI2020_TOTALPRO < 3.37 ~ "TOTALPRO_Q1",
    HEI2020_TOTALPRO >= 3.37 & HEI2020_TOTALPRO < 4.38 ~ "TOTALPRO_Q2",
    HEI2020_TOTALPRO >= 4.38 & HEI2020_TOTALPRO <= 5.0 ~ "TOTALPRO_Q3")) 
df$Quartile_TOTALPRO_Q3 <- factor(df$Quartile_TOTALPRO_Q3, 
                                  levels = c("TOTALPRO_Q1",
                                             "TOTALPRO_Q2",
                                             "TOTALPRO_Q3"))

df <- df %>%
  mutate( Quartile_SEAPLANTPRO_Q4 = case_when(
    HEI2020_SEAPLANTPRO >= 0 & HEI2020_SEAPLANTPRO < 0.08 ~ "SEAPLANTPRO_Q1",
    HEI2020_SEAPLANTPRO >= 0.08 & HEI2020_SEAPLANTPRO < 2.50 ~ "SEAPLANTPRO_Q2",
    HEI2020_SEAPLANTPRO >= 2.50 & HEI2020_SEAPLANTPRO < 3.13 ~ "SEAPLANTPRO_Q3",
    HEI2020_SEAPLANTPRO >= 3.13 & HEI2020_SEAPLANTPRO <= 5 ~ "SEAPLANTPRO_Q4")) 
df$Quartile_SEAPLANTPRO_Q4 <- factor(df$Quartile_SEAPLANTPRO_Q4, 
                                     levels = c("SEAPLANTPRO_Q1",
                                                "SEAPLANTPRO_Q2",
                                                "SEAPLANTPRO_Q3",
                                                "SEAPLANTPRO_Q4"))

df <- df %>%
  mutate( Quartile_DR1TPROT = case_when(
    DR1TPROT >= 0 & DR1TPROT < 47.85 ~ "DR1TPROT_Q1",
    DR1TPROT >= 47.85 & DR1TPROT < 67.44 ~ "DR1TPROT_Q2",
    DR1TPROT >= 67.44 & DR1TPROT < 94.32 ~ "DR1TPROT_Q3",
    DR1TPROT >= 94.32 ~ "DR1TPROT_Q4")) 
df$Quartile_DR1TPROT <- factor(df$Quartile_DR1TPROT, 
                               levels = c("DR1TPROT_Q1",
                                          "DR1TPROT_Q2",
                                          "DR1TPROT_Q3",
                                          "DR1TPROT_Q4"))

NHANES_design_df <- svydesign(
  data = df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust") # 可以加上 survey.lonely.psu = "adjust" 避免1个PSU报错

summary(NHANES_design_df)

# 提取所需变量
df_cox <- df[, c("SEQN","survey_year",
                 "RIAGENDR","RIDAGEYR","RIDRETH3","PIR.factor2",
                 "DR1TPROT","HEI2020_TOTALPRO","HEI2020_SEAPLANTPRO",
                 "Quartile_TOTALPRO_Q4","Quartile_SEAPLANTPRO_Q4","Quartile_DR1TPROT",
                 "BMXBMI","BMI.logistic2","BMI.factor",
                 "ELIGSTAT","MORTSTAT","UCOD_LEADING", "DIABETES",
                 "HYPERTEN", "PERMTH_INT","PERMTH_EXM", "SURVEY_YEAR",
                 "SDMVPSU", "SDMVSTRA","WTDRD1_adjust")]
df_cox<- df_cox[df_cox$ELIGSTAT == 1, ]
table(df_cox$BMI.logistic2)
table(df_cox$BMI.factor)
# write_xlsx(df_cox, "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/2024.07.01_R-Cox中介分析data_23237+22.xlsx")
# write_xlsx(df_cox, "D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/2024.07.02_R-Cox中介分析data_14527+26.xlsx")

# Cox+中介分析 加权版本 论文中暂定版本 
rm(list = ls())
library(mediation)
# Df <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/2024.07.01_R-Cox中介分析data_23237+22.xlsx",col_names = T)
Df <- read_excel("D:/BaiduSyncdisk/1-博士小论文撰写-2024.04.24创建/1 宏观营养素与肥胖-NHANES-2024.04.25创建/R-data/2024.07.02_R-Cox中介分析data_14527+26.xlsx",col_names = T)
Df <- Df[Df$ELIGSTAT == 1, ]
# 提取所需变量
Df <- Df[, c("RIAGENDR","RIDAGEYR","RIDRETH3","PIR.factor2",
             "DR1TPROT","HEI2020_TOTALPRO","HEI2020_SEAPLANTPRO",
             "Quartile_TOTALPRO_Q4","Quartile_SEAPLANTPRO_Q4","Quartile_DR1TPROT",
             "BMXBMI","BMI.logistic2","BMI.factor",
             "MORTSTAT", "PERMTH_INT", 
             "SDMVPSU", "SDMVSTRA","WTDRD1_adjust")]

Df <- Df %>%
  mutate(RIAGENDR = recode(RIAGENDR, 
                           `Male` = "1", 
                           `Female` = "2"))

Df <- Df %>%
  mutate(RIDRETH3 = recode(RIDRETH3, 
                           `Mexican American` = "1", 
                           `Other Hispanic` = "2", 
                           `Non-Hispanic White` = "3", 
                           `Non-Hispanic Black` = "4", 
                           `Non-Hispanic Asian` = "6", 
                           `Other/Multi-Racial` = "7"))

Df <- Df %>%
  mutate(PIR.factor2 = recode(PIR.factor2, 
                              `≤ 1.30` = "1", 
                              `1.31-3.50` = "2", 
                              `> 3.50` = "3"))

Df <- Df %>%
  mutate(Quartile_TOTALPRO_Q4= recode(Quartile_TOTALPRO_Q4, 
                                      `TOTALPRO_Q1` = "1", 
                                      `TOTALPRO_Q2` = "2", 
                                      `TOTALPRO_Q3` = "3", 
                                      `TOTALPRO_Q4` = "4"))

Df <- Df %>%
  mutate(Quartile_SEAPLANTPRO_Q4= recode(Quartile_SEAPLANTPRO_Q4, 
                                         `SEAPLANTPRO_Q1` = "1", 
                                         `SEAPLANTPRO_Q2` = "2", 
                                         `SEAPLANTPRO_Q3` = "3", 
                                         `SEAPLANTPRO_Q4` = "4"))

Df <- Df %>%
  mutate(Quartile_DR1TPROT= recode(Quartile_DR1TPROT, 
                                   `DR1TPROT_Q1` = "1", 
                                   `DR1TPROT_Q2` = "2", 
                                   `DR1TPROT_Q3` = "3", 
                                   `DR1TPROT_Q4` = "4"))

Df <- Df %>%
  mutate(BMI.factor= recode(BMI.factor, 
                            `Underweight` = "1", 
                            `Normal Weight` = "2", 
                            `Overweight` = "3", 
                            `Obesity` = "4"))

Df$BMI.factor <- as.factor(Df$BMI.factor)
Df$RIAGENDR <- as.factor(Df$RIAGENDR)
Df$RIDRETH3 <- as.factor(Df$RIDRETH3)
Df$PIR.factor2 <- as.factor(Df$PIR.factor2)
Df$Quartile_TOTALPRO_Q4 <- as.factor(Df$Quartile_TOTALPRO_Q4)
Df$Quartile_SEAPLANTPRO_Q4 <- as.factor(Df$Quartile_SEAPLANTPRO_Q4)
Df$Quartile_DR1TPROT <- as.factor(Df$Quartile_DR1TPROT)
Df$BMI.logistic2 <- as.factor(Df$BMI.logistic2)

# 加载必要的包
library(survey)
library(survival)
library(mets)

# 1.1 植物蛋白质-中介BMI 
# 创建复杂的调查设计对象
NHANES_design_df <- svydesign(
  data = Df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust"
)
summary(NHANES_design_df)

med.fit <- svyglm(BMXBMI ~ HEI2020_SEAPLANTPRO +
                    RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                  design = NHANES_design_df)
summary(med.fit)
# 使用 svySurv 和 svycoxph 进行加权的生存回归
out.fit <- svysurvreg(Surv(PERMTH_INT, MORTSTAT) ~ BMXBMI + HEI2020_SEAPLANTPRO + 
                        RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                      design = NHANES_design_df)
summary(out.fit)

set.seed(123) #保证结果可以复现
# 进行中介分析
med.out <- mediate(med.fit, out.fit, 
                   treat = "HEI2020_SEAPLANTPRO",
                   mediator = "BMXBMI", 
                   sims = 500,
                   design = NHANES_design_df)

# 输出中介分析结果
med_sum <- summary(med.out)
med_sum

par(cex = 1.5)
plot(med.out)
# 添加图例
legend("topright",           # 图例的位置
       legend = c("treated", "control"),  # 图例的标签
       lty = 1:2,
       pch = c(16, 1),  # 点的形状
       bty = "n")  # 去除外框线


# 1.2 BMI-中介植物蛋白质 
# 创建复杂的调查设计对象
NHANES_design_df <- svydesign(
  data = Df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust"
)
summary(NHANES_design_df)

med.fit <- svyglm(HEI2020_SEAPLANTPRO ~ BMXBMI +
                    RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                  design = NHANES_design_df)
summary(med.fit)
# 使用 svySurv 和 svycoxph 进行加权的生存回归
out.fit <- svysurvreg(Surv(PERMTH_INT, MORTSTAT) ~ HEI2020_SEAPLANTPRO + BMXBMI + 
                        RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                      design = NHANES_design_df)
summary(out.fit)

set.seed(123) #保证结果可以复现
# 进行中介分析
med.out <- mediate(med.fit, out.fit, 
                   treat = "BMXBMI",
                   mediator = "HEI2020_SEAPLANTPRO", 
                   sims = 500,
                   design = NHANES_design_df)

# 输出中介分析结果
med_sum <- summary(med.out)
med_sum

par(cex = 1.5)
plot(med.out)
# 添加图例
legend("topleft",           # 图例的位置
       legend = c("treated", "control"),  # 图例的标签
       lty = 1:2,
       pch = c(16, 1),  # 点的形状
       bty = "n")  # 去除外框线



# 2.1 总蛋白质-中介BMI 
# 创建复杂的调查设计对象
NHANES_design_df <- svydesign(
  data = Df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust"
)
summary(NHANES_design_df)

med.fit <- svyglm(BMXBMI ~ HEI2020_TOTALPRO +
                    RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                  design = NHANES_design_df)
summary(med.fit)
# 使用 svySurv 和 svycoxph 进行加权的生存回归
out.fit <- svysurvreg(Surv(PERMTH_INT, MORTSTAT) ~ BMXBMI + HEI2020_TOTALPRO + 
                        RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                      design = NHANES_design_df)
summary(out.fit)

set.seed(123) #保证结果可以复现
# 进行中介分析
med.out <- mediate(med.fit, out.fit, 
                   treat = "HEI2020_TOTALPRO",
                   mediator = "BMXBMI", 
                   sims = 500,
                   design = NHANES_design_df)

# 输出中介分析结果
med_sum <- summary(med.out)
med_sum

par(cex = 1.5)
plot(med.out)
# 添加图例
legend("topright",           # 图例的位置
       legend = c("treated", "control"),  # 图例的标签
       lty = 1:2,
       pch = c(16, 1),  # 点的形状
       bty = "n")  # 去除外框线


# 2.2 BMI-中介总蛋白质 
# 创建复杂的调查设计对象
NHANES_design_df <- svydesign(
  data = Df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust"
)
summary(NHANES_design_df)

med.fit <- svyglm(HEI2020_TOTALPRO ~ BMXBMI +
                    RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                  design = NHANES_design_df)
summary(med.fit)
# 使用 svySurv 和 svycoxph 进行加权的生存回归
out.fit <- svysurvreg(Surv(PERMTH_INT, MORTSTAT) ~ HEI2020_TOTALPRO + BMXBMI + 
                        RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                      design = NHANES_design_df)
summary(out.fit)

set.seed(123) #保证结果可以复现
# 进行中介分析
med.out <- mediate(med.fit, out.fit, 
                   treat = "BMXBMI",
                   mediator = "HEI2020_TOTALPRO", 
                   sims = 500,
                   design = NHANES_design_df)

# 输出中介分析结果
med_sum <- summary(med.out)
med_sum

par(cex = 1.5)
plot(med.out)
# 添加图例
legend("topleft",           # 图例的位置
       legend = c("treated", "control"),  # 图例的标签
       lty = 1:2,
       pch = c(16, 1),  # 点的形状
       bty = "n")  # 去除外框线


# 3.1 原始蛋白质-中介BMI 
# 创建复杂的调查设计对象
NHANES_design_df <- svydesign(
  data = Df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust"
)
summary(NHANES_design_df)

med.fit <- svyglm(BMXBMI ~ DR1TPROT +
                    RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                  design = NHANES_design_df)
summary(med.fit)
# 使用 svySurv 和 svycoxph 进行加权的生存回归
out.fit <- svysurvreg(Surv(PERMTH_INT, MORTSTAT) ~ BMXBMI + DR1TPROT + 
                        RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                      design = NHANES_design_df)
summary(out.fit)

set.seed(123) #保证结果可以复现
# 进行中介分析
med.out <- mediate(med.fit, out.fit, 
                   treat = "DR1TPROT",
                   mediator = "BMXBMI", 
                   sims = 500,
                   design = NHANES_design_df)

# 输出中介分析结果
med_sum <- summary(med.out)
med_sum

par(cex = 1.5)
plot(med.out)
# 添加图例
legend("topright",           # 图例的位置
       legend = c("treated", "control"),  # 图例的标签
       lty = 1:2,
       pch = c(16, 1),  # 点的形状
       bty = "n")  # 去除外框线


# 3.2 BMI-中介原始蛋白质 
# 创建复杂的调查设计对象
NHANES_design_df <- svydesign(
  data = Df, 
  ids = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  nest = TRUE, 
  weights = ~WTDRD1_adjust,
  survey.lonely.psu = "adjust"
)
summary(NHANES_design_df)

med.fit <- svyglm(DR1TPROT ~ BMXBMI +
                    RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                  design = NHANES_design_df)
summary(med.fit)
# 使用 svySurv 和 svycoxph 进行加权的生存回归
out.fit <- svysurvreg(Surv(PERMTH_INT, MORTSTAT) ~ DR1TPROT + BMXBMI + 
                        RIAGENDR + RIDRETH3 + RIDAGEYR + PIR.factor2,
                      design = NHANES_design_df)
summary(out.fit)

set.seed(123) #保证结果可以复现
# 进行中介分析
med.out <- mediate(med.fit, out.fit, 
                   treat = "BMXBMI",
                   mediator = "DR1TPROT", 
                   sims = 500,
                   design = NHANES_design_df)

# 输出中介分析结果
med_sum <- summary(med.out)
med_sum

par(cex = 1.5)
plot(med.out)
# 添加图例
legend("topleft",           # 图例的位置
       legend = c("treated", "control"),  # 图例的标签
       lty = 1:2,
       pch = c(16, 1),  # 点的形状
       bty = "n")  # 去除外框线







