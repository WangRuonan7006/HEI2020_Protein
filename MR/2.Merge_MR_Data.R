rm(list=ls())
setwd("E:\\work\\wrn\\")

# Read processed MR Data
BCAA_BMI<-read.csv("result_BCAA_BMI.csv")
Leucine_BMI<-read.csv("result_Leucine_BMI.csv")
Isoleucine_BMI<-read.csv("result_Isoleucine_BMI.csv")
Valine_BMI<-read.csv("result_Valine_BMI.csv")
Methionine_BMI<-read.csv("result_Methionine_BMI.csv")
Phenylalanine_BMI<-read.csv("result_Phenylalanine_BMI.csv")
Threonine_BMI<-read.csv("result_Threonine_BMI.csv")
Tryptophan_BMI<-read.csv("result_Tryptophan_BMI.csv")
Lysine_BMI<-read.csv("result_Lysine_BMI.csv")
# Add mark column
BCAA_BMI$Source <- 'BCAA_BMI'
Leucine_BMI$Source <- 'Leucine_BMI'
Isoleucine_BMI$Source <- 'Isoleucine_BMI'
Valine_BMI$Source <- 'Valine_BMI'
Methionine_BMI$Source <- 'Methionine_BMI'
Phenylalanine_BMI$Source <- 'Phenylalanine_BMI'
Threonine_BMI$Source <- 'Threonine_BMI'
Tryptophan_BMI$Source <- 'Tryptophan_BMI'
Lysine_BMI$Source <- 'Lysine_BMI'


AAs_merge <- rbind(BCAA_BMI, Leucine_BMI, Isoleucine_BMI, Valine_BMI, Methionine_BMI, 
                   Phenylalanine_BMI, Threonine_BMI, Tryptophan_BMI, Lysine_BMI)

write.csv(AAs_merge,'AAs_merge1.csv', row.names = F)
