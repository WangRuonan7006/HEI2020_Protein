# BCAA####
rm(list=ls())
setwd("E:\\work\\wrn\\")
library(TwoSampleMR)
library(ieugwasr)
library(MRPRESSO)
library(dplyr)

# Load data
exposure_dat <- extract_instruments(c('met-d-Total_BCAA'))
# exposure_dat <- extract_instruments(c('ebi-a-GCST90092891'))
# exposure_dat <- extract_instruments(c('met-d-Total_BCAA'),p1 = 5e-08)
exposure_dat$samplesize.exposure<-115047

outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ebi-a-GCST90018947'), proxies = 1, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
write.csv(exposure_dat, file="exposure_BCAA.csv",row.names = F)
write.csv(outcome_dat, file="outcome_BCAA_BMI.csv",row.names = F)
exposure_dat <- read.csv("exposure_BCAA.csv")
outcome_dat <- read.csv("outcome_BCAA_BMI.csv")
# Remove weak instrumental variables
source("calculate_R2_and_F.R",encoding = "utf-8")
result <- calculate_R2_and_F(exposure_dat$SNP,exposure_dat$eaf.exposure, exposure_dat$beta.exposure, exposure_dat$se.exposure, exposure_dat$samplesize.exposure)
result
write.csv(result, file="F_BCAA_BMI.csv",row.names = F)

# Merge Data
dat <- harmonise_data(exposure_dat, outcome_dat)
write.csv(dat, file="harmonise_BCAA_BMI.csv",row.names = F)

# Perform heterogeneity test
heterogeneity_results <- mr_heterogeneity(dat)

# Perform different analyses
mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))

# Run MR-PRESSO analysis, iteratively removing horizontal pleiotropy ####
presso_results <- run_mr_presso(dat, NbDistribution = 1000)

# Extract p-values for outlier and global tests
outlier_pvals <- presso_results[[1]]$`MR-PRESSO results`$`Outlier Test`

dat$NO<-as.numeric(rownames(dat))
outlier_pvals$NO<-as.numeric(rownames(outlier_pvals))
matched_indices <- merge(outlier_pvals, dat[, c("NO", "SNP")], by = "NO", all.x = TRUE)


snp_data <-matched_indices

snp_data <- snp_data[order(snp_data$Pvalue), ]
global_pval <- presso_results[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Initialize a list to store SNPs to be removed
removed_snps <- list()

# Initialize a list to store the recalculated results each time
all_results <- list()

# Remove SNPs one by one and re-run the global test
while (global_pval <= 0.05 && nrow(snp_data) > 0) {
  # Remove the SNP with the smallest p-value
  snp_to_remove <- snp_data$SNP[1]
  removed_snps <- c(removed_snps, snp_to_remove)
  
  # Remove the SNP from the data
  snp_data <- snp_data[-1, ]
  
  # Remove the corresponding SNP from 'dat'
  dat <- dat[!dat$SNP %in% snp_to_remove, ]
  
  # Recalculate 'MR-PRESSO global'
  
  # Recalculate 'presso_results'
  presso_results1 <- run_mr_presso(dat, NbDistribution = 1000)
  global_pval1 <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue
  # Save each recalculated result into the list
  all_results[[length(all_results) + 1]] <- list(
    snp_removed = snp_to_remove,
    global_pval = global_pval1,
    remaining_snps = dat$snp
  )
  # Update the current global p-value
  global_pval <- global_pval1
}

# Output the list of remaining SNPs
# remaining_snps <- dat$SNP
# remaining_snps

# Output the processed harmonized data
write.csv(dat, file="harmonise_BCAA_BMI_detail.csv",row.names = F)
dat <- read.csv("harmonise_BCAA_BMI_detail.csv")

# Recalculate MR

mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))
FEOR <-generate_odds_ratios(mr_results)
# Extract IVW and MR-Egger results
ivw_result <- FEOR %>% filter(method == "Inverse variance weighted (fixed effects)")
ivw_result_random <- FEOR %>% filter(method == "Inverse variance weighted (multiplicative random effects)")
egger_result <- FEOR %>% filter(method == "MR Egger")
weighted_median_result <- FEOR %>% filter(method == "Weighted median")
Weighted_mode_result<- FEOR %>% filter(method == "Weighted mode")

# Re-run heterogeneity test ####
heterogeneity_results <- mr_heterogeneity(dat)
# Extract heterogeneity results
ivw_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted")
egger_heterogeneity <- heterogeneity_results %>% filter(method == "MR Egger")
weighted_median_heterogeneity <- heterogeneity_results %>% filter(method == "Weighted median")
Weighted_mode_heterogeneity<- heterogeneity_results %>% filter(method == "Weighted mode")
ivw_random_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted (multiplicative random effects)")

# Extract MR-PRESSO results
presso_global_pval <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Extract pleiotropy test results
egger_pleiotropy <-  mr_pleiotropy_test(dat)
#pleiotropy_results %>% filter(method == "MR Egger regression")

# Create a results table ####
results_table <- data.frame(
  Method = c("BCAA-BMI","IVW-fixed","IVW-random","Weighted-median","Weighted-mode","MR-Egger"),
  Beta = c(NA,ivw_result$b,ivw_result_random$b,weighted_median_result$b ,Weighted_mode_result$b ,egger_result$b),
  SE = c(NA,ivw_result$se, ivw_result_random$se,weighted_median_result$se,Weighted_mode_result$b , egger_result$se),
  P_value = c(NA,ivw_result$pval, ivw_result_random$pval,weighted_median_result$pval, Weighted_mode_result$pval,egger_result$pval),
  CI_lower = c(NA,ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
  CI_upper = c(NA,ivw_result$or_uci95,ivw_result_random$or_uci95,  weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95),
  P_value_CI = c(NA,sprintf("%.3f (%.3f, %.3f)",
                            c(ivw_result$or  , ivw_result_random$or  ,weighted_median_result$or  , Weighted_mode_result$or  ,egger_result$or  ), 
                            c(ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
                            c(ivw_result$or_uci95, ivw_result_random$or_uci95, weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95))))

results_table1 <- data.frame(
  N_of_SNP=c(nrow(dat),NA,NA,NA,NA,NA),
  Heterogeneity_test = c(round(egger_heterogeneity$Q_pval,3),NA,NA,NA,NA,NA),#Perform heterogeneity testMREgger
  Pleiotropy_test = c(round(egger_pleiotropy$pval,3),NA,NA,NA,NA,NA),#多效性
  PRESSO_Global_test = c(round(presso_global_pval,3),NA,NA,NA,NA,NA))#全局多效性

#Print results table
result_merge1<-cbind(results_table,results_table1)
print(result_merge1)
write.csv(result_merge1,"result_BCAA_BMI.csv",row.names = F)


# Leucine####
rm(list=ls())
setwd("E:\\work\\wrn\\")
library(TwoSampleMR)
library(ieugwasr)
library(MRPRESSO)
library(dplyr)

# Load data

exposure_dat <- extract_instruments(c('met-d-Leu'))#ebi-a-GCST90092891
# exposure_dat <- extract_instruments(c('ebi-a-GCST90018947'))
# exposure_dat <- extract_instruments(c('met-d-Total_Leucine'),p1 = 5e-08)
exposure_dat$samplesize.exposure<-115074
# GCST90092891
outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ebi-a-GCST90018947'), proxies = 1, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
write.csv(exposure_dat, file="exposure_Leucine.csv",row.names = F)
write.csv(outcome_dat, file="outcome_Leucine_BMI.csv",row.names = F)
exposure_dat <- read.csv("exposure_Leucine.csv")
outcome_dat <- read.csv("outcome_Leucine_BMI.csv")
# Remove weak instrumental variables
source("calculate_R2_and_F.R",encoding = "utf-8")
result <- calculate_R2_and_F(exposure_dat$SNP,exposure_dat$eaf.exposure, exposure_dat$beta.exposure, exposure_dat$se.exposure, exposure_dat$samplesize.exposure)
result
write.csv(result, file="F_Leucine_BMI.csv",row.names = F)


# Merge Data
dat <- harmonise_data(exposure_dat, outcome_dat)
write.csv(dat, file="harmonise_Leucine_BMI.csv",row.names = F)

# Perform heterogeneity test

heterogeneity_results <- mr_heterogeneity(dat)


# Perform different analyses
mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))



# Run MR-PRESSO analysis, iteratively removing horizontal pleiotropy####
presso_results <- run_mr_presso(dat, NbDistribution = 1000)

# Extract p-values for outlier and global tests
outlier_pvals <- presso_results[[1]]$`MR-PRESSO results`$`Outlier Test`


dat$NO<-as.numeric(rownames(dat))
outlier_pvals$NO<-as.numeric(rownames(outlier_pvals))
matched_indices <- merge(outlier_pvals, dat[, c("NO", "SNP")], by = "NO", all.x = TRUE)

snp_data <-matched_indices


snp_data <- snp_data[order(snp_data$Pvalue), ]
global_pval <- presso_results[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Initialize a list to store SNPs to be removed
removed_snps <- list()

# Initialize a list to store the recalculated results each time
all_results <- list()

# Remove SNPs one by one and re-run the global test
while (global_pval <= 0.05 && nrow(snp_data) > 0) {
  # Remove the SNP with the smallest p-value
  snp_to_remove <- snp_data$SNP[1]
  removed_snps <- c(removed_snps, snp_to_remove)
  
  # Remove the SNP from the data
  snp_data <- snp_data[-1, ]
  
  # Remove the corresponding SNP from 'dat'
  dat <- dat[!dat$SNP %in% snp_to_remove, ]
  
  # Recalculate 'MR-PRESSO global'
  
  # Recalculate 'presso_results'
  presso_results1 <- run_mr_presso(dat, NbDistribution = 1000)
  global_pval1 <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue
  # Save each recalculated result into the list
  all_results[[length(all_results) + 1]] <- list(
    snp_removed = snp_to_remove,
    global_pval = global_pval1,
    remaining_snps = dat$snp
  )
  # Update the current global p-value
  global_pval <- global_pval1
}

# Output the list of remaining SNPs
# remaining_snps <- dat$SNP
# remaining_snps

# Output the processed harmonized data
write.csv(dat, file="harmonise_Leucine_BMI_detail.csv",row.names = F)
dat <- read.csv("harmonise_Leucine_BMI_detail.csv")

# Recalculate MR

mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))
FEOR <-generate_odds_ratios(mr_results)
# Extract IVW and MR-Egger results
ivw_result <- FEOR %>% filter(method == "Inverse variance weighted (fixed effects)")
ivw_result_random <- FEOR %>% filter(method == "Inverse variance weighted (multiplicative random effects)")
egger_result <- FEOR %>% filter(method == "MR Egger")
weighted_median_result <- FEOR %>% filter(method == "Weighted median")
Weighted_mode_result<- FEOR %>% filter(method == "Weighted mode")

# Re-run heterogeneity test ####
heterogeneity_results <- mr_heterogeneity(dat)
# Extract heterogeneity results
ivw_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted")
egger_heterogeneity <- heterogeneity_results %>% filter(method == "MR Egger")
weighted_median_heterogeneity <- heterogeneity_results %>% filter(method == "Weighted median")
Weighted_mode_heterogeneity<- heterogeneity_results %>% filter(method == "Weighted mode")
ivw_random_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted (multiplicative random effects)")

# Extract MR-PRESSO results
presso_global_pval <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Extract pleiotropy test results
egger_pleiotropy <-  mr_pleiotropy_test(dat)
#pleiotropy_results %>% filter(method == "MR Egger regression")

# Create a results table ####
results_table <- data.frame(
  Method = c("Leucine-BMI","IVW-fixed","IVW-random","Weighted-median","Weighted-mode","MR-Egger"),
  Beta = c(NA,ivw_result$b,ivw_result_random$b,weighted_median_result$b ,Weighted_mode_result$b ,egger_result$b),
  SE = c(NA,ivw_result$se, ivw_result_random$se,weighted_median_result$se,Weighted_mode_result$b , egger_result$se),
  P_value = c(NA,ivw_result$pval, ivw_result_random$pval,weighted_median_result$pval, Weighted_mode_result$pval,egger_result$pval),
  CI_lower = c(NA,ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
  CI_upper = c(NA,ivw_result$or_uci95,ivw_result_random$or_uci95,  weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95),
  P_value_CI = c(NA,sprintf("%.3f (%.3f, %.3f)",
                            c(ivw_result$or  , ivw_result_random$or  ,weighted_median_result$or  , Weighted_mode_result$or  ,egger_result$or  ), 
                            c(ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
                            c(ivw_result$or_uci95, ivw_result_random$or_uci95, weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95))))

results_table1 <- data.frame(
  N_of_SNP=c(nrow(dat),NA,NA,NA,NA,NA),
  Heterogeneity_test = c(round(egger_heterogeneity$Q_pval,3),NA,NA,NA,NA,NA),#Perform heterogeneity testMREgger
  Pleiotropy_test = c(round(egger_pleiotropy$pval,3),NA,NA,NA,NA,NA),#多效性
  PRESSO_Global_test = c(round(presso_global_pval,3),NA,NA,NA,NA,NA))#全局多效性

#Print results table
result_merge1<-cbind(results_table,results_table1)
print(result_merge1)
write.csv(result_merge1,"result_Leucine_BMI.csv",row.names = F)

# Isoleucine####
rm(list=ls())
setwd("E:\\work\\wrn\\")
library(TwoSampleMR)
library(ieugwasr)
library(MRPRESSO)
library(dplyr)

# Load data

exposure_dat <- extract_instruments(c('met-d-Ile'))
# exposure_dat <- extract_instruments(c('ebi-a-GCST90092891'))
# exposure_dat <- extract_instruments(c('met-d-Total_Isoleucine'),p1 = 5e-08)
exposure_dat$samplesize.exposure<-115075

outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ebi-a-GCST90018947'), proxies = 1, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
write.csv(exposure_dat, file="exposure_Isoleucine.csv",row.names = F)
write.csv(outcome_dat, file="outcome_Isoleucine_BMI.csv",row.names = F)
exposure_dat <- read.csv("exposure_Isoleucine.csv")
outcome_dat <- read.csv("outcome_Isoleucine_BMI.csv")
# Remove weak instrumental variables
source("calculate_R2_and_F.R",encoding = "utf-8")
result <- calculate_R2_and_F(exposure_dat$SNP,exposure_dat$eaf.exposure, exposure_dat$beta.exposure, exposure_dat$se.exposure, exposure_dat$samplesize.exposure)
result
write.csv(result, file="F_Isoleucine_BMI.csv",row.names = F)


# Merge Data
dat <- harmonise_data(exposure_dat, outcome_dat)
write.csv(dat, file="harmonise_Isoleucine_BMI.csv",row.names = F)

# Perform heterogeneity test

heterogeneity_results <- mr_heterogeneity(dat)


# Perform different analyses
mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))



# Run MR-PRESSO analysis, iteratively removing horizontal pleiotropy####
presso_results <- run_mr_presso(dat, NbDistribution = 1000)

# Extract p-values for outlier and global tests
outlier_pvals <- presso_results[[1]]$`MR-PRESSO results`$`Outlier Test`


dat$NO<-as.numeric(rownames(dat))
outlier_pvals$NO<-as.numeric(rownames(outlier_pvals))
matched_indices <- merge(outlier_pvals, dat[, c("NO", "SNP")], by = "NO", all.x = TRUE)




snp_data <-matched_indices


snp_data <- snp_data[order(snp_data$Pvalue), ]
global_pval <- presso_results[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Initialize a list to store SNPs to be removed
removed_snps <- list()

# Initialize a list to store the recalculated results each time
all_results <- list()

# Remove SNPs one by one and re-run the global test
while (global_pval <= 0.05 && nrow(snp_data) > 0) {
  # Remove the SNP with the smallest p-value
  snp_to_remove <- snp_data$SNP[1]
  removed_snps <- c(removed_snps, snp_to_remove)
  
  # Remove the SNP from the data
  snp_data <- snp_data[-1, ]
  
  # Remove the corresponding SNP from 'dat'
  dat <- dat[!dat$SNP %in% snp_to_remove, ]
  
  # Recalculate 'MR-PRESSO global'
  
  # Recalculate 'presso_results'
  presso_results1 <- run_mr_presso(dat, NbDistribution = 1000)
  global_pval1 <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue
  # Save each recalculated result into the list
  all_results[[length(all_results) + 1]] <- list(
    snp_removed = snp_to_remove,
    global_pval = global_pval1,
    remaining_snps = dat$snp
  )
  # Update the current global p-value
  global_pval <- global_pval1
}

# Output the list of remaining SNPs
# remaining_snps <- dat$SNP
# remaining_snps

# Output the processed harmonized data
write.csv(dat, file="harmonise_Isoleucine_BMI_detail.csv",row.names = F)
dat <- read.csv("harmonise_Isoleucine_BMI_detail.csv")

# Recalculate MR

mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))
FEOR <-generate_odds_ratios(mr_results)
# Extract IVW and MR-Egger results
ivw_result <- FEOR %>% filter(method == "Inverse variance weighted (fixed effects)")
ivw_result_random <- FEOR %>% filter(method == "Inverse variance weighted (multiplicative random effects)")
egger_result <- FEOR %>% filter(method == "MR Egger")
weighted_median_result <- FEOR %>% filter(method == "Weighted median")
Weighted_mode_result<- FEOR %>% filter(method == "Weighted mode")

# Re-run heterogeneity test ####
heterogeneity_results <- mr_heterogeneity(dat)
# Extract heterogeneity results
ivw_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted")
egger_heterogeneity <- heterogeneity_results %>% filter(method == "MR Egger")
weighted_median_heterogeneity <- heterogeneity_results %>% filter(method == "Weighted median")
Weighted_mode_heterogeneity<- heterogeneity_results %>% filter(method == "Weighted mode")
ivw_random_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted (multiplicative random effects)")

# Extract MR-PRESSO results
presso_global_pval <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Extract pleiotropy test results
egger_pleiotropy <-  mr_pleiotropy_test(dat)
#pleiotropy_results %>% filter(method == "MR Egger regression")

# Create a results table ####
results_table <- data.frame(
  Method = c("Isoleucine-BMI","IVW-fixed","IVW-random","Weighted-median","Weighted-mode","MR-Egger"),
  Beta = c(NA,ivw_result$b,ivw_result_random$b,weighted_median_result$b ,Weighted_mode_result$b ,egger_result$b),
  SE = c(NA,ivw_result$se, ivw_result_random$se,weighted_median_result$se,Weighted_mode_result$b , egger_result$se),
  P_value = c(NA,ivw_result$pval, ivw_result_random$pval,weighted_median_result$pval, Weighted_mode_result$pval,egger_result$pval),
  CI_lower = c(NA,ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
  CI_upper = c(NA,ivw_result$or_uci95,ivw_result_random$or_uci95,  weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95),
  P_value_CI = c(NA,sprintf("%.3f (%.3f, %.3f)",
                            c(ivw_result$or  , ivw_result_random$or  ,weighted_median_result$or  , Weighted_mode_result$or  ,egger_result$or  ), 
                            c(ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
                            c(ivw_result$or_uci95, ivw_result_random$or_uci95, weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95))))

results_table1 <- data.frame(
  N_of_SNP=c(nrow(dat),NA,NA,NA,NA,NA),
  Heterogeneity_test = c(round(egger_heterogeneity$Q_pval,3),NA,NA,NA,NA,NA),#Perform heterogeneity testMREgger
  Pleiotropy_test = c(round(egger_pleiotropy$pval,3),NA,NA,NA,NA,NA),#多效性
  PRESSO_Global_test = c(round(presso_global_pval,3),NA,NA,NA,NA,NA))#全局多效性

#Print results table
result_merge1<-cbind(results_table,results_table1)
print(result_merge1)
write.csv(result_merge1,"result_Isoleucine_BMI.csv",row.names = F)



# install.packages("devtools")
# devtools::install_github("MRCIEU/TwoSampleMR")
# install.packages("MRPRESSO")
# Valine####
rm(list=ls())
setwd("E:\\work\\wrn\\")
library(TwoSampleMR)
library(ieugwasr)
library(MRPRESSO)
library(dplyr)

# Load data

exposure_dat <- extract_instruments(c('met-d-Val'))
# exposure_dat <- extract_instruments(c('ebi-a-GCST90092891'))
# exposure_dat <- extract_instruments(c('met-d-Total_Valine'),p1 = 5e-08)
exposure_dat$samplesize.exposure<-115075

outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ebi-a-GCST90018947'), proxies = 1, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
write.csv(exposure_dat, file="exposure_Valine.csv",row.names = F)
write.csv(outcome_dat, file="outcome_Valine_BMI.csv",row.names = F)
exposure_dat <- read.csv("exposure_Valine.csv")
outcome_dat <- read.csv("outcome_Valine_BMI.csv")
# Remove weak instrumental variables
source("calculate_R2_and_F.R",encoding = "utf-8")
result <- calculate_R2_and_F(exposure_dat$SNP,exposure_dat$eaf.exposure, exposure_dat$beta.exposure, exposure_dat$se.exposure, exposure_dat$samplesize.exposure)
result
write.csv(result, file="F_Valine_BMI.csv",row.names = F)


# Merge Data
dat <- harmonise_data(exposure_dat, outcome_dat)
write.csv(dat, file="harmonise_Valine_BMI.csv",row.names = F)

# Perform heterogeneity test

heterogeneity_results <- mr_heterogeneity(dat)


# Perform different analyses
mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))



# Run MR-PRESSO analysis, iteratively removing horizontal pleiotropy####
presso_results <- run_mr_presso(dat, NbDistribution = 1000)

# Extract p-values for outlier and global tests
outlier_pvals <- presso_results[[1]]$`MR-PRESSO results`$`Outlier Test`


dat$NO<-as.numeric(rownames(dat))
outlier_pvals$NO<-as.numeric(rownames(outlier_pvals))
matched_indices <- merge(outlier_pvals, dat[, c("NO", "SNP")], by = "NO", all.x = TRUE)




snp_data <-matched_indices


snp_data <- snp_data[order(snp_data$Pvalue), ]
global_pval <- presso_results[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Initialize a list to store SNPs to be removed
removed_snps <- list()

# Initialize a list to store the recalculated results each time
all_results <- list()

# Remove SNPs one by one and re-run the global test
while (global_pval <= 0.05 && nrow(snp_data) > 0) {
  # Remove the SNP with the smallest p-value
  snp_to_remove <- snp_data$SNP[1]
  removed_snps <- c(removed_snps, snp_to_remove)
  
  # Remove the SNP from the data
  snp_data <- snp_data[-1, ]
  
  # Remove the corresponding SNP from 'dat'
  dat <- dat[!dat$SNP %in% snp_to_remove, ]
  
  # Recalculate 'MR-PRESSO global'
  
  # Recalculate 'presso_results'
  presso_results1 <- run_mr_presso(dat, NbDistribution = 1000)
  global_pval1 <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue
  # Save each recalculated result into the list
  all_results[[length(all_results) + 1]] <- list(
    snp_removed = snp_to_remove,
    global_pval = global_pval1,
    remaining_snps = dat$snp
  )
  # Update the current global p-value
  global_pval <- global_pval1
}

# Output the list of remaining SNPs
# remaining_snps <- dat$SNP
# remaining_snps

# Output the processed harmonized data
write.csv(dat, file="harmonise_Valine_BMI_detail.csv",row.names = F)
dat <- read.csv("harmonise_Valine_BMI_detail.csv")

# Recalculate MR

mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))
FEOR <-generate_odds_ratios(mr_results)
# Extract IVW and MR-Egger results
ivw_result <- FEOR %>% filter(method == "Inverse variance weighted (fixed effects)")
ivw_result_random <- FEOR %>% filter(method == "Inverse variance weighted (multiplicative random effects)")
egger_result <- FEOR %>% filter(method == "MR Egger")
weighted_median_result <- FEOR %>% filter(method == "Weighted median")
Weighted_mode_result<- FEOR %>% filter(method == "Weighted mode")

# Re-run heterogeneity test ####
heterogeneity_results <- mr_heterogeneity(dat)
# Extract heterogeneity results
ivw_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted")
egger_heterogeneity <- heterogeneity_results %>% filter(method == "MR Egger")
weighted_median_heterogeneity <- heterogeneity_results %>% filter(method == "Weighted median")
Weighted_mode_heterogeneity<- heterogeneity_results %>% filter(method == "Weighted mode")
ivw_random_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted (multiplicative random effects)")

# Extract MR-PRESSO results
presso_global_pval <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Extract pleiotropy test results
egger_pleiotropy <-  mr_pleiotropy_test(dat)
#pleiotropy_results %>% filter(method == "MR Egger regression")

# Create a results table ####
results_table <- data.frame(
  Method = c("Valine-BMI","IVW-fixed","IVW-random","Weighted-median","Weighted-mode","MR-Egger"),
  Beta = c(NA,ivw_result$b,ivw_result_random$b,weighted_median_result$b ,Weighted_mode_result$b ,egger_result$b),
  SE = c(NA,ivw_result$se, ivw_result_random$se,weighted_median_result$se,Weighted_mode_result$b , egger_result$se),
  P_value = c(NA,ivw_result$pval, ivw_result_random$pval,weighted_median_result$pval, Weighted_mode_result$pval,egger_result$pval),
  CI_lower = c(NA,ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
  CI_upper = c(NA,ivw_result$or_uci95,ivw_result_random$or_uci95,  weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95),
  P_value_CI = c(NA,sprintf("%.3f (%.3f, %.3f)",
                            c(ivw_result$or  , ivw_result_random$or  ,weighted_median_result$or  , Weighted_mode_result$or  ,egger_result$or  ), 
                            c(ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
                            c(ivw_result$or_uci95, ivw_result_random$or_uci95, weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95))))

results_table1 <- data.frame(
  N_of_SNP=c(nrow(dat),NA,NA,NA,NA,NA),
  Heterogeneity_test = c(round(egger_heterogeneity$Q_pval,3),NA,NA,NA,NA,NA),#Perform heterogeneity testMREgger
  Pleiotropy_test = c(round(egger_pleiotropy$pval,3),NA,NA,NA,NA,NA),#多效性
  PRESSO_Global_test = c(round(presso_global_pval,3),NA,NA,NA,NA,NA))#全局多效性

#Print results table
result_merge1<-cbind(results_table,results_table1)
print(result_merge1)
write.csv(result_merge1,"result_Valine_BMI.csv",row.names = F)

# Threonine####
rm(list=ls())
setwd("E:\\work\\wrn\\")
library(TwoSampleMR)
library(ieugwasr)
library(MRPRESSO)
library(dplyr)

# Load data

exposure_dat <- extract_instruments(c('met-a-324'),p1=5e-06)
# exposure_dat <- extract_instruments(c('ebi-a-GCST90092891'))

# exposure_dat <- extract_instruments(c('met-d-Total_Threonine'),p1 = 5e-08)
exposure_dat$samplesize.exposure<-6020

outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ebi-a-GCST90018947'), proxies = 1, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
write.csv(exposure_dat, file="exposure_Threonine.csv",row.names = F)
write.csv(outcome_dat, file="outcome_Threonine_BMI.csv",row.names = F)
exposure_dat <- read.csv("exposure_Threonine.csv")
outcome_dat <- read.csv("outcome_Threonine_BMI.csv")
# Remove weak instrumental variables
source("calculate_R2_and_F.R",encoding = "utf-8")
result <- calculate_R2_and_F(exposure_dat$SNP,exposure_dat$eaf.exposure, exposure_dat$beta.exposure, exposure_dat$se.exposure, exposure_dat$samplesize.exposure)
result
write.csv(result, file="F_Threonine_BMI.csv",row.names = F)


# Merge Data
dat <- harmonise_data(exposure_dat, outcome_dat)
write.csv(dat, file="harmonise_Threonine_BMI.csv",row.names = F)

# Perform heterogeneity test

heterogeneity_results <- mr_heterogeneity(dat)


# Perform different analyses
mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))



# Run MR-PRESSO analysis, iteratively removing horizontal pleiotropy####
presso_results <- run_mr_presso(dat, NbDistribution = 1000)

# Extract p-values for outlier and global tests
outlier_pvals <- presso_results[[1]]$`MR-PRESSO results`$`Outlier Test`


dat$NO<-as.numeric(rownames(dat))
outlier_pvals$NO<-as.numeric(rownames(outlier_pvals))
matched_indices <- merge(outlier_pvals, dat[, c("NO", "SNP")], by = "NO", all.x = TRUE)




snp_data <-matched_indices


snp_data <- snp_data[order(snp_data$Pvalue), ]
global_pval <- presso_results[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Initialize a list to store SNPs to be removed
removed_snps <- list()

# Initialize a list to store the recalculated results each time
all_results <- list()

# Remove SNPs one by one and re-run the global test
while (global_pval <= 0.05 && nrow(snp_data) > 0) {
  # Remove the SNP with the smallest p-value
  snp_to_remove <- snp_data$SNP[1]
  removed_snps <- c(removed_snps, snp_to_remove)
  
  # Remove the SNP from the data
  snp_data <- snp_data[-1, ]
  
  # Remove the corresponding SNP from 'dat'
  dat <- dat[!dat$SNP %in% snp_to_remove, ]
  
  # Recalculate 'MR-PRESSO global'
  
  # Recalculate 'presso_results'
  presso_results1 <- run_mr_presso(dat, NbDistribution = 1000)
  global_pval1 <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue
  # Save each recalculated result into the list
  all_results[[length(all_results) + 1]] <- list(
    snp_removed = snp_to_remove,
    global_pval = global_pval1,
    remaining_snps = dat$snp
  )
  # Update the current global p-value
  global_pval <- global_pval1
}

# Output the list of remaining SNPs
# remaining_snps <- dat$SNP
# remaining_snps

# Output the processed harmonized data
write.csv(dat, file="harmonise_Threonine_BMI_detail.csv",row.names = F)
dat <- read.csv("harmonise_Threonine_BMI_detail.csv")

# Recalculate MR

mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))
FEOR <-generate_odds_ratios(mr_results)
# Extract IVW and MR-Egger results
ivw_result <- FEOR %>% filter(method == "Inverse variance weighted (fixed effects)")
ivw_result_random <- FEOR %>% filter(method == "Inverse variance weighted (multiplicative random effects)")
egger_result <- FEOR %>% filter(method == "MR Egger")
weighted_median_result <- FEOR %>% filter(method == "Weighted median")
Weighted_mode_result<- FEOR %>% filter(method == "Weighted mode")

# Re-run heterogeneity test ####
heterogeneity_results <- mr_heterogeneity(dat)
# Extract heterogeneity results
ivw_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted")
egger_heterogeneity <- heterogeneity_results %>% filter(method == "MR Egger")
weighted_median_heterogeneity <- heterogeneity_results %>% filter(method == "Weighted median")
Weighted_mode_heterogeneity<- heterogeneity_results %>% filter(method == "Weighted mode")
ivw_random_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted (multiplicative random effects)")

# Extract MR-PRESSO results
presso_global_pval <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Extract pleiotropy test results
egger_pleiotropy <-  mr_pleiotropy_test(dat)
#pleiotropy_results %>% filter(method == "MR Egger regression")

# Create a results table ####
results_table <- data.frame(
  Method = c("Threonine-BMI","IVW-fixed","IVW-random","Weighted-median","Weighted-mode","MR-Egger"),
  Beta = c(NA,ivw_result$b,ivw_result_random$b,weighted_median_result$b ,Weighted_mode_result$b ,egger_result$b),
  SE = c(NA,ivw_result$se, ivw_result_random$se,weighted_median_result$se,Weighted_mode_result$b , egger_result$se),
  P_value = c(NA,ivw_result$pval, ivw_result_random$pval,weighted_median_result$pval, Weighted_mode_result$pval,egger_result$pval),
  CI_lower = c(NA,ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
  CI_upper = c(NA,ivw_result$or_uci95,ivw_result_random$or_uci95,  weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95),
  P_value_CI = c(NA,sprintf("%.3f (%.3f, %.3f)",
                            c(ivw_result$or  , ivw_result_random$or  ,weighted_median_result$or  , Weighted_mode_result$or  ,egger_result$or  ), 
                            c(ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
                            c(ivw_result$or_uci95, ivw_result_random$or_uci95, weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95))))

results_table1 <- data.frame(
  N_of_SNP=c(nrow(dat),NA,NA,NA,NA,NA),
  Heterogeneity_test = c(round(egger_heterogeneity$Q_pval,3),NA,NA,NA,NA,NA),#Perform heterogeneity testMREgger
  Pleiotropy_test = c(round(egger_pleiotropy$pval,3),NA,NA,NA,NA,NA),#多效性
  PRESSO_Global_test = c(round(presso_global_pval,3),NA,NA,NA,NA,NA))#全局多效性

#Print results table
result_merge1<-cbind(results_table,results_table1)
print(result_merge1)
write.csv(result_merge1,"result_Threonine_BMI.csv",row.names = F)


# Tryptophan####
rm(list=ls())
setwd("E:\\work\\wrn\\")
library(TwoSampleMR)
library(ieugwasr)
library(MRPRESSO)
library(dplyr)

# Load data

exposure_dat <- extract_instruments(c('met-a-304'))
# exposure_dat <- extract_instruments(c('ebi-a-GCST90092891'))
# exposure_dat <- extract_instruments(c('met-d-Total_Tryptophan'),p1 = 5e-08)
exposure_dat$samplesize.exposure<-7804

outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ebi-a-GCST90018947'), proxies = 1, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
write.csv(exposure_dat, file="exposure_Tryptophan.csv",row.names = F)
write.csv(outcome_dat, file="outcome_Tryptophan_BMI.csv",row.names = F)
exposure_dat <- read.csv("exposure_Tryptophan.csv")
outcome_dat <- read.csv("outcome_Tryptophan_BMI.csv")
# Remove weak instrumental variables
source("calculate_R2_and_F.R",encoding = "utf-8")
result <- calculate_R2_and_F(exposure_dat$SNP,exposure_dat$eaf.exposure, exposure_dat$beta.exposure, exposure_dat$se.exposure, exposure_dat$samplesize.exposure)
result
write.csv(result, file="F_Tryptophan_BMI.csv",row.names = F)


# Merge Data
dat <- harmonise_data(exposure_dat, outcome_dat)
write.csv(dat, file="harmonise_Tryptophan_BMI.csv",row.names = F)

# Perform heterogeneity test

heterogeneity_results <- mr_heterogeneity(dat)


# Perform different analyses
mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))



# Run MR-PRESSO analysis, iteratively removing horizontal pleiotropy####
presso_results <- run_mr_presso(dat, NbDistribution = 1000)

# Extract p-values for outlier and global tests####
outlier_pvals <- presso_results[[1]]$`MR-PRESSO results`$`Outlier Test`


dat$NO<-as.numeric(rownames(dat))
outlier_pvals$NO<-as.numeric(rownames(outlier_pvals))
matched_indices <- merge(outlier_pvals, dat[, c("NO", "SNP")], by = "NO", all.x = TRUE)




snp_data <-matched_indices


snp_data <- snp_data[order(snp_data$Pvalue), ]
global_pval <- presso_results[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Initialize a list to store SNPs to be removed
removed_snps <- list()

# Initialize a list to store the recalculated results each time
all_results <- list()

# Remove SNPs one by one and re-run the global test
while (global_pval <= 0.05 && nrow(snp_data) > 0) {
  # Remove the SNP with the smallest p-value
  snp_to_remove <- snp_data$SNP[1]
  removed_snps <- c(removed_snps, snp_to_remove)

  # Remove the SNP from the data
  snp_data <- snp_data[-1, ]

  # Remove the corresponding SNP from 'dat'
  dat <- dat[!dat$SNP %in% snp_to_remove, ]

  # Recalculate 'MR-PRESSO global'
  
  # Recalculate 'presso_results'
  presso_results1 <- run_mr_presso(dat, NbDistribution = 1000)
  global_pval1 <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue
  # Save each recalculated result into the list
  all_results[[length(all_results) + 1]] <- list(
    snp_removed = snp_to_remove,
    global_pval = global_pval1,
    remaining_snps = dat$snp
  )
    # Update the current global p-value
  global_pval <- global_pval1
}

# Output the list of remaining SNPs
# remaining_snps <- dat$SNP
# remaining_snps

# Output the processed harmonized data####
write.csv(dat, file="harmonise_Tryptophan_BMI_detail.csv",row.names = F)
dat <- read.csv("harmonise_Tryptophan_BMI_detail.csv")

# Recalculate MR

mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))
FEOR <-generate_odds_ratios(mr_results)
# Extract IVW and MR-Egger results
ivw_result <- FEOR %>% filter(method == "Inverse variance weighted (fixed effects)")
ivw_result_random <- FEOR %>% filter(method == "Inverse variance weighted (multiplicative random effects)")
egger_result <- FEOR %>% filter(method == "MR Egger")
weighted_median_result <- FEOR %>% filter(method == "Weighted median")
Weighted_mode_result<- FEOR %>% filter(method == "Weighted mode")

# Re-run heterogeneity test ####
heterogeneity_results <- mr_heterogeneity(dat)
# Extract heterogeneity results
ivw_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted")
egger_heterogeneity <- heterogeneity_results %>% filter(method == "MR Egger")
weighted_median_heterogeneity <- heterogeneity_results %>% filter(method == "Weighted median")
Weighted_mode_heterogeneity<- heterogeneity_results %>% filter(method == "Weighted mode")
ivw_random_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted (multiplicative random effects)")

# Extract MR-PRESSO results
presso_global_pval <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue
# presso_global_pval <- presso_results[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Extract pleiotropy test results
egger_pleiotropy <-  mr_pleiotropy_test(dat)
#pleiotropy_results %>% filter(method == "MR Egger regression")

# Create a results table ####
results_table <- data.frame(
  Method = c("Tryptophan-BMI","IVW-fixed","IVW-random","Weighted-median","Weighted-mode","MR-Egger"),
  Beta = c(NA,ivw_result$b,ivw_result_random$b,weighted_median_result$b ,Weighted_mode_result$b ,egger_result$b),
  SE = c(NA,ivw_result$se, ivw_result_random$se,weighted_median_result$se,Weighted_mode_result$b , egger_result$se),
  P_value = c(NA,ivw_result$pval, ivw_result_random$pval,weighted_median_result$pval, Weighted_mode_result$pval,egger_result$pval),
  CI_lower = c(NA,ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
  CI_upper = c(NA,ivw_result$or_uci95,ivw_result_random$or_uci95,  weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95),
  P_value_CI = c(NA,sprintf("%.3f (%.3f, %.3f)",
                            c(ivw_result$or  , ivw_result_random$or  ,weighted_median_result$or  , Weighted_mode_result$or  ,egger_result$or  ), 
                            c(ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
                            c(ivw_result$or_uci95, ivw_result_random$or_uci95, weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95))))

results_table1 <- data.frame(
  N_of_SNP=c(nrow(dat),NA,NA,NA,NA,NA),
  Heterogeneity_test = c(round(egger_heterogeneity$Q_pval,3),NA,NA,NA,NA,NA),#Perform heterogeneity testMREgger
  Pleiotropy_test = c(round(egger_pleiotropy$pval,3),NA,NA,NA,NA,NA),#多效性
  PRESSO_Global_test = c(round(presso_global_pval,3),NA,NA,NA,NA,NA))#全局多效性

#Print results table
result_merge1<-cbind(results_table,results_table1)
print(result_merge1)
write.csv(result_merge1,"result_Tryptophan_BMI.csv",row.names = F)

# Lysine####
rm(list=ls())
setwd("E:\\work\\wrn\\")
library(TwoSampleMR)
library(ieugwasr)
library(MRPRESSO)
library(dplyr)

# Load data

exposure_dat <- extract_instruments(c('met-a-326'),p1=5e-06)
# exposure_dat <- extract_instruments(c('ebi-a-GCST90018947'))
# exposure_dat <- extract_instruments(c('met-d-Total_Lysine'),p1 = 5e-08)
exposure_dat$samplesize.exposure<-7812

outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ebi-a-GCST90018947'), proxies = 1, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
write.csv(exposure_dat, file="exposure_Lysine.csv",row.names = F)
write.csv(outcome_dat, file="outcome_Lysine_BMI.csv",row.names = F)
exposure_dat <- read.csv("exposure_Lysine.csv")
outcome_dat <- read.csv("outcome_Lysine_BMI.csv")
# Remove weak instrumental variables
source("calculate_R2_and_F.R",encoding = "utf-8")
result <- calculate_R2_and_F(exposure_dat$SNP,exposure_dat$eaf.exposure, exposure_dat$beta.exposure, exposure_dat$se.exposure, exposure_dat$samplesize.exposure)
result
write.csv(result, file="F_Lysine_BMI.csv",row.names = F)


# Merge Data
dat <- harmonise_data(exposure_dat, outcome_dat)
write.csv(dat, file="harmonise_Lysine_BMI.csv",row.names = F)

# Perform heterogeneity test

heterogeneity_results <- mr_heterogeneity(dat)


# Perform different analyses
mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))



# Run MR-PRESSO analysis, iteratively removing horizontal pleiotropy####
presso_results <- run_mr_presso(dat, NbDistribution = 1000)

# Extract p-values for outlier and global tests
outlier_pvals <- presso_results[[1]]$`MR-PRESSO results`$`Outlier Test`


dat$NO<-as.numeric(rownames(dat))
outlier_pvals$NO<-as.numeric(rownames(outlier_pvals))
matched_indices <- merge(outlier_pvals, dat[, c("NO", "SNP")], by = "NO", all.x = TRUE)




snp_data <-matched_indices


snp_data <- snp_data[order(snp_data$Pvalue), ]
global_pval <- presso_results[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Initialize a list to store SNPs to be removed
removed_snps <- list()

# Initialize a list to store the recalculated results each time
all_results <- list()

# Remove SNPs one by one and re-run the global test
while (global_pval <= 0.05 && nrow(snp_data) > 0) {
  # Remove the SNP with the smallest p-value
  snp_to_remove <- snp_data$SNP[1]
  removed_snps <- c(removed_snps, snp_to_remove)
  
  # Remove the SNP from the data
  snp_data <- snp_data[-1, ]
  
  # Remove the corresponding SNP from 'dat'
  dat <- dat[!dat$SNP %in% snp_to_remove, ]
  
  # Recalculate 'MR-PRESSO global'
  
  # Recalculate 'presso_results'
  presso_results1 <- run_mr_presso(dat, NbDistribution = 1000)
  global_pval1 <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue
  # Save each recalculated result into the list
  all_results[[length(all_results) + 1]] <- list(
    snp_removed = snp_to_remove,
    global_pval = global_pval1,
    remaining_snps = dat$snp
  )
  # Update the current global p-value
  global_pval <- global_pval1
}

# Output the list of remaining SNPs
# remaining_snps <- dat$SNP
# remaining_snps

# Output the processed harmonized data
write.csv(dat, file="harmonise_Lysine_BMI_detail.csv",row.names = F)
dat <- read.csv("harmonise_Lysine_BMI_detail.csv")

# Recalculate MR

mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))
FEOR <-generate_odds_ratios(mr_results)
# Extract IVW and MR-Egger results
ivw_result <- FEOR %>% filter(method == "Inverse variance weighted (fixed effects)")
ivw_result_random <- FEOR %>% filter(method == "Inverse variance weighted (multiplicative random effects)")
egger_result <- FEOR %>% filter(method == "MR Egger")
weighted_median_result <- FEOR %>% filter(method == "Weighted median")
Weighted_mode_result<- FEOR %>% filter(method == "Weighted mode")

# Re-run heterogeneity test ####
heterogeneity_results <- mr_heterogeneity(dat)
# Extract heterogeneity results
ivw_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted")
egger_heterogeneity <- heterogeneity_results %>% filter(method == "MR Egger")
weighted_median_heterogeneity <- heterogeneity_results %>% filter(method == "Weighted median")
Weighted_mode_heterogeneity<- heterogeneity_results %>% filter(method == "Weighted mode")
ivw_random_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted (multiplicative random effects)")

# Extract MR-PRESSO results
presso_global_pval <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Extract pleiotropy test results
egger_pleiotropy <-  mr_pleiotropy_test(dat)
#pleiotropy_results %>% filter(method == "MR Egger regression")

# Create a results table ####
results_table <- data.frame(
  Method = c("Lysine-BMI","IVW-fixed","IVW-random","Weighted-median","Weighted-mode","MR-Egger"),
  Beta = c(NA,ivw_result$b,ivw_result_random$b,weighted_median_result$b ,Weighted_mode_result$b ,egger_result$b),
  SE = c(NA,ivw_result$se, ivw_result_random$se,weighted_median_result$se,Weighted_mode_result$b , egger_result$se),
  P_value = c(NA,ivw_result$pval, ivw_result_random$pval,weighted_median_result$pval, Weighted_mode_result$pval,egger_result$pval),
  CI_lower = c(NA,ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
  CI_upper = c(NA,ivw_result$or_uci95,ivw_result_random$or_uci95,  weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95),
  P_value_CI = c(NA,sprintf("%.3f (%.3f, %.3f)",
                            c(ivw_result$or  , ivw_result_random$or  ,weighted_median_result$or  , Weighted_mode_result$or  ,egger_result$or  ), 
                            c(ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
                            c(ivw_result$or_uci95, ivw_result_random$or_uci95, weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95))))

results_table1 <- data.frame(
  N_of_SNP=c(nrow(dat),NA,NA,NA,NA,NA),
  Heterogeneity_test = c(round(egger_heterogeneity$Q_pval,3),NA,NA,NA,NA,NA),#Perform heterogeneity testMREgger
  Pleiotropy_test = c(round(egger_pleiotropy$pval,3),NA,NA,NA,NA,NA),#多效性
  PRESSO_Global_test = c(round(presso_global_pval,3),NA,NA,NA,NA,NA))#全局多效性

#Print results table
result_merge1<-cbind(results_table,results_table1)
print(result_merge1)
write.csv(result_merge1,"result_Lysine_BMI.csv",row.names = F)

# Methionine####
rm(list=ls())
setwd("E:\\work\\wrn\\")
library(TwoSampleMR)
library(ieugwasr)
library(MRPRESSO)
library(dplyr)

# Load data

exposure_dat <- extract_instruments(c('met-a-327'),p1=5e-06)
# exposure_dat <- extract_instruments(c('ebi-a-GCST90092891'))
# exposure_dat <- extract_instruments(c('met-d-Total_Methionine'),p1 = 5e-08)
exposure_dat$samplesize.exposure<-7795

outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ebi-a-GCST90018947'), proxies = 1, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
write.csv(exposure_dat, file="exposure_Methionine.csv",row.names = F)
write.csv(outcome_dat, file="outcome_Methionine_BMI.csv",row.names = F)
exposure_dat <- read.csv("exposure_Methionine.csv")
outcome_dat <- read.csv("outcome_Methionine_BMI.csv")
# Remove weak instrumental variables
source("calculate_R2_and_F.R",encoding = "utf-8")
result <- calculate_R2_and_F(exposure_dat$SNP,exposure_dat$eaf.exposure, exposure_dat$beta.exposure, exposure_dat$se.exposure, exposure_dat$samplesize.exposure)
result
write.csv(result, file="F_Methionine_BMI.csv",row.names = F)


# Merge Data
dat <- harmonise_data(exposure_dat, outcome_dat)
write.csv(dat, file="harmonise_Methionine_BMI.csv",row.names = F)

# Perform heterogeneity test

heterogeneity_results <- mr_heterogeneity(dat)


# Perform different analyses
mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))



# Run MR-PRESSO analysis, iteratively removing horizontal pleiotropy####
presso_results <- run_mr_presso(dat, NbDistribution = 1000)


# Extract p-values for outlier and global tests
outlier_pvals <- presso_results[[1]]$`MR-PRESSO results`$`Outlier Test`


dat$NO<-as.numeric(rownames(dat))
outlier_pvals$NO<-as.numeric(rownames(outlier_pvals))
matched_indices <- merge(outlier_pvals, dat[, c("NO", "SNP")], by = "NO", all.x = TRUE)




snp_data <-matched_indices


snp_data <- snp_data[order(snp_data$Pvalue), ]
global_pval <- presso_results[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Initialize a list to store SNPs to be removed
removed_snps <- list()

# Initialize a list to store the recalculated results each time
all_results <- list()

# Remove SNPs one by one and re-run the global test
while (global_pval <= 0.05 && nrow(snp_data) > 0) {
  # Remove the SNP with the smallest p-value
  snp_to_remove <- snp_data$SNP[1]
  removed_snps <- c(removed_snps, snp_to_remove)
  
  # Remove the SNP from the data
  snp_data <- snp_data[-1, ]
  
  # Remove the corresponding SNP from 'dat'
  dat <- dat[!dat$SNP %in% snp_to_remove, ]
  
  # Recalculate 'MR-PRESSO global'
  
  # Recalculate 'presso_results'
  presso_results1 <- run_mr_presso(dat, NbDistribution = 1000)
  global_pval1 <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue
  # Save each recalculated result into the list
  all_results[[length(all_results) + 1]] <- list(
    snp_removed = snp_to_remove,
    global_pval = global_pval1,
    remaining_snps = dat$snp
  )
  # Update the current global p-value
  global_pval <- global_pval1
}

# Output the list of remaining SNPs
# remaining_snps <- dat$SNP
# remaining_snps

# Output the processed harmonized data
write.csv(dat, file="harmonise_Methionine_BMI_detail.csv",row.names = F)
dat <- read.csv("harmonise_Methionine_BMI_detail.csv")

# Recalculate MR

mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))
FEOR <-generate_odds_ratios(mr_results)
# Extract IVW and MR-Egger results
ivw_result <- FEOR %>% filter(method == "Inverse variance weighted (fixed effects)")
ivw_result_random <- FEOR %>% filter(method == "Inverse variance weighted (multiplicative random effects)")
egger_result <- FEOR %>% filter(method == "MR Egger")
weighted_median_result <- FEOR %>% filter(method == "Weighted median")
Weighted_mode_result<- FEOR %>% filter(method == "Weighted mode")

# Re-run heterogeneity test ####
heterogeneity_results <- mr_heterogeneity(dat)
# Extract heterogeneity results
ivw_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted")
egger_heterogeneity <- heterogeneity_results %>% filter(method == "MR Egger")
weighted_median_heterogeneity <- heterogeneity_results %>% filter(method == "Weighted median")
Weighted_mode_heterogeneity<- heterogeneity_results %>% filter(method == "Weighted mode")
ivw_random_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted (multiplicative random effects)")

# Extract MR-PRESSO results 无异质性更改
presso_global_pval <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue
# presso_global_pval <- presso_results[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Extract pleiotropy test results
egger_pleiotropy <-  mr_pleiotropy_test(dat)
#pleiotropy_results %>% filter(method == "MR Egger regression")

# Create a results table ####
results_table <- data.frame(
  Method = c("Methionine-BMI","IVW-fixed","IVW-random","Weighted-median","Weighted-mode","MR-Egger"),
  Beta = c(NA,ivw_result$b,ivw_result_random$b,weighted_median_result$b ,Weighted_mode_result$b ,egger_result$b),
  SE = c(NA,ivw_result$se, ivw_result_random$se,weighted_median_result$se,Weighted_mode_result$b , egger_result$se),
  P_value = c(NA,ivw_result$pval, ivw_result_random$pval,weighted_median_result$pval, Weighted_mode_result$pval,egger_result$pval),
  CI_lower = c(NA,ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
  CI_upper = c(NA,ivw_result$or_uci95,ivw_result_random$or_uci95,  weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95),
  P_value_CI = c(NA,sprintf("%.3f (%.3f, %.3f)",
                            c(ivw_result$or  , ivw_result_random$or  ,weighted_median_result$or  , Weighted_mode_result$or  ,egger_result$or  ), 
                            c(ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
                            c(ivw_result$or_uci95, ivw_result_random$or_uci95, weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95))))

results_table1 <- data.frame(
  N_of_SNP=c(nrow(dat),NA,NA,NA,NA,NA),
  Heterogeneity_test = c(round(egger_heterogeneity$Q_pval,3),NA,NA,NA,NA,NA),#Perform heterogeneity testMREgger
  Pleiotropy_test = c(round(egger_pleiotropy$pval,3),NA,NA,NA,NA,NA),#多效性
  PRESSO_Global_test = c(round(presso_global_pval,3),NA,NA,NA,NA,NA))#全局多效性

#Print results table
result_merge1<-cbind(results_table,results_table1)
print(result_merge1)
write.csv(result_merge1,"result_Methionine_BMI.csv",row.names = F)

# Phenylalanine####
rm(list=ls())
setwd("E:\\work\\wrn\\")
library(TwoSampleMR)
library(ieugwasr)
library(MRPRESSO)
library(dplyr)

# Load data

exposure_dat <- extract_instruments(c('met-d-Phe'))
# exposure_dat <- extract_instruments(c('ebi-a-GCST90092891'))
# exposure_dat <- extract_instruments(c('met-d-Total_Phenylalanine'),p1 = 5e-08)
exposure_dat$samplesize.exposure<-115025

outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ebi-a-GCST90018947'), proxies = 1, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
write.csv(exposure_dat, file="exposure_Phenylalanine.csv",row.names = F)
write.csv(outcome_dat, file="outcome_Phenylalanine_BMI.csv",row.names = F)
exposure_dat <- read.csv("exposure_Phenylalanine.csv")
outcome_dat <- read.csv("outcome_Phenylalanine_BMI.csv")
# Remove weak instrumental variables
source("calculate_R2_and_F.R",encoding = "utf-8")
result <- calculate_R2_and_F(exposure_dat$SNP,exposure_dat$eaf.exposure, exposure_dat$beta.exposure, exposure_dat$se.exposure, exposure_dat$samplesize.exposure)
result
write.csv(result, file="F_Phenylalanine_BMI.csv",row.names = F)


# Merge Data
dat <- harmonise_data(exposure_dat, outcome_dat)
write.csv(dat, file="harmonise_Phenylalanine_BMI.csv",row.names = F)

# Perform heterogeneity test

heterogeneity_results <- mr_heterogeneity(dat)


# Perform different analyses
mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))



# Run MR-PRESSO analysis, iteratively removing horizontal pleiotropy####
presso_results <- run_mr_presso(dat, NbDistribution = 1000)

# Extract p-values for outlier and global tests
outlier_pvals <- presso_results[[1]]$`MR-PRESSO results`$`Outlier Test`


dat$NO<-as.numeric(rownames(dat))
outlier_pvals$NO<-as.numeric(rownames(outlier_pvals))
matched_indices <- merge(outlier_pvals, dat[, c("NO", "SNP")], by = "NO", all.x = TRUE)




snp_data <-matched_indices


snp_data <- snp_data[order(snp_data$Pvalue), ]
global_pval <- presso_results[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Initialize a list to store SNPs to be removed
removed_snps <- list()

# Initialize a list to store the recalculated results each time
all_results <- list()

# Remove SNPs one by one and re-run the global test
while (global_pval <= 0.05 && nrow(snp_data) > 0) {
  # Remove the SNP with the smallest p-value
  snp_to_remove <- snp_data$SNP[1]
  removed_snps <- c(removed_snps, snp_to_remove)
  
  # Remove the SNP from the data
  snp_data <- snp_data[-1, ]
  
  # Remove the corresponding SNP from 'dat'
  dat <- dat[!dat$SNP %in% snp_to_remove, ]
  
  # Recalculate 'MR-PRESSO global'
  
  # Recalculate 'presso_results'
  presso_results1 <- run_mr_presso(dat, NbDistribution = 1000)
  global_pval1 <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue
  # Save each recalculated result into the list
  all_results[[length(all_results) + 1]] <- list(
    snp_removed = snp_to_remove,
    global_pval = global_pval1,
    remaining_snps = dat$snp
  )
  # Update the current global p-value
  global_pval <- global_pval1
}

# Output the list of remaining SNPs
# remaining_snps <- dat$SNP
# remaining_snps

# Output the processed harmonized data
write.csv(dat, file="harmonise_Phenylalanine_BMI_detail.csv",row.names = F)
dat <- read.csv("harmonise_Phenylalanine_BMI_detail.csv")

# Recalculate MR

mr_results <- mr(dat, method_list = c("mr_ivw_mre","mr_ivw_fe", "mr_weighted_median","mr_egger_regression","mr_weighted_mode","mr_simple_mode"))
FEOR <-generate_odds_ratios(mr_results)
# Extract IVW and MR-Egger results
ivw_result <- FEOR %>% filter(method == "Inverse variance weighted (fixed effects)")
ivw_result_random <- FEOR %>% filter(method == "Inverse variance weighted (multiplicative random effects)")
egger_result <- FEOR %>% filter(method == "MR Egger")
weighted_median_result <- FEOR %>% filter(method == "Weighted median")
Weighted_mode_result<- FEOR %>% filter(method == "Weighted mode")

# Re-run heterogeneity test ####
heterogeneity_results <- mr_heterogeneity(dat)
# Extract heterogeneity results
ivw_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted")
egger_heterogeneity <- heterogeneity_results %>% filter(method == "MR Egger")
weighted_median_heterogeneity <- heterogeneity_results %>% filter(method == "Weighted median")
Weighted_mode_heterogeneity<- heterogeneity_results %>% filter(method == "Weighted mode")
ivw_random_heterogeneity <- heterogeneity_results %>% filter(method == "Inverse variance weighted (multiplicative random effects)")

# Extract MR-PRESSO results
presso_global_pval <- presso_results1[[1]]$`MR-PRESSO results`$`Global Test`$Pvalue

# Extract pleiotropy test results
egger_pleiotropy <-  mr_pleiotropy_test(dat)
#pleiotropy_results %>% filter(method == "MR Egger regression")

# Create a results table ####

results_table <- data.frame(
  Method = c("Phenylalanine-BMI","IVW-fixed","IVW-random","Weighted-median","Weighted-mode","MR-Egger"),
  Beta = c(NA,ivw_result$b,ivw_result_random$b,weighted_median_result$b ,Weighted_mode_result$b ,egger_result$b),
  SE = c(NA,ivw_result$se, ivw_result_random$se,weighted_median_result$se,Weighted_mode_result$b , egger_result$se),
  P_value = c(NA,ivw_result$pval, ivw_result_random$pval,weighted_median_result$pval, Weighted_mode_result$pval,egger_result$pval),
  CI_lower = c(NA,ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
  CI_upper = c(NA,ivw_result$or_uci95,ivw_result_random$or_uci95,  weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95),
  P_value_CI = c(NA,sprintf("%.3f (%.3f, %.3f)",
                            c(ivw_result$or  , ivw_result_random$or  ,weighted_median_result$or  , Weighted_mode_result$or  ,egger_result$or  ), 
                            c(ivw_result$or_lci95, ivw_result_random$or_lci95,weighted_median_result$or_lci95,Weighted_mode_result$or_lci95, egger_result$or_lci95),
                            c(ivw_result$or_uci95, ivw_result_random$or_uci95, weighted_median_result$or_uci95,Weighted_mode_result$or_uci95, egger_result$or_uci95))))

results_table1 <- data.frame(
  N_of_SNP=c(nrow(dat),NA,NA,NA,NA,NA),
  Heterogeneity_test = c(round(egger_heterogeneity$Q_pval,3),NA,NA,NA,NA,NA),#Perform heterogeneity testMREgger
  Pleiotropy_test = c(round(egger_pleiotropy$pval,3),NA,NA,NA,NA,NA),#多效性
  PRESSO_Global_test = c(round(presso_global_pval,3),NA,NA,NA,NA,NA))#全局多效性

#Print results table
result_merge1<-cbind(results_table,results_table1)
print(result_merge1)
write.csv(result_merge1,"result_Phenylalanine_BMI.csv",row.names = F)

