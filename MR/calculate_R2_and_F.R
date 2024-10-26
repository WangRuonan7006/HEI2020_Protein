calculate_R2_and_F <- function(SNP,EAF, beta, SE, N) {
  # calculate R2
  R2 <- (2 * EAF * (1 - EAF) * beta^2) / ((2 * EAF * (1 - EAF) * beta^2) + (2 * EAF * (1 - EAF) * N * SE^2))
  
  # calculate value of F
  F <- (R2 * (N - 2)) / (1 - R2)
  
  # create a dateframe including R2 and P value of F
  data.frame(SNP=SNP,R2 = R2, `P value of F` = F)
  
}

