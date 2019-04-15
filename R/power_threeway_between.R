power_threeway_between <- function(design_result, alpha_level=0.05){

  mu_array <- array(design_result$mu, dim = c(length(design_result$labelnames[[1]]),
                                              length(design_result$labelnames[[2]]),
                                              length(design_result$labelnames[[3]])))
  #Gender
  mu_A <- apply(mu_array,c(3),mean)
  mu_A
  #Country
  mu_B <- apply(mu_array,c(2),mean)
  mu_B
  #Position
  mu_C <- apply(mu_array,c(1),mean)
  mu_C
  
  #GENDER*NATIONALITY
  mu_AB <- apply(mu_array,c(2,3),mean)
  mu_AB
  mu_AB <- mu_AB - (mean(design_result$mu) + sweep(mu_AB, 1, rowMeans(mu_AB)) + sweep(mu_AB, 2, colMeans(mu_AB)))
  mu_AB
  
  #SIZE*LOAD
  mu_AC <- apply(mu_array,c(1,3),mean)
  mu_AC
  mu_AC <- mu_AC - (mean(design_result$mu) + sweep(mu_AC, 2, colMeans(mu_AC)) + sweep(mu_AC, 1, rowMeans(mu_AC)))
  mu_AC
  
  #COLOR*LOAD
  mu_BC <- apply(mu_array,c(1,2),mean)
  mu_BC
  mu_BC <- mu_BC - (mean(design_result$mu) + sweep(mu_BC,1, rowMeans(mu_BC)) + sweep(mu_BC,2, colMeans(mu_BC)))
  mu_BC
  
  # Calculate degrees of freedom
  
  df_A <- (length(design_result$labelnames[[1]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
  df_B <- (length(design_result$labelnames[[2]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
  df_C <- (length(design_result$labelnames[[3]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
  
  df_AB <- (length(design_result$labelnames[[1]]) - 1) * (length(design_result$labelnames[[2]]) - 1)
  df_AC <- (length(design_result$labelnames[[1]]) - 1) * (length(design_result$labelnames[[3]]) - 1)
  df_BC <- (length(design_result$labelnames[[2]]) - 1) * (length(design_result$labelnames[[3]]) - 1)
  
  df_ABC <- (length(design_result$labelnames[[1]]) - 1) * (length(design_result$labelnames[[2]]) - 1) * (length(design_result$labelnames[[3]]) - 1)
  
  df_error <- (design_result$n * length(design_result$mu)) - (length(design_result$labelnames[[1]])) * (length(design_result$labelnames[[2]])) * (length(design_result$labelnames[[3]]))
  df_total <- df_error + df_A + df_B + df_C + df_AB + df_AC + df_BC + df_ABC
  
  # Calculate sum of squares
  
  MS_A <- design_result$n * length(design_result$labelnames[[2]]) * length(design_result$labelnames[[3]]) * (sum((mu_A - mean(mu_A))^2)/(length(design_result$labelnames[[2]])-1))
  SS_A <- design_result$n * length(design_result$labelnames[[2]]) * length(design_result$labelnames[[3]]) * sum((mu_A - mean(mu_A))^2)
  
  MS_B <- design_result$n * length(design_result$labelnames[[1]]) * length(design_result$labelnames[[3]]) * (sum((mu_B - mean(mu_B))^2)/(length(design_result$labelnames[[2]])-1))
  SS_B <- design_result$n * length(design_result$labelnames[[1]]) * length(design_result$labelnames[[3]]) * sum((mu_B - mean(mu_B))^2)
  
  MS_C <- design_result$n * length(design_result$labelnames[[1]]) * length(design_result$labelnames[[2]]) * (sum((mu_C - mean(mu_C))^2)/(length(design_result$labelnames[[2]])-1))
  SS_C <- design_result$n * length(design_result$labelnames[[1]]) * length(design_result$labelnames[[2]]) * sum((mu_C - mean(mu_C))^2)
  
  MS_AB <- design_result$n * length(design_result$labelnames[[3]]) * sum(mu_AB^2)/((length(design_result$labelnames[[1]])-1) * (length(design_result$labelnames[[2]])-1))
  SS_AB <- design_result$n * length(design_result$labelnames[[3]]) * sum(mu_AB^2)
  
  SS_AB_between <- design_result$n * length(design_result$labelnames[[3]]) * sum((apply(mu_array,c(2,3),mean) - mean(apply(mu_array,c(2,3),mean)))^2)
  SS_AB_2 <- SS_AB_between - SS_A - SS_B 
  
  MS_AC <- design_result$n * length(design_result$labelnames[[2]]) * sum(mu_AC^2)/((length(design_result$labelnames[[1]])-1) * (length(design_result$labelnames[[3]])-1))
  SS_AC <- design_result$n * length(design_result$labelnames[[2]]) * sum(mu_AC^2)
  
  SS_AC_between <- design_result$n * length(design_result$labelnames[[2]]) * sum((apply(mu_array,c(1,3),mean) - mean(apply(mu_array,c(1,3),mean)))^2)
  SS_AC_2 <- SS_AC_between - SS_A - SS_C 
  
  MS_BC <- design_result$n * length(design_result$labelnames[[1]]) * sum(mu_BC^2)/((length(design_result$labelnames[[2]])-1) * (length(design_result$labelnames[[3]])-1))
  SS_BC <- design_result$n * length(design_result$labelnames[[1]]) * sum(mu_BC^2)
  
  SS_BC_between <- design_result$n * length(design_result$labelnames[[1]]) * sum((apply(mu_array,c(1,2),mean) - mean(apply(mu_array,c(1,2),mean)))^2)
  SS_BC_2 <- SS_BC_between - SS_B - SS_C 
  
  MS_total <- design_result$sd^2
  SS_total <- MS_total * df_total
  
  SS_ABC_between <- design_result$n * sum((design_result$mu - mean(design_result$mu))^2)
  SS_ABC <- SS_ABC_between - SS_A - SS_B - SS_C - SS_AB - SS_AC - SS_BC

  SS_error <- SS_total - SS_A - SS_B - SS_C - SS_AB - SS_AC - SS_BC - SS_ABC
  MS_error <- SS_error/df_error
  
  # Calculate eta-squared
  # Note we are using df_total calculating SS_total. eta_p_2 is  SS_A/(SS_A + SS_total)
  # But ss_total is based on df_total, but we need the total sample size instead. 
  eta_p_2_A <- SS_A/(SS_A + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_A
  eta_p_2_B <- SS_B/(SS_B + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_B
  eta_p_2_C <- SS_C/(SS_C + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_C
  eta_p_2_AB <- SS_AB/(SS_AB + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_AB
  eta_p_2_AC <- SS_AC/(SS_AC + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_AC
  eta_p_2_BC <- SS_BC/(SS_BC + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_BC
  eta_p_2_ABC <- SS_ABC/(SS_ABC + (MS_total * design_result$n * length(design_result$mu)))
  eta_p_2_ABC
  
  # Cohen f and squared
   
  f_2_A <- eta_p_2_A/(1-eta_p_2_A)
  f_2_B <- eta_p_2_B/(1-eta_p_2_B)
  f_2_C <- eta_p_2_C/(1-eta_p_2_C)
  f_2_AB <- eta_p_2_AB/(1-eta_p_2_AB)
  f_2_AC <- eta_p_2_AC/(1-eta_p_2_AC)
  f_2_BC <- eta_p_2_BC/(1-eta_p_2_BC)
  f_2_ABC <- eta_p_2_ABC/(1-eta_p_2_ABC)
  
  Cohen_f_A <- sqrt(f_2_A)
  Cohen_f_B <- sqrt(f_2_B)
  Cohen_f_C <- sqrt(f_2_C)
  Cohen_f_AB <- sqrt(f_2_AB)
  Cohen_f_AC <- sqrt(f_2_AC)
  Cohen_f_BC <- sqrt(f_2_BC)
  Cohen_f_ABC <- sqrt(f_2_ABC)
  
  # Calculate Lambda
  lambda_A <- design_result$n * length(design_result$mu) * f_2_A
  lambda_B <- design_result$n * length(design_result$mu) * f_2_B
  lambda_C <- design_result$n * length(design_result$mu) * f_2_C
  
  lambda_AB <- design_result$n * length(design_result$mu) * f_2_AB
  lambda_AC <- design_result$n * length(design_result$mu) * f_2_AC
  lambda_BC <- design_result$n * length(design_result$mu) * f_2_BC
  
  lambda_ABC <- design_result$n * length(design_result$mu) * f_2_ABC
  
  # Calculate Critical F
  
  F_critical_A <- qf(alpha_level, df_A, df_error, lower.tail=FALSE) 
  F_critical_B <- qf(alpha_level, df_B, df_error, lower.tail=FALSE) 
  F_critical_C <- qf(alpha_level, df_C, df_error, lower.tail=FALSE) 
  F_critical_AB <- qf(alpha_level, df_AB, df_error, lower.tail=FALSE) 
  F_critical_AC <- qf(alpha_level, df_AC, df_error, lower.tail=FALSE) 
  F_critical_BC <- qf(alpha_level, df_BC, df_error, lower.tail=FALSE) 
  F_critical_ABC <- qf(alpha_level, df_ABC, df_error, lower.tail=FALSE) 
  
  #Calculate Power
  
  power_A <- pf(F_critical_A, df_A, df_error, lambda_A, lower.tail = FALSE)
  power_B <- pf(F_critical_B, df_B, df_error, lambda_B, lower.tail = FALSE)
  power_C <- pf(F_critical_C, df_C, df_error, lambda_C, lower.tail = FALSE)
  power_AB <- pf(F_critical_AB, df_AB, df_error, lambda_AB, lower.tail = FALSE)
  power_AC <- pf(F_critical_AC, df_AC, df_error, lambda_AC, lower.tail = FALSE)
  power_BC <- pf(F_critical_BC, df_BC, df_error, lambda_BC, lower.tail = FALSE)
  power_ABC <- pf(F_critical_ABC, df_ABC, df_error, lambda_ABC, lower.tail = FALSE)
  
  # F-Value
  # F_A <- MS_A/MS_error
  # F_B <- MS_B/MS_error
  # F_C <- MS_C/MS_error
  # F_AB <- MS_AB/MS_error
  # F_AC <- MS_AC/MS_error
  # F_BC <- MS_BC/MS_error
  # F_ABC <- MS_ABC/MS_error
  # 
  
  invisible(list(mu = design_result$mu,
                 sigma = design_result$sd,
                 n = design_result$n, 
                 alpha_level = alpha_level,
                 Cohen_f_A = Cohen_f_A,
                 Cohen_f_B = Cohen_f_B,
                 Cohen_f_C = Cohen_f_C,
                 Cohen_f_AB = Cohen_f_AB,
                 Cohen_f_AC = Cohen_f_AC,
                 Cohen_f_BC = Cohen_f_BC,
                 Cohen_f_ABC = Cohen_f_ABC,
                 f_2_A = f_2_A,
                 f_2_B = f_2_B,
                 f_2_C = f_2_C,
                 f_2_AB = f_2_AB,
                 f_2_AC = f_2_AC,
                 f_2_BC = f_2_BC,
                 f_2_ABC = f_2_ABC,
                 lambda_A = lambda_A,
                 lambda_B = lambda_B,
                 lambda_C = lambda_C,
                 lambda_AB = lambda_AB,
                 lambda_AC = lambda_AC,
                 lambda_BC = lambda_BC,
                 lambda_ABC = lambda_ABC,
                 F_critical_A = F_critical_A,
                 F_critical_B = F_critical_B,
                 F_critical_C = F_critical_C,
                 F_critical_AB = F_critical_AB,
                 F_critical_AC = F_critical_AC,
                 F_critical_BC = F_critical_BC,
                 F_critical_ABC = F_critical_ABC,
                 power_A = power_A,
                 power_B = power_B,
                 power_C = power_C,
                 power_AB = power_AB,
                 power_AC = power_AC,
                 power_BC = power_BC,
                 power_ABC = power_ABC,
                 df_A = df_A,
                 df_B = df_B,
                 df_C = df_C,
                 df_AB = df_AB,
                 df_AC = df_AC,
                 df_BC = df_BC,
                 df_error = df_error,
                 eta_p_2_A = eta_p_2_A,
                 eta_p_2_B = eta_p_2_B,
                 eta_p_2_C = eta_p_2_C,
                 eta_p_2_AB = eta_p_2_AB,
                 eta_p_2_AC = eta_p_2_AC,
                 eta_p_2_BC = eta_p_2_BC,
                 eta_p_2_ABC = eta_p_2_ABC,
                 mean_mat = mu_array))
}

