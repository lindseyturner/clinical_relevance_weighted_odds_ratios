# simulation setup 
# data generation scenarios 
# two different data generations each with 5 categories
# the first scenario will mimic the covid out data
# 70%, 18%, 9%, 2%, 1%
# second scenario will have equal distribution to outcomes 
# 20%, 20%, 20%, 20%, 20%

# different treatment effects 
# null scenario - treatment and control are the same 
# proportional odds scenario - 2 different settings - one small effect and one large effect
# non proportional odds scenario - relatively constant risk difference

# what to output: 
# point estimates and credible intervals for each model (PPO, PO, polr, mlr) and 
# summary measure (RD, weighted RD, OR, weighted OR, WR)

# what to measure: 
# power, average z score (standardized effect size), bias, rMSE, coverage rates

# to do: 
# calculate the small and large effects for the proportional odds scenario 
# determine the non proportional odds scenario
# generate data 
# calculate point estimates and credible intervals 
# calculate measures



# generate data 
# function to generate data 
# start with 50% to treatment and 50% to control 
#

determine_start_i2 <- function(val) {
  if (val%%4 == 1){
    start <- 1
  } else if (val%%4 == 2) {
    start <- 501
  } else if (val%%4 == 3) {
    start <- 1001
  } else if (val%%4 == 0) {
    start <- 1501
  }
  
  return(start)
}


determine_start_i40 <- function(val) {
  start_seq <- seq(1, 2000, 50)
  start <- start_seq[val%%40 + 1]
  return(start)
}


# data generation by array number 
DataGen_array <- function(n, array) {
  pc1 <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  pc2 <- c(0.70, 0.18, 0.09, 0.02, 0.01)
  
  if (array %in% 1:4) {
    pc <- pc1
    pt <- pc1
  } else if (array %in% 5:8) {
    # RD low  is 0.06
    pc <- pc1
    pt <- c(0.26, 0.2, 0.2, 0.2, 0.14)
  } else if (array %in% 9:12) {
    # RD high  is 0.08
    pc <- pc1
    # was 0.28 and 0.12
    pt <- c(0.27, 0.2, 0.2, 0.2, 0.13)
  } else if (array %in% 13:16) {
    #RR low 
    pc <- pc1
    pt <- c(0.225, 0.225, 0.225, 0.225, 0.10)
  } else if (array %in% 17:20) {
    #RR high is 1.2
    pc <- pc1
    # old: c(0.34, 0.165, 0.165, 0.165, 0.165)
    pt <- c(0.23, 0.23, 0.23, 0.23, 0.08)
  } else if (array %in% 21:24) {
    #PO low is OR = 1.37
    pc <- pc1
    beta1 <- log(1.37)
    pt <- c(invlogit(logit(pc1[1]) + beta1),
            invlogit(logit(pc1[1] + pc1[2]) + beta1) - invlogit(logit(pc1[1]) + beta1),
            invlogit(logit(pc1[1] + pc1[2] + pc1[3]) + beta1) - invlogit(logit(pc1[1] + pc1[2]) + beta1),
            invlogit(logit(pc1[1] + pc1[2] + pc1[3] + pc1[4]) + beta1) - invlogit(logit(pc1[1] + pc1[2] + pc1[3]) + beta1),
            1 - invlogit(logit(pc1[1] + pc1[2] + pc1[3] + pc1[4]) + beta1))
  } else if (array %in% 25:28) {
    #PO high is OR = 1.47
    pc <- pc1
    beta2 <- log(1.47)
    pt <- c(invlogit(logit(pc1[1]) + beta2),
            invlogit(logit(pc1[1] + pc1[2]) + beta2) - invlogit(logit(pc1[1]) + beta2),
            invlogit(logit(pc1[1] + pc1[2] + pc1[3]) + beta2) - invlogit(logit(pc1[1] + pc1[2]) + beta2),
            invlogit(logit(pc1[1] + pc1[2] + pc1[3] + pc1[4]) + beta2) - invlogit(logit(pc1[1] + pc1[2] + pc1[3]) + beta2),
            1 - invlogit(logit(pc1[1] + pc1[2] + pc1[3] + pc1[4]) + beta2))
  } else if (array %in% 29:32) {
    # 20% reduction in the last 3 levels and then rest is put in level 1
    pc <- pc1
    # was c(0.32, 0.2, 0.16, 0.16, 0.16)
    pt <- c(0.29, 0.2, 0.17, 0.17, 0.17)
  } else if (array %in% 33:36) {
    # 25% reduction in the last 3 levels and then rest is put in level 2
    pc <- pc1
    pt <- c(0.2, 0.35, 0.15, 0.15, 0.15)
  } else if (array %in% 37:40) {
    pc <- pc2
    pt <- pc2
  } else if (array %in% 41:44) {
    #RD low is 0.03ish
    pc <- pc2
    pt <- c(0.76, 0.18, 0.05, 0.005, 0.005)
  } else if (array %in% 45:48) {
    #RD high is 0.05ish
    pc <- pc2
    pt <- c(0.78, 0.17, 0.04, 0.005, 0.005)
  } else if (array %in% 49:52) {
    #RR low is 0.9
    pc <- pc2
    #pt <- c(0.73, 0.162, 0.081, 0.018, 0.009)
    # 0.75
    pt <- c(0.78, 0.185, 0.025, 0.0075, 0.0025)
  } else if (array %in% 53:56) {
    #RR high is 0.8
    pc <- pc2
    #pt <- c(0.76, 0.144, 0.072, 0.016, 0.008)
    #try 0.6
    pt <- c(0.77, 0.18, 0.025, 0.0175, 0.0075)
  } else if (array %in% 57:60) {
    #OR low is 1.47
    pc <- pc2
    beta1_2 <- log(1.47)
    
    pt <- c(invlogit(logit(pc2[1]) + beta1_2),
            invlogit(logit(pc2[1] + pc2[2]) + beta1_2) - invlogit(logit(pc2[1]) + beta1_2),
            invlogit(logit(pc2[1] + pc2[2] + pc2[3]) + beta1_2) - invlogit(logit(pc2[1] + pc2[2]) + beta1_2),
            invlogit(logit(pc2[1] + pc2[2] + pc2[3] + pc2[4]) + beta1_2) - invlogit(logit(pc2[1] + pc2[2] + pc2[3]) + beta1_2),
            1 - invlogit(logit(pc2[1] + pc2[2] + pc2[3] + pc2[4]) + beta1_2))
    
  } else if (array %in% 61:64) {
    #OR high is 1.60
    pc <- pc2
    beta2_2 <- log(1.60)
    pt <- c(invlogit(logit(pc2[1]) + beta2_2),
            invlogit(logit(pc2[1] + pc2[2]) + beta2_2) - invlogit(logit(pc2[1]) + beta2_2),
            invlogit(logit(pc2[1] + pc2[2] + pc2[3]) + beta2_2) - invlogit(logit(pc2[1] + pc2[2]) + beta2_2),
            invlogit(logit(pc2[1] + pc2[2] + pc2[3] + pc2[4]) + beta2_2) - invlogit(logit(pc2[1] + pc2[2] + pc2[3]) + beta2_2),
            1 - invlogit(logit(pc2[1] + pc2[2] + pc2[3] + pc2[4]) + beta2_2))
  } else if (array %in% 65:68) {
    # 50% reduction in the last 3 levels and then rest is put in level 1
    pc <- pc2
    pt <- c(0.772, 0.18, 0.036, 0.008, 0.004)
  } else if (array %in% 69:72) {
    # 50% reduction in the last 3 levels and then rest is put in level 2
    pc <- pc2
    #pt <- c(0.70, 0.252, 0.036, 0.008, 0.004)
    #try 75% reduction 
    pt <- c(0.70, 0.27, 0.0225, 0.0050, 0.0025)
    
  }
  
  df <- data.frame(matrix(NA, nrow = n, ncol = 3))
  colnames(df) <- c("y", "trt", "trt_05")
  df$trt <- rbinom(n, 1, 0.5)
  df$trt_05 <- case_when(df$trt == 0 ~ -0.5, 
                         df$trt == 1 ~ 0.5)
  df$y <- case_when(df$trt == 0 ~ sample(1:5,n,replace=TRUE,prob=pc), 
                    df$trt == 1 ~ sample(1:5,n,replace=TRUE,prob=pt))
  
  return(df)
  
}

# data_setup <- function(formula, ppo, data, stratum) {
#   # data <- rms::modelData(data, formula, ppo, 
#   #                        na.action = na.action)
#   # controls are strata 1, treatment is strata 2
#   stratas <- case_when(data$trt == -0.5 ~ 1, data$trt == 0.5 ~ 2)
#   X <- data$trt
#   Y <- data$y
#   n <- nrow(data)
#   p <- ncol(X)
#   q <- ncol(X)
#   k <- length(unique(Y))
#   conc = 1/(0.8 + 0.35 * max(k, 3))
#   priorsdppo <- rep(100, length = 1)
#   sdsppo <- as.array(priorsdppo)
#   Nc <- 0
#   psigma <- 1
#   rsdmean <- 0
#   rsdsd <- 1
#   cluster <- 0
#   
#   
#   d <- list(X = X, y = Y, N = n, p = p, q = q, k = k, conc = conc, sdsppo = sdsppo,
#             Nc = Nc, psigma = psigma, rsdmean = rsdmean, rsdsd = rsdsd, cluster = cluster)
#   d$S <- length(unique(stratas))
#   d$stratum <- stratas
#   
#   return(d)
# }

# data generation array with covariate adjustment 
DataGen_array_adjusted <- function(n, array) {
  pc2 <- c(0.70, 0.18, 0.09, 0.02, 0.01)
  df <- data.frame(matrix(NA, nrow = n, ncol = 5))
  colnames(df) <- c("y", "trt", "trt_05", "x2", "x3")
  df$trt <- rbinom(n, 1, 0.5)
  df$trt_05 <- case_when(df$trt == 0 ~ -0.5, 
                         df$trt == 1 ~ 0.5)
  df$x2 <- rbinom(n, 1, 0.5)
  df$x2 <- case_when(df$x2 == 0 ~ -0.5, 
                     df$x2 == 1 ~ 0.5)
  beta2 <- 0.95 #2.0
  df$x3 <- rnorm(n, 0, 1)
  beta3 <- -1.0
  alphas <- logit(cumsum(pc2)) - beta2 * 0 - beta3 * 0
  if (array %in% 1:40) {
    # Null scenario
    prob1 <- expit(alphas[1] + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- 1 - expit(alphas[4] + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5)
  } else if (array %in% 41:80) {
    # PO with power about 80%
    beta1 <- log(1.47)
    prob1 <- expit(alphas[1] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- 1 - expit(alphas[4] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5)
  } else if (array %in% 81:120) {
    # PO with power about 80%
    beta1 <- log(1.60)
    prob1 <- expit(alphas[1] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- 1 - expit(alphas[4] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5)
  } else if (array %in% 121:160) {
    # NP 1
    beta11 <- log(1.36)
    beta12 <- log(2.14)
    beta13 <- log(3.06)
    beta14 <- log(2.01)
    prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- 1 - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5)  
  } else if (array %in% 161:200) {
    # NP 2
    beta11 <- log(1.52)
    beta12 <- log(2.59)
    beta13 <- log(3.06)
    beta14 <- log(2.01)
    prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- 1 - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5)  
  } else if (array %in% 201:240) {
    # NP 3
    beta11 <- log(1.43)
    beta12 <- log(2.59)
    beta13 <- log(1.21)
    beta14 <- log(1.34)
    prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- 1 - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5)  
  } else if (array %in% 241:280) {
    # NP 4
    beta11 <- log(1.52)
    beta12 <- log(3)
    beta13 <- log(3.25)
    beta14 <- log(4.03)
    prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- 1 - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5)  
  } else if (array %in% 281:320) {
    # NP 5
    beta11 <- log(1.45)
    beta12 <- log(2.70)
    beta13 <- log(2.55)
    beta14 <- log(2.52)
    prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- 1 - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5)  
  } else if (array %in% 321:360) {
    # NP 6
    beta11 <- log(1.00)
    beta12 <- log(4.41)
    beta13 <- log(4.09)
    beta14 <- log(4.03)
    prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- 1 - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5)  
  }
  
  
  df$y <- apply(probs, 1, sample, x = 1:5, size = 1, replace = TRUE)
  
  
  return(df)
  
}

DataGen_array_adjusted7 <- function(n, array) {
  #pc2 <- c(0.68, 0.18, 0.09, 0.02, 0.015, 0.01, 0.005)
  pc2 <- c(0.68, 0.18, 0.005, 0.09, 0.03, 0.01, 0.005)
  df <- data.frame(matrix(NA, nrow = n, ncol = 7))
  colnames(df) <- c("y", "trt", "trt_05", "x2", "x3")
  df$trt <- rbinom(n, 1, 0.5)
  df$trt_05 <- case_when(df$trt == 0 ~ -0.5, 
                         df$trt == 1 ~ 0.5)
  df$x2 <- rbinom(n, 1, 0.5)
  df$x2 <- case_when(df$x2 == 0 ~ -0.5, 
                     df$x2 == 1 ~ 0.5)
  beta2 <- 0.95 #2.0
  df$x3 <- rnorm(n, 0, 1)
  beta3 <- -1.0
  alphas <- logit(cumsum(pc2)) - beta2 * 0 - beta3 * 0
  if (array %in% 1:40) {
    # Null scenario
    prob1 <- expit(alphas[1] + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- expit(alphas[5] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[4] + beta2 * df$x2 + beta3 * df$x3)
    prob6 <- expit(alphas[6] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[5] + beta2 * df$x2 + beta3 * df$x3)
    prob7 <- 1 - expit(alphas[6] + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5, prob6, prob7)
  } else if (array %in% 41:80) {
    # PO with power about 80%
    beta1 <- log(1.47)
    prob1 <- expit(alphas[1] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- expit(alphas[5] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[4] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob6 <- expit(alphas[6] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[5] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob7 <- 1 - expit(alphas[6] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5, prob6, prob7)
  # } else if (array %in% 81:120) {
  #   # PO with power about 80%
  #   beta1 <- log(1.60)
  #   prob1 <- expit(alphas[1] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob2 <- expit(alphas[2] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob3 <- expit(alphas[3] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob4 <- expit(alphas[4] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob5 <- 1 - expit(alphas[4] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   probs <- cbind(prob1, prob2, prob3, prob4, prob5)
  } else if (array %in% 81:120) {
    # NP 1
    beta11 <- log(1.36)
    # beta12 <- log(2.14)
    beta12 <- log(2.14)
    beta13 <- log(3.06) #3.06
    beta14 <- log(2.01)
    beta15 <- log(1.84)
    beta16 <- log(2.65)
    betas <- c(beta11, beta12, beta13, beta14, beta15, beta16, Inf)
   # pc2 <- c(0.70, 0.18, 0.005, 0.08, 0.03, 0.0025, 0.0025)
    alphas <- logit(cumsum(pc2)) + 0.5*betas - beta2 * 0 - beta3 * 0
    prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- expit(alphas[5] + beta15 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob6 <- expit(alphas[6] + beta16 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[5] + beta15 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob7 <- 1 - expit(alphas[6] + beta16 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5, prob6, prob7)  
  # } else if (array %in% 161:200) {
  #   # NP 2
  #   beta11 <- log(1.52)
  #   beta12 <- log(2.59)
  #   beta13 <- log(3.06)
  #   beta14 <- log(2.01)
  #   prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob5 <- 1 - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   probs <- cbind(prob1, prob2, prob3, prob4, prob5)  
  # } else if (array %in% 201:240) {
  #   # NP 3
  #   beta11 <- log(1.43)
  #   beta12 <- log(2.59)
  #   beta13 <- log(1.21)
  #   beta14 <- log(1.34)
  #   prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob5 <- 1 - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   probs <- cbind(prob1, prob2, prob3, prob4, prob5)  
  # } else if (array %in% 241:280) {
  #   # NP 4
  #   beta11 <- log(1.52)
  #   beta12 <- log(3)
  #   beta13 <- log(3.25)
  #   beta14 <- log(4.03)
  #   prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob5 <- 1 - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   probs <- cbind(prob1, prob2, prob3, prob4, prob5)  
  # } else if (array %in% 281:320) {
  #   # NP 5
  #   beta11 <- log(1.45)
  #   beta12 <- log(2.70)
  #   beta13 <- log(2.55)
  #   beta14 <- log(2.52)
  #   prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   prob5 <- 1 - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
  #   probs <- cbind(prob1, prob2, prob3, prob4, prob5)  
  } else if (array %in% 121:160) {
    # NP 6
    beta11 <- log(1.00)
    beta12 <- log(4.41)
    beta13 <- log(4.09)
    beta14 <- log(4.03)
    beta15 <- log(3.91)
    beta16 <- log(3.70)
    prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- expit(alphas[5] + beta15 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob6 <- expit(alphas[6] + beta16 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[5] + beta15 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob7 <- 1 - expit(alphas[6] + beta16 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5, prob6, prob7)  
  }
  
  
  df$y <- apply(probs, 1, sample, x = 1:7, size = 1, replace = TRUE)
  
  
  return(df)
  
}

DataGen_array_adjusted7_071525 <- function(n, array) {
  #pc2 <- c(0.68, 0.18, 0.09, 0.02, 0.015, 0.01, 0.005)
  pc2 <- c(0.68, 0.18, 0.005, 0.09, 0.03, 0.01, 0.005)
  df <- data.frame(matrix(NA, nrow = n, ncol = 7))
  colnames(df) <- c("y", "trt", "trt_05", "x2", "x3")
  df$trt <- rbinom(n, 1, 0.5)
  df$trt_05 <- case_when(df$trt == 0 ~ -0.5, 
                         df$trt == 1 ~ 0.5)
  df$x2 <- rbinom(n, 1, 0.5)
  df$x2 <- case_when(df$x2 == 0 ~ -0.5, 
                     df$x2 == 1 ~ 0.5)
  beta2 <- 0.95 #2.0
  df$x3 <- rnorm(n, 0, 1)
  beta3 <- -1.0
  if (array %in% 1:40) {
    # Null scenario
    alphas <- logit(cumsum(pc2)) - beta2 * 0 - beta3 * 0
    prob1 <- expit(alphas[1] + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- expit(alphas[5] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[4] + beta2 * df$x2 + beta3 * df$x3)
    prob6 <- expit(alphas[6] + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[5] + beta2 * df$x2 + beta3 * df$x3)
    prob7 <- 1 - expit(alphas[6] + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5, prob6, prob7)
  } else if (array %in% 41:80) {
    # PO with power about 80%
    beta1 <- log(1.47)
    alphas <- logit(cumsum(pc2)) + 0.5*beta1 - beta2 * 0 - beta3 * 0
    prob1 <- expit(alphas[1] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- expit(alphas[5] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[4] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob6 <- expit(alphas[6] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[5] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob7 <- 1 - expit(alphas[6] + beta1 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5, prob6, prob7)
  } else if (array %in% 81:120) {
    # NP 1
    beta11 <- log(1.36)
    beta12 <- log(2.14)
    beta13 <- log(3.06) #3.06
    beta14 <- log(2.01)
    beta15 <- log(1.84)
    beta16 <- log(2.65)
    betas <- c(beta11, beta12, beta13, beta14, beta15, beta16, Inf)
    # pc2 <- c(0.70, 0.18, 0.005, 0.08, 0.03, 0.0025, 0.0025)
    alphas <- logit(cumsum(pc2)) + 0.5*betas - beta2 * 0 - beta3 * 0
    prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- expit(alphas[5] + beta15 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob6 <- expit(alphas[6] + beta16 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[5] + beta15 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob7 <- 1 - expit(alphas[6] + beta16 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5, prob6, prob7)  
  } else if (array %in% 121:160) {
    # NP 6
    beta11 <- log(1.00)
    beta12 <- log(4.41)
    beta13 <- log(4.09)
    beta14 <- log(4.03)
    beta15 <- log(3.91)
    beta16 <- log(3.70)
    betas <- c(beta11, beta12, beta13, beta14, beta15, beta16, Inf)
    alphas <- logit(cumsum(pc2)) + 0.5*betas - beta2 * 0 - beta3 * 0
    prob1 <- expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob2 <- expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[1] + beta11 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob3 <- expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[2] + beta12 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob4 <- expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[3] + beta13 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob5 <- expit(alphas[5] + beta15 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[4] + beta14 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob6 <- expit(alphas[6] + beta16 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3) - expit(alphas[5] + beta15 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    prob7 <- 1 - expit(alphas[6] + beta16 * df$trt_05 + beta2 * df$x2 + beta3 * df$x3)
    probs <- cbind(prob1, prob2, prob3, prob4, prob5, prob6, prob7)  
  }
  
  
  df$y <- apply(probs, 1, sample, x = 1:7, size = 1, replace = TRUE)
  
  
  return(df)
  
}

data_setup <- function (formula, ppo = NULL, data = environment(formula), 
                        na.action = na.delete, stratum = 2,
                        iprior = 0, conc = 1/(0.8 + 0.35 * max(k, 3)), 
                        cppo = NULL,
                        priorsdppo = rep(100, pppo), ascale = 1, 
                        psigma = 1, rsdmean = if (psigma == 1) 0 else 1, rsdsd = 1, 
                        normcppo = FALSE, pcontrast = NULL, npcontrast = NULL, backend = c("rstan", 
                                                                                           "cmdstan"), iter = 2000, warmup = iter/2, chains = 4, 
                        refresh = 0, progress = if (refresh > 0) "stan-progress.txt" else "", 
                        x = TRUE, y = TRUE, loo = n <= 1000, ppairs = NULL, method = c("both", 
                                                                                       "sampling", "optimizing"), inito = if (length(ppo)) 0 else "random", 
                        inits = inito, standata = FALSE,  debug = FALSE, 
                        sampling.args = NULL, showopt = FALSE, ...) {
  
  
  data <- rms::modelData(data, formula, ppo, 
                         na.action = na.action)
  
  stratas <- case_when(data$trt == -0.5 ~ 1, data$trt == 0.5 ~ 2)
  if (length(ppo)) {
    # data2 will have the PPO terms
    data2 <- attr(data, "data2")
    attr(data, "data2") <- NULL
  }
  
  nact <- NULL
  yname <- as.character(formula[2])
  en <- environment(formula)
  requireNamespace("rstan", quietly = TRUE)
  #X <- rms::Design(data, formula = formula)
  X <- rms::Design(data, formula = formula, specials = "cluster")
  cluster <- attr(X, "cluster")
  clustername <- attr(X, "clustername")
  atrx <- attributes(X)
  sformula <- atrx$sformula
  nact <- atrx$na.action
  Terms <- atrx$terms
  attr(Terms, "formula") <- formula
  atr <- atrx$Design
  mmcolnames <- atr$mmcolnames
  Y <- model.extract(X, "response")
  isOcens <- inherits(Y, "Ocens") || NCOL(Y) == 2
  Ncens <- c(left = 0, right = 0, interval = 0)
  if (!isOcens) {
    Y <- Ocens(Y)
    ay <- attributes(Y)
    k <- length(ay$levels)
  } else {
    ay <- attributes(Y)
    k <- length(ay$levels)
    a <- Y[, 1]
    b <- Y[, 2]
    Ncens <- c(left = sum(a == 1 & b > 1), right = sum(b == 
                                                         k & a < k), interval = sum(a != b & a > 1 & b < k))
  }
  offs <- atrx$offset
  if (!length(offs))  
    offs <- 0
  X <- model.matrix(Terms, X)
  alt <- attr(mmcolnames, "alt")
  if (!all(mmcolnames %in% colnames(X)) && length(alt)) 
    mmcolnames <- alt
  X <- X[, mmcolnames, drop = FALSE]
  colnames(X) <- atr$colnames
  Z <- zatr <- zsformula <- NULL
  if (length(ppo)) {
    Z <- rms::Design(data2, formula = ppo)
    zatrx <- attributes(Z)
    zsformula <- zatrx$sformula
    zTerms <- zatrx$terms
    attr(zTerms, "formula") <- ppo
    zatr <- zatrx$Design
    mmcolnames <- zatr$mmcolnames
    Z <- model.matrix(zTerms, Z)
    alt <- attr(mmcolnames, "alt")
    if (!all(mmcolnames %in% colnames(Z)) && length(alt)) 
      mmcolnames <- alt
    Z <- Z[, mmcolnames, drop = FALSE]
    colnames(Z) <- zatr$colnames
  }
  zbar <- NULL
  trimit <- function(x) {
    attr(x, "scaled:center") <- NULL
    dimnames(x) <- NULL
    x
  }
  wqrX <- selectedQr(X, center = TRUE)
  Xs <- trimit(wqrX$X)
  xbar <- wqrX$xbar
  xqrsd <- apply(Xs, 2, sd)
  wqrX$X <- NULL
  if (length(ppo)) {
    wqrZ <- selectedQr(Z, center = TRUE)
    Zs <- trimit(wqrZ$X)
    zbar <- wqrZ$xbar
    wqrZ$X <- NULL
  }
  n <- nrow(X)
  p <- ncol(X)
  ylev <- ay$levels
  mediany <- ay$median
  midy <- ay$mid
  numy <- ay$freq
  names(numy) <- ylev
  kmid <- if (length(mediany)) 
    max(1, which(ylev == mediany) - 1L) 
  else max(1, which((1L:length(ylev)) == mediany) - 1L)
  pppo <- if (length(ppo))  ncol(Z) else 0
  if (length(cppo) && (pppo == 0)) 
    stop("may not specify cppo without ppo")
  nrp <- length(ylev) - 1L
  if (nrp == 1) 
    kmid <- 1
  ass <- rms::DesignAssign(atr, nrp, Terms)
  zass <- if (pppo > 0) 
    rms::DesignAssign(zatr, 0, zTerms)
  if (k > 2 && length(ppo) && !length(cppo)) 
    Y <- as.integer(Y[, 1])
  d <- if (iprior == 0) 
    list(X = Xs, y = Y, N = n, p = p, k = k, conc = conc)
  else list(X = Xs, y = Y, N = n, p = p, k = k, iprior = iprior, 
            ascale = ascale)
  alphasign <- if (iprior == 0) 
    1
  else -1
  if (length(cppo)) {
    if (normcppo) {
      Cppo <- function(y, fun, center, scale) (fun(y) - 
                                                 center)/scale
      formals(Cppo)$fun <- cppo
      cppoy <- cppo(ylev[Y[, 1]])
      formals(Cppo)$center <- mean(cppoy)
      formals(Cppo)$scale <- sd(cppoy)
      cppo <- Cppo
    }
    d$pposcore <- cppo(ylev)
  } else d$pposcore <- numeric()
  d$lpposcore <- length(d$pposcore)
  Nc <- 0
  d$psigma <- as.integer(psigma)
  if (length(cluster)) {
    d$rsdmean <- as.array(rsdmean)
    d$rsdsd <- if (psigma == 1) 
      as.array(rsdsd)
    else numeric(0)
    cl <- as.integer(as.factor(cluster))
    Nc <- max(cl, na.rm = TRUE)
    d$Nc <- Nc
    d$cluster <- cl
    method <- "sampling"
  } else {
    Nc <- d$Nc <- 0L
    d$rsdmean <- d$rsdsd <- numeric()
    d$cluster <- integer()
  }
  d$Z <- if (pppo) 
    Zs
  else matrix(0, nrow = n, ncol = 0)
  d$q <- pppo
  d$cn <- d$cn2 <- 0L
  d$cmus <- d$cmus2 <- d$csds <- d$csds2 <- array(numeric(0))
  d$C <- array(0, c(0, p))
  d$C2 <- array(0, c(0, pppo))
  if (any(is.na(Xs)) | any(is.na(Y))) 
    stop("program logic error")
  unconstrainedppo <- pppo > 0 && length(cppo) == 0
  if (unconstrainedppo) {
    priorsdppo <- rep(priorsdppo, length = pppo)
    d$sdsppo <- as.array(priorsdppo)
  }
  
  d$S <- stratum
  d$stratum <- stratas
  
  return(list(d = d, 
              wqrX = wqrX,
              wqrZ = wqrZ))
}

data_setup_adj <- function (formula, ppo = NULL, data = environment(formula), 
                            na.action = na.delete, stratum = 2,
                            iprior = 0, conc = 1/(0.8 + 0.35 * max(k, 3)), 
                            cppo = NULL,
                            priorsdppo = rep(100, pppo), ascale = 1, 
                            psigma = 1, rsdmean = if (psigma == 1) 0 else 1, rsdsd = 1, 
                            normcppo = FALSE, pcontrast = NULL, npcontrast = NULL, backend = c("rstan", 
                                                                                               "cmdstan"), iter = 2000, warmup = iter/2, chains = 4, 
                            refresh = 0, progress = if (refresh > 0) "stan-progress.txt" else "", 
                            x = TRUE, y = TRUE, loo = n <= 1000, ppairs = NULL, method = c("both", 
                                                                                           "sampling", "optimizing"), inito = if (length(ppo)) 0 else "random", 
                            inits = inito, standata = FALSE,  debug = FALSE, 
                            sampling.args = NULL, showopt = FALSE, ...) {
  
  
  data <- rms::modelData(data, formula, ppo, 
                         na.action = na.action) # gets data set up in a data frame with outcome and covariates
  
  stratas <- case_when(data$trt == -0.5 ~ 1, data$trt == 0.5 ~ 2) #defines which observations are in each strata
  if (length(ppo)) {
    # data2 will have the PPO terms and redefine data2 as null
    data2 <- attr(data, "data2")
    attr(data, "data2") <- NULL
  }
  
  nact <- NULL
  yname <- as.character(formula[2]) # get outcome variable name
  en <- environment(formula) # get environment of formula
  requireNamespace("rstan", quietly = TRUE)
  X <- rms::Design(data, formula = formula, specials = "cluster")
  cluster <- attr(X, "cluster") # get cluster variable (if applicable )
  clustername <- attr(X, "clustername") # get clustername variable (if applicable)
  atrx <- attributes(X)
  sformula <- atrx$sformula
  nact <- atrx$na.action
  Terms <- atrx$terms
  attr(Terms, "formula") <- formula
  atr <- atrx$Design
  mmcolnames <- atr$mmcolnames #gets column names (maybe just ppo?)
  Y <- model.extract(X, "response")
  isOcens <- inherits(Y, "Ocens") || NCOL(Y) == 2
  Ncens <- c(left = 0, right = 0, interval = 0)
  if (!isOcens) {
    Y <- Ocens(Y)
    ay <- attributes(Y)
    k <- length(ay$levels)
  } else {
    ay <- attributes(Y)
    k <- length(ay$levels)
    a <- Y[, 1]
    b <- Y[, 2]
    Ncens <- c(left = sum(a == 1 & b > 1), right = sum(b == 
                                                         k & a < k), interval = sum(a != b & a > 1 & b < k))
  }
  offs <- atrx$offset
  if (!length(offs))  
    offs <- 0
  X <- model.matrix(Terms, X)
  alt <- attr(mmcolnames, "alt")
  if (!all(mmcolnames %in% colnames(X)) && length(alt)) 
    mmcolnames <- alt
  X <- X[, mmcolnames, drop = FALSE]
  colnames(X) <- atr$colnames
  Z <- zatr <- zsformula <- NULL
  if (length(ppo)) {
    Z <- rms::Design(data2, formula = ppo)
    zatrx <- attributes(Z)
    zsformula <- zatrx$sformula
    zTerms <- zatrx$terms
    attr(zTerms, "formula") <- ppo
    zatr <- zatrx$Design
    mmcolnames <- zatr$mmcolnames
    Z <- model.matrix(zTerms, Z)
    alt <- attr(mmcolnames, "alt")
    if (!all(mmcolnames %in% colnames(Z)) && length(alt)) 
      mmcolnames <- alt
    Z <- Z[, mmcolnames, drop = FALSE]
    colnames(Z) <- zatr$colnames
  }
  zbar <- NULL
  trimit <- function(x) {
    attr(x, "scaled:center") <- NULL
    dimnames(x) <- NULL
    x
  }
  wqrX <- selectedQr(X, not = 1, center = TRUE)
  Xs <- trimit(wqrX$X)
  xbar <- wqrX$xbar
  xqrsd <- apply(Xs, 2, sd)
  wqrX$X <- NULL
  if (length(ppo)) {
    wqrZ <- selectedQr(Z, center = TRUE)
    Zs <- trimit(wqrZ$X)
    zbar <- wqrZ$xbar
    wqrZ$X <- NULL
  }
  n <- nrow(X)
  p <- 1 #ncol(X)
  ylev <- ay$levels
  mediany <- ay$median
  midy <- ay$mid
  numy <- ay$freq
  names(numy) <- ylev
  kmid <- if (length(mediany)) 
    max(1, which(ylev == mediany) - 1L) 
  else max(1, which((1L:length(ylev)) == mediany) - 1L)
  pppo <- if (length(ppo))  ncol(Z) else 0
  if (length(cppo) && (pppo == 0)) 
    stop("may not specify cppo without ppo")
  nrp <- length(ylev) - 1L
  if (nrp == 1) 
    kmid <- 1
  ass <- rms::DesignAssign(atr, nrp, Terms)
  zass <- if (pppo > 0) 
    rms::DesignAssign(zatr, 0, zTerms)
  if (k > 2 && length(ppo) && !length(cppo)) 
    Y <- as.integer(Y[, 1])
  d <- if (iprior == 0) 
    list(X = Xs, y = Y, N = n, p = p, k = k, conc = conc)
  else list(X = Xs, y = Y, N = n, p = p, k = k, iprior = iprior, 
            ascale = ascale)
  alphasign <- if (iprior == 0) 
    1
  else -1
  if (length(cppo)) {
    if (normcppo) {
      Cppo <- function(y, fun, center, scale) (fun(y) - 
                                                 center)/scale
      formals(Cppo)$fun <- cppo
      cppoy <- cppo(ylev[Y[, 1]])
      formals(Cppo)$center <- mean(cppoy)
      formals(Cppo)$scale <- sd(cppoy)
      cppo <- Cppo
    }
    d$pposcore <- cppo(ylev)
  } else d$pposcore <- numeric()
  d$lpposcore <- length(d$pposcore)
  Nc <- 0
  d$psigma <- as.integer(psigma)
  if (length(cluster)) {
    d$rsdmean <- as.array(rsdmean)
    d$rsdsd <- if (psigma == 1) 
      as.array(rsdsd)
    else numeric(0)
    cl <- as.integer(as.factor(cluster))
    Nc <- max(cl, na.rm = TRUE)
    d$Nc <- Nc
    d$cluster <- cl
    method <- "sampling"
  } else {
    Nc <- d$Nc <- 0L
    d$rsdmean <- d$rsdsd <- numeric()
    d$cluster <- integer()
  }
  d$Z <- if (pppo) 
    Zs
  else matrix(0, nrow = n, ncol = 0)
  d$q <- pppo
  d$cn <- d$cn2 <- 0L
  d$cmus <- d$cmus2 <- d$csds <- d$csds2 <- array(numeric(0))
  d$C <- array(0, c(0, p))
  d$C2 <- array(0, c(0, pppo))
  if (any(is.na(Xs)) | any(is.na(Y))) 
    stop("program logic error")
  unconstrainedppo <- pppo > 0 && length(cppo) == 0
  if (unconstrainedppo) {
    priorsdppo <- rep(priorsdppo, length = pppo)
    d$sdsppo <- as.array(priorsdppo)
  }
  
  d$S <- stratum
  d$stratum <- stratas
  
  d$W <- d$X[, c(2:3)]
  d$w <- ncol(d$W)
  
  d$X <- d$Z
  
  return(list(d = d, 
              wqrX = wqrX,
              wqrZ = wqrZ))
}


data_setup_LT <- function (formula, ppo = NULL, data = environment(formula), 
                               na.action = na.delete, stratum = 2,
                               conc = 1/(0.8 + 0.35 * max(k, 3)), 
                               priorsdppo = rep(100, pppo), 
                               psigma = 1,  iter = 2000, warmup = iter/2, chains = 4, 
                               refresh = 0, progress = if (refresh > 0) "stan-progress.txt" else "", 
                               x = TRUE, y = TRUE,  inito = if (length(ppo)) 0 else "random", 
                               inits = inito) {
  
  
  data <- rms::modelData(data, formula, ppo, 
                         na.action = na.action) # gets data set up in a data frame with outcome and covariates
  
  stratas <- case_when(data$trt == -0.5 ~ 1, data$trt == 0.5 ~ 2) #defines which observations are in each strata
  if (length(ppo)) {
    # data2 will have the PPO terms and redefine data2 as null
    data2 <- attr(data, "data2")
    attr(data, "data2") <- NULL
  }
  
  nact <- NULL
  yname <- as.character(formula[2]) # get outcome variable name
  en <- environment(formula) # get environment of formula
  requireNamespace("rstan", quietly = TRUE)
  X <- rms::Design(data, formula = formula) #, specials = "cluster"
  #cluster <- attr(X, "cluster") # get cluster variable (if applicable )
  #clustername <- attr(X, "clustername") # get clustername variable (if applicable)
  atrx <- attributes(X)
  sformula <- atrx$sformula
  nact <- atrx$na.action
  Terms <- atrx$terms
  attr(Terms, "formula") <- formula
  atr <- atrx$Design
  mmcolnames <- atr$mmcolnames #gets column names 
  Y <- model.extract(X, "response")
  isOcens <- inherits(Y, "Ocens") || NCOL(Y) == 2
  Ncens <- c(left = 0, right = 0, interval = 0)
  if (!isOcens) {
    Y <- Ocens(Y)
    ay <- attributes(Y)
    k <- length(ay$levels)
  }
  # } else {
  #   ay <- attributes(Y)
  #   k <- length(ay$levels)
  #   a <- Y[, 1]
  #   b <- Y[, 2]
  #   Ncens <- c(left = sum(a == 1 & b > 1), right = sum(b == 
  #                                                        k & a < k), interval = sum(a != b & a > 1 & b < k))
  # }
  offs <- atrx$offset
  # if (!length(offs))  
  #   offs <- 0
  X <- model.matrix(Terms, X)
  alt <- attr(mmcolnames, "alt")
  # if (!all(mmcolnames %in% colnames(X)) && length(alt)) 
  #   mmcolnames <- alt
  X <- X[, mmcolnames, drop = FALSE]
  colnames(X) <- atr$colnames
  Z <- zatr <- zsformula <- NULL
  if (length(ppo)) {
    Z <- rms::Design(data2, formula = ppo)
    zatrx <- attributes(Z)
    zsformula <- zatrx$sformula
    zTerms <- zatrx$terms
    attr(zTerms, "formula") <- ppo
    zatr <- zatrx$Design
    mmcolnames <- zatr$mmcolnames
    Z <- model.matrix(zTerms, Z)
    alt <- attr(mmcolnames, "alt")
    # if (!all(mmcolnames %in% colnames(Z)) && length(alt)) 
    #   mmcolnames <- alt
    Z <- Z[, mmcolnames, drop = FALSE]
    colnames(Z) <- zatr$colnames
  }
  zbar <- NULL
  trimit <- function(x) {
    attr(x, "scaled:center") <- NULL
    dimnames(x) <- NULL
    x
  }
  wqrX <- selectedQr(X, not = 1, center = TRUE)
  Xs <- trimit(wqrX$X)
  xbar <- wqrX$xbar
  xqrsd <- apply(Xs, 2, sd)
  wqrX$X <- NULL
  if (length(ppo)) {
    wqrZ <- selectedQr(Z, center = TRUE)
    Zs <- trimit(wqrZ$X)
    zbar <- wqrZ$xbar
    wqrZ$X <- NULL
  }
  n <- nrow(X)
  p <- 1 #ncol(X)
  ylev <- ay$levels
  mediany <- ay$median
  midy <- ay$mid
  numy <- ay$freq
  names(numy) <- ylev
  kmid <- if (length(mediany)) 
    max(1, which(ylev == mediany) - 1L) 
  else max(1, which((1L:length(ylev)) == mediany) - 1L)
  pppo <- if (length(ppo))  ncol(Z) else 0
  # if (length(cppo) && (pppo == 0)) 
  #   stop("may not specify cppo without ppo")
  nrp <- length(ylev) - 1L
  if (nrp == 1) 
    kmid <- 1
  ass <- rms::DesignAssign(atr, nrp, Terms)
  zass <- if (pppo > 0) 
    rms::DesignAssign(zatr, 0, zTerms)
  if (k > 2 && length(ppo)) #&& !length(cppo) 
    Y <- as.integer(Y[, 1])
  d <- list(X = Xs, y = Y, N = n, p = p, k = k, conc = conc)
  # if (iprior == 0) 
  #   list(X = Xs, y = Y, N = n, p = p, k = k, conc = conc)
  # else list(X = Xs, y = Y, N = n, p = p, k = k, iprior = iprior, 
  #           ascale = ascale)
  alphasign <- 1
  # alphasign <- if (iprior == 0) 
  #   1
  # else -1
  # if (length(cppo)) {
  #   if (normcppo) {
  #     Cppo <- function(y, fun, center, scale) (fun(y) - 
  #                                                center)/scale
  #     formals(Cppo)$fun <- cppo
  #     cppoy <- cppo(ylev[Y[, 1]])
  #     formals(Cppo)$center <- mean(cppoy)
  #     formals(Cppo)$scale <- sd(cppoy)
  #     cppo <- Cppo
  #   }
  #   d$pposcore <- cppo(ylev)
  # } else d$pposcore <- numeric()
  d$pposcore <- numeric()
  d$lpposcore <- length(d$pposcore)
  # Nc <- 0
  d$psigma <- as.integer(psigma)
  # if (length(cluster)) {
  #   d$rsdmean <- as.array(rsdmean)
  #   d$rsdsd <- if (psigma == 1) 
  #     as.array(rsdsd)
  #   else numeric(0)
  #   cl <- as.integer(as.factor(cluster))
  #   Nc <- max(cl, na.rm = TRUE)
  #   d$Nc <- Nc
  #   d$cluster <- cl
  #   method <- "sampling"
  # } else {
  #   Nc <- d$Nc <- 0L
  #   d$rsdmean <- d$rsdsd <- numeric()
  #   d$cluster <- integer()
  # }
  d$Z <- Zs
  #   if (pppo) 
  #   Zs
  # else matrix(0, nrow = n, ncol = 0)
  d$q <- pppo
  # d$cn <- d$cn2 <- 0L
  # d$cmus <- d$cmus2 <- d$csds <- d$csds2 <- array(numeric(0))
  # d$C <- array(0, c(0, p))
  # d$C2 <- array(0, c(0, pppo))
  if (any(is.na(Xs)) | any(is.na(Y))) 
    stop("program logic error")
  unconstrainedppo <- pppo > 0  #&& length(cppo) == 0
  # if (unconstrainedppo) {
  #   priorsdppo <- rep(priorsdppo, length = pppo)
  #   d$sdsppo <- as.array(priorsdppo)
  # }
  priorsdppo <- rep(priorsdppo, length = pppo)
  d$sdsppo <- as.array(priorsdppo)
  
  d$S <- stratum
  d$stratum <- stratas
  
  return(list(d = d, 
              wqrX = wqrX,
              wqrZ = wqrZ))
}

data_setup_adj_LT <- function (formula, ppo = NULL, data = environment(formula), 
                            na.action = na.delete, stratum = 2,
                            conc = 1/(0.8 + 0.35 * max(k, 3)), 
                            priorsdppo = rep(100, pppo), 
                            psigma = 1,  iter = 2000, warmup = iter/2, chains = 4, 
                            refresh = 0, progress = if (refresh > 0) "stan-progress.txt" else "", 
                            x = TRUE, y = TRUE,  inito = if (length(ppo)) 0 else "random", 
                            inits = inito) {
  
  
  data <- rms::modelData(data, formula, ppo, 
                         na.action = na.action) # gets data set up in a data frame with outcome and covariates
  
  stratas <- case_when(data$trt == -0.5 ~ 1, data$trt == 0.5 ~ 2) #defines which observations are in each strata
  if (length(ppo)) {
    # data2 will have the PPO terms and redefine data2 as null
    data2 <- attr(data, "data2")
    attr(data, "data2") <- NULL
  }
  
  nact <- NULL
  yname <- as.character(formula[2]) # get outcome variable name
  en <- environment(formula) # get environment of formula
  requireNamespace("rstan", quietly = TRUE)
  X <- rms::Design(data, formula = formula) #, specials = "cluster"
  #cluster <- attr(X, "cluster") # get cluster variable (if applicable )
  #clustername <- attr(X, "clustername") # get clustername variable (if applicable)
  atrx <- attributes(X)
  sformula <- atrx$sformula
  nact <- atrx$na.action
  Terms <- atrx$terms
  attr(Terms, "formula") <- formula
  atr <- atrx$Design
  mmcolnames <- atr$mmcolnames #gets column names 
  Y <- model.extract(X, "response")
  isOcens <- inherits(Y, "Ocens") || NCOL(Y) == 2
  Ncens <- c(left = 0, right = 0, interval = 0)
  if (!isOcens) {
    Y <- Ocens(Y)
    ay <- attributes(Y)
    k <- length(ay$levels)
  }
  # } else {
  #   ay <- attributes(Y)
  #   k <- length(ay$levels)
  #   a <- Y[, 1]
  #   b <- Y[, 2]
  #   Ncens <- c(left = sum(a == 1 & b > 1), right = sum(b == 
  #                                                        k & a < k), interval = sum(a != b & a > 1 & b < k))
  # }
  offs <- atrx$offset
  # if (!length(offs))  
  #   offs <- 0
  X <- model.matrix(Terms, X)
  alt <- attr(mmcolnames, "alt")
  # if (!all(mmcolnames %in% colnames(X)) && length(alt)) 
  #   mmcolnames <- alt
  X <- X[, mmcolnames, drop = FALSE]
  colnames(X) <- atr$colnames
  Z <- zatr <- zsformula <- NULL
  if (length(ppo)) {
    Z <- rms::Design(data2, formula = ppo)
    zatrx <- attributes(Z)
    zsformula <- zatrx$sformula
    zTerms <- zatrx$terms
    attr(zTerms, "formula") <- ppo
    zatr <- zatrx$Design
    mmcolnames <- zatr$mmcolnames
    Z <- model.matrix(zTerms, Z)
    alt <- attr(mmcolnames, "alt")
    # if (!all(mmcolnames %in% colnames(Z)) && length(alt)) 
    #   mmcolnames <- alt
    Z <- Z[, mmcolnames, drop = FALSE]
    colnames(Z) <- zatr$colnames
  }
  zbar <- NULL
  trimit <- function(x) {
    attr(x, "scaled:center") <- NULL
    dimnames(x) <- NULL
    x
  }
  wqrX <- selectedQr(X, not = 1, center = TRUE)
  Xs <- trimit(wqrX$X)
  xbar <- wqrX$xbar
  xqrsd <- apply(Xs, 2, sd)
  wqrX$X <- NULL
  if (length(ppo)) {
    wqrZ <- selectedQr(Z, center = TRUE)
    Zs <- trimit(wqrZ$X)
    zbar <- wqrZ$xbar
    wqrZ$X <- NULL
  }
  n <- nrow(X)
  p <- 1 #ncol(X)
  ylev <- ay$levels
  mediany <- ay$median
  midy <- ay$mid
  numy <- ay$freq
  names(numy) <- ylev
  kmid <- if (length(mediany)) 
    max(1, which(ylev == mediany) - 1L) 
  else max(1, which((1L:length(ylev)) == mediany) - 1L)
  pppo <- if (length(ppo))  ncol(Z) else 0
  # if (length(cppo) && (pppo == 0)) 
  #   stop("may not specify cppo without ppo")
  nrp <- length(ylev) - 1L
  if (nrp == 1) 
    kmid <- 1
  ass <- rms::DesignAssign(atr, nrp, Terms)
  zass <- if (pppo > 0) 
    rms::DesignAssign(zatr, 0, zTerms)
  if (k > 2 && length(ppo)) #&& !length(cppo) 
    Y <- as.integer(Y[, 1])
  d <- list(X = Xs, y = Y, N = n, p = p, k = k, conc = conc)
  # if (iprior == 0) 
  #   list(X = Xs, y = Y, N = n, p = p, k = k, conc = conc)
  # else list(X = Xs, y = Y, N = n, p = p, k = k, iprior = iprior, 
  #           ascale = ascale)
  alphasign <- 1
  # alphasign <- if (iprior == 0) 
  #   1
  # else -1
  # if (length(cppo)) {
  #   if (normcppo) {
  #     Cppo <- function(y, fun, center, scale) (fun(y) - 
  #                                                center)/scale
  #     formals(Cppo)$fun <- cppo
  #     cppoy <- cppo(ylev[Y[, 1]])
  #     formals(Cppo)$center <- mean(cppoy)
  #     formals(Cppo)$scale <- sd(cppoy)
  #     cppo <- Cppo
  #   }
  #   d$pposcore <- cppo(ylev)
  # } else d$pposcore <- numeric()
  d$pposcore <- numeric()
  d$lpposcore <- length(d$pposcore)
  # Nc <- 0
  d$psigma <- as.integer(psigma)
  # if (length(cluster)) {
  #   d$rsdmean <- as.array(rsdmean)
  #   d$rsdsd <- if (psigma == 1) 
  #     as.array(rsdsd)
  #   else numeric(0)
  #   cl <- as.integer(as.factor(cluster))
  #   Nc <- max(cl, na.rm = TRUE)
  #   d$Nc <- Nc
  #   d$cluster <- cl
  #   method <- "sampling"
  # } else {
  #   Nc <- d$Nc <- 0L
  #   d$rsdmean <- d$rsdsd <- numeric()
  #   d$cluster <- integer()
  # }
  d$Z <- Zs
  #   if (pppo) 
  #   Zs
  # else matrix(0, nrow = n, ncol = 0)
  d$q <- pppo
  # d$cn <- d$cn2 <- 0L
  # d$cmus <- d$cmus2 <- d$csds <- d$csds2 <- array(numeric(0))
  # d$C <- array(0, c(0, p))
  # d$C2 <- array(0, c(0, pppo))
  if (any(is.na(Xs)) | any(is.na(Y))) 
    stop("program logic error")
  unconstrainedppo <- pppo > 0  #&& length(cppo) == 0
  # if (unconstrainedppo) {
  #   priorsdppo <- rep(priorsdppo, length = pppo)
  #   d$sdsppo <- as.array(priorsdppo)
  # }
  priorsdppo <- rep(priorsdppo, length = pppo)
  d$sdsppo <- as.array(priorsdppo)
  
  d$S <- stratum
  d$stratum <- stratas
  
  d$W <- d$X[, c(2:3)]
  d$w <- ncol(d$W)
  
  d$X <- d$Z
  
  return(list(d = d, 
              wqrX = wqrX,
              wqrZ = wqrZ))
}

# code to calculate estimands 

order_weighted_RD2_strat <- function(stanfit, iprior = 1, dat, trt, outcome, utility, outcome_levels) { 
  modfit <- as.data.frame(stanfit)
  
  alphas <- modfit[,grep('alpha\\[', colnames(modfit))]
  alpha <- colMeans(alphas)
  # first calculate P(y >= i | ctl) and P(y >= i | trt) for all i 
  PPO_ctl_geq <- NA 
  PPO_trt_geq <- NA 
  PPO_ctl_leq <- NA 
  PPO_trt_leq <- NA 
  p_y_ctl <- NA
  p_y_trt <- NA
  p_y_ctl_est <- NA
  p_y_trt_est <- NA
  p_y <- NA
  c <- length(alpha)/2 +1
  c_true <- outcome_levels
  
  if (c_true != c) {
    #this scenario, (at least) one level had no participants 
    # find which level(s) had no participants 
    no_part_level <- which(!(1:c_true %in% unique(dat$y)))
    utility <- utility[-no_part_level]
  }
  
  
  
  for (i in 2:c) {
    # for the first level there is not a partial term, so only need first term and metformin term 
    PPO_ctl_geq[i] <- plogis(alpha[2*i - 3]) 
    PPO_trt_geq[i] <- plogis(alpha[2*i - 2])
    PPO_ctl_leq[i - 1] <- 1 - PPO_ctl_geq[i]
    PPO_trt_leq[i - 1] <- 1 - PPO_trt_geq[i]
    
  }
  
  # calculate p(Y = i | control) for i = 1, ... , c
  for (i in 1:c) {
    if (i == 1) {
      p_y_ctl_est[1] <-  PPO_ctl_leq[1]
      p_y_trt_est[1] <-  PPO_trt_leq[1]
    } else if (i == c) {
      p_y_ctl_est[c] <- 1 - sum(p_y_ctl_est[1:(c-1)])
      p_y_trt_est[c] <- 1 - sum(p_y_trt_est[1:(c-1)])
    } else {
      p_y_ctl_est[i] <-  PPO_ctl_leq[i] - sum(p_y_ctl_est[1:(i-1)])
      p_y_trt_est[i] <-  PPO_trt_leq[i] - sum(p_y_trt_est[1:(i-1)])
    }
  }
  
  # correct for any negative probabilities 
  if (any(p_y_ctl_est < 0)) {
    p_y_ctl_est[which(p_y_ctl_est < 0)] <- 0 
    p_y_ctl_est <- p_y_ctl_est/sum(p_y_ctl_est)
  }
  
  if (any(p_y_trt_est < 0)) {
    p_y_trt_est[which(p_y_trt_est < 0)] <- 0 
    p_y_trt_est <- p_y_trt_est/sum(p_y_trt_est)
  }
  
  p_y_est_overall <- (p_y_ctl_est + p_y_trt_est)/2
  
  
  # get weights from the data 
  p_y_ctl <- unname(table(dat[[trt]], dat[[outcome]])[1,])/sum(dat[[trt]] == -0.5)
  p_y_trt <- unname(table(dat[[trt]], dat[[outcome]])[2,])/sum(dat[[trt]] == 0.5)
  p_y <- unname(table(dat[[outcome]]))/nrow(dat)
  
  
  RD_leq <- PPO_trt_leq - PPO_ctl_leq
  RD_geq <- PPO_ctl_geq - PPO_trt_geq
  
  # put control in the numerator so the OR is greater than one if metformin has a positive effect
  OR_le <- (PPO_trt_leq / (1- PPO_trt_leq)) / (PPO_ctl_leq / (1- PPO_ctl_leq))
  log_OR_le <- log((PPO_trt_leq / (1- PPO_trt_leq)) / (PPO_ctl_leq / (1- PPO_ctl_leq)))
  OR_ge <- (PPO_ctl_geq / (1- PPO_ctl_geq)) / (PPO_trt_geq / (1- PPO_trt_geq))
  # use this for the iprior = 1: (PPO_ctl_geq / (1- PPO_ctl_geq))/(PPO_trt_geq / (1 - PPO_trt_geq))
  
  # calculate RR 
  log_RR_le <- log(PPO_trt_leq/PPO_ctl_leq)
  log_RR_ge <- log(PPO_ctl_geq/PPO_trt_geq)
  
  j <- 1:(c-1)
  k <- 2:c
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_RD <- numerator/denominator
  # 
  # 
  # numerator <- sum((p_y[j] + p_y[k]) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # weighted_RD_overall <- numerator/denominator
  
  #fully bayesian approach 
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (abs(utility[j] - utility[k]))  * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  denominator <- sum((p_y_ctl_est[j]) + sum(p_y_ctl_est[k]) * (abs(utility[j] - utility[k])))
  weighted_RD_B <- numerator/denominator
  
  # fully bayesian with only cumulative weights
  numerator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * (abs(utility[j] - utility[k]))  * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  denominator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * (abs(utility[j] - utility[k])) )
  weighted_RD_cum <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (abs(utility[j] - utility[k])) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  denominator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (abs(utility[j] - utility[k]))  * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_RD_both <- numerator/denominator
  
  # fully bayesian with just utility
  numerator <- sum((abs(utility[j] - utility[k])) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  denominator <- sum((abs(utility[j] - utility[k])))
  weighted_RD_util <- numerator/denominator
  
  # numerator <- sum((p_y_est_overall[j] + p_y_est_overall[k]) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  # denominator <- sum(p_y_est_overall[j]) + sum(p_y_est_overall[k])
  # weighted_RD_Boverall <- numerator/denominator
  
  # unweighted RD 
  unweighted_RD <- mean(PPO_trt_leq - PPO_ctl_leq)
  
  # # calculate weighted OR 
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k])  * log(OR_le[j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_OR <- exp(numerator/denominator)
  # weighted_log_OR <- numerator/denominator
  # 
  # #calculate weighted OR using overall probability as weights
  # numerator <- sum((p_y[j] + p_y[k]) * log(OR_le[j]))
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # weighted_OR_overall <- exp(numerator/denominator)
  # weighted_log_OR_overall <- numerator/denominator
  
  #bayesian approach for OR
  #calculate weighted OR 
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (abs(utility[j] - utility[k]))   * log(OR_le[j]))
  denominator <- sum((p_y_ctl_est[j]) + sum(p_y_ctl_est[k]) * (abs(utility[j] - utility[k])))
  weighted_OR_B <- exp(numerator/denominator)
  weighted_log_OR_B <- numerator/denominator
  
  #calculate weighted OR using overall probability as weights
  # numerator <- sum((p_y_est_overall[j] + p_y_est_overall[k]) * log(OR_le[j]))
  # denominator <- sum(p_y_est_overall[j]) + sum(p_y_est_overall[k])
  # weighted_OR_Boverall <- exp(numerator/denominator)
  # weighted_log_OR_Boverall <- numerator/denominator
  
  # fully bayesian with only cumulative weights
  numerator <- sum((PPO_ctl_leq[j] * (abs(utility[j] - utility[k])) * (1-PPO_ctl_leq[j])) * log(OR_le[j]))
  denominator <- sum((PPO_ctl_leq[j] * (abs(utility[j] - utility[k])) * (1-PPO_ctl_leq[j])))
  weighted_OR_cum <- exp(numerator/denominator)
  weighted_log_OR_cum <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (abs(utility[j] - utility[k])) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * log(OR_le[j]))
  denominator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (abs(utility[j] - utility[k])) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_OR_both <- exp(numerator/denominator)
  weighted_log_OR_both <- numerator/denominator
  
  # fully bayesian with utility only weights
  numerator <- sum((abs(utility[j] - utility[k])) * log(OR_le[j]))
  denominator <- sum((abs(utility[j] - utility[k])))
  weighted_OR_util <- exp(numerator/denominator)
  weighted_log_OR_util <- numerator/denominator
  
  # unweighted OR 
  unweighted_log_OR <- mean(log(OR_le))
  unweighted_OR <- exp(mean(log(OR_le)))
  
  # # calculate weighted RR with control probabilities
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log_RR_le[j])
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_log_RR_control <- numerator/denominator
  # 
  # # calculate weighted RR with overall probabilities
  # numerator <- sum((p_y[j] + p_y[k]) * log_RR_le[j])
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # weighted_log_RR_overall <- numerator/denominator
  
  # bayesian approach RR 
  # calculate weighted RR with control probabilities 
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (abs(utility[j] - utility[k]))  * log_RR_le[j])
  denominator <- sum(p_y_ctl_est[j]) + sum(p_y_ctl_est[k])
  weighted_log_RR_Bcontrol <- numerator/denominator
  
  # fully bayesian with only cumulative weights
  numerator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * (abs(utility[j] - utility[k]))  * log_RR_le[j])
  denominator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * (abs(utility[j] - utility[k])))
  weighted_RR_cum <- exp(numerator/denominator)
  weighted_log_RR_cum <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (abs(utility[j] - utility[k])) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * log_RR_le[j])
  denominator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (abs(utility[j] - utility[k])) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_RR_both <- exp(numerator/denominator)
  weighted_log_RR_both <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((abs(utility[j] - utility[k])) * log_RR_le[j])
  denominator <- sum((abs(utility[j] - utility[k])))
  weighted_RR_util <- exp(numerator/denominator)
  weighted_log_RR_util <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (abs(utility[j] - utility[k])) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * log_RR_ge[k])
  denominator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (abs(utility[j] - utility[k])) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_RR_ge_both <- exp(numerator/denominator)
  weighted_log_RR_ge_both <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((abs(utility[j] - utility[k])) * log_RR_ge[k])
  denominator <- sum((abs(utility[j] - utility[k])))
  weighted_RR_ge_util <- exp(numerator/denominator)
  weighted_log_RR_ge_util <- numerator/denominator
  
  # # calculate weighted RR with overall probabilities 
  # numerator <- sum((p_y_est_overall[j] + p_y_est_overall[k]) * log_RR_le[j]) 
  # denominator <- sum(p_y_est_overall[j]) + sum(p_y_est_overall[k])
  # weighted_log_RR_Boverall <- numerator/denominator
  
  # unweighted RR 
  unweighted_log_RR <- mean(log_RR_le)
  unweighted_RR <- exp(mean(log_RR_le))
  
  # unweighted RR ge
  unweighted_log_RR_ge <- mean(log_RR_ge, na.rm = T)
  unweighted_RR_ge <- exp(mean(log_RR_ge, na.rm = T))
  
  # calculate NB 
  NB <- sum(p_y_trt_est[j] * PPO_ctl_geq[j+1]) - (sum(p_y_ctl_est[j] * PPO_trt_geq[j+1]))
  
  # mean utility
  l = 1:c
  delta_t <- sum(utility[l] * p_y_trt_est[l])
  delta_c <- sum(utility[l] * p_y_ctl_est[l])
  delta <- delta_t - delta_c
  
  weights_split <- ((p_y_ctl_est[j] + p_y_ctl_est[k]) * abs(utility[j] - utility[k])) / sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * abs(utility[j] - utility[k]))
  weights_cum <- (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j]) * abs(utility[j] - utility[k])) / sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * abs(utility[j] - utility[k]))
  weights_both <- (p_y_ctl_est[j] + p_y_ctl_est[k])* abs(utility[j] - utility[k]) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) / 
    sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * abs(utility[j] - utility[k]) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weights_util <- (abs(utility[j] - utility[k])) / sum((abs(utility[j] - utility[k])))
  
  if (c_true != c) {
    if (length(no_part_level) == 1) { 
      p_y_ctl = append(p_y_ctl, NA, no_part_level - 1)
      p_y_trt = append(p_y_trt, NA, no_part_level - 1)
      RD_leq = append(RD_leq, NA, no_part_level - 1)
      RD_geq = append(RD_geq, NA, no_part_level - 1)
      log_RR_le = append(log_RR_le, NA, no_part_level - 1)
      log_RR_ge = append(log_RR_ge, NA, no_part_level - 1)
      OR_le = append(OR_le, NA, no_part_level - 1)
      log_OR_le = append(log_OR_le, NA, no_part_level - 1)
      p_y_trt_est = append(p_y_trt_est, NA, no_part_level - 1)
      p_y_ctl_est = append(p_y_ctl_est, NA, no_part_level - 1)
      weights_split = append(weights_split, NA, no_part_level - 1)
      weights_cum = append(weights_cum, NA, no_part_level - 1)
      weights_both = append(weights_both, NA, no_part_level - 1)
      weights_util = append(weights_util, NA, no_part_level - 1)
      
    } else if (length(no_part_level) >1) {
      for (i in 1:length(no_part_level)) {
        p_y_ctl = append(p_y_ctl, NA, no_part_level[i] - 1)
        p_y_trt = append(p_y_trt, NA, no_part_level[i] - 1)
        RD_leq = append(RD_leq, NA, no_part_level[i] - 1)
        RD_geq = append(RD_geq, NA, no_part_level[i] - 1)
        log_RR_le = append(log_RR_le, NA, no_part_level[i] - 1)
        log_RR_ge = append(log_RR_ge, NA, no_part_level[i] - 1)
        OR_le = append(OR_le, NA, no_part_level[i] - 1)
        log_OR_le = append(log_OR_le, NA, no_part_level[i] - 1)
        p_y_trt_est = append(p_y_trt_est, NA, no_part_level[i] - 1)
        p_y_ctl_est = append(p_y_ctl_est, NA, no_part_level[i] - 1)
        weights_split = append(weights_split, NA, no_part_level[i] - 1)
        weights_cum = append(weights_cum, NA, no_part_level[i] - 1)
        weights_both = append(weights_both, NA, no_part_level[i] - 1)
        weights_util = append(weights_util, NA, no_part_level[i] - 1)
        
      }
    }
  }
  
  
  return(c(p_y_ctl = p_y_ctl,
           p_y_trt = p_y_trt,
           RD_leq = RD_leq,
           RD_geq = RD_geq,
           OR = OR_le,
           log_OR = log_OR_le,
           log_RR_leq = log_RR_le,
           log_RR_geq = log_RR_ge,
           # weighted_RD_control = weighted_RD, 
           # weighted_RD_overall = weighted_RD_overall,
           weighted_RD_Bcontrol = weighted_RD_B, 
           weighted_RD_cum = weighted_RD_cum,
           weighted_RD_both = weighted_RD_both,
           weighted_RD_util = weighted_RD_util,
           # weighted_RD_Boverall = weighted_RD_Boverall,
           unweighted_RD = unweighted_RD,
           # weighted_OR_control = weighted_OR,
           # weighted_OR_overall = weighted_OR_overall,
           weighted_OR_Bcontrol = weighted_OR_B,
           weighted_OR_cum = weighted_OR_cum,
           weighted_OR_both = weighted_OR_both,
           weighted_OR_util = weighted_OR_util,
           # weighted_OR_Boverall = weighted_OR_Boverall,
           unweighted_OR = unweighted_OR,
           # weighted_log_OR_overall = weighted_log_OR_overall,
           # weighted_log_OR_control = weighted_log_OR,
           # weighted_log_OR_Boverall = weighted_log_OR_Boverall,
           weighted_log_OR_cum = weighted_log_OR_cum,
           weighted_log_OR_both = weighted_log_OR_both,
           weighted_log_OR_Bcontrol = weighted_log_OR_B,
           weighted_log_OR_util = weighted_log_OR_util,
           unweighted_log_OR = unweighted_log_OR,
           # weighted_log_RR_control = weighted_log_RR_control, 
           # weighted_log_RR_overall = weighted_log_RR_overall,
           weighted_log_RR_Bcontrol = weighted_log_RR_Bcontrol, 
           weighted_log_RR_cum = weighted_log_RR_cum,
           weighted_log_RR_both = weighted_log_RR_both,
           weighted_log_RR_ge_both = weighted_log_RR_ge_both,
           weighted_log_RR_util = weighted_log_RR_util,
           weighted_log_RR_ge_util = weighted_log_RR_ge_util,
           # weighted_log_RR_Boverall = weighted_log_RR_Boverall,
           unweighted_log_RR = unweighted_log_RR,
           unweighted_RR = unweighted_RR,
           unweighted_log_RR_ge = unweighted_log_RR_ge,
           unweighted_RR_ge = unweighted_RR_ge,
           #WR = WR, 
           NB = NB,
           delta_t = delta_t, 
           delta_c = delta_c,
           delta = delta,
           p_y_trt_est = p_y_trt_est,
           p_y_ctl_est = p_y_ctl_est,
           weights_split = weights_split,
           weights_cum = weights_cum,
           weights_both = weights_both,
           weights_util = weights_util))
  
}


# calculate the credible interval 
# assumes a partial proportional odds model 
# assumes the treatment is coded as -0.5 for control and 0.5 for treatment 
# b is the model 
# NOTE: this only works for the iprior = 0 because I didn't negate the alphas
order_weighted_RD2_CI_strat <- function(stanfit, n_post_draws = 4000, dat, trt, 
                                        outcome, utility, outcome_levels) { 
  # first calculate P(y >= i | ctl) and P(y >= i | trt) for all i
  stanmod <- as.data.frame(stanfit)
  alphas_post <- stanmod[,grep('alpha\\[', colnames(stanmod))]
  c <- ncol(alphas_post)/2 +1
  c_true <- outcome_levels
  
  if (c_true != c) {
    #this scenario, (at least) one level had no participants 
    # find which level(s) had no participants 
    no_part_level <- which(!(1:c_true %in% unique(dat$y)))
    utility <- utility[-no_part_level]
  }
  
  PPO_ctl_geq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_trt_geq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_ctl_leq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_trt_leq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  p_y_ctl_est <- matrix(data = NA, nrow = n_post_draws, ncol = c)
  p_y_trt_est <- matrix(data = NA, nrow = n_post_draws, ncol = c)
  
  
  for (i in 2:c) {
    PPO_ctl_geq[,i] <- plogis(alphas_post[,2*i - 3])
    PPO_trt_geq[,i] <- plogis(alphas_post[,2*i - 2])
    PPO_ctl_leq[,(i - 1)] <- 1 - PPO_ctl_geq[,i]
    PPO_trt_leq[,(i - 1)] <- 1 - PPO_trt_geq[,i]
  }
  
  # calculate p(Y = i | control) using the model for i = 1, ... , c
  for (i in 1:c) {
    if (i == 1) {
      p_y_ctl_est[,1] <-  PPO_ctl_leq[,1]
      p_y_trt_est[,1] <-  PPO_trt_leq[,1]
    } else if (i == 2){
      p_y_ctl_est[,2] <- PPO_ctl_leq[,i] - PPO_ctl_leq[,1]
      p_y_trt_est[,2] <- PPO_trt_leq[,i] - PPO_trt_leq[,1]
    } else if (i == c) {
      p_y_ctl_est[,c] <- 1 - rowSums(p_y_ctl_est[,1:(c-1)])
      p_y_trt_est[,c] <- 1 - rowSums(p_y_trt_est[,1:(c-1)])
    } else {
      p_y_ctl_est[,i] <-  PPO_ctl_leq[,i] - apply(p_y_ctl_est[,1:(i-1)], 1, sum)
      p_y_trt_est[,i] <-  PPO_trt_leq[,i] - apply(p_y_trt_est[,1:(i-1)], 1, sum)
    }
  }
  
  # correct for any negative probabilities 
  if (any(p_y_ctl_est < 0)) {
    p_y_ctl_est[which(p_y_ctl_est < 0)] <- 0 
    p_y_ctl_est <- p_y_ctl_est/rowSums(p_y_ctl_est)
  }
  
  if (any(p_y_trt_est < 0)) {
    p_y_trt_est[which(p_y_trt_est < 0)] <- 0 
    p_y_trt_est <- p_y_trt_est/rowSums(p_y_trt_est)
  }
  
  # correct for any probabilities of 1  
  if (any(PPO_ctl_leq == 1, na.rm = T)) {
    PPO_ctl_leq[which(PPO_ctl_leq == 1)] <- 1 - 1e-7 
  }
  
  if (any(PPO_trt_leq == 1, na.rm = T)) {
    PPO_trt_leq[which(PPO_trt_leq == 1)] <- 1 - 1e-7 
  }
  
  
  
  p_y_est <- (p_y_ctl_est + p_y_trt_est)/2
  
  # get weights from the data 
  p_y_ctl <- unname(table(dat[[trt]], dat[[outcome]])[1,])/sum(dat[[trt]] == -0.5)
  p_y_trt <- unname(table(dat[[trt]], dat[[outcome]])[2,])/sum(dat[[trt]] == 0.5)
  p_y <- as.vector(unname(table(dat[[outcome]])))/nrow(dat)
  
  RD_leq <- PPO_trt_leq - PPO_ctl_leq
  RD_leq_95CI <- apply(RD_leq[,], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  RD_geq <- PPO_ctl_geq - PPO_trt_geq
  RD_geq_95CI <- apply(RD_geq[,], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  # put control in the numerator so the OR is greater than one if metformin has a positive effect
  OR_le <- (PPO_trt_leq / (1- PPO_trt_leq)) / (PPO_ctl_leq / (1- PPO_ctl_leq))
  OR_le_95CI <- apply(OR_le[, 1:(c-1)], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  log_OR_le <- log((PPO_trt_leq / (1- PPO_trt_leq)) / (PPO_ctl_leq / (1- PPO_ctl_leq)))
  log_OR_le_95CI <- apply(log_OR_le[, 1:(c-1)], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  
  OR_ge <- (PPO_ctl_geq / (1- PPO_ctl_geq)) / (PPO_trt_geq / (1- PPO_trt_geq))
  OR_ge_95CI <- apply(OR_ge[, 1:(c-1)], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  #for iprior = 1: (PPO_ctl_geq / (1- PPO_ctl_geq))/(PPO_trt_geq / (1 - PPO_trt_geq))
  
  log_RR_le <- log(PPO_trt_leq / PPO_ctl_leq)
  log_RR_le_95CI <- apply(log_RR_le[, 1:(c-1)], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  log_RR_ge <- log(PPO_ctl_geq / PPO_trt_geq)
  log_RR_ge_95CI <- apply(log_RR_ge[, 2:(c)], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  j <- 1:(c-1)
  k <- 2:c
  # numerator <- rowSums((p_y_ctl[j] + p_y_ctl[k]) * (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_RD <- numerator/denominator
  # weighted_RD_95CI <- quantile(weighted_RD, probs = c(0.025, 0.975))
  # weighted_RD_p <- 1 - sum(weighted_RD >= 0)/n_post_draws
  # weighted_RD_sd <- sd(weighted_RD)
  
  # numerator <- rowSums((p_y[j] + p_y[k]) * (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # weighted_RD_overall <- numerator/denominator
  # weighted_RD_95CI_overall <- quantile(weighted_RD_overall, probs = c(0.025, 0.975))
  # weighted_RD_p_overall <- 1 - sum(weighted_RD_overall >= 0)/n_post_draws
  # weighted_RD_sd_overall <- sd(weighted_RD_overall)
  
  # fully bayesian weighted RD 
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) *
                         abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) * 
                         (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  denominator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_RD_B <- numerator/denominator
  weighted_RD_B95CI <- quantile(weighted_RD_B, probs = c(0.025, 0.975))
  weighted_RD_Bp <- 1 - sum(weighted_RD_B >= 0)/n_post_draws
  weighted_RD_Bsd <- sd(weighted_RD_B)
  
  numerator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                         abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                         (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  denominator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                           abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_RD_cum <- numerator/denominator
  weighted_RD_cum95CI <- quantile(weighted_RD_cum, probs = c(0.025, 0.975))
  weighted_RD_cump <- 1 - sum(weighted_RD_cum >= 0)/n_post_draws
  weighted_RD_cumsd <- sd(weighted_RD_cum)
  
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
                         abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                         (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  denominator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
                           abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                           (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])))
  weighted_RD_both <- numerator/denominator
  weighted_RD_both95CI <- quantile(weighted_RD_both, probs = c(0.025, 0.975))
  weighted_RD_bothp <- 1 - sum(weighted_RD_both >= 0)/n_post_draws
  weighted_RD_bothsd <- sd(weighted_RD_both)
  
  numerator <- rowSums(abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                         (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  denominator <- rowSums(abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_RD_util <- numerator/denominator
  weighted_RD_util95CI <- quantile(weighted_RD_util, probs = c(0.025, 0.975))
  
  # numerator <- rowSums((p_y_est[j] + p_y_est[k]) * (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  # denominator <- sum(p_y_est[j]) + sum(p_y_est[k])
  # weighted_RD_Boverall <- numerator/denominator
  # weighted_RD_B95CI_overall <- quantile(weighted_RD_overall, probs = c(0.025, 0.975))
  # weighted_RD_Bp_overall <- 1 - sum(weighted_RD_overall >= 0)/n_post_draws
  # weighted_RD_Bsd_overall <- sd(weighted_RD_overall)
  
  #unweighted RD
  unweighted_RD <- rowMeans(RD_leq[,1:(c-1)])
  unweighted_RD_95CI <- quantile(unweighted_RD, probs = c(0.025, 0.975))
  unweighted_RD_p <- 1 - sum(unweighted_RD >= 0)/n_post_draws
  unweighted_RD_sd <- sd(unweighted_RD)
  
  
  #calculate weighted OR 
  # numerator <- rowSums((p_y_ctl[j] + p_y_ctl[k]) * log(OR_le[,j])) 
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_OR <- exp(numerator/denominator)
  # weighted_OR_95CI <- quantile(weighted_OR, probs = c(0.025, 0.975))
  # weighted_OR_p <- 1 - sum(weighted_OR >= 1)/n_post_draws
  # weighted_OR_sd <- sd(weighted_OR)
  
  # weighted_log_OR <- numerator/denominator
  # weighted_log_OR_95CI <- quantile(weighted_log_OR, probs = c(0.025, 0.975))
  # weighted_log_OR_p <- 1 - sum(weighted_log_OR >= 1)/n_post_draws
  # weighted_log_OR_sd <- sd(weighted_log_OR)
  
  # #calculate weighted OR with overall probability as weights 
  # numerator <- rowSums((p_y[j] + p_y[k]) * log(OR_le[,j])) 
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # weighted_OR_overall <- exp(numerator/denominator)
  # weighted_OR_95CI_overall <- quantile(weighted_OR_overall, probs = c(0.025, 0.975))
  # weighted_OR_p_overall <- 1 - sum(weighted_OR_overall >= 1)/n_post_draws
  # weighted_OR_sd_overall <- sd(weighted_OR_overall)
  # 
  # weighted_log_OR_overall <- numerator/denominator
  # weighted_log_OR_95CI_overall <- quantile(weighted_log_OR_overall, probs = c(0.025, 0.975))
  # weighted_log_OR_p_overall <- 1 - sum(weighted_log_OR_overall >= 1)/n_post_draws
  # weighted_log_OR_sd_overall <- sd(weighted_log_OR_overall)
  
  # fully bayesian estimates
  #calculate weighted OR 
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
                         abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                         log(OR_le[,j])) 
  denominator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
                           abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_OR_B <- exp(numerator/denominator)
  weighted_OR_B95CI <- quantile(weighted_OR_B, probs = c(0.025, 0.975))
  weighted_OR_Bp <- 1 - sum(weighted_OR_B >= 1)/n_post_draws
  weighted_OR_Bsd <- sd(weighted_OR_B)
  
  weighted_log_OR_B <- numerator/denominator
  weighted_log_OR_B95CI <- quantile(weighted_log_OR_B, probs = c(0.025, 0.975))
  weighted_log_OR_Bp <- 1 - sum(weighted_log_OR_B >= 1)/n_post_draws
  weighted_log_OR_Bsd <- sd(weighted_log_OR_B)
  
  # cumulative weights only
  numerator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                         abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                         log(OR_le[,j]))
  denominator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                           abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_OR_cum <- exp(numerator/denominator)
  weighted_OR_cum95CI <- quantile(weighted_OR_cum, probs = c(0.025, 0.975))
  weighted_OR_cump <- 1 - sum(weighted_OR_cum >= 0)/n_post_draws
  weighted_OR_cumsd <- sd(weighted_OR_cum)
  
  weighted_log_OR_cum <- numerator/denominator
  weighted_log_OR_cum95CI <- quantile(weighted_log_OR_cum, probs = c(0.025, 0.975))
  weighted_log_OR_cump <- 1 - sum(weighted_log_OR_cum >= 0)/n_post_draws
  weighted_log_OR_cumsd <- sd(weighted_log_OR_cum)
  
  # both weights
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                         abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) * log(OR_le[,j]))
  denominator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                           abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_OR_both <- exp(numerator/denominator)
  weighted_OR_both95CI <- quantile(weighted_OR_both, probs = c(0.025, 0.975))
  weighted_OR_bothp <- 1 - sum(weighted_OR_both >= 0)/n_post_draws
  weighted_OR_bothsd <- sd(weighted_OR_both)
  
  weighted_log_OR_both <- numerator/denominator
  weighted_log_OR_both95CI <- quantile(weighted_log_OR_both, probs = c(0.025, 0.975))
  weighted_log_OR_bothp <- 1 - sum(weighted_log_OR_both >= 0)/n_post_draws
  weighted_log_OR_bothsd <- sd(weighted_log_OR_both)
  
  # utility only weights
  numerator <- rowSums(abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                         log(OR_le[,j])) 
  denominator <- rowSums(abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_OR_util <- exp(numerator/denominator)
  weighted_OR_util95CI <- quantile(weighted_OR_util, probs = c(0.025, 0.975))
  weighted_log_OR_util <- numerator/denominator
  weighted_log_OR_util95CI <- quantile(weighted_log_OR_util, probs = c(0.025, 0.975))
  
  #calculate weighted OR with overall probability as weights 
  # numerator <- rowSums((p_y_est[j] + p_y_est[k]) * log(OR_le[,j])) 
  # denominator <- sum(p_y_est[j]) + sum(p_y_est[k])
  # weighted_OR_Boverall <- exp(numerator/denominator)
  # weighted_OR_B95CI_overall <- quantile(weighted_OR_overall, probs = c(0.025, 0.975))
  # weighted_OR_Bp_overall <- 1 - sum(weighted_OR_overall >= 1)/n_post_draws
  # weighted_OR_Bsd_overall <- sd(weighted_OR_overall)
  # 
  # weighted_log_OR_Boverall <- numerator/denominator
  # weighted_log_OR_B95CI_overall <- quantile(weighted_log_OR_overall, probs = c(0.025, 0.975))
  # weighted_log_OR_Bp_overall <- 1 - sum(weighted_log_OR_overall >= 1)/n_post_draws
  # weighted_log_OR_Bsd_overall <- sd(weighted_log_OR_overall)
  
  #unweighted OR 
  unweighted_log_OR <- rowMeans(log_OR_le[,1:(c-1)])
  unweighted_log_OR_95CI <- quantile(unweighted_log_OR, probs = c(0.025, 0.975))
  unweighted_log_OR_p <- 1 - sum(unweighted_log_OR >= 0)/n_post_draws
  unweighted_log_OR_sd <- sd(unweighted_log_OR)
  
  unweighted_OR_95CI <- quantile(exp(unweighted_log_OR), probs = c(0.025, 0.975))
  unweighted_OR_p <- 1 - sum(exp(unweighted_log_OR) >= 0)/n_post_draws
  unweighted_OR_sd <- sd(exp(unweighted_log_OR))
  
  # # calculate weighted RR with control probabilities 
  # numerator <- rowSums((p_y_ctl[j] + p_y_ctl[k]) * log_RR_le[,j])
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_log_RR_control <- numerator/denominator
  # weighted_log_RR_95CI_control <- quantile(weighted_log_RR_control, probs = c(0.025, 0.975))
  # weighted_log_RR_p_control <- 1 - sum(weighted_log_RR_control >= 0)/n_post_draws
  # weighted_log_RR_sd_control <- sd(weighted_log_RR_control)
  
  # # calculate weighted RR with overall probabilities 
  # numerator <- rowSums((p_y[j] + p_y[k]) * log_RR_le[,j])
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # weighted_log_RR_overall <- numerator/denominator
  # weighted_log_RR_95CI_overall <- quantile(weighted_log_RR_overall, probs = c(0.025, 0.975))
  # weighted_log_RR_p_overall <- 1 - sum(weighted_log_RR_overall >= 0)/n_post_draws
  # weighted_log_RR_sd_overall <- sd(weighted_log_RR_overall)
  
  # fully bayesian weighted estimate 
  # calculate weighted RR with control probabilities 
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
                         abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                         log_RR_le[,j])
  denominator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
                           abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_log_RR_Bcontrol <- numerator/denominator
  weighted_log_RR_B95CI_control <- quantile(weighted_log_RR_Bcontrol, probs = c(0.025, 0.975))
  weighted_log_RR_Bp_control <- 1 - sum(weighted_log_RR_Bcontrol >= 0)/n_post_draws
  weighted_log_RR_Bsd_control <- sd(weighted_log_RR_Bcontrol)
  
  # cumulative weights only
  numerator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                         abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                         log_RR_le[,j])
  denominator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                           abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_log_RR_cum <- numerator/denominator
  weighted_log_RR_cum95CI <- quantile(weighted_log_RR_cum, probs = c(0.025, 0.975))
  weighted_log_RR_cump <- 1 - sum(weighted_log_RR_cum >= 0)/n_post_draws
  weighted_log_RR_cumsd <- sd(weighted_log_RR_cum)
  
  # both weights
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                         abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) * log_RR_le[,j])
  denominator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                           abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_log_RR_both <- numerator/denominator
  weighted_log_RR_both95CI <- quantile(weighted_log_RR_both, probs = c(0.025, 0.975))
  weighted_log_RR_bothp <- 1 - sum(weighted_log_RR_both >= 0)/n_post_draws
  weighted_log_RR_bothsd <- sd(weighted_log_RR_both)
  
  # calculate weighted RR with utilities
  numerator <- rowSums(abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                         log_RR_le[,j])
  denominator <- rowSums(abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_log_RR_util <- numerator/denominator
  weighted_log_RR_util95CI <- quantile(weighted_log_RR_util, probs = c(0.025, 0.975))
  
  # both weights RR greater than or equal to 
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                         abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                         log_RR_ge[,k])
  denominator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                           abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_log_RR_ge_both <- numerator/denominator
  weighted_log_RR_ge_both95CI <- quantile(weighted_log_RR_ge_both, probs = c(0.025, 0.975))
  weighted_log_RR_ge_bothp <- 1 - sum(weighted_log_RR_ge_both >= 0)/n_post_draws
  weighted_log_RR_ge_bothsd <- sd(weighted_log_RR_ge_both)
  
  # utility weights only 
  numerator <- rowSums(abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])) *
                         log_RR_ge[,k])
  denominator <- rowSums(abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weighted_log_RR_ge_util <- numerator/denominator
  weighted_log_RR_ge_util95CI <- quantile(weighted_log_RR_ge_util, probs = c(0.025, 0.975))
  
  # # calculate weighted RR with overall probabilities 
  # numerator <- rowSums((p_y_est[j] + p_y_est[k]) * log_RR_le[,j])
  # denominator <- sum(p_y_est[j]) + sum(p_y_est[k])
  # weighted_log_RR_Boverall <- numerator/denominator
  # weighted_log_RR_B95CI_overall <- quantile(weighted_log_RR_overall, probs = c(0.025, 0.975))
  # weighted_log_RR_Bp_overall <- 1 - sum(weighted_log_RR_overall >= 0)/n_post_draws
  # weighted_log_RR_Bsd_overall <- sd(weighted_log_RR_overall)
  
  #unweighted RR
  unweighted_log_RR <- rowMeans(log_RR_le[, 1:(c-1)])
  unweighted_log_RR_95CI <- quantile(unweighted_log_RR, probs = c(0.025, 0.975))
  unweighted_log_RR_p <- 1 - sum(unweighted_log_RR >= 0)/n_post_draws
  unweighted_log_RR_sd <- sd(unweighted_log_RR)
  
  unweighted_RR_95CI <- quantile(exp(unweighted_log_RR), probs = c(0.025, 0.975))
  unweighted_RR_p <- 1 - sum(exp(unweighted_log_RR) >= 0)/n_post_draws
  unweighted_RR_sd <- sd(exp(unweighted_log_RR))
  
  #unweighted RR greater than or equal to
  unweighted_log_RR_ge <- rowMeans(log_RR_ge[, 2:(c)])
  unweighted_log_RR_ge_95CI <- quantile(unweighted_log_RR_ge, probs = c(0.025, 0.975))
  unweighted_log_RR_ge_p <- 1 - sum(unweighted_log_RR_ge >= 0)/n_post_draws
  unweighted_log_RR_ge_sd <- sd(unweighted_log_RR_ge)
  
  unweighted_RR_ge_95CI <- quantile(exp(unweighted_log_RR_ge), probs = c(0.025, 0.975))
  unweighted_RR_ge_p <- 1 - sum(exp(unweighted_log_RR_ge) >= 0)/n_post_draws
  unweighted_RR_ge_sd <- sd(exp(unweighted_log_RR_ge))
  
  # calculate NB 
  NB <- rowSums(p_y_trt_est[,j] * PPO_ctl_geq[,(j+1)]) - (rowSums(p_y_ctl_est[,j] * PPO_trt_geq[,(j+1)]))
  NB_95CI <- quantile(NB, probs = c(0.025, 0.975))
  NB_p <- 1 - sum(NB >= 0)/n_post_draws
  NB_sd <- sd(NB)
  
  # mean utility
  l = 1:c
  delta_t <- rowSums(t(replicate(n_post_draws, utility))[,l] * p_y_trt_est[,l])
  delta_t_95CI <- quantile(delta_t, probs = c(0.025, 0.975))
  delta_c <- rowSums(t(replicate(n_post_draws, utility))[,l] * p_y_ctl_est[,l])
  delta_c_95CI <- quantile(delta_c, probs = c(0.025, 0.975))
  delta <- delta_t - delta_c
  delta_95CI <- quantile(delta, probs = c(0.025, 0.975))
  
  #weights 
  weights_split <- ((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
                      abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k]))) / 
    rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
              abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weights_split <- apply(weights_split, 2, quantile, probs = c(0.025, 0.975))
  
  weights_cum <- (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j]) * 
                    abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k]))) / 
    rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
              abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weights_cum <- apply(weights_cum, 2, quantile, probs = c(0.025, 0.975))
  
  weights_both <- ((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * 
                     abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k]))) /
    rowSums(((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j]))) * 
              abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weights_both <- apply(weights_both, 2, quantile, probs = c(0.025, 0.975))
  
  weights_util <- (abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k]))) / 
    rowSums(abs((t(replicate(n_post_draws, utility))[,j] - t(replicate(n_post_draws, utility))[,k])))
  weights_util <- apply(weights_util, 2, quantile, probs = c(0.025, 0.975))
  
  if (c_true != c) {
    if (length(no_part_level) == 1) { 
      RD_leq_95CI = append(RD_leq_95CI, c(NA, NA), 2*(no_part_level - 1))
      RD_geq_95CI = append(RD_geq_95CI, c(NA, NA), 2*(no_part_level - 1))
      log_RR_le_95CI = append(log_RR_le_95CI, c(NA, NA), 2*(no_part_level - 1))
      log_RR_ge_95CI = append(log_RR_ge_95CI, c(NA, NA), 2*(no_part_level - 1))
      OR_le_95CI = append(OR_le_95CI, c(NA, NA), 2*(no_part_level - 1))
      log_OR_le_95CI = append(log_OR_le_95CI, c(NA, NA), 2*(no_part_level - 1))
      weights_split = append(weights_split, c(NA, NA), 2*(no_part_level - 1))
      weights_cum = append(weights_cum, c(NA, NA), 2*(no_part_level - 1))
      weights_both = append(weights_both, c(NA, NA), 2*(no_part_level - 1))
      weights_util = append(weights_util, c(NA, NA), 2*(no_part_level - 1))
    } else if (length(no_part_level) >1) {
      for (i in 1:length(no_part_level)) {
        RD_leq_95CI = append(RD_leq_95CI, c(NA, NA), 2*(no_part_level[i] - 1))
        RD_geq_95CI = append(RD_geq_95CI, c(NA, NA), 2*(no_part_level[i] - 1))
        log_RR_le_95CI = append(log_RR_le_95CI, c(NA, NA), 2*(no_part_level[i] - 1))
        log_RR_ge_95CI = append(log_RR_ge_95CI, c(NA, NA), 2*(no_part_level[i] - 1))
        OR_le_95CI = append(OR_le_95CI, c(NA, NA), 2*(no_part_level[i] - 1))
        log_OR_le_95CI = append(log_OR_le_95CI, c(NA, NA), 2*(no_part_level[i] - 1))
        weights_split = append(weights_split, c(NA, NA), 2*(no_part_level[i] - 1))
        weights_cum = append(weights_cum, c(NA, NA), 2*(no_part_level[i] - 1))
        weights_both = append(weights_both, c(NA, NA), 2*(no_part_level[i] - 1))
        weights_util = append(weights_util, c(NA, NA), 2*(no_part_level[i] - 1))
      }
    }
  }
  
  return(c(RD_leq_CI = RD_leq_95CI,
           RD_geq_CI = RD_geq_95CI,
           OR_CI = OR_le_95CI,
           log_OR_CI = log_OR_le_95CI,
           log_RR_leq_CI = log_RR_le_95CI,
           log_RR_geq_CI = log_RR_ge_95CI,
           # weighted_RD_control_CI = weighted_RD_95CI,
           # weighted_RD_p_control = weighted_RD_p,
           # weighted_RD_sd = weighted_RD_sd, 
           # weighted_RD_sd_overall = weighted_RD_sd_overall,
           # weighted_RD_overall_CI = weighted_RD_95CI_overall,
           # weighted_RD_p_overall = weighted_RD_p_overall,
           weighted_RD_Bcontrol_CI = weighted_RD_B95CI,
           weighted_RD_Bp_control = weighted_RD_Bp,
           weighted_RD_Bsd = weighted_RD_Bsd, 
           weighted_RD_cum_CI = weighted_RD_cum95CI,
           weighted_RD_cum_p = weighted_RD_cump,
           weighted_RD_cum_sd = weighted_RD_cumsd,
           weighted_RD_both_CI = weighted_RD_both95CI,
           weighted_RD_both_p = weighted_RD_bothp,
           weighted_RD_both_sd = weighted_RD_bothsd, 
           weighted_RD_util_CI = weighted_RD_util95CI,
           # weighted_RD_Bsd_overall = weighted_RD_Bsd_overall,
           # weighted_RD_Boverall_CI = weighted_RD_B95CI_overall,
           # weighted_RD_Bp_overall = weighted_RD_Bp_overall,
           # weighted_OR_CI = weighted_OR_95CI, 
           # weighted_OR_p = weighted_OR_p,
           # weighted_OR_sd = weighted_OR_sd,
           # weighted_OR_overall_CI = weighted_OR_95CI_overall,
           # weighted_OR_p_overall = weighted_OR_p_overall,
           # weighted_OR_sd_overall = weighted_OR_sd_overall,
           # weighted_log_OR_control_CI = weighted_log_OR_95CI,
           # weighted_log_OR_p_control = weighted_log_OR_p,
           # weighted_log_OR_sd_control = weighted_log_OR_sd,
           # weighted_log_OR_overall_CI = weighted_log_OR_95CI_overall,
           # weighted_log_OR_p_overall = weighted_log_OR_p_overall,
           # weighted_log_OR_sd_overall = weighted_log_OR_sd_overall,
           weighted_log_OR_Bcontrol_CI = weighted_log_OR_B95CI,
           weighted_log_OR_Bp_control = weighted_log_OR_Bp,
           weighted_log_OR_Bsd_control = weighted_log_OR_Bsd,
           weighted_log_OR_cum_CI = weighted_log_OR_cum95CI,
           weighted_log_OR_cum_p = weighted_log_OR_cump,
           weighted_log_OR_cum_sd = weighted_log_OR_cumsd,
           weighted_log_OR_both_CI = weighted_log_OR_both95CI,
           weighted_log_OR_both_p = weighted_log_OR_bothp,
           weighted_log_OR_both_sd = weighted_log_OR_bothsd,
           weighted_log_OR_util_CI = weighted_log_OR_util95CI,
           # weighted_log_OR_Boverall_CI = weighted_log_OR_B95CI_overall,
           # weighted_log_OR_Bp_overall = weighted_log_OR_Bp_overall,
           # weighted_log_OR_Bsd_overall = weighted_log_OR_Bsd_overall,
           # weighted_log_RR_95CI_control = weighted_log_RR_95CI_control,
           # weighted_log_RR_p_control = weighted_log_RR_p_control, 
           # weighted_log_RR_sd_control = weighted_log_RR_sd_control,
           # weighted_log_RR_95CI_overall = weighted_log_RR_95CI_overall,
           # weighted_log_RR_p_overall = weighted_log_RR_p_overall, 
           # weighted_log_RR_sd_overall = weighted_log_RR_sd_overall,
           weighted_log_RR_B95CI_control = weighted_log_RR_B95CI_control,
           weighted_log_RR_Bp_control = weighted_log_RR_Bp_control, 
           weighted_log_RR_Bsd_control = weighted_log_RR_Bsd_control,
           weighted_log_RR_cum_CI = weighted_log_RR_cum95CI,
           weighted_log_RR_cum_p = weighted_log_RR_cump,
           weighted_log_RR_cum_sd = weighted_log_RR_cumsd,
           weighted_log_RR_both_CI = weighted_log_RR_both95CI,
           weighted_log_RR_both_p = weighted_log_RR_bothp,
           weighted_log_RR_both_sd = weighted_log_RR_bothsd,
           weighted_log_RR_util_CI = weighted_log_RR_util95CI,
           weighted_log_RR_ge_both_CI = weighted_log_RR_ge_both95CI,
           weighted_log_RR_ge_both_p = weighted_log_RR_ge_bothp,
           weighted_log_RR_ge_both_sd = weighted_log_RR_ge_bothsd,
           weighted_log_RR_ge_util_CI = weighted_log_RR_ge_util95CI,
           # weighted_log_RR_B95CI_overall = weighted_log_RR_B95CI_overall,
           # weighted_log_RR_Bp_overall = weighted_log_RR_Bp_overall, 
           # weighted_log_RR_Bsd_overall = weighted_log_RR_Bsd_overall,
           
           unweighted_RD_CI = unweighted_RD_95CI,
           unweighted_RD_p = unweighted_RD_p,
           unweighted_RD_sd = unweighted_RD_sd, 
           unweighted_OR_CI = unweighted_OR_95CI,
           unweighted_OR_p = unweighted_OR_p,
           unweighted_OR_sd = unweighted_OR_sd,  
           unweighted_log_OR_CI = unweighted_log_OR_95CI,
           unweighted_log_OR_p = unweighted_log_OR_p,
           unweighted_log_OR_sd = unweighted_log_OR_sd,
           unweighted_RR_CI = unweighted_RR_95CI,
           unweighted_RR_p = unweighted_RR_p,
           unweighted_RR_sd = unweighted_RR_sd,  
           unweighted_log_RR_CI = unweighted_log_RR_95CI,
           unweighted_log_RR_p = unweighted_log_RR_p,
           unweighted_log_RR_sd = unweighted_log_RR_sd,
           unweighted_RR_ge_CI = unweighted_RR_ge_95CI,
           unweighted_RR_ge_p = unweighted_RR_ge_p,
           unweighted_RR_ge_sd = unweighted_RR_ge_sd,  
           unweighted_log_RR_ge_CI = unweighted_log_RR_ge_95CI,
           unweighted_log_RR_ge_p = unweighted_log_RR_ge_p,
           unweighted_log_RR_ge_sd = unweighted_log_RR_ge_sd,
           
           #WR = WR_95CI,
           NB_CI = NB_95CI,
           NB_p = NB_p,
           NB_sd = NB_sd,
           delta_t_CI = delta_t_95CI,
           delta_c_CI = delta_c_95CI,
           delta_CI = delta_95CI,
           # 
           weights_split_CI = weights_split,
           weights_cum_CI = weights_cum, 
           weights_both_CI = weights_both,
           weights_util_CI = weights_util))
  
}



# covs is going to be a list of covariates included in the model in the same order at the model 
strat_adjust <- function(stanfit, dat, structured_dat, 
                         outcome, trt, covs, utility) { 
  modfit <- as.data.frame(stanfit)
  
  alphas <- modfit[,grep('alpha\\[', colnames(modfit))]
  alpha <- colMeans(alphas)
  
  betas <- modfit[,grep('beta\\[', colnames(modfit))]
  beta <- colMeans(betas)
  beta <- matrix(beta, nrow=1) %*% t(structured_dat$wqrX$Rinv[2:3, 2:3])
  
  c <- length(alpha)/2 +1
  
  predicted_trt_geq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  predicted_ctl_geq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  predicted_trt_leq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  predicted_ctl_leq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  p_y_ctl <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
  p_y_trt <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
  p_y_ctl_est <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
  p_y_trt_est <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
  
  
  
  num_covs <- length(covs)
  covariate_mat <- as.data.frame(matrix(data = NA, nrow = nrow(dat), ncol = num_covs - 1))
  
  # assume the first covariate is the treatment arm 
  for(i in 1:num_covs) {
    if (is.numeric(dat[[covs[i]]]) == T) {
      covariate_mat[,i] <- beta[i] * dat[[covs[i]]]
    } else {
      # only works for numeric and binary with "Yes" as the value
      covariate_mat[,i] <- beta[i] * (dat[[covs[i]]] == "Yes")
    }
  }
  
  for (i in 2:c) {
    predicted_trt_geq[,i-1] <- plogis(alpha[2*i - 2] + rowSums(covariate_mat))
    predicted_ctl_geq[,i-1] <- plogis(alpha[2*i - 3] + rowSums(covariate_mat))
  }
  
  
  predicted_trt_leq <-  1 - predicted_trt_geq
  predicted_ctl_leq <-  1 - predicted_ctl_geq
  
  # calculate p(Y = i | control) for i = 1, ... , c
  for (i in 1:c) {
    if (i == 1) {
      p_y_ctl_est[,1] <-  predicted_ctl_leq[,1]
      p_y_trt_est[,1] <-  predicted_trt_leq[,1]
    } else if (i == 2) {
      p_y_ctl_est[,i] <-  predicted_ctl_leq[,i] - p_y_ctl_est[,1]
      p_y_trt_est[,i] <-  predicted_trt_leq[,i] - p_y_trt_est[,1]
    } else if (i == c) {
      p_y_ctl_est[,c] <- 1 - rowSums(p_y_ctl_est[,1:(c-1)])
      p_y_trt_est[,c] <- 1 - rowSums(p_y_trt_est[,1:(c-1)])
    } else {
      p_y_ctl_est[,i] <-  predicted_ctl_leq[,i] - rowSums(p_y_ctl_est[,1:(i-1)])
      p_y_trt_est[,i] <-  predicted_trt_leq[,i] - rowSums(p_y_trt_est[,1:(i-1)])
    }
  }
  
  # correct for any negative probabilities 
  if (any(p_y_ctl_est < 0)) {
    p_y_ctl_est[which(p_y_ctl_est < 0)] <- 0 
    p_y_ctl_est <- p_y_ctl_est/rowSums(p_y_ctl_est)
  }
  
  if (any(p_y_trt_est < 0)) {
    p_y_trt_est[which(p_y_trt_est < 0)] <- 0 
    p_y_trt_est <- p_y_trt_est/rowSums(p_y_trt_est)
  }
  
  
  p_y_est <- (p_y_ctl_est + p_y_trt_est)/2
  
  # get weights from the data 
  p_y_ctl <- unname(table(dat[[trt]], dat[[outcome]])[1,])/sum(dat[[trt]] == -0.5)
  p_y_trt <- unname(table(dat[[trt]], dat[[outcome]])[2,])/sum(dat[[trt]] == 0.5)
  p_y <- unname(table(dat[[outcome]]))/nrow(dat)
  
  # use Bayesian bootstrap to marginalize the probabilities
  weights <- t(rdirichlet(1, rep(1, nrow(dat))))
  p_y_ctl_est_bb <- apply(p_y_ctl_est, 2, weighted.mean, w = weights)
  p_y_trt_est_bb <- apply(p_y_trt_est, 2, weighted.mean, w = weights)
  p_y_est_bb <- (p_y_ctl_est_bb + p_y_trt_est_bb) /2
  
  p_y_ctl_leq_average <- apply(predicted_ctl_leq, 2, weighted.mean, w = weights)
  p_y_trt_leq_average <- apply(predicted_trt_leq, 2, weighted.mean, w = weights)
  p_y_ctl_geq_average <- apply(predicted_ctl_geq, 2, weighted.mean, w = weights)
  p_y_trt_geq_average <- apply(predicted_trt_geq, 2, weighted.mean, w = weights)
  
  #p_y_est_average <- apply(p_y_est, 2, weighted.mean, w = weights)
  #p_y_ctl_est_average <- apply(p_y_ctl_est, 2, weighted.mean, w = weights)
  
  
  RD_leq <- p_y_trt_leq_average - p_y_ctl_leq_average
  RD_geq <- p_y_ctl_geq_average - p_y_trt_geq_average
  
  
  OR <- (p_y_trt_leq_average / (1- p_y_trt_leq_average)) / (p_y_ctl_leq_average / (1- p_y_ctl_leq_average))
  
  RR_leq <- p_y_trt_leq_average / p_y_ctl_leq_average
  RR_geq <- p_y_ctl_geq_average / p_y_trt_geq_average
  
  j <- 1:(c-1)
  k <- 2:c
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_RD <- numerator/denominator
  # 
  # #calculated weighted RD with overall probability as weights
  # numerator <- sum((p_y[j] + p_y[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # weighted_RD_overall <- numerator/denominator
  
  # fully bayesian approach 
  # control weighted
  # numerator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum(p_y_ctl_est_bb[j]) + sum(p_y_ctl_est_bb[k])
  # weighted_RD_B <- numerator/denominator
  
  # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  # weighted_RD_cum <- numerator/denominator
  
  numerator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  denominator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weighted_RD_both <- numerator/denominator
  
  numerator <- sum((abs(utility[j] - utility[k])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  denominator <- sum((abs(utility[j] - utility[k])))
  weighted_RD_util <- numerator/denominator
  
  # # calculated weighted RD with overall probability as weights
  # numerator <- sum((p_y_est_bb[j] + p_y_est_bb[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum(p_y_est_bb[j]) + sum(p_y_est_bb[k])
  # weighted_RD_Boverall <- numerator/denominator
  
  # unweighted RD 
  unweighted_RD <- mean(p_y_trt_leq_average - p_y_ctl_leq_average)
  
  # #calculate weighted OR with control probability as weights
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log(OR[j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_OR <- exp(numerator/denominator)
  # log_weighted_OR <- numerator/denominator
  # 
  # #calculate weighted OR with overall probability as weights
  # numerator <- sum((p_y[j] + p_y[k]) * log(OR[j]))
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # log_weighted_OR_overall <- numerator/denominator
  # weighted_OR_overall <- exp(numerator/denominator)
  
  # bayesian approach
  #calculate weighted OR with control probability as weights
  # numerator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * log(OR[j]))
  # denominator <- sum(p_y_ctl_est_bb[j]) + sum(p_y_ctl_est_bb[k])
  # weighted_OR_B <- exp(numerator/denominator)
  # log_weighted_OR_B <- numerator/denominator
  
  # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(OR[j]))
  # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  # weighted_OR_cum <- exp(numerator/denominator)
  # log_weighted_OR_cum <- numerator/denominator
  
  numerator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(OR[j]))
  denominator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weighted_OR_both <- exp(numerator/denominator)
  log_weighted_OR_both <- numerator/denominator
  
  numerator <- sum((abs(utility[j] - utility[k]))  * log(OR[j]))
  denominator <- sum((abs(utility[j] - utility[k])))
  weighted_OR_util <- exp(numerator/denominator)
  log_weighted_OR_util <- numerator/denominator
  
  # #calculate weighted OR with overall probability as weights
  # numerator <- sum((p_y_est_bb[j] + p_y_est_bb[k]) * log(OR[j]))
  # denominator <- sum(p_y_est_bb[j]) + sum(p_y_est_bb[k])
  # log_weighted_OR_Boverall <- numerator/denominator
  # weighted_OR_Boverall <- exp(numerator/denominator)
  
  # unweighted OR 
  unweighted_log_OR <- mean(log(OR))
  unweighted_OR <- exp(mean(log(OR)))
  
  # #calculate weighted RR with control probability as weights
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log(RR_leq[j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_RR <- exp(numerator/denominator)
  # log_weighted_RR <- numerator/denominator
  # 
  # #calculate weighted RR with overall probability as weights
  # numerator <- sum((p_y[j] + p_y[k]) * log(RR_leq[j]))
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # log_weighted_RR_overall <- numerator/denominator
  # weighted_RR_overall <- exp(numerator/denominator)
  
  # bayesian approach
  #calculate weighted RR with control probability as weights
  # numerator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * log(RR_leq[j]))
  # denominator <- sum(p_y_ctl_est_bb[j]) + sum(p_y_ctl_est_bb[k])
  # weighted_RR_B <- exp(numerator/denominator)
  # log_weighted_RR_B <- numerator/denominator
  
  # cumulative weights 
  # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_leq[j]))
  # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  # weighted_RR_cum <- exp(numerator/denominator)
  # log_weighted_RR_cum <- numerator/denominator
  
  # both weights
  numerator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_leq[j]))
  denominator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weighted_RR_both <- exp(numerator/denominator)
  log_weighted_RR_both <- numerator/denominator
  
  # both weights RR greater than or equal to
  numerator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_geq[j]))
  denominator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weighted_RR_ge_both <- exp(numerator/denominator)
  log_weighted_RR_ge_both <- numerator/denominator
  
  
  # both weights
  numerator <- sum((abs(utility[j] - utility[k]))* log(RR_leq[j]))
  denominator <- sum((abs(utility[j] - utility[k])))
  weighted_RR_util <- exp(numerator/denominator)
  log_weighted_RR_util <- numerator/denominator
  
  # both weights RR greater than or equal to
  numerator <- sum((abs(utility[j] - utility[k])) * log(RR_geq[j]))
  denominator <- sum((abs(utility[j] - utility[k])))
  weighted_RR_ge_util <- exp(numerator/denominator)
  log_weighted_RR_ge_util <- numerator/denominator
  
  # #calculate weighted RR with overall probability as weights
  # numerator <- sum((p_y_est_bb[j] + p_y_est_bb[k]) * log(RR_leq[j]))
  # denominator <- sum(p_y_est_bb[j]) + sum(p_y_est_bb[k])
  # log_weighted_RR_Boverall <- numerator/denominator
  # weighted_RR_Boverall <- exp(numerator/denominator)
  
  # unweighted RR 
  unweighted_log_RR <- mean(log(RR_leq))
  unweighted_RR <- exp(mean(log(RR_leq)))
  
  # unweighted RR greater than or equal to  
  unweighted_log_RR_ge <- mean(log(RR_geq))
  unweighted_RR_ge <- exp(mean(log(RR_geq)))
  
  #calculate Net Benefit    
  l <- 1:c
  NB <- sum(p_y_trt[j] * p_y_ctl_geq_average[j]) - (sum(p_y_ctl[j] * p_y_trt_geq_average[j]))
  
  #save the weights 
  weights_split <- (p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) / sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]))
  weights_cum <- (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) / sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weights_both <- ((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j]))) /
    sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  
  if (c == 4) {
    p_y_ctl = c(p_y_ctl, NA)
    p_y_trt = c(p_y_trt, NA)
    RD_leq = c(RD_leq, NA)
    RD_geq = c(RD_geq, NA)
    RR_leq = c(RR_leq, NA)
    RR_geq = c(RR_geq, NA)
    OR = c(OR, NA)
    weights_split = c(weights_split, NA)
    weights_cum = c(weights_cum, NA)
    weights_both = c(weights_both, NA)
  }
  return(c(p_y_ctl = p_y_ctl, 
              p_y_trt = p_y_trt,
              RD_leq = RD_leq, 
              RD_geq = RD_geq, 
              RR_leq = RR_leq,
              RR_geq = RR_geq,
              OR = OR,
              log_RR_leq = log(RR_leq),
              log_RR_geq = log(RR_geq),
              log_OR = log(OR), 
              # weighted_RD = weighted_RD, 
              # weighted_RD_overall = weighted_RD_overall,
              # weighted_RD_B = weighted_RD_B, 
              # weighted_RD_cum = weighted_RD_cum, 
              weighted_RD_both = weighted_RD_both, 
              weighted_RD_util = weighted_RD_util, 
              # weighted_RD_Boverall = weighted_RD_Boverall,
              unweighted_RD = unweighted_RD,
              # weighted_OR = weighted_OR,
              # log_weighted_OR = log_weighted_OR,
              # weighted_OR_overall = weighted_OR_overall,
              # log_weighted_OR_overall = log_weighted_OR_overall,
              # weighted_OR_B = weighted_OR_B,
              # log_weighted_OR_B = log_weighted_OR_B,
              # weighted_OR_cum = weighted_OR_cum,
              # log_weighted_OR_cum = log_weighted_OR_cum,
              weighted_OR_both = weighted_OR_both,
              log_weighted_OR_both = log_weighted_OR_both,
              weighted_OR_util = weighted_OR_util,
              log_weighted_OR_util = log_weighted_OR_util,
              # weighted_OR_Boverall = weighted_OR_Boverall,
              # log_weighted_OR_Boverall = log_weighted_OR_Boverall,
              unweighted_log_OR = unweighted_log_OR,
              unweighted_OR = unweighted_OR,
              # weighted_RR = weighted_RR,
              # log_weighted_RR = log_weighted_RR,
              # weighted_RR_overall = weighted_RR_overall,
              # log_weighted_RR_overall = log_weighted_RR_overall,
              # weighted_RR_B = weighted_RR_B,
              # log_weighted_RR_B = log_weighted_RR_B,
              # weighted_RR_cum = weighted_RR_cum,
              # log_weighted_RR_cum = log_weighted_RR_cum,
              weighted_RR_both = weighted_RR_both,
              log_weighted_RR_both = log_weighted_RR_both,
              weighted_RR_ge_both = weighted_RR_ge_both,
              log_weighted_RR_ge_both = log_weighted_RR_ge_both,
              weighted_RR_util = weighted_RR_util,
              log_weighted_RR_util = log_weighted_RR_util,
              weighted_RR_ge_util = weighted_RR_ge_util,
              log_weighted_RR_ge_util = log_weighted_RR_ge_util,
              # weighted_RR_Boverall = weighted_RR_Boverall,
              # log_weighted_RR_Boverall = log_weighted_RR_Boverall,
              unweighted_log_RR = unweighted_log_RR,
              unweighted_RR = unweighted_RR,
              unweighted_log_RR_ge = unweighted_log_RR_ge,
              unweighted_RR_ge = unweighted_RR_ge,
              NB = NB,
              weights_split = weights_split,
              weights_cum = weights_cum,
              weights_both = weights_both))
  
}



# stratified adjusted CI 

strat_adjust_CI <- function(stanfit, n_post_draws = 4000, 
                            dat, structured_dat,
                            outcome, trt, covs, utility) { 
  
  # first calculate P(y >= i | ctl) and P(y >= i | trt) for all i
  stanmod <- as.data.frame(stanfit)
  alphas_post <- stanmod[,grep('alpha\\[', colnames(stanmod))]
  c <- ncol(alphas_post)/2 +1
  
  betas <- stanmod[,grep('beta\\[', colnames(stanmod))]
  betas_post <- as.matrix(betas) %*% t(structured_dat$wqrX$Rinv[2:3, 2:3])
  
  
  PPO_ctl_geq_averages <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_trt_geq_averages <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_ctl_leq_averages <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_trt_leq_averages <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  
  wRD_control <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRD_overall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wOR_control <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wOR_overall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wOR_control <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wOR_overall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRR_control <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRR_overall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_control <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_overall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  
  # wRD_bcontrol <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wRD_boverall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wRD_cum <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRD_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRD_util <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wOR_bcontrol <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wOR_boverall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wOR_cum <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wOR_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wOR_bcontrol <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wOR_boverall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wOR_cum <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wOR_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wOR_util <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wRR_bcontrol <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wRR_boverall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wRR_cum <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRR_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRR_ge_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wRR_bcontrol <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wRR_boverall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wRR_cum <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_ge_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_util <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_ge_util <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  
  weights_split <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  weights_cum <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  weights_both <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  
  #NB <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  RD_leq_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  OR_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  RR_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  RR_save_geq <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  log_OR_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  log_RR_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  log_RR_ge_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  
  foreach (s = 1:n_post_draws) %do% {
    
    predicted_trt_geq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
    predicted_ctl_geq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
    predicted_trt_leq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
    predicted_ctl_leq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
    
    # if (iprior == 0){
    #   coefs <- posterior_draws[i, ]
    # } else {
    #   coefs <- c(-coef(b)[1:3], coef(b)[4:9])
    # }
    
    alpha <- alphas_post[s, ]
    beta <- betas_post[s, ]
    
    num_covs <- length(covs)
    covariate_mat <- as.data.frame(matrix(data = NA, nrow = nrow(dat), ncol = num_covs - 1))
    
    # assume the first covariate is the treatment arm 
    for(i in 1:num_covs) {
      if (is.numeric(dat[[covs[i]]]) == T) {
        covariate_mat[,i] <- beta[i] * dat[[covs[i]]]
      } else {
        # only works for numeric and binary with "Yes" as the value
        covariate_mat[,i] <- beta[i] * (dat[[covs[i]]] == "Yes")
      }
    }
    
    for (i in 2:c) {
      predicted_trt_geq[,i-1] <- plogis(as.numeric(alpha[2*i - 2]) + rowSums(covariate_mat))
      predicted_ctl_geq[,i-1] <- plogis(as.numeric(alpha[2*i - 3]) + rowSums(covariate_mat))
    }
    
    predicted_trt_leq <-  1 - predicted_trt_geq
    predicted_ctl_leq <-  1 - predicted_ctl_geq
    
    p_y_trt_est <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
    p_y_ctl_est <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
    for (i in 1:c) {
      if (i == 1) {
        p_y_ctl_est[,1] <-  predicted_ctl_leq[,1]
        p_y_trt_est[,1] <-  predicted_trt_leq[,1]
      } else if (i == 2){
        p_y_ctl_est[,2] <- predicted_ctl_leq[,i] - predicted_ctl_leq[,1]
        p_y_trt_est[,2] <- predicted_trt_leq[,i] - predicted_trt_leq[,1]
      } else if (i == c) {
        p_y_ctl_est[,c] <- 1 - rowSums(p_y_ctl_est[,1:(c-1)])
        p_y_trt_est[,c] <- 1 - rowSums(p_y_trt_est[,1:(c-1)])
      } else {
        p_y_ctl_est[,i] <-  predicted_ctl_leq[,i] - apply(p_y_ctl_est[,1:(i-1)], 1, sum)
        p_y_trt_est[,i] <-  predicted_trt_leq[,i] - apply(p_y_trt_est[,1:(i-1)], 1, sum)
      }
    }
    
    # correct for any negative probabilities 
    if (any(p_y_ctl_est < 0)) {
      p_y_ctl_est[which(p_y_ctl_est < 0)] <- 0 
      p_y_ctl_est <- p_y_ctl_est/rowSums(p_y_ctl_est)
    }
    
    if (any(p_y_trt_est < 0)) {
      p_y_trt_est[which(p_y_trt_est < 0)] <- 0 
      p_y_trt_est <- p_y_trt_est/rowSums(p_y_trt_est)
    }
    
    
    p_y_est <- (p_y_ctl_est + p_y_trt_est)/2
    
    # get weights from the data 
    p_y_ctl <- unname(table(dat[[trt]], dat[[outcome]])[1,])/sum(dat[[trt]] == -0.5)
    p_y_trt <- unname(table(dat[[trt]], dat[[outcome]])[2,])/sum(dat[[trt]] == 0.5)
    p_y <- as.vector(unname(table(dat[[outcome]]))/nrow(dat))
    
    weights <- t(rdirichlet(1, rep(1, nrow(dat))))
    
    p_y_ctl_leq_average <- apply(predicted_ctl_leq, 2, weighted.mean, w = weights)
    p_y_trt_leq_average <- apply(predicted_trt_leq, 2, weighted.mean, w = weights)
    p_y_ctl_geq_average <- apply(predicted_ctl_geq, 2, weighted.mean, w = weights)
    p_y_trt_geq_average <- apply(predicted_trt_geq, 2, weighted.mean, w = weights)
    
    p_y_est_average <- apply(p_y_est, 2, weighted.mean, w = weights)
    p_y_ctl_est_average <- apply(p_y_ctl_est, 2, weighted.mean, w = weights)
    
    # correct for any probabilities of 1  
    if (any(p_y_ctl_leq_average == 1, na.rm = T)) {
      p_y_ctl_leq_average[which(p_y_ctl_leq_average == 1)] <- 1 - 1e-7 
    }
    
    if (any(p_y_trt_leq_average == 1, na.rm = T)) {
      p_y_trt_leq_average[which(p_y_trt_leq_average == 1)] <- 1 - 1e-7 
    }
    
    RD_leq <- p_y_trt_leq_average - p_y_ctl_leq_average
    RD_leq_save[s,] <- RD_leq
    RD_geq <- p_y_ctl_geq_average - p_y_trt_geq_average
    
    
    OR <- (p_y_trt_leq_average/ (1- p_y_trt_leq_average)) / (p_y_ctl_leq_average / (1- p_y_ctl_leq_average))
    OR_save[s,] <- OR
    log_OR_save[s,] <- log(OR)
    
    RR_leq <-  p_y_trt_leq_average / p_y_ctl_leq_average
    RR_save[s,] <- RR_leq
    log_RR_save[s,] <- log(RR_leq)
    RR_geq <- p_y_ctl_geq_average / p_y_trt_geq_average
    RR_save_geq[s,] <- RR_geq
    log_RR_ge_save[s,] <- log(RR_geq)
    
    j <- 1:(c-1)
    k <- 2:c
    # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
    # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
    # wRD_control[s,1] <- numerator/denominator
    # 
    # #calculated weighted RD with overall probability as weights
    # numerator <- sum((p_y[j] + p_y[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
    # denominator <- sum(p_y[j]) + sum(p_y[k])
    # wRD_overall[s,1] <- numerator/denominator
    
    # bayesian approach 
    # numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (abs(utility[j] - utility[k])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
    # denominator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (abs(utility[j] - utility[k])))
    # wRD_bcontrol[s,1] <- numerator/denominator
    
    #cumulative weights 
    # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
    # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    # wRD_cum[s,1] <- numerator/denominator
    
    #both weights 
    numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
    denominator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    wRD_both[s,1] <- numerator/denominator
    
    numerator <- sum((abs(utility[j] - utility[k])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
    denominator <- sum((abs(utility[j] - utility[k])))
    wRD_util[s,1] <- numerator/denominator
    
    # #calculated weighted RD with overall probability as weights
    # numerator <- sum((p_y_est_average[j] + p_y_est_average[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
    # denominator <- sum(p_y_est_average[j]) + sum(p_y_est_average[k])
    # wRD_boverall[s,1] <- numerator/denominator
    
    # #calculate log weighted OR with control probability as weights
    # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log(OR[j]))
    # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
    # log_wOR_control[s,1] <- numerator/denominator
    # wOR_control[s,1] <- exp(numerator/denominator)
    # 
    # #calculate log weighted OR with overall probability as weights
    # numerator <- sum((p_y[j] + p_y[k])  * log(OR[j])) 
    # denominator <- sum(p_y[j]) + sum(p_y[k])
    # log_wOR_overall[s,1] <- numerator/denominator
    # wOR_overall[s,1] <- exp(numerator/denominator)
    
    # bayesian approach 
    #calculate log weighted OR with control probability as weights
    # numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * log(OR[j]))
    # denominator <- sum(p_y_ctl_est_average[j]) + sum(p_y_ctl_est_average[k])
    # log_wOR_bcontrol[s,1] <- numerator/denominator
    # wOR_bcontrol[s,1] <- exp(numerator/denominator)
    
    # cumulative weights 
    # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(OR[j]))
    # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    # log_wOR_cum[s,1] <- numerator/denominator
    # wOR_cum[s,1] <- exp(numerator/denominator)
    
    # both weights 
    numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(OR[j]))
    denominator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    log_wOR_both[s,1] <- numerator/denominator
    wOR_both[s,1] <- exp(numerator/denominator)
    
    numerator <- sum((abs(utility[j] - utility[k])) * log(OR[j]))
    denominator <- sum((abs(utility[j] - utility[k])))
    log_wOR_util[s,1] <- numerator/denominator
    
    # #calculate log weighted OR with overall probability as weights
    # numerator <- sum((p_y_est_average[j] + p_y_est_average[k]) * log(OR[j])) 
    # denominator <- sum(p_y_est_average[j]) + sum(p_y_est_average[k])
    # log_wOR_boverall[s,1] <- numerator/denominator
    # wOR_boverall[s,1] <- exp(numerator/denominator)
    
    # #calculate log weighted RR with control probability as weights
    # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log(RR_leq[j]))
    # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
    # log_wRR_control[s,1] <- numerator/denominator
    # wRR_control[s,1] <- exp(numerator/denominator)
    # 
    # #calculate log weighted RR with overall probability as weights
    # numerator <- sum((p_y[j] + p_y[k])  * log(RR_leq[j]))
    # denominator <- sum(p_y[j]) + sum(p_y[k])
    # log_wRR_overall[s,1] <- numerator/denominator
    # wRR_overall[s,1] <- exp(numerator/denominator)
    
    # bayesian approach
    #calculate log weighted RR with control probability as weights
    # numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * log(RR_leq[j]))
    # denominator <- sum(p_y_ctl_est_average[j]) + sum(p_y_ctl_est_average[k])
    # log_wRR_bcontrol[s,1] <- numerator/denominator
    # wRR_bcontrol[s,1] <- exp(numerator/denominator)
    # 
    # # cumulative weights 
    # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_leq[j]))
    # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    # log_wRR_cum[s,1] <- numerator/denominator
    # wRR_cum[s,1] <- exp(numerator/denominator)
    
    # both weights 
    numerator <- sum((abs(utility[j] - utility[k])) * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_leq[j]))
    denominator <- sum((abs(utility[j] - utility[k])) * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    log_wRR_both[s,1] <- numerator/denominator
    wRR_both[s,1] <- exp(numerator/denominator)
    
    # both weights 
    numerator <- sum((abs(utility[j] - utility[k])) * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_geq[j]))
    denominator <- sum((abs(utility[j] - utility[k])) * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    log_wRR_ge_both[s,1] <- numerator/denominator
    wRR_ge_both[s,1] <- exp(numerator/denominator)
    
    # both weights 
    numerator <- sum((abs(utility[j] - utility[k])) * log(RR_leq[j]))
    denominator <- sum((abs(utility[j] - utility[k])))
    log_wRR_util[s,1] <- numerator/denominator
    
    # both weights 
    numerator <- sum((abs(utility[j] - utility[k])) *  log(RR_geq[j]))
    denominator <- sum((abs(utility[j] - utility[k])))
    log_wRR_ge_util[s,1] <- numerator/denominator
    
    # #calculate log weighted RR with overall probability as weights
    # numerator <- sum((p_y_est_average[j] + p_y_est_average[k]) * log(RR_leq[j]))
    # denominator <- sum(p_y_est_average[j]) + sum(p_y_est_average[k])
    # log_wRR_boverall[s,1] <- numerator/denominator
    # wRR_boverall[s,1] <- exp(numerator/denominator)
    
    #calculate WR    
    #l <- 1:c
    #NB[s,1] <- sum(p_y_trt[j] * p_y_ctl_geq_average[j]) - (sum(p_y_ctl[j] * p_y_trt_geq_average[j]))
    
    weights_split[s,] <- (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) / sum(p_y_ctl_est_average[j] + p_y_ctl_est_average[k])
    weights_cum[s,] <- (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) / sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    weights_both[s,] <- (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) / 
      sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    
  }
  
  RD_leq_95CI <- apply(RD_leq_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  RD_leq_postprob <- colSums(RD_leq_save > 0)/n_post_draws
  
  #unweighted_RD
  unweighted_RD <- rowMeans(RD_leq_save)
  unweighted_RD_95CI <- quantile(unweighted_RD, probs = c(0.025, 0.975))
  
  OR_95CI <- apply(OR_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  OR_postprob <- colSums(OR_save > 1)/n_post_draws
  
  log_OR_95CI <- apply(log_OR_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  
  #unweighted_OR
  unweighted_log_OR <- rowMeans(log_OR_save)
  unweighted_log_OR_95CI <- quantile(unweighted_log_OR, probs = c(0.025, 0.975))
  unweighted_OR_95CI <- quantile(exp(unweighted_log_OR), probs = c(0.025, 0.975))
  
  RR_leq_95CI <- apply(RR_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  RR_leq_postprob <- colSums(RR_save > 1)/n_post_draws
  
  log_RR_leq_95CI <- apply(log_RR_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  
  #unweighted_RR
  unweighted_log_RR <- rowMeans(log_RR_save)
  unweighted_log_RR_95CI <- quantile(unweighted_log_RR, probs = c(0.025, 0.975))
  unweighted_RR_95CI <- quantile(exp(unweighted_log_RR), probs = c(0.025, 0.975))
  
  #unweighted_RR
  unweighted_log_RR_ge <- rowMeans(log_RR_ge_save)
  unweighted_log_RR_ge_95CI <- quantile(unweighted_log_RR_ge, probs = c(0.025, 0.975))
  unweighted_RR_ge_95CI <- quantile(exp(unweighted_log_RR_ge), probs = c(0.025, 0.975))
  
  RR_geq_95CI <- apply(RR_save_geq, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  RR_geq_postprob <- colSums(RR_save_geq > 1)/n_post_draws
  
  log_RR_geq_95CI <- apply(log_RR_ge_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  
  
  # wRD_control_95CI <- quantile(wRD_control, probs = c(0.025, 0.975))
  # wRD_control_postprob <- sum(wRD_control > 0)/n_post_draws
  # 
  # wRD_overall_95CI <- quantile(wRD_overall, probs = c(0.025, 0.975))
  # wRD_overall_postprob <- sum(wRD_overall > 0)/n_post_draws
  
  # wRD_bcontrol_95CI <- quantile(wRD_bcontrol, probs = c(0.025, 0.975))
  # wRD_bcontrol_postprob <- sum(wRD_bcontrol > 0)/n_post_draws
  # 
  # wRD_cum_95CI <- quantile(wRD_cum, probs = c(0.025, 0.975))
  # wRD_cum_postprob <- sum(wRD_cum > 0)/n_post_draws
  
  wRD_both_95CI <- quantile(wRD_both, probs = c(0.025, 0.975))
  wRD_both_postprob <- sum(wRD_both > 0)/n_post_draws
  
  wRD_util_95CI <- quantile(wRD_util, probs = c(0.025, 0.975))
  
  # wRD_boverall_95CI <- quantile(wRD_boverall, probs = c(0.025, 0.975))
  # wRD_boverall_postprob <- sum(wRD_boverall > 0)/n_post_draws
  # 
  # wOR_control_95CI <- quantile(wOR_control, probs = c(0.025, 0.975))
  # wOR_control_postprob <- sum(wOR_control > 1)/n_post_draws
  # 
  # wOR_overall_95CI <- quantile(wOR_overall, probs = c(0.025, 0.975))
  # wOR_overall_postprob <- sum(wOR_overall > 1)/n_post_draws
  
  # wOR_bcontrol_95CI <- quantile(wOR_bcontrol, probs = c(0.025, 0.975))
  # wOR_bcontrol_postprob <- sum(wOR_bcontrol > 1)/n_post_draws
  # 
  # wOR_cum_95CI <- quantile(wOR_cum, probs = c(0.025, 0.975))
  # wOR_cum_postprob <- sum(wOR_cum > 1)/n_post_draws
  
  wOR_both_95CI <- quantile(wOR_both, probs = c(0.025, 0.975))
  wOR_both_postprob <- sum(wOR_both > 1)/n_post_draws
  
  log_wOR_util_95CI <- quantile(log_wOR_util, probs = c(0.025, 0.975))
  
  # wOR_boverall_95CI <- quantile(wOR_boverall, probs = c(0.025, 0.975))
  # wOR_boverall_postprob <- sum(wOR_boverall > 1)/n_post_draws
  # 
  # log_wOR_control_95CI <- quantile(log_wOR_control, probs = c(0.025, 0.975))
  # log_wOR_control_postprob <- sum(log_wOR_control > 0)/n_post_draws
  # 
  # log_wOR_overall_95CI <- quantile(log_wOR_overall, probs = c(0.025, 0.975))
  # log_wOR_overall_postprob <- sum(log_wOR_overall > 0)/n_post_draws
  
  # log_wOR_bcontrol_95CI <- quantile(log_wOR_bcontrol, probs = c(0.025, 0.975))
  # log_wOR_bcontrol_postprob <- sum(log_wOR_bcontrol > 0)/n_post_draws
  # 
  # log_wOR_cum_95CI <- quantile(log_wOR_cum, probs = c(0.025, 0.975))
  # log_wOR_cum_postprob <- sum(log_wOR_cum > 0)/n_post_draws
  
  log_wOR_both_95CI <- quantile(log_wOR_both, probs = c(0.025, 0.975))
  log_wOR_both_postprob <- sum(log_wOR_both > 0)/n_post_draws
  
  # log_wOR_boverall_95CI <- quantile(log_wOR_boverall, probs = c(0.025, 0.975))
  # log_wOR_boverall_postprob <- sum(log_wOR_boverall > 0)/n_post_draws
  # 
  # wRR_control_95CI <- quantile(wRR_control, probs = c(0.025, 0.975))
  # wRR_control_postprob <- sum(wRR_control > 1)/n_post_draws
  # 
  # wRR_overall_95CI <- quantile(wRR_overall, probs = c(0.025, 0.975))
  # wRR_overall_postprob <- sum(wRR_overall > 1)/n_post_draws
  # 
  # log_wRR_control_95CI <- quantile(log_wRR_control, probs = c(0.025, 0.975))
  # log_wRR_control_postprob <- sum(log_wRR_control > 0)/n_post_draws
  # 
  # log_wRR_overall_95CI <- quantile(log_wRR_overall, probs = c(0.025, 0.975))
  # log_wRR_overall_postprob <- sum(log_wRR_overall > 0)/n_post_draws
  
  # wRR_bcontrol_95CI <- quantile(wRR_bcontrol, probs = c(0.025, 0.975))
  # wRR_bcontrol_postprob <- sum(wRR_bcontrol > 1)/n_post_draws
  # 
  # wRR_cum_95CI <- quantile(wRR_cum, probs = c(0.025, 0.975))
  # wRR_cum_postprob <- sum(wRR_cum > 1)/n_post_draws
  
  wRR_both_95CI <- quantile(wRR_both, probs = c(0.025, 0.975))
  wRR_both_postprob <- sum(wRR_both > 1)/n_post_draws
  
  wRR_ge_both_95CI <- quantile(wRR_ge_both, probs = c(0.025, 0.975))
  wRR_ge_both_postprob <- sum(wRR_ge_both > 1)/n_post_draws
  
  # wRR_boverall_95CI <- quantile(wRR_boverall, probs = c(0.025, 0.975))
  # wRR_boverall_postprob <- sum(wRR_boverall > 1)/n_post_draws
  
  # log_wRR_bcontrol_95CI <- quantile(log_wRR_bcontrol, probs = c(0.025, 0.975))
  # log_wRR_bcontrol_postprob <- sum(log_wRR_bcontrol > 0)/n_post_draws
  # 
  # log_wRR_cum_95CI <- quantile(log_wRR_cum, probs = c(0.025, 0.975))
  # log_wRR_cum_postprob <- sum(log_wRR_cum > 0)/n_post_draws
  
  log_wRR_both_95CI <- quantile(log_wRR_both, probs = c(0.025, 0.975))
  log_wRR_both_postprob <- sum(log_wRR_both > 0)/n_post_draws
  
  log_wRR_ge_both_95CI <- quantile(log_wRR_ge_both, probs = c(0.025, 0.975))
  log_wRR_ge_both_postprob <- sum(log_wRR_ge_both > 0)/n_post_draws
  
  log_wRR_util_95CI <- quantile(log_wRR_util, probs = c(0.025, 0.975))
  log_wRR_ge_util_95CI <- quantile(log_wRR_ge_util, probs = c(0.025, 0.975))
  
  # log_wRR_boverall_95CI <- quantile(log_wRR_boverall, probs = c(0.025, 0.975))
  # log_wRR_boverall_postprob <- sum(log_wRR_boverall > 0)/n_post_draws
  
  #NB_95CI <- quantile(NB, probs = c(0.025, 0.975))
  #NB_postprob <- sum(NB > 0)/n_post_draws
  
  weights_split_95CI <- apply(weights_split, 2, quantile, probs = c(0.025, 0.975))
  weights_cum_95CI <- apply(weights_cum, 2, quantile, probs = c(0.025, 0.975))
  weights_both_95CI <- apply(weights_both, 2, quantile, probs = c(0.025, 0.975))
  
  if (c == 4) {
    RD_leq_95CI = c(RD_leq_95CI, NA, NA)
    OR_95CI = c(OR_95CI, NA, NA)
    log_OR_95CI = c(log_OR_95CI, NA, NA)
    RR_leq_95CI = c(RR_leq_95CI, NA, NA)
    log_RR_leq_95CI = c(log_RR_leq_95CI, NA, NA) 
    RR_geq_95CI = c(RR_geq_95CI, NA, NA)
    log_RR_geq_95CI = c(log_RR_geq_95CI, NA, NA)
    weights_split_95CI = c(weights_split_95CI, NA, NA)
    weights_cum_95CI = c(weights_cum_95CI, NA, NA)
    weights_both_95CI = c(weights_both_95CI, NA, NA)
  }
  
  return(c(RD_leq_95CI = RD_leq_95CI, 
              # RD_leq_postprob = RD_leq_postprob, 
              OR_95CI = OR_95CI, 
              log_OR_95CI = log_OR_95CI, 
              # OR_postprob = OR_postprob, 
              RR_leq_95CI = RR_leq_95CI, 
              log_RR_leq_95CI = log_RR_leq_95CI, 
              # RR_leq_postprob = RR_leq_postprob, 
              RR_geq_95CI = RR_geq_95CI,
              log_RR_geq_95CI = log_RR_geq_95CI,
              # wRD_control_95CI = wRD_control_95CI, 
              # wRD_control_postprob = wRD_control_postprob, 
              # wRD_overall_95CI = wRD_overall_95CI, 
              # wRD_overall_postprob = wRD_overall_postprob, 
              # wRD_bcontrol_95CI = wRD_bcontrol_95CI, 
              # wRD_bcontrol_postprob = wRD_bcontrol_postprob, 
              # wRD_cum_95CI = wRD_cum_95CI, 
              # wRD_cum_postprob = wRD_cum_postprob,
              wRD_both_95CI = wRD_both_95CI, 
              wRD_both_postprob = wRD_both_postprob,
              wRD_util_95CI = wRD_util_95CI, 
              # wRD_boverall_95CI = wRD_boverall_95CI, 
              # wRD_boverall_postprob = wRD_boverall_postprob, 
              # wOR_control_95CI = wOR_control_95CI, 
              # wOR_control_postprob = wOR_control_postprob, 
              # wOR_overall_95CI = wOR_overall_95CI, 
              # wOR_overall_postprob = wOR_overall_postprob, 
              # log_wOR_control_95CI = log_wOR_control_95CI, 
              # log_wOR_control_postprob = log_wOR_control_postprob, 
              # log_wOR_overall_95CI = log_wOR_overall_95CI, 
              # log_wOR_overall_postprob = log_wOR_overall_postprob, 
              # wOR_bcontrol_95CI = wOR_bcontrol_95CI,
              # wOR_bcontrol_postprob = wOR_bcontrol_postprob,
              # wOR_cum_95CI = wOR_cum_95CI,
              # wOR_cum_postprob = wOR_cum_postprob,
              wOR_both_95CI = wOR_both_95CI,
              wOR_both_postprob = wOR_both_postprob,
              # wOR_boverall_95CI = wOR_boverall_95CI, 
              # wOR_boverall_postprob = wOR_boverall_postprob, 
              # log_wOR_bcontrol_95CI = log_wOR_bcontrol_95CI, 
              # log_wOR_bcontrol_postprob = log_wOR_bcontrol_postprob, 
              # log_wOR_cum_95CI = log_wOR_cum_95CI, 
              # log_wOR_cum_postprob = log_wOR_cum_postprob, 
              log_wOR_both_95CI = log_wOR_both_95CI, 
              log_wOR_both_postprob = log_wOR_both_postprob, 
              log_wOR_util_95CI = log_wOR_util_95CI, 
              # log_wOR_boverall_95CI = log_wOR_boverall_95CI, 
              # log_wOR_boverall_postprob = log_wOR_boverall_postprob, 
              # wRR_control_95CI = wRR_control_95CI, 
              # wRR_control_postprob = wRR_control_postprob, 
              # wRR_overall_95CI = wRR_overall_95CI, 
              # wRR_overall_postprob = wRR_overall_postprob, 
              # log_wRR_control_95CI = log_wRR_control_95CI, 
              # log_wRR_control_postprob = log_wRR_control_postprob, 
              # log_wRR_overall_95CI = log_wRR_overall_95CI, 
              # log_wRR_overall_postprob = log_wRR_overall_postprob, 
              # wRR_bcontrol_95CI = wRR_bcontrol_95CI, 
              # wRR_bcontrol_postprob = wRR_bcontrol_postprob, 
              # wRR_cum_95CI = wRR_cum_95CI, 
              # wRR_cum_postprob = wRR_cum_postprob,
              wRR_both_95CI = wRR_both_95CI, 
              wRR_both_postprob = wRR_both_postprob,
              wRR_ge_both_95CI = wRR_ge_both_95CI, 
              wRR_ge_both_postprob = wRR_ge_both_postprob,
              # wRR_boverall_95CI = wRR_boverall_95CI, 
              # wRR_boverall_postprob = wRR_boverall_postprob, 
              # log_wRR_bcontrol_95CI = log_wRR_bcontrol_95CI, 
              # log_wRR_bcontrol_postprob = log_wRR_bcontrol_postprob, 
              # log_wRR_cum_95CI = log_wRR_cum_95CI, 
              # log_wRR_cum_postprob = log_wRR_cum_postprob, 
              log_wRR_both_95CI = log_wRR_both_95CI, 
              log_wRR_both_postprob = log_wRR_both_postprob, 
              log_wRR_util_95CI = log_wRR_util_95CI, 
              log_wRR_ge_both_95CI = log_wRR_ge_both_95CI, 
              log_wRR_ge_both_postprob = log_wRR_ge_both_postprob, 
              log_wRR_ge_util_95CI = log_wRR_ge_util_95CI, 
              # log_wRR_boverall_95CI = log_wRR_boverall_95CI, 
              # log_wRR_boverall_postprob = log_wRR_boverall_postprob, 
              unweighted_RD_95CI = unweighted_RD_95CI,
              unweighted_log_OR_95CI = unweighted_log_OR_95CI,
              unweighted_OR_95CI = unweighted_OR_95CI,
              unweighted_log_RR_95CI = unweighted_log_RR_95CI,
              unweighted_RR_95CI = unweighted_RR_95CI,
              unweighted_log_RR_ge_95CI = unweighted_log_RR_ge_95CI,
              unweighted_RR_ge_95CI = unweighted_RR_ge_95CI,
              #NB_95CI = NB_95CI, 
              #NB_postprob = NB_postprob,
              weights_split_95CI = weights_split_95CI,
              weights_cum_95CI = weights_cum_95CI,
              weights_both_95CI = weights_both_95CI))
  
}

blrm_adj_bb_PPO_est <- function(b = b, dat, iprior,
                                outcome, trt, covs = 1, utility) { 
  c <- length(b$ylevels)
  
  predicted_trt_geq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  predicted_ctl_geq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  predicted_trt_leq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  predicted_ctl_leq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  p_y_ctl <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
  p_y_trt <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
  p_y_ctl_est <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
  p_y_trt_est <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
  
  if (iprior == 0){
    coefs <- coef(b)
  } else {
    coefs <- c(-coef(b)[1:4], coef(b)[5:10])
  }
  
  # covariates <- unlist(str_split(deparse(b_ppo_model_adj$call$formula[[3]]), " \\+ "))
  # covariates <- covariates[-which(covariates == trt)]
  # num_covs <- length(covariates)
  
  covs <- b$Design$colnames
  num_covs <- length(covs)
  covariate_mat <- as.data.frame(matrix(data = NA, nrow = nrow(dat), ncol = num_covs - 1))
  
  # assume the first covariate is the treatment arm 
  for(i in 2:num_covs) {
    if (is.numeric(dat[[b$Design$name[i]]]) == T) {
      covariate_mat[,i-1] <- coefs[[covs[i]]] * (dat[[b$Design$name[i]]])
    } else {
      # only works for numeric and binary
      covariate_mat[,i-1] <- coefs[[covs[i]]] * (dat[[b$Design$name[i]]] == b$Design$parms[[b$Design$name[i]]][2])
    }
  }
  
  for (i in 2:c) {
    if (i == 2) {
      predicted_trt_geq[,i-1] <- plogis(coefs[i-1] + coefs[[trt]] * 0.5 + rowSums(covariate_mat))
      predicted_ctl_geq[,i-1] <- plogis(coefs[i-1] + coefs[[trt]] * -0.5 + rowSums(covariate_mat))
    } else {
      phrase <- paste0("y>=", i)
      coefs_new <- coefs[str_detect(names(coefs), phrase)]
      predicted_trt_geq[,i-1] <- plogis(coefs_new[1] + coefs_new[2] * 0.5 + coefs[[trt]] * 0.5 + rowSums(covariate_mat))
      predicted_ctl_geq[,i-1] <- plogis(coefs_new[1] + coefs_new[2] * -0.5 + coefs[[trt]] * -0.5 + rowSums(covariate_mat))
    }
  }
  
  
  predicted_trt_leq <-  1 - predicted_trt_geq
  predicted_ctl_leq <-  1 - predicted_ctl_geq
  
  # calculate p(Y = i | control) for i = 1, ... , c
  for (i in 1:c) {
    if (i == 1) {
      p_y_ctl_est[,1] <-  predicted_ctl_leq[,1]
      p_y_trt_est[,1] <-  predicted_trt_leq[,1]
    } else if (i == 2) {
      p_y_ctl_est[,i] <-  predicted_ctl_leq[,i] - p_y_ctl_est[,1]
      p_y_trt_est[,i] <-  predicted_trt_leq[,i] - p_y_trt_est[,1]
    } else if (i == c) {
      p_y_ctl_est[,c] <- 1 - rowSums(p_y_ctl_est[,1:(c-1)])
      p_y_trt_est[,c] <- 1 - rowSums(p_y_trt_est[,1:(c-1)])
    } else {
      p_y_ctl_est[,i] <-  predicted_ctl_leq[,i] - rowSums(p_y_ctl_est[,1:(i-1)])
      p_y_trt_est[,i] <-  predicted_trt_leq[,i] - rowSums(p_y_trt_est[,1:(i-1)])
    }
  }
  
  # correct for any negative probabilities 
  if (any(p_y_ctl_est < 0)) {
    p_y_ctl_est[which(p_y_ctl_est < 0)] <- 0 
    p_y_ctl_est <- p_y_ctl_est/rowSums(p_y_ctl_est)
  }
  
  if (any(p_y_trt_est < 0)) {
    p_y_trt_est[which(p_y_trt_est < 0)] <- 0 
    p_y_trt_est <- p_y_trt_est/rowSums(p_y_trt_est)
  }
  
  
  p_y_est <- (p_y_ctl_est + p_y_trt_est)/2
  
  # get weights from the data 
  p_y_ctl <- unname(table(dat[[trt]], dat[[outcome]])[1,])/sum(dat[[trt]] == -0.5)
  p_y_trt <- unname(table(dat[[trt]], dat[[outcome]])[2,])/sum(dat[[trt]] == 0.5)
  p_y <- unname(table(dat[[outcome]]))/nrow(dat)
  
  # use Bayesian bootstrap to marginalize the probabilities
  weights <- t(rdirichlet(1, rep(1, nrow(dat))))
  p_y_ctl_est_bb <- apply(p_y_ctl_est, 2, weighted.mean, w = weights)
  p_y_trt_est_bb <- apply(p_y_trt_est, 2, weighted.mean, w = weights)
  p_y_est_bb <- (p_y_ctl_est_bb + p_y_trt_est_bb) /2
  
  p_y_ctl_leq_average <- apply(predicted_ctl_leq, 2, weighted.mean, w = weights)
  p_y_trt_leq_average <- apply(predicted_trt_leq, 2, weighted.mean, w = weights)
  p_y_ctl_geq_average <- apply(predicted_ctl_geq, 2, weighted.mean, w = weights)
  p_y_trt_geq_average <- apply(predicted_trt_geq, 2, weighted.mean, w = weights)
  
  #p_y_est_average <- apply(p_y_est, 2, weighted.mean, w = weights)
  #p_y_ctl_est_average <- apply(p_y_ctl_est, 2, weighted.mean, w = weights)
  
  
  RD_leq <- p_y_trt_leq_average - p_y_ctl_leq_average
  RD_geq <- p_y_ctl_geq_average - p_y_trt_geq_average
  
  
  OR <- (p_y_trt_leq_average / (1- p_y_trt_leq_average)) / (p_y_ctl_leq_average / (1- p_y_ctl_leq_average))
  
  RR_leq <- p_y_trt_leq_average / p_y_ctl_leq_average
  RR_geq <- p_y_ctl_geq_average / p_y_trt_geq_average
  
  j <- 1:(c-1)
  k <- 2:c
  
  utility_diff <- abs(utility[j] - utility[k])
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_RD <- numerator/denominator
  # 
  # #calculated weighted RD with overall probability as weights
  # numerator <- sum((p_y[j] + p_y[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # weighted_RD_overall <- numerator/denominator
  
  # fully bayesian approach 
  # control weighted
  # numerator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum(p_y_ctl_est_bb[j]) + sum(p_y_ctl_est_bb[k])
  # weighted_RD_B <- numerator/denominator
  # 
  # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  # weighted_RD_cum <- numerator/denominator
  
  numerator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * utility_diff * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  denominator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * utility_diff * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weighted_RD_both <- numerator/denominator
  
  numerator <- sum(utility_diff * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  denominator <- sum(utility_diff)
  weighted_RD_util <- numerator/denominator
  
  # # calculated weighted RD with overall probability as weights
  # numerator <- sum((p_y_est_bb[j] + p_y_est_bb[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum(p_y_est_bb[j]) + sum(p_y_est_bb[k])
  # weighted_RD_Boverall <- numerator/denominator
  
  # unweighted RD 
  unweighted_RD <- mean(p_y_trt_leq_average - p_y_ctl_leq_average)
  
  # #calculate weighted OR with control probability as weights
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log(OR[j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_OR <- exp(numerator/denominator)
  # log_weighted_OR <- numerator/denominator
  # 
  # #calculate weighted OR with overall probability as weights
  # numerator <- sum((p_y[j] + p_y[k]) * log(OR[j]))
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # log_weighted_OR_overall <- numerator/denominator
  # weighted_OR_overall <- exp(numerator/denominator)
  
  # bayesian approach
  #calculate weighted OR with control probability as weights
  # numerator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * log(OR[j]))
  # denominator <- sum(p_y_ctl_est_bb[j]) + sum(p_y_ctl_est_bb[k])
  # weighted_OR_B <- exp(numerator/denominator)
  # log_weighted_OR_B <- numerator/denominator
  # 
  # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(OR[j]))
  # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  # weighted_OR_cum <- exp(numerator/denominator)
  # log_weighted_OR_cum <- numerator/denominator
  
  numerator <- sum(utility_diff * (p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(OR[j]))
  denominator <- sum(utility_diff * (p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weighted_OR_both <- exp(numerator/denominator)
  log_weighted_OR_both <- numerator/denominator
  
  numerator <- sum(utility_diff * log(OR[j]))
  denominator <- sum(utility_diff)
  weighted_OR_util <- exp(numerator/denominator)
  log_weighted_OR_util <- numerator/denominator
  
  # #calculate weighted OR with overall probability as weights
  # numerator <- sum((p_y_est_bb[j] + p_y_est_bb[k]) * log(OR[j]))
  # denominator <- sum(p_y_est_bb[j]) + sum(p_y_est_bb[k])
  # log_weighted_OR_Boverall <- numerator/denominator
  # weighted_OR_Boverall <- exp(numerator/denominator)
  
  # unweighted OR 
  unweighted_log_OR <- mean(log(OR))
  unweighted_OR <- exp(mean(log(OR)))
  
  # #calculate weighted RR with control probability as weights
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log(RR_leq[j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_RR <- exp(numerator/denominator)
  # log_weighted_RR <- numerator/denominator
  # 
  # #calculate weighted RR with overall probability as weights
  # numerator <- sum((p_y[j] + p_y[k]) * log(RR_leq[j]))
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # log_weighted_RR_overall <- numerator/denominator
  # weighted_RR_overall <- exp(numerator/denominator)
  
  # bayesian approach
  #calculate weighted RR with control probability as weights
  # numerator <- sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * log(RR_leq[j]))
  # denominator <- sum(p_y_ctl_est_bb[j]) + sum(p_y_ctl_est_bb[k])
  # weighted_RR_B <- exp(numerator/denominator)
  # log_weighted_RR_B <- numerator/denominator
  # 
  # # cumulative weights 
  # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_leq[j]))
  # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  # weighted_RR_cum <- exp(numerator/denominator)
  # log_weighted_RR_cum <- numerator/denominator
  
  # both weights
  numerator <- sum(utility_diff * (p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_leq[j]))
  denominator <- sum(utility_diff * (p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weighted_RR_both <- exp(numerator/denominator)
  log_weighted_RR_both <- numerator/denominator
  
  # both weights RR greater than or equal to
  numerator <- sum(utility_diff * (p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_geq[j]))
  denominator <- sum(utility_diff * (p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weighted_RR_ge_both <- exp(numerator/denominator)
  log_weighted_RR_ge_both <- numerator/denominator
  
  #utilities only 
  numerator <- sum(utility_diff * log(RR_leq[j]))
  denominator <- sum(utility_diff)
  weighted_RR_util <- exp(numerator/denominator)
  log_weighted_RR_util <- numerator/denominator
  
  # both weights RR greater than or equal to
  numerator <- sum(utility_diff * log(RR_geq[j]))
  denominator <- sum(utility_diff)
  weighted_RR_ge_util <- exp(numerator/denominator)
  log_weighted_RR_ge_util <- numerator/denominator
  
  # #calculate weighted RR with overall probability as weights
  # numerator <- sum((p_y_est_bb[j] + p_y_est_bb[k]) * log(RR_leq[j]))
  # denominator <- sum(p_y_est_bb[j]) + sum(p_y_est_bb[k])
  # log_weighted_RR_Boverall <- numerator/denominator
  # weighted_RR_Boverall <- exp(numerator/denominator)
  
  # unweighted RR 
  unweighted_log_RR <- mean(log(RR_leq))
  unweighted_RR <- exp(mean(log(RR_leq)))
  
  # unweighted RR greater than or equal to  
  unweighted_log_RR_ge <- mean(log(RR_geq))
  unweighted_RR_ge <- exp(mean(log(RR_geq)))
  
  #calculate Net Benefit    
  l <- 1:c
  NB <- sum(p_y_trt[j] * p_y_ctl_geq_average[j]) - (sum(p_y_ctl[j] * p_y_trt_geq_average[j]))
  
  #save the weights 
  weights_split <- (p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) / sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]))
  weights_cum <- (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) / sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weights_both <- ((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j]))) /
    sum((p_y_ctl_est_bb[j] + p_y_ctl_est_bb[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  
  if (c == 4) {
    p_y_ctl = c(p_y_ctl, NA)
    p_y_trt = c(p_y_trt, NA)
    RD_leq = c(RD_leq, NA)
    RD_geq = c(RD_geq, NA)
    RR_leq = c(RR_leq, NA)
    RR_geq = c(RR_geq, NA)
    OR = c(OR, NA)
    weights_split = c(weights_split, NA)
    weights_cum = c(weights_cum, NA)
    weights_both = c(weights_both, NA)
  }
  
  return(c(p_y_ctl = p_y_ctl, 
              p_y_trt = p_y_trt,
              RD_leq = RD_leq, 
              RD_geq = RD_geq, 
              RR_leq = RR_leq,
              RR_geq = RR_geq,
              OR = OR,
              log_RR_leq = log(RR_leq),
              log_RR_geq = log(RR_geq),
              log_OR = log(OR), 
              # weighted_RD = weighted_RD, 
              # weighted_RD_overall = weighted_RD_overall,
              # weighted_RD_B = weighted_RD_B, 
              # weighted_RD_cum = weighted_RD_cum, 
              weighted_RD_both = weighted_RD_both, 
              weighted_RD_util = weighted_RD_util, 
              # weighted_RD_Boverall = weighted_RD_Boverall,
              unweighted_RD = unweighted_RD,
              # weighted_OR = weighted_OR,
              # log_weighted_OR = log_weighted_OR,
              # weighted_OR_overall = weighted_OR_overall,
              # log_weighted_OR_overall = log_weighted_OR_overall,
              # weighted_OR_B = weighted_OR_B,
              # log_weighted_OR_B = log_weighted_OR_B,
              # weighted_OR_cum = weighted_OR_cum,
              # log_weighted_OR_cum = log_weighted_OR_cum,
              weighted_OR_both = weighted_OR_both,
              log_weighted_OR_both = log_weighted_OR_both,
              weighted_OR_util = weighted_OR_util,
              log_weighted_OR_util = log_weighted_OR_util,
              # weighted_OR_Boverall = weighted_OR_Boverall,
              # log_weighted_OR_Boverall = log_weighted_OR_Boverall,
              unweighted_log_OR = unweighted_log_OR,
              unweighted_OR = unweighted_OR,
              # weighted_RR = weighted_RR,
              # log_weighted_RR = log_weighted_RR,
              # weighted_RR_overall = weighted_RR_overall,
              # log_weighted_RR_overall = log_weighted_RR_overall,
              # weighted_RR_B = weighted_RR_B,
              # log_weighted_RR_B = log_weighted_RR_B,
              # weighted_RR_cum = weighted_RR_cum,
              # log_weighted_RR_cum = log_weighted_RR_cum,
              weighted_RR_both = weighted_RR_both,
              log_weighted_RR_both = log_weighted_RR_both,
              weighted_RR_ge_both = weighted_RR_ge_both,
              log_weighted_RR_ge_both = log_weighted_RR_ge_both,
              weighted_RR_util = weighted_RR_util,
              log_weighted_RR_util = log_weighted_RR_util,
              weighted_RR_ge_util = weighted_RR_ge_util,
              log_weighted_RR_ge_util = log_weighted_RR_ge_util,
              # weighted_RR_Boverall = weighted_RR_Boverall,
              # log_weighted_RR_Boverall = log_weighted_RR_Boverall,
              unweighted_log_RR = unweighted_log_RR,
              unweighted_RR = unweighted_RR,
              unweighted_log_RR_ge = unweighted_log_RR_ge,
              unweighted_RR_ge = unweighted_RR_ge,
              NB = NB,
              weights_split = weights_split,
              weights_cum = weights_cum,
              weights_both = weights_both))
  
}

#blrm CI 

blrm_CI_adj_bb <- function(b, n_post_draws = 4000, dat, iprior, 
                           outcome, trt, covs = 1, utility) { 
  
  # first calculate P(y >= i | ctl) and P(y >= i | trt) for all i
  posterior_draws <- as.matrix(b$draws)
  c <- length(b$ylevels)
  PPO_ctl_geq_averages <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_trt_geq_averages <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_ctl_leq_averages <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_trt_leq_averages <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  
  wRD_control <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRD_overall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wOR_control <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wOR_overall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wOR_control <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wOR_overall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRR_control <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRR_overall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_control <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_overall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  
  # wRD_bcontrol <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wRD_boverall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wRD_cum <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRD_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRD_util <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wOR_bcontrol <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wOR_boverall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wOR_cum <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wOR_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wOR_bcontrol <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wOR_boverall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wOR_cum <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wOR_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wOR_util <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wRR_bcontrol <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wRR_boverall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # wRR_cum <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRR_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  wRR_ge_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wRR_bcontrol <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wRR_boverall <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  # log_wRR_cum <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_ge_both <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_util <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  log_wRR_ge_util <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  
  weights_split <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  weights_cum <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  weights_both <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  
  NB <- matrix(data = NA, nrow = n_post_draws, ncol = 1)
  RD_leq_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  OR_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  RR_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  RR_save_geq <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  log_OR_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  log_RR_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  log_RR_ge_save <- matrix(data = NA, nrow = n_post_draws, ncol = c-1)
  p_y_trt_est_average_save <- matrix(data = NA, nrow = n_post_draws, ncol = c)
  p_y_ctl_est_average_save <- matrix(data = NA, nrow = n_post_draws, ncol = c)
  
  foreach (s = 1:n_post_draws) %do% {
    
    predicted_trt_geq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
    predicted_ctl_geq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
    predicted_trt_leq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
    predicted_ctl_leq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
    
    if (iprior == 0){
      coefs <- posterior_draws[s, ]
    } else {
      coefs <- c(-posterior_draws[s,1:4], posterior_draws[s,5:10])
    }
    
    #coefs <- posterior_draws[s, ]
    covs <- b$Design$colnames
    num_covs <- length(covs)
    covariate_mat <- as.data.frame(matrix(data = NA, nrow = nrow(dat), ncol = num_covs - 1))
    
    # assume the first covariate is the treatment arm 
    for(i in 2:num_covs) {
      if (is.numeric(dat[[b$Design$name[i]]]) == T) {
        covariate_mat[,i-1] <- coefs[[covs[i]]] * (dat[[b$Design$name[i]]])
      } else {
        # only works for numeric and binary
        covariate_mat[,i-1] <- coefs[[covs[i]]] * (dat[[b$Design$name[i]]] == b$Design$parms[[b$Design$name[i]]][2])
      }
    }
    
    for (i in 2:c) {
      if (i == 2) {
        predicted_trt_geq[,i-1] <- plogis(coefs[i-1] + coefs[[trt]] * 0.5 + rowSums(covariate_mat))
        predicted_ctl_geq[,i-1] <- plogis(coefs[i-1] + coefs[[trt]] * -0.5 + rowSums(covariate_mat))
      } else {
        phrase <- paste0("y>=", i)
        coefs_new <- coefs[str_detect(names(coefs), phrase)]
        predicted_trt_geq[,i-1] <- plogis(coefs_new[1] + coefs_new[2] * 0.5 + coefs[[trt]] * 0.5 + rowSums(covariate_mat))
        predicted_ctl_geq[,i-1] <- plogis(coefs_new[1] + coefs_new[2] * -0.5 + coefs[[trt]] * -0.5 + rowSums(covariate_mat))
      }
    }
    
    
    predicted_trt_leq <-  1 - predicted_trt_geq
    predicted_ctl_leq <-  1 - predicted_ctl_geq
    
    p_y_trt_est <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
    p_y_ctl_est <- matrix(data = NA, nrow = nrow(dat), ncol = c) 
    for (i in 1:c) {
      if (i == 1) {
        p_y_ctl_est[,1] <-  predicted_ctl_leq[,1]
        p_y_trt_est[,1] <-  predicted_trt_leq[,1]
      } else if (i == 2){
        p_y_ctl_est[,2] <- predicted_ctl_leq[,i] - predicted_ctl_leq[,1]
        p_y_trt_est[,2] <- predicted_trt_leq[,i] - predicted_trt_leq[,1]
      } else if (i == c) {
        p_y_ctl_est[,c] <- 1 - rowSums(p_y_ctl_est[,1:(c-1)])
        p_y_trt_est[,c] <- 1 - rowSums(p_y_trt_est[,1:(c-1)])
      } else {
        p_y_ctl_est[,i] <-  predicted_ctl_leq[,i] - apply(p_y_ctl_est[,1:(i-1)], 1, sum)
        p_y_trt_est[,i] <-  predicted_trt_leq[,i] - apply(p_y_trt_est[,1:(i-1)], 1, sum)
      }
    }
    
    # correct for any negative probabilities 
    if (any(p_y_ctl_est < 0)) {
      p_y_ctl_est[which(p_y_ctl_est < 0)] <- 0 
      p_y_ctl_est <- p_y_ctl_est/rowSums(p_y_ctl_est)
    }
    
    if (any(p_y_trt_est < 0)) {
      p_y_trt_est[which(p_y_trt_est < 0)] <- 0 
      p_y_trt_est <- p_y_trt_est/rowSums(p_y_trt_est)
    }
    
    p_y_est <- (p_y_ctl_est + p_y_trt_est)/2
    
    # get weights from the data 
    p_y_ctl <- unname(table(dat[[trt]], dat[[outcome]])[1,])/sum(dat[[trt]] == -0.5)
    p_y_trt <- unname(table(dat[[trt]], dat[[outcome]])[2,])/sum(dat[[trt]] == 0.5)
    p_y <- as.vector(unname(table(dat[[outcome]]))/nrow(dat))
    
    weights <- t(rdirichlet(1, rep(1, nrow(dat))))
    
    p_y_ctl_leq_average <- apply(predicted_ctl_leq, 2, weighted.mean, w = weights)
    p_y_trt_leq_average <- apply(predicted_trt_leq, 2, weighted.mean, w = weights)
    p_y_ctl_geq_average <- apply(predicted_ctl_geq, 2, weighted.mean, w = weights)
    p_y_trt_geq_average <- apply(predicted_trt_geq, 2, weighted.mean, w = weights)
    
    p_y_est_average <- apply(p_y_est, 2, weighted.mean, w = weights)
    p_y_ctl_est_average <- apply(p_y_ctl_est, 2, weighted.mean, w = weights)
    p_y_ctl_est_average_save[s,] <- p_y_ctl_est_average
    p_y_trt_est_average <- apply(p_y_trt_est, 2, weighted.mean, w = weights)
    p_y_trt_est_average_save[s,] <- p_y_trt_est_average
    
    RD_leq <- p_y_trt_leq_average - p_y_ctl_leq_average
    RD_leq_save[s,] <- RD_leq
    RD_geq <- p_y_ctl_geq_average - p_y_trt_geq_average
    
    
    OR <- (p_y_trt_leq_average/ (1- p_y_trt_leq_average)) / (p_y_ctl_leq_average / (1- p_y_ctl_leq_average))
    OR_save[s,] <- OR
    log_OR_save[s,] <- log(OR)
    
    RR_leq <-  p_y_trt_leq_average / p_y_ctl_leq_average
    RR_save[s,] <- RR_leq
    log_RR_save[s,] <- log(RR_leq)
    RR_geq <- p_y_ctl_geq_average / p_y_trt_geq_average
    RR_save_geq[s,] <- RR_geq
    log_RR_ge_save[s,] <- log(RR_geq)
    
    j <- 1:(c-1)
    k <- 2:c
    
    utility_diff <- abs(utility[j] - utility[k])
    # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
    # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
    # wRD_control[s,1] <- numerator/denominator
    # 
    # #calculated weighted RD with overall probability as weights
    # numerator <- sum((p_y[j] + p_y[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
    # denominator <- sum(p_y[j]) + sum(p_y[k])
    # wRD_overall[s,1] <- numerator/denominator
    
    # # bayesian approach 
    # numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
    # denominator <- sum(p_y_ctl_est_average[j]) + sum(p_y_ctl_est_average[k])
    # wRD_bcontrol[s,1] <- numerator/denominator
    # 
    # #cumulative weights 
    # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
    # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    # wRD_cum[s,1] <- numerator/denominator
    
    #both weights 
    numerator <- sum(utility_diff * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
    denominator <- sum(utility_diff * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    wRD_both[s,1] <- numerator/denominator
    
    numerator <- sum(utility_diff * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
    denominator <- sum(utility_diff)
    wRD_util[s,1] <- numerator/denominator
    
    # #calculated weighted RD with overall probability as weights
    # numerator <- sum((p_y_est_average[j] + p_y_est_average[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
    # denominator <- sum(p_y_est_average[j]) + sum(p_y_est_average[k])
    # wRD_boverall[s,1] <- numerator/denominator
    
    # #calculate log weighted OR with control probability as weights
    # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log(OR[j]))
    # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
    # log_wOR_control[s,1] <- numerator/denominator
    # wOR_control[s,1] <- exp(numerator/denominator)
    # 
    # #calculate log weighted OR with overall probability as weights
    # numerator <- sum((p_y[j] + p_y[k])  * log(OR[j])) 
    # denominator <- sum(p_y[j]) + sum(p_y[k])
    # log_wOR_overall[s,1] <- numerator/denominator
    # wOR_overall[s,1] <- exp(numerator/denominator)
    
    # bayesian approach 
    #calculate log weighted OR with control probability as weights
    # numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * log(OR[j]))
    # denominator <- sum(p_y_ctl_est_average[j]) + sum(p_y_ctl_est_average[k])
    # log_wOR_bcontrol[s,1] <- numerator/denominator
    # wOR_bcontrol[s,1] <- exp(numerator/denominator)
    # 
    # # cumulative weights 
    # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(OR[j]))
    # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    # log_wOR_cum[s,1] <- numerator/denominator
    # wOR_cum[s,1] <- exp(numerator/denominator)
    
    # both weights 
    numerator <- sum(utility_diff * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(OR[j]))
    denominator <- sum(utility_diff * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    log_wOR_both[s,1] <- numerator/denominator
    wOR_both[s,1] <- exp(numerator/denominator)
    
    numerator <- sum(utility_diff * log(OR[j]))
    denominator <- sum(utility_diff)
    log_wOR_util[s,1] <- numerator/denominator
    
    # #calculate log weighted OR with overall probability as weights
    # numerator <- sum((p_y_est_average[j] + p_y_est_average[k]) * log(OR[j])) 
    # denominator <- sum(p_y_est_average[j]) + sum(p_y_est_average[k])
    # log_wOR_boverall[s,1] <- numerator/denominator
    # wOR_boverall[s,1] <- exp(numerator/denominator)
    
    # #calculate log weighted RR with control probability as weights
    # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log(RR_leq[j]))
    # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
    # log_wRR_control[s,1] <- numerator/denominator
    # wRR_control[s,1] <- exp(numerator/denominator)
    # 
    # #calculate log weighted RR with overall probability as weights
    # numerator <- sum((p_y[j] + p_y[k])  * log(RR_leq[j]))
    # denominator <- sum(p_y[j]) + sum(p_y[k])
    # log_wRR_overall[s,1] <- numerator/denominator
    # wRR_overall[s,1] <- exp(numerator/denominator)
    
    # bayesian approach
    #calculate log weighted RR with control probability as weights
    # numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * log(RR_leq[j]))
    # denominator <- sum(p_y_ctl_est_average[j]) + sum(p_y_ctl_est_average[k])
    # log_wRR_bcontrol[s,1] <- numerator/denominator
    # wRR_bcontrol[s,1] <- exp(numerator/denominator)
    # 
    # # cumulative weights 
    # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_leq[j]))
    # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    # log_wRR_cum[s,1] <- numerator/denominator
    # wRR_cum[s,1] <- exp(numerator/denominator)
    
    # both weights 
    numerator <- sum(utility_diff * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_leq[j]))
    denominator <- sum(utility_diff* (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    log_wRR_both[s,1] <- numerator/denominator
    wRR_both[s,1] <- exp(numerator/denominator)
    
    # both weights 
    numerator <- sum(utility_diff * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_geq[j]))
    denominator <- sum(utility_diff * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    log_wRR_ge_both[s,1] <- numerator/denominator
    wRR_ge_both[s,1] <- exp(numerator/denominator)
    
    # both weights 
    numerator <- sum(utility_diff * log(RR_leq[j]))
    denominator <- sum(utility_diff)
    log_wRR_util[s,1] <- numerator/denominator
    
    # both weights 
    numerator <- sum(utility_diff * log(RR_geq[j]))
    denominator <- sum(utility_diff)
    log_wRR_ge_util[s,1] <- numerator/denominator
    
    # #calculate log weighted RR with overall probability as weights
    # numerator <- sum((p_y_est_average[j] + p_y_est_average[k]) * log(RR_leq[j]))
    # denominator <- sum(p_y_est_average[j]) + sum(p_y_est_average[k])
    # log_wRR_boverall[s,1] <- numerator/denominator
    # wRR_boverall[s,1] <- exp(numerator/denominator)
    
    #calculate WR    
    l <- 1:c
    NB[s,1] <- sum(p_y_trt[j] * p_y_ctl_geq_average[j]) - (sum(p_y_ctl[j] * p_y_trt_geq_average[j]))
    
    weights_split[s,] <- (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) / sum(p_y_ctl_est_average[j] + p_y_ctl_est_average[k])
    weights_cum[s,] <- (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) / sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    weights_both[s,] <- (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) / 
      sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
    
  }
  
  RD_leq_95CI <- apply(RD_leq_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  RD_leq_postprob <- colSums(RD_leq_save > 0)/n_post_draws
  
  #unweighted_RD
  unweighted_RD <- rowMeans(RD_leq_save)
  unweighted_RD_95CI <- quantile(unweighted_RD, probs = c(0.025, 0.975))
  
  OR_95CI <- apply(OR_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  OR_postprob <- colSums(OR_save > 1)/n_post_draws
  
  log_OR_95CI <- apply(log_OR_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  
  #unweighted_OR
  unweighted_log_OR <- rowMeans(log_OR_save)
  unweighted_log_OR_95CI <- quantile(unweighted_log_OR, probs = c(0.025, 0.975))
  unweighted_OR_95CI <- quantile(exp(unweighted_log_OR), probs = c(0.025, 0.975))
  
  RR_leq_95CI <- apply(RR_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  RR_leq_postprob <- colSums(RR_save > 1)/n_post_draws
  
  log_RR_leq_95CI <- apply(log_RR_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  
  #unweighted_RR
  unweighted_log_RR <- rowMeans(log_RR_save)
  unweighted_log_RR_95CI <- quantile(unweighted_log_RR, probs = c(0.025, 0.975))
  unweighted_RR_95CI <- quantile(exp(unweighted_log_RR), probs = c(0.025, 0.975))
  
  #unweighted_RR
  unweighted_log_RR_ge <- rowMeans(log_RR_ge_save)
  unweighted_log_RR_ge_95CI <- quantile(unweighted_log_RR_ge, probs = c(0.025, 0.975))
  unweighted_RR_ge_95CI <- quantile(exp(unweighted_log_RR_ge), probs = c(0.025, 0.975))
  
  RR_geq_95CI <- apply(RR_save_geq, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  RR_geq_postprob <- colSums(RR_save_geq > 1)/n_post_draws
  
  log_RR_geq_95CI <- apply(log_RR_ge_save, 2, function (x) quantile(x, probs = c(0.025, 0.975)))
  
  # wRD_control_95CI <- quantile(wRD_control, probs = c(0.025, 0.975))
  # wRD_control_postprob <- sum(wRD_control > 0)/n_post_draws
  # 
  # wRD_overall_95CI <- quantile(wRD_overall, probs = c(0.025, 0.975))
  # wRD_overall_postprob <- sum(wRD_overall > 0)/n_post_draws
  
  # wRD_bcontrol_95CI <- quantile(wRD_bcontrol, probs = c(0.025, 0.975))
  # wRD_bcontrol_postprob <- sum(wRD_bcontrol > 0)/n_post_draws
  # 
  # wRD_cum_95CI <- quantile(wRD_cum, probs = c(0.025, 0.975))
  # wRD_cum_postprob <- sum(wRD_cum > 0)/n_post_draws
  
  wRD_both_95CI <- quantile(wRD_both, probs = c(0.025, 0.975))
  wRD_both_postprob <- sum(wRD_both > 0)/n_post_draws
  
  wRD_util_95CI <- quantile(wRD_util, probs = c(0.025, 0.975))
  
  # wRD_boverall_95CI <- quantile(wRD_boverall, probs = c(0.025, 0.975))
  # wRD_boverall_postprob <- sum(wRD_boverall > 0)/n_post_draws
  # 
  # wOR_control_95CI <- quantile(wOR_control, probs = c(0.025, 0.975))
  # wOR_control_postprob <- sum(wOR_control > 1)/n_post_draws
  # 
  # wOR_overall_95CI <- quantile(wOR_overall, probs = c(0.025, 0.975))
  # wOR_overall_postprob <- sum(wOR_overall > 1)/n_post_draws
  
  # wOR_bcontrol_95CI <- quantile(wOR_bcontrol, probs = c(0.025, 0.975))
  # wOR_bcontrol_postprob <- sum(wOR_bcontrol > 1)/n_post_draws
  # 
  # wOR_cum_95CI <- quantile(wOR_cum, probs = c(0.025, 0.975))
  # wOR_cum_postprob <- sum(wOR_cum > 1)/n_post_draws
  
  wOR_both_95CI <- quantile(wOR_both, probs = c(0.025, 0.975))
  wOR_both_postprob <- sum(wOR_both > 1)/n_post_draws
  
  # wOR_boverall_95CI <- quantile(wOR_boverall, probs = c(0.025, 0.975))
  # wOR_boverall_postprob <- sum(wOR_boverall > 1)/n_post_draws
  # 
  # log_wOR_control_95CI <- quantile(log_wOR_control, probs = c(0.025, 0.975))
  # log_wOR_control_postprob <- sum(log_wOR_control > 0)/n_post_draws
  # 
  # log_wOR_overall_95CI <- quantile(log_wOR_overall, probs = c(0.025, 0.975))
  # log_wOR_overall_postprob <- sum(log_wOR_overall > 0)/n_post_draws
  
  # log_wOR_bcontrol_95CI <- quantile(log_wOR_bcontrol, probs = c(0.025, 0.975))
  # log_wOR_bcontrol_postprob <- sum(log_wOR_bcontrol > 0)/n_post_draws
  # 
  # log_wOR_cum_95CI <- quantile(log_wOR_cum, probs = c(0.025, 0.975))
  # log_wOR_cum_postprob <- sum(log_wOR_cum > 0)/n_post_draws
  
  log_wOR_both_95CI <- quantile(log_wOR_both, probs = c(0.025, 0.975))
  log_wOR_both_postprob <- sum(log_wOR_both > 0)/n_post_draws
  
  log_wOR_util_95CI <- quantile(log_wOR_util, probs = c(0.025, 0.975))
  
  # log_wOR_boverall_95CI <- quantile(log_wOR_boverall, probs = c(0.025, 0.975))
  # log_wOR_boverall_postprob <- sum(log_wOR_boverall > 0)/n_post_draws
  # 
  # wRR_control_95CI <- quantile(wRR_control, probs = c(0.025, 0.975))
  # wRR_control_postprob <- sum(wRR_control > 1)/n_post_draws
  # 
  # wRR_overall_95CI <- quantile(wRR_overall, probs = c(0.025, 0.975))
  # wRR_overall_postprob <- sum(wRR_overall > 1)/n_post_draws
  # 
  # log_wRR_control_95CI <- quantile(log_wRR_control, probs = c(0.025, 0.975))
  # log_wRR_control_postprob <- sum(log_wRR_control > 0)/n_post_draws
  # 
  # log_wRR_overall_95CI <- quantile(log_wRR_overall, probs = c(0.025, 0.975))
  # log_wRR_overall_postprob <- sum(log_wRR_overall > 0)/n_post_draws
  
  # wRR_bcontrol_95CI <- quantile(wRR_bcontrol, probs = c(0.025, 0.975))
  # wRR_bcontrol_postprob <- sum(wRR_bcontrol > 1)/n_post_draws
  # 
  # wRR_cum_95CI <- quantile(wRR_cum, probs = c(0.025, 0.975))
  # wRR_cum_postprob <- sum(wRR_cum > 1)/n_post_draws
  
  wRR_both_95CI <- quantile(wRR_both, probs = c(0.025, 0.975))
  wRR_both_postprob <- sum(wRR_both > 1)/n_post_draws
  
  wRR_ge_both_95CI <- quantile(wRR_ge_both, probs = c(0.025, 0.975))
  wRR_ge_both_postprob <- sum(wRR_ge_both > 1)/n_post_draws
  
  # wRR_boverall_95CI <- quantile(wRR_boverall, probs = c(0.025, 0.975))
  # wRR_boverall_postprob <- sum(wRR_boverall > 1)/n_post_draws
  
  # log_wRR_bcontrol_95CI <- quantile(log_wRR_bcontrol, probs = c(0.025, 0.975))
  # log_wRR_bcontrol_postprob <- sum(log_wRR_bcontrol > 0)/n_post_draws
  # 
  # log_wRR_cum_95CI <- quantile(log_wRR_cum, probs = c(0.025, 0.975))
  # log_wRR_cum_postprob <- sum(log_wRR_cum > 0)/n_post_draws
  
  log_wRR_both_95CI <- quantile(log_wRR_both, probs = c(0.025, 0.975))
  log_wRR_both_postprob <- sum(log_wRR_both > 0)/n_post_draws
  
  log_wRR_ge_both_95CI <- quantile(log_wRR_ge_both, probs = c(0.025, 0.975))
  log_wRR_ge_both_postprob <- sum(log_wRR_ge_both > 0)/n_post_draws
  
  log_wRR_util_95CI <- quantile(log_wRR_util, probs = c(0.025, 0.975))
  log_wRR_ge_util_95CI <- quantile(log_wRR_ge_util, probs = c(0.025, 0.975))
  
  # log_wRR_boverall_95CI <- quantile(log_wRR_boverall, probs = c(0.025, 0.975))
  # log_wRR_boverall_postprob <- sum(log_wRR_boverall > 0)/n_post_draws
  
  NB_95CI <- quantile(NB, probs = c(0.025, 0.975))
  NB_postprob <- sum(NB > 0)/n_post_draws
  
  weights_split_95CI <- apply(weights_split, 2, quantile, probs = c(0.025, 0.975))
  weights_cum_95CI <- apply(weights_cum, 2, quantile, probs = c(0.025, 0.975))
  weights_both_95CI <- apply(weights_both, 2, quantile, probs = c(0.025, 0.975))
  
  
  if (c == 4) {
    RD_leq_95CI = c(RD_leq_95CI, NA, NA)
    OR_95CI = c(OR_95CI, NA, NA)
    log_OR_95CI = c(log_OR_95CI, NA, NA)
    RR_leq_95CI = c(RR_leq_95CI, NA, NA)
    log_RR_leq_95CI = c(log_RR_leq_95CI, NA, NA) 
    RR_geq_95CI = c(RR_geq_95CI, NA, NA)
    log_RR_geq_95CI = c(log_RR_geq_95CI, NA, NA)
    weights_split_95CI = c(weights_split_95CI, NA, NA)
    weights_cum_95CI = c(weights_cum_95CI, NA, NA)
    weights_both_95CI = c(weights_both_95CI, NA, NA)
  }
  
  return(c(RD_leq_95CI = RD_leq_95CI, 
              # RD_leq_postprob = RD_leq_postprob, 
              OR_95CI = OR_95CI, 
              log_OR_95CI = log_OR_95CI, 
              # OR_postprob = OR_postprob, 
              RR_leq_95CI = RR_leq_95CI, 
              log_RR_leq_95CI = log_RR_leq_95CI, 
              # RR_leq_postprob = RR_leq_postprob, 
              RR_geq_95CI = RR_geq_95CI,
              log_RR_geq_95CI = log_RR_geq_95CI,
              # wRD_control_95CI = wRD_control_95CI, 
              # wRD_control_postprob = wRD_control_postprob, 
              # wRD_overall_95CI = wRD_overall_95CI, 
              # wRD_overall_postprob = wRD_overall_postprob, 
              # wRD_bcontrol_95CI = wRD_bcontrol_95CI, 
              # wRD_bcontrol_postprob = wRD_bcontrol_postprob, 
              # wRD_cum_95CI = wRD_cum_95CI, 
              # wRD_cum_postprob = wRD_cum_postprob,
              wRD_both_95CI = wRD_both_95CI, 
              wRD_both_postprob = wRD_both_postprob,
              wRD_util_95CI = wRD_util_95CI, 
              # wRD_boverall_95CI = wRD_boverall_95CI, 
              # wRD_boverall_postprob = wRD_boverall_postprob, 
              # wOR_control_95CI = wOR_control_95CI, 
              # wOR_control_postprob = wOR_control_postprob, 
              # wOR_overall_95CI = wOR_overall_95CI, 
              # wOR_overall_postprob = wOR_overall_postprob, 
              # log_wOR_control_95CI = log_wOR_control_95CI, 
              # log_wOR_control_postprob = log_wOR_control_postprob, 
              # log_wOR_overall_95CI = log_wOR_overall_95CI, 
              # log_wOR_overall_postprob = log_wOR_overall_postprob, 
              # wOR_bcontrol_95CI = wOR_bcontrol_95CI,
              # wOR_bcontrol_postprob = wOR_bcontrol_postprob,
              # wOR_cum_95CI = wOR_cum_95CI,
              # wOR_cum_postprob = wOR_cum_postprob,
              wOR_both_95CI = wOR_both_95CI,
              wOR_both_postprob = wOR_both_postprob,
              # wOR_boverall_95CI = wOR_boverall_95CI, 
              # wOR_boverall_postprob = wOR_boverall_postprob, 
              # log_wOR_bcontrol_95CI = log_wOR_bcontrol_95CI, 
              # log_wOR_bcontrol_postprob = log_wOR_bcontrol_postprob, 
              # log_wOR_cum_95CI = log_wOR_cum_95CI, 
              # log_wOR_cum_postprob = log_wOR_cum_postprob, 
              log_wOR_both_95CI = log_wOR_both_95CI, 
              log_wOR_both_postprob = log_wOR_both_postprob, 
              log_wOR_util_95CI = log_wOR_util_95CI, 
              # log_wOR_boverall_95CI = log_wOR_boverall_95CI, 
              # log_wOR_boverall_postprob = log_wOR_boverall_postprob, 
              # wRR_control_95CI = wRR_control_95CI, 
              # wRR_control_postprob = wRR_control_postprob, 
              # wRR_overall_95CI = wRR_overall_95CI, 
              # wRR_overall_postprob = wRR_overall_postprob, 
              # log_wRR_control_95CI = log_wRR_control_95CI, 
              # log_wRR_control_postprob = log_wRR_control_postprob, 
              # log_wRR_overall_95CI = log_wRR_overall_95CI, 
              # log_wRR_overall_postprob = log_wRR_overall_postprob, 
              # wRR_bcontrol_95CI = wRR_bcontrol_95CI, 
              # wRR_bcontrol_postprob = wRR_bcontrol_postprob, 
              # wRR_cum_95CI = wRR_cum_95CI, 
              # wRR_cum_postprob = wRR_cum_postprob,
              wRR_both_95CI = wRR_both_95CI, 
              wRR_both_postprob = wRR_both_postprob,
              wRR_ge_both_95CI = wRR_ge_both_95CI, 
              wRR_ge_both_postprob = wRR_ge_both_postprob,
              # wRR_boverall_95CI = wRR_boverall_95CI, 
              # wRR_boverall_postprob = wRR_boverall_postprob, 
              # log_wRR_bcontrol_95CI = log_wRR_bcontrol_95CI,
              # log_wRR_bcontrol_postprob = log_wRR_bcontrol_postprob,
              # log_wRR_cum_95CI = log_wRR_cum_95CI,
              # log_wRR_cum_postprob = log_wRR_cum_postprob,
              log_wRR_both_95CI = log_wRR_both_95CI,
              log_wRR_both_postprob = log_wRR_both_postprob,
              log_wRR_ge_both_95CI = log_wRR_ge_both_95CI, 
              log_wRR_ge_both_postprob = log_wRR_ge_both_postprob, 
              log_wRR_util_95CI = log_wRR_util_95CI,
              log_wRR_ge_util_95CI = log_wRR_ge_util_95CI, 
              # log_wRR_boverall_95CI = log_wRR_boverall_95CI, 
              # log_wRR_boverall_postprob = log_wRR_boverall_postprob, 
              unweighted_RD_95CI = unweighted_RD_95CI,
              unweighted_log_OR_95CI = unweighted_log_OR_95CI,
              unweighted_OR_95CI = unweighted_OR_95CI,
              unweighted_log_RR_95CI = unweighted_log_RR_95CI,
              unweighted_RR_95CI = unweighted_RR_95CI,
              unweighted_log_RR_ge_95CI = unweighted_log_RR_ge_95CI,
              unweighted_RR_ge_95CI = unweighted_RR_ge_95CI,
              NB_95CI = NB_95CI, 
              NB_postprob = NB_postprob,
              weights_split_95CI = weights_split_95CI,
              weights_cum_95CI = weights_cum_95CI,
              weights_both_95CI = weights_both_95CI))
  
}




blrm_adj_bb_PO_est <- function(b = b, dat, trt_variable,
                            outcome, utility) { 
  c <- length(b$ylevels)
  dat_trt <- dat 
  dat_trt[[trt_variable]] <- 0.5
  
  dat_ctl <- dat 
  dat_ctl[[trt_variable]] <- -0.5
  
  predicted_trt_geq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  predicted_ctl_geq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  predicted_trt_leq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  predicted_ctl_leq <- matrix(data = NA, nrow = nrow(dat), ncol = c-1) 
  
  p_y_ctl_est <- matrix(data = NA, nrow = nrow(dat), ncol = c)
  p_y_trt_est <- matrix(data = NA, nrow = nrow(dat), ncol = c)
  
  #predicted_trt_geq and predicted_ctl_geq are in the format where the first column is the P(y >= 1 |trt, covariates)
  #predicted_trt_leq and predicted_ctl_leq are in the format where the first column is the P(y <= 1 |trt, covariates)
  for (i in 1:(c-1)){
    predicted_trt_geq[,i] <-  plogis(predict(b, dat_trt, kint = i)$linear.predictors)
    predicted_ctl_geq[,i] <-  plogis(predict(b, dat_ctl, kint = i)$linear.predictors)
  }
  
  predicted_trt_leq <-  1 - predicted_trt_geq
  predicted_ctl_leq <-  1 - predicted_ctl_geq
  
  # calculate p(Y = i | control) for i = 1, ... , c
  for (i in 1:c) {
    if (i == 1) {
      p_y_ctl_est[,1] <-  predicted_ctl_leq[,1]
      p_y_trt_est[,1] <-  predicted_trt_leq[,1]
    } else if (i == 2) {
      p_y_ctl_est[,i] <-  predicted_ctl_leq[,i] - p_y_ctl_est[,1]
      p_y_trt_est[,i] <-  predicted_trt_leq[,i] - p_y_trt_est[,1]
    } else if (i == c) {
      p_y_ctl_est[,c] <- 1 - rowSums(p_y_ctl_est[,1:(c-1)])
      p_y_trt_est[,c] <- 1 - rowSums(p_y_trt_est[,1:(c-1)])
    } else {
      p_y_ctl_est[,i] <-  predicted_ctl_leq[,i] - rowSums(p_y_ctl_est[,1:(i-1)])
      p_y_trt_est[,i] <-  predicted_trt_leq[,i] - rowSums(p_y_trt_est[,1:(i-1)])
    }
  }
  
  p_y_est <- (p_y_ctl_est + p_y_trt_est)/2
  
  
  # get weights from the data 
  p_y_ctl <- unname(table(dat[[trt_variable]], dat[[outcome]])[1,])/sum(dat[[trt_variable]] == -0.5)
  p_y_trt <- unname(table(dat[[trt_variable]], dat[[outcome]])[2,])/sum(dat[[trt_variable]] == 0.5)
  p_y <- unname(table(dat[[outcome]]))/nrow(dat)
  
  weights <- t(rdirichlet(1, rep(1, nrow(dat))))
  
  p_y_ctl_leq_average <- apply(predicted_ctl_leq, 2, weighted.mean, w = weights)
  p_y_trt_leq_average <- apply(predicted_trt_leq, 2, weighted.mean, w = weights)
  p_y_ctl_geq_average <- apply(predicted_ctl_geq, 2, weighted.mean, w = weights)
  p_y_trt_geq_average <- apply(predicted_trt_geq, 2, weighted.mean, w = weights)
  
  p_y_ctl_est_average <- apply(p_y_ctl_est, 2, weighted.mean, w = weights)
  p_y_est_average <- apply(p_y_est, 2, weighted.mean, w = weights)
  
  RD_leq <- p_y_trt_leq_average - p_y_ctl_leq_average
  unweighted_RD <- mean(RD_leq)
  #unweighted_RD <- weighted.mean(unweighted_RD_all, w = weights)
  RD_geq <- p_y_ctl_geq_average - p_y_trt_geq_average
  
  
  OR <- (p_y_trt_leq_average / (1- p_y_trt_leq_average)) / (p_y_ctl_leq_average / (1- p_y_ctl_leq_average))
  unweighted_log_OR <- mean(log(OR))
  unweighted_OR <- exp(mean(log(OR)))
  
  RR_leq <- p_y_trt_leq_average / p_y_ctl_leq_average
  unweighted_log_RR <- mean(log(RR_leq))
  unweighted_RR <- exp(mean(log(RR_leq)))
  RR_geq <- p_y_ctl_geq_average / p_y_trt_geq_average
  unweighted_log_RR_ge <- mean(log(RR_geq))
  unweighted_RR_ge <- exp(mean(log(RR_geq)))
  
  j <- 1:(c-1)
  k <- 2:c
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k])  * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # weighted_RD <- numerator/denominator
  
  
  #calculated weighted RD with overall probability as weights
  # numerator <- sum((p_y[j] + p_y[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # weighted_RD_overall <- numerator/denominator
  
  # fully bayesian approach
  # numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum(p_y_ctl_est_average[j]) + sum(p_y_ctl_est_average[k])
  # weighted_RD_b <- numerator/denominator
  
  # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  # weighted_RD_cum <- numerator/denominator
  
  numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  denominator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weighted_RD_both <- numerator/denominator 
  
  numerator <- sum((abs(utility[j] - utility[k])) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j]))
  denominator <- sum((abs(utility[j] - utility[k])))
  weighted_RD_util <- numerator/denominator 
  
  #calculated weighted RD with overall probability as weights
  # numerator <- sum((p_y_est_average[j] + p_y_est_average[k]) * (p_y_trt_leq_average[j] - p_y_ctl_leq_average[j])) 
  # denominator <- sum(p_y_est_average[j]) + sum(p_y_est_average[k])
  # weighted_RD_boverall <- numerator/denominator
  
  #calculate WR    
  l <- 1:c
  WP <- sum(p_y_trt[j] * p_y_ctl_geq_average[j]) + 0.5* sum(p_y_trt[l] * p_y_ctl[l])
  WR <- WP/(1-WP)
  
  
  
  #calculate weighted OR with control probability as weights
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k])  * log(OR[j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # log_weighted_OR <- numerator/denominator
  # weighted_OR <- exp(numerator/denominator)
  # 
  # #calculate weighted OR with overall probability as weights
  # numerator <- sum((p_y[j] + p_y[k])  * log(OR[j])) 
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # log_weighted_OR_overall <- numerator/denominator
  # weighted_OR_overall <- exp(numerator/denominator)
  
  #fully bayesian approach
  #calculate weighted OR with control probability as weights
  # numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * log(OR[j]))
  # denominator <- sum(p_y_ctl_est_average[j]) + sum(p_y_ctl_est_average[k])
  # log_weighted_OR_b <- numerator/denominator
  # weighted_OR_b <- exp(numerator/denominator)
  # 
  # # cumulative weights
  # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(OR[j]))
  # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  # log_weighted_OR_cum <- numerator/denominator
  # weighted_OR_cum <- exp(numerator/denominator)
  
  # both weights
  numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(OR[j]))
  denominator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (abs(utility[j] - utility[k])) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  log_weighted_OR_both <- numerator/denominator
  weighted_OR_both <- exp(numerator/denominator)
  
  numerator <- sum((abs(utility[j] - utility[k])) * log(OR[j]))
  denominator <- sum((abs(utility[j] - utility[k])))
  log_weighted_OR_util <- numerator/denominator
  weighted_OR_util <- exp(numerator/denominator)
  
  
  # #calculate weighted OR with overall probability as weights
  # numerator <- sum((p_y_est_average[j] + p_y_est_average[k]) * log(OR[j])) 
  # denominator <- sum(p_y_est_average[j]) + sum(p_y_est_average[k])
  # log_weighted_OR_boverall <- numerator/denominator
  # weighted_OR_boverall <- exp(numerator/denominator)
  
  
  # #calculate weighted RR with control probability as weights
  # numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log(RR_leq[j]))
  # denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  # log_weighted_RR <- numerator/denominator
  # weighted_RR <- exp(numerator/denominator)
  
  #calculate weighted RR with overall probability as weights
  # numerator <- sum((p_y[j] + p_y[k])  * log(RR_leq[j]))
  # denominator <- sum(p_y[j]) + sum(p_y[k])
  # log_weighted_RR_overall <- numerator/denominator
  # weighted_RR_overall <- exp(numerator/denominator)
  
  # fully bayesian approach
  #calculate weighted RR with control probability as weights
  # numerator <- sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * log(RR_leq[j]))
  # denominator <- sum(p_y_ctl_est_average[j]) + sum(p_y_ctl_est_average[k])
  # log_weighted_RR_b <- numerator/denominator
  # weighted_RR_b <- exp(numerator/denominator)
  # 
  # # cumulative weights 
  # numerator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_leq[j]))
  # denominator <- sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  # log_weighted_RR_cum <- numerator/denominator
  # weighted_RR_cum <- exp(numerator/denominator)
  
  # both weights
  numerator <- sum((abs(utility[j] - utility[k])) * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_leq[j]))
  denominator <- sum((abs(utility[j] - utility[k])) * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  log_weighted_RR_both <- numerator/denominator
  weighted_RR_both <- exp(numerator/denominator)
  
  # both weights RR greater than or equal to 
  numerator <- sum((abs(utility[j] - utility[k])) * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) * log(RR_geq[j]))
  denominator <- sum((abs(utility[j] - utility[k])) * (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  log_weighted_RR_ge_both <- numerator/denominator
  weighted_RR_ge_both <- exp(numerator/denominator)
  
  # both weights
  numerator <- sum((abs(utility[j] - utility[k])) * log(RR_leq[j]))
  denominator <- sum((abs(utility[j] - utility[k])))
  log_weighted_RR_util <- numerator/denominator
  weighted_RR_util <- exp(numerator/denominator)
  
  # both weights RR greater than or equal to 
  numerator <- sum((abs(utility[j] - utility[k])) * log(RR_geq[j]))
  denominator <- sum((abs(utility[j] - utility[k])))
  log_weighted_RR_ge_util <- numerator/denominator
  weighted_RR_ge_util <- exp(numerator/denominator)
  
  #calculate weighted RR with overall probability as weights
  # numerator <- sum((p_y_est[j] + p_y_est[k]) * log(RR_leq[j]))
  # denominator <- sum(p_y_est[j]) + sum(p_y_est[k])
  # log_weighted_RR_boverall <- numerator/denominator
  # weighted_RR_boverall <- exp(numerator/denominator)
  
  #calculate WR    
  l <- 1:c
  NB <- sum(p_y_trt[j] * p_y_ctl_geq_average[j]) - (sum(p_y_ctl[j] * p_y_trt_geq_average[j]))
  
  #weights 
  weights_split <- (p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) / sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]))
  weights_cum <- (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])) / sum((p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  weights_both <- ((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j]))) / 
    sum((p_y_ctl_est_average[j] + p_y_ctl_est_average[k]) * (p_y_ctl_leq_average[j] * (1 - p_y_ctl_leq_average[j])))
  
  if (c == 4) {
    p_y_ctl = c(p_y_ctl, NA)
    p_y_trt = c(p_y_trt, NA)
    RD_leq = c(RD_leq, NA)
    RD_geq = c(RD_geq, NA)
    RR_leq = c(RR_leq, NA)
    RR_geq = c(RR_geq, NA)
    OR = c(OR, NA)
    weights_split = c(weights_split, NA)
    weights_cum = c(weights_cum, NA)
    weights_both = c(weights_both, NA)
  }
  
  return(c(p_y_ctl = p_y_ctl, 
              p_y_trt = p_y_trt,
              RD_leq = RD_leq,
              RD_geq = RD_geq,
              OR = OR,
              RR_leq = RR_leq,
              RR_geq = RR_geq,
              log_OR = log(OR),
              log_RR_leq = log(RR_leq),
              # weighted_RD = weighted_RD, 
              # weighted_RD_overall = weighted_RD_overall,
              # weighted_OR = weighted_OR,
              # log_weighted_OR = log_weighted_OR,
              # weighted_OR_overall =  weighted_OR_overall,
              # log_weighted_OR_overall = log_weighted_OR_overall,
              # weighted_RR = weighted_RR,
              # log_weighted_RR = log_weighted_RR,
              # weighted_RR_overall =  weighted_RR_overall,
              # log_weighted_RR_overall = log_weighted_RR_overall,
              # weighted_RD_b = weighted_RD_b, 
              # weighted_RD_cum = weighted_RD_cum, 
              weighted_RD_both = weighted_RD_both, 
              weighted_RD_util = weighted_RD_util, 
              # weighted_RD_boverall = weighted_RD_boverall,
              # weighted_OR_b = weighted_OR_b,
              # log_weighted_OR_b = log_weighted_OR_b,
              # weighted_OR_cum = weighted_OR_cum,
              # log_weighted_OR_cum = log_weighted_OR_cum,
              weighted_OR_both = weighted_OR_both,
              log_weighted_OR_both = log_weighted_OR_both,
              weighted_OR_util = weighted_OR_util,
              log_weighted_OR_util = log_weighted_OR_util,
              # weighted_OR_boverall =  weighted_OR_boverall,
              # log_weighted_OR_boverall = log_weighted_OR_boverall,
              # weighted_RR_b = weighted_RR_b,
              # log_weighted_RR_b = log_weighted_RR_b,
              # weighted_RR_cum = weighted_RR_cum,
              # log_weighted_RR_cum = log_weighted_RR_cum,
              weighted_RR_both = weighted_RR_both,
              log_weighted_RR_both = log_weighted_RR_both,
              weighted_RR_ge_both = weighted_RR_ge_both,
              log_weighted_RR_ge_both = log_weighted_RR_ge_both,
              weighted_RR_util = weighted_RR_util,
              log_weighted_RR_util = log_weighted_RR_util,
              weighted_RR_ge_util = weighted_RR_ge_util,
              log_weighted_RR_ge_util = log_weighted_RR_ge_util,
              # weighted_RR_boverall =  weighted_RR_boverall,
              # log_weighted_RR_boverall = log_weighted_RR_boverall,
              unweighted_RD = unweighted_RD,
              unweighted_log_OR = unweighted_log_OR,
              unweighted_OR = unweighted_OR,
              unweighted_log_RR = unweighted_log_RR,
              unweighted_RR = unweighted_RR,
              unweighted_log_RR_ge = unweighted_log_RR_ge,
              unweighted_RR_ge = unweighted_RR_ge,
              NB = NB,
              WR = WR,
              weights_split = weights_split,
              weights_cum = weights_cum,
              weights_both = weights_both))
  
}














# functions for partial proportional odds model, proportional odds model, polr model
# functions for both point estimates and confidence intervals

# new function for weighted risk difference 
# assumes a partial proportional odds model 
# assumes the treatment is coded as -0.5 for control and 0.5 for treatment 
order_weighted_RD2 <- function(b = b, iprior = 1, dat) { 
  # first calculate P(y >= i | ctl) and P(y >= i | trt) for all i 
  PPO_ctl_geq <- NA 
  PPO_trt_geq <- NA 
  PPO_ctl_leq <- NA 
  PPO_trt_leq <- NA 
  p_y_ctl <- NA
  p_y_trt <- NA
  p_y_ctl_est <- NA
  p_y_trt_est <- NA
  p_y <- NA
  c <- length(b$ylevels)
  
  if (iprior == 1) {
    new_coef <- c(-coef(b)[1:(c-1)], coef(b)[c:length(coef(b))])
  } else {
    new_coef <- coef(b)
  }
  
  for (i in 2:c) {
    # for the first level there is not a partial term, so only need first term and metformin term 
    if (i == 2) {
      PPO_ctl_geq[i] <- plogis(new_coef[1] - 0.5 * new_coef[c]) 
      PPO_trt_geq[i] <- plogis(new_coef[1] + 0.5 * new_coef[c])
      PPO_ctl_leq[i - 1] <- 1 - PPO_ctl_geq[i]
      PPO_trt_leq[i - 1] <- 1 - PPO_trt_geq[i]
    } else{
      phrase <- paste0("y>=", i)
      coefs <- new_coef[str_detect(names(new_coef), phrase)]
      PPO_ctl_geq[i] <- plogis(coefs[1] - 0.5 * coefs[2] - 0.5 * coef(b)[c]) 
      PPO_trt_geq[i] <- plogis(coefs[1] + 0.5 * coefs[2] + 0.5 * coef(b)[c])
      PPO_ctl_leq[i - 1] <- 1 - PPO_ctl_geq[i]
      PPO_trt_leq[i - 1] <- 1 - PPO_trt_geq[i]
    }
  }
  
  PPO_leq <- (PPO_ctl_leq + PPO_trt_leq)/2
  
  # calculate p(Y = i | control) for i = 1, ... , c
  for (i in 1:c) {
    if (i == 1) {
      p_y_ctl_est[1] <-  PPO_ctl_leq[1]
      p_y_trt_est[1] <-  PPO_trt_leq[1]
    } else if (i == c) {
      p_y_ctl_est[c] <- 1 - sum(p_y_ctl_est[1:(c-1)])
      p_y_trt_est[c] <- 1 - sum(p_y_trt_est[1:(c-1)])
    } else {
      p_y_ctl_est[i] <-  PPO_ctl_leq[i] - sum(p_y_ctl_est[1:(i-1)])
      p_y_trt_est[i] <-  PPO_trt_leq[i] - sum(p_y_trt_est[1:(i-1)])
    }
  }
  
  p_y_est <- (p_y_ctl_est + p_y_trt_est)/2
  
  # get weights from the data 
  p_y_ctl <- unname(table(dat$trt, dat$y)[1,])/500
  p_y_trt <- unname(table(dat$trt, dat$y)[2,])/500
  p_y <- unname(table(dat$y))/1000
  
  
  RD_leq <- PPO_trt_leq - PPO_ctl_leq
  RD_geq <- PPO_ctl_geq - PPO_trt_geq
  
  # put control in the numerator so the OR is greater than one if metformin has a positive effect
  OR_le <- (PPO_trt_leq / (1- PPO_trt_leq)) / (PPO_ctl_leq / (1- PPO_ctl_leq))
  log_OR_le <- log((PPO_trt_leq / (1- PPO_trt_leq)) / (PPO_ctl_leq / (1- PPO_ctl_leq)))
  OR_ge <- (PPO_ctl_geq / (1- PPO_ctl_geq)) / (PPO_trt_geq / (1- PPO_trt_geq))
  # use this for the iprior = 1: (PPO_ctl_geq / (1- PPO_ctl_geq))/(PPO_trt_geq / (1 - PPO_trt_geq))
  
  # calculate RR 
  log_RR_le <- log(PPO_trt_leq/PPO_ctl_leq)
  log_RR_ge <- log(PPO_ctl_geq/PPO_trt_geq)
  
  j <- 1:(c-1)
  k <- 2:c
  numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  weighted_RD <- numerator/denominator
  
  
  numerator <- sum((p_y[j] + p_y[k]) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  denominator <- sum(p_y[j]) + sum(p_y[k])
  weighted_RD_overall <- numerator/denominator
  
  # fully bayesian approach
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  denominator <- sum(p_y_ctl_est[j]) + sum(p_y_ctl_est[k])
  weighted_RD_b <- numerator/denominator
  
  
  numerator <- sum((p_y_est[j] + p_y_est[k]) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  denominator <- sum(p_y_est[j]) + sum(p_y_est[k])
  weighted_RD_boverall <- numerator/denominator
  
  unweighted_RD <- mean(PPO_trt_leq - PPO_ctl_leq)
  
  # fully bayesian approach with cumulative weights 
  numerator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  denominator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_RD_cum <- numerator/denominator
  
  # fully bayesian approach with both weights 
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  denominator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_RD_both <- numerator/denominator
  
  # fully bayesian approach with both weights with overall prob
  numerator <- sum((p_y_est[j] + p_y_est[k]) * (PPO_leq[j] * (1-PPO_leq[j])) * (PPO_trt_leq[j] - PPO_ctl_leq[j]))
  denominator <- sum((p_y_est[j] + p_y_est[k]) * (PPO_leq[j] * (1-PPO_leq[j])))
  weighted_RD_both_ov <- numerator/denominator
  
  #calculate weighted OR 
  numerator <- sum((p_y_ctl[j] + p_y_ctl[k])  * log(OR_le[j]))
  denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  weighted_OR <- exp(numerator/denominator)
  weighted_log_OR <- numerator/denominator
  
  #calculate weighted OR using overall probability as weights
  numerator <- sum((p_y[j] + p_y[k]) * log(OR_le[j]))
  denominator <- sum(p_y[j]) + sum(p_y[k])
  weighted_OR_overall <- exp(numerator/denominator)
  weighted_log_OR_overall <- numerator/denominator
  
  # fully bayesian approach 
  #calculate weighted OR 
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k])  * log(OR_le[j]))
  denominator <- sum(p_y_ctl_est[j]) + sum(p_y_ctl_est[k])
  weighted_OR_b <- exp(numerator/denominator)
  weighted_log_OR_b <- numerator/denominator
  
  #calculate weighted OR using overall probability as weights
  numerator <- sum((p_y_est[j] + p_y_est[k]) * log(OR_le[j]))
  denominator <- sum(p_y_est[j]) + sum(p_y_est[k])
  weighted_OR_boverall <- exp(numerator/denominator)
  weighted_log_OR_boverall <- numerator/denominator
  
  # fully bayesian with only cumulative weights
  numerator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * log(OR_le[j]))
  denominator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_OR_cum <- exp(numerator/denominator)
  weighted_log_OR_cum <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * log(OR_le[j]))
  denominator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_OR_both <- exp(numerator/denominator)
  weighted_log_OR_both <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((p_y_est[j] + p_y_est[k]) * (PPO_leq[j] * (1-PPO_leq[j])) * log(OR_le[j]))
  denominator <- sum((p_y_est[j] + p_y_est[k]) * (PPO_leq[j] * (1-PPO_leq[j])))
  weighted_OR_both_ov <- exp(numerator/denominator)
  weighted_log_OR_both_ov <- numerator/denominator
  
  unweighted_log_OR <- mean(log(OR_le))
  unweighted_OR <- exp(mean(log(OR_le)))
  
  # calculate weighted RR with control probabilities 
  numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log_RR_le[j])
  denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  weighted_log_RR_control <- numerator/denominator
  
  # calculate weighted RR with overall probabilities 
  numerator <- sum((p_y[j] + p_y[k]) * log_RR_le[j]) 
  denominator <- sum(p_y[j]) + sum(p_y[k])
  weighted_log_RR_overall <- numerator/denominator
  
  # fully bayesian approach 
  # calculate weighted RR with control probabilities 
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * log_RR_le[j])
  denominator <- sum(p_y_ctl_est[j]) + sum(p_y_ctl_est[k])
  weighted_log_RR_bcontrol <- numerator/denominator
  
  # calculate weighted RR with overall probabilities 
  numerator <- sum((p_y_est[j] + p_y_est[k]) * log_RR_le[j]) 
  denominator <- sum(p_y_est[j]) + sum(p_y_est[k])
  weighted_log_RR_boverall <- numerator/denominator
  
  # fully bayesian with only cumulative weights
  numerator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * log_RR_le[j])
  denominator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_RR_cum <- exp(numerator/denominator)
  weighted_log_RR_cum <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * log_RR_le[j])
  denominator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_RR_both <- exp(numerator/denominator)
  weighted_log_RR_both <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((p_y_est[j] + p_y_est[k]) * (PPO_leq[j] * (1-PPO_leq[j])) * log_RR_le[j])
  denominator <- sum((p_y_est[j] + p_y_est[k]) * (PPO_leq[j] * (1-PPO_leq[j])))
  weighted_RR_both_ov <- exp(numerator/denominator)
  weighted_log_RR_both_ov <- numerator/denominator
  
  ## for relative risk also look at using log_RR_ge 
  # fully bayesian approach 
  # calculate weighted RR with control probabilities 
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * log_RR_ge[k])
  denominator <- sum(p_y_ctl_est[j]) + sum(p_y_ctl_est[k])
  weighted_log_RR_bcontrol_ge <- numerator/denominator
  
  # calculate weighted RR with overall probabilities 
  numerator <- sum((p_y_est[j] + p_y_est[k]) * log_RR_ge[k]) 
  denominator <- sum(p_y_est[j]) + sum(p_y_est[k])
  weighted_log_RR_boverall_ge <- numerator/denominator
  
  # fully bayesian with only cumulative weights
  numerator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * log_RR_ge[k])
  denominator <- sum((PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_RR_cum_ge <- exp(numerator/denominator)
  weighted_log_RR_cum_ge <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])) * log_RR_ge[k])
  denominator <- sum((p_y_ctl_est[j] + p_y_ctl_est[k]) * (PPO_ctl_leq[j] * (1-PPO_ctl_leq[j])))
  weighted_RR_both_ge <- exp(numerator/denominator)
  weighted_log_RR_both_ge <- numerator/denominator
  
  # fully bayesian with both cumulative and split weights
  numerator <- sum((p_y_est[j] + p_y_est[k]) * (PPO_leq[j] * (1-PPO_leq[j])) * log_RR_ge[k])
  denominator <- sum((p_y_est[j] + p_y_est[k]) * (PPO_leq[j] * (1-PPO_leq[j])))
  weighted_RR_both_ov_ge <- exp(numerator/denominator)
  weighted_log_RR_both_ov_ge <- numerator/denominator
  
  unweighted_log_RR <- mean(log_RR_le)
  unweighted_RR <- exp(mean(log_RR_le))
  
  unweighted_log_RR_ge <- mean(log_RR_ge, na.rm = T)
  unweighted_RR_ge <- exp(mean(log_RR_ge, na.rm = T))
  
  
  # calculate NB 
  NB <- sum(p_y_trt_est[j] * PPO_ctl_geq[j+1]) - (sum(p_y_ctl_est[j] * PPO_trt_geq[j+1]))
  
  
  return(c(#p_y_ctl = p_y_ctl,
    #             p_y_trt = p_y_trt,
    #             RD_leq = RD_leq, 
    #             RD_geq = RD_geq, 
    #             OR = OR_le, 
    #             log_OR = log_OR_le,
    weighted_RD_control_PPO = weighted_RD, 
    weighted_RD_overall_PPO = weighted_RD_overall,
    weighted_RD_cum_PPO = weighted_RD_cum,
    weighted_RD_both_PPO = weighted_RD_both,
    weighted_RD_both_ov_PPO = weighted_RD_both_ov,
    unweighted_RD_PPO = unweighted_RD,
    weighted_OR_control_PPO = weighted_OR,
    weighted_OR_overall_PPO = weighted_OR_overall,
    weighted_log_OR_overall_PPO = weighted_log_OR_overall,
    weighted_log_OR_control_PPO = weighted_log_OR,
    unweighted_log_OR_PPO = unweighted_log_OR,
    unweighted_OR_PPO = unweighted_OR,
    weighted_log_RR_control_PPO = weighted_log_RR_control, 
    weighted_log_RR_overall_PPO = weighted_log_RR_overall,
    unweighted_log_RR_PPO = unweighted_log_RR,
    unweighted_RR_PPO = unweighted_RR,
    weighted_RD_bcontrol_PPO = weighted_RD_b, 
    weighted_RD_boverall_PPO = weighted_RD_boverall,
    weighted_OR_bcontrol_PPO = weighted_OR_b,
    weighted_OR_boverall_PPO = weighted_OR_boverall,
    weighted_OR_cum_PPO = weighted_OR_cum,
    weighted_OR_both_PPO = weighted_OR_both,
    weighted_OR_both_ov_PPO = weighted_OR_both_ov,
    weighted_log_OR_boverall_PPO = weighted_log_OR_boverall,
    weighted_log_OR_bcontrol_PPO = weighted_log_OR_b,
    weighted_log_OR_cum_PPO = weighted_log_OR_cum,
    weighted_log_OR_both_PPO = weighted_log_OR_both,
    weighted_log_OR_both_ov_PPO = weighted_log_OR_both_ov,
    weighted_log_RR_bcontrol_PPO = weighted_log_RR_bcontrol, 
    weighted_log_RR_boverall_PPO = weighted_log_RR_boverall,
    weighted_log_RR_cum_PPO = weighted_log_RR_cum,
    weighted_log_RR_both_PPO = weighted_log_RR_both,
    weighted_log_RR_both_ov_PPO = weighted_log_RR_both_ov,
    
    
    weighted_log_RR_bcontrol_ge_PPO = weighted_log_RR_bcontrol_ge, 
    weighted_log_RR_boverall_ge_PPO = weighted_log_RR_boverall_ge,
    weighted_log_RR_cum_ge_PPO = weighted_log_RR_cum_ge,
    weighted_log_RR_both_ge_PPO = weighted_log_RR_both_ge,
    weighted_log_RR_both_ov_ge_PPO = weighted_log_RR_both_ov_ge,
    unweighted_log_RR_ge_PPO = unweighted_log_RR_ge,
    unweighted_RR_ge_PPO = unweighted_RR_ge,
    #WR = WR, 
    NB_PPO = NB))
  
}


# calculate the credible interval 
# assumes a partial proportional odds model 
# assumes the treatment is coded as -0.5 for control and 0.5 for treatment 
# b is the model 
# NOTE: this only works for the iprior = 1 because I negated the alphas in rows 566, 567, 574, 576
order_weighted_RD2_CI <- function(b, n_post_draws = 4000, dat) { 
  # first calculate P(y >= i | ctl) and P(y >= i | trt) for all i
  posterior_draws <- as.matrix(b$draws)
  c <- length(b$ylevels)
  PPO_ctl_geq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_trt_geq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_ctl_leq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PPO_trt_leq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  p_y_ctl_est <- matrix(data = NA, nrow = n_post_draws, ncol = c)
  p_y_trt_est <- matrix(data = NA, nrow = n_post_draws, ncol = c)
  
  
  for (i in 2:c) {
    # for the first level there is not a partial term, so only need first term and metformin term 
    if (i == 2) {
      PPO_ctl_geq[,i] <- apply(posterior_draws, 1, function (b) plogis(b[1] - 0.5 * b[c]))
      PPO_trt_geq[,i] <- apply(posterior_draws, 1, function (b) plogis(b[1] + 0.5 * b[c]))
      PPO_ctl_leq[,(i - 1)] <-  1 - PPO_ctl_geq[,i]
      PPO_trt_leq[,(i - 1)] <- 1 - PPO_trt_geq[,i]
    } else{
      phrase <- paste0("y>=", i)
      coefs <- cbind(posterior_draws[,str_detect(colnames(posterior_draws), phrase)], posterior_draws[,c])
      PPO_ctl_geq[,i] <- apply(coefs, 1, 
                               function (b) plogis(b[1] - 0.5 * b[2] - 0.5 * b[3]))
      PPO_trt_geq[,i] <- apply(coefs, 1, 
                               function (b) plogis(b[1] + 0.5 * b[2] + 0.5 * b[3]))
      PPO_ctl_leq[,(i - 1)] <- 1 - PPO_ctl_geq[,i]
      PPO_trt_leq[,(i - 1)] <- 1 - PPO_trt_geq[,i]
    }
  }
  
  PPO_leq <- (PPO_ctl_leq + PPO_trt_leq)/2
  
  # calculate p(Y = i | control) using the model for i = 1, ... , c
  for (i in 1:c) {
    if (i == 1) {
      p_y_ctl_est[,1] <-  PPO_ctl_leq[,1]
      p_y_trt_est[,1] <-  PPO_trt_leq[,1]
    } else if (i == 2){
      p_y_ctl_est[,2] <- PPO_ctl_leq[,i] - PPO_ctl_leq[,1]
      p_y_trt_est[,2] <- PPO_trt_leq[,i] - PPO_trt_leq[,1]
    } else if (i == c) {
      p_y_ctl_est[,c] <- 1 - rowSums(p_y_ctl_est[,1:(c-1)])
      p_y_trt_est[,c] <- 1 - rowSums(p_y_trt_est[,1:(c-1)])
    } else {
      p_y_ctl_est[,i] <-  PPO_ctl_leq[,i] - apply(p_y_ctl_est[,1:(i-1)], 1, sum)
      p_y_trt_est[,i] <-  PPO_trt_leq[,i] - apply(p_y_trt_est[,1:(i-1)], 1, sum)
    }
  }
  
  p_y_est <- (p_y_trt_est + p_y_ctl_est)/2
  
  # use data to properly weight each levels 
  p_y_ctl <- unname(table(dat$trt, dat$y)[1,])/sum(table(dat$trt, dat$y)[1,])
  p_y_trt <- unname(table(dat$trt, dat$y)[2,])/sum(table(dat$trt, dat$y)[2,])
  p_y <- as.vector(unname(table(dat$y))/sum(table(dat$y)))
  
  RD_leq <- PPO_trt_leq - PPO_ctl_leq
  RD_leq_95CI <- apply(RD_leq[,], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  RD_geq <- PPO_ctl_geq - PPO_trt_geq
  RD_geq_95CI <- apply(RD_geq[,], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  # put control in the numerator so the OR is greater than one if metformin has a positive effect
  OR_le <- (PPO_trt_leq / (1- PPO_trt_leq)) / (PPO_ctl_leq / (1- PPO_ctl_leq))
  OR_le_95CI <- apply(OR_le[, 1:(c-1)], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  log_OR_le <- log((PPO_trt_leq / (1- PPO_trt_leq)) / (PPO_ctl_leq / (1- PPO_ctl_leq)))
  log_OR_le_95CI <- apply(log_OR_le[, 1:(c-1)], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  
  OR_ge <- (PPO_ctl_geq / (1- PPO_ctl_geq)) / (PPO_trt_geq / (1- PPO_trt_geq))
  OR_ge_95CI <- apply(OR_ge[, 2:c], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  #for iprior = 1: (PPO_ctl_geq / (1- PPO_ctl_geq))/(PPO_trt_geq / (1 - PPO_trt_geq))
  
  log_RR_le <- log(PPO_trt_leq / PPO_ctl_leq)
  log_RR_le_95CI <- apply(log_RR_le[, 1:(c-1)], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  log_RR_ge <- log(PPO_ctl_geq / PPO_trt_geq)
  log_RR_ge_95CI <- apply(log_RR_ge[, 1:(c-1)], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  j <- 1:(c-1)
  k <- 2:c
  numerator <- rowSums((t(replicate(n_post_draws, p_y_ctl))[,j] + t(replicate(n_post_draws, p_y_ctl))[,k]) * 
                         (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  weighted_RD <- numerator/denominator
  weighted_RD_95CI <- quantile(weighted_RD, probs = c(0.025, 0.975))
  weighted_RD_p <- 1 - sum(weighted_RD >= 0)/n_post_draws
  weighted_RD_sd <- sd(weighted_RD)
  
  numerator <- rowSums((t(replicate(n_post_draws, p_y))[,j] + t(replicate(n_post_draws, p_y))[,k]) * 
                         (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  denominator <- sum(p_y[j]) + sum(p_y[k])
  weighted_RD_overall <- numerator/denominator
  weighted_RD_95CI_overall <- quantile(weighted_RD_overall, probs = c(0.025, 0.975))
  weighted_RD_p_overall <- 1 - sum(weighted_RD_overall >= 0)/n_post_draws
  weighted_RD_sd_overall <- sd(weighted_RD_overall)
  
  # fully bayesian approach
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
                         (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  denominator <- rowSums(p_y_ctl_est[,j]) + rowSums(p_y_ctl_est[,k])
  weighted_RD_b <- numerator/denominator
  weighted_RD_95CI_b <- quantile(weighted_RD_b, probs = c(0.025, 0.975))
  weighted_RD_p_b <- 1 - sum(weighted_RD_b >= 0)/n_post_draws
  weighted_RD_sd_b <- sd(weighted_RD_b)
  
  numerator <- rowSums((p_y_est[,j] + p_y_est[,k]) * 
                         (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  denominator <- rowSums(p_y_est[,j]) + rowSums(p_y_est[,k])
  weighted_RD_boverall <- numerator/denominator
  weighted_RD_95CI_boverall <- quantile(weighted_RD_boverall, probs = c(0.025, 0.975))
  weighted_RD_p_boverall <- 1 - sum(weighted_RD_boverall >= 0)/n_post_draws
  weighted_RD_sd_boverall <- sd(weighted_RD_boverall)
  
  # cumulative weights
  numerator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  denominator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])))
  weighted_RD_cum <- numerator/denominator
  weighted_RD_cum95CI <- quantile(weighted_RD_cum, probs = c(0.025, 0.975))
  weighted_RD_cump <- 1 - sum(weighted_RD_cum >= 0)/n_post_draws
  weighted_RD_cumsd <- sd(weighted_RD_cum)
  
  # both weights
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  denominator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])))
  weighted_RD_both <- numerator/denominator
  weighted_RD_both95CI <- quantile(weighted_RD_both, probs = c(0.025, 0.975))
  weighted_RD_bothp <- 1 - sum(weighted_RD_both >= 0)/n_post_draws
  weighted_RD_bothsd <- sd(weighted_RD_both)
  
  # both weights
  numerator <- rowSums((p_y_est[,j] + p_y_est[,k]) * (PPO_leq[,j] * (1-PPO_leq[,j])) * (PPO_trt_leq[,j] - PPO_ctl_leq[,j]))
  denominator <- rowSums((p_y_est[,j] + p_y_est[,k]) * (PPO_leq[,j] * (1-PPO_leq[,j])))
  weighted_RD_both_ov <- numerator/denominator
  weighted_RD_both_ov95CI <- quantile(weighted_RD_both_ov, probs = c(0.025, 0.975))
  weighted_RD_both_ovp <- 1 - sum(weighted_RD_both_ov >= 0)/n_post_draws
  weighted_RD_both_ovsd <- sd(weighted_RD_both_ov)
  
  #unweighted RD
  unweighted_RD <- rowMeans(RD_leq, na.rm = T)
  unweighted_RD_95CI <- quantile(unweighted_RD, probs = c(0.025, 0.975))
  unweighted_RD_p <- 1 - sum(unweighted_RD >= 0)/n_post_draws
  unweighted_RD_sd <- sd(unweighted_RD)
  
  #calculate weighted OR 
  numerator <- rowSums((t(replicate(n_post_draws, p_y_ctl))[,j] + t(replicate(n_post_draws, p_y_ctl))[,k]) * 
                         log(OR_le[,j]))
  denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  weighted_OR <- exp(numerator/denominator)
  weighted_OR_95CI <- quantile(weighted_OR, probs = c(0.025, 0.975))
  weighted_OR_p <- 1 - sum(weighted_OR >= 1)/n_post_draws
  weighted_OR_sd <- sd(weighted_OR)
  
  weighted_log_OR <- numerator/denominator
  weighted_log_OR_95CI <- quantile(weighted_log_OR, probs = c(0.025, 0.975))
  weighted_log_OR_p <- 1 - sum(weighted_log_OR >= 0)/n_post_draws
  weighted_log_OR_sd <- sd(weighted_log_OR)
  
  #calculate weighted OR with overall probability as weights 
  numerator <- rowSums((t(replicate(n_post_draws, p_y))[,j] + t(replicate(n_post_draws, p_y))[,k]) * 
                         log(OR_le[,j]))
  denominator <- sum(p_y[j]) + sum(p_y[k])
  weighted_OR_overall <- exp(numerator/denominator)
  weighted_OR_95CI_overall <- quantile(weighted_OR_overall, probs = c(0.025, 0.975))
  weighted_OR_p_overall <- 1 - sum(weighted_OR_overall >= 1)/n_post_draws
  weighted_OR_sd_overall <- sd(weighted_OR_overall)
  
  weighted_log_OR_overall <- numerator/denominator
  weighted_log_OR_95CI_overall <- quantile(weighted_log_OR_overall, probs = c(0.025, 0.975))
  weighted_log_OR_p_overall <- 1 - sum(weighted_log_OR_overall >= 0)/n_post_draws
  weighted_log_OR_sd_overall <- sd(weighted_log_OR_overall)
  
  # fully bayesian approach 
  #calculate weighted OR 
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
                         log(OR_le[,j]))
  denominator <- rowSums(p_y_ctl_est[,j]) + rowSums(p_y_ctl_est[,k])
  weighted_OR_b <- exp(numerator/denominator)
  weighted_OR_95CI_b <- quantile(weighted_OR_b, probs = c(0.025, 0.975))
  weighted_OR_p_b <- 1 - sum(weighted_OR_b >= 1)/n_post_draws
  weighted_OR_sd_b <- sd(weighted_OR_b)
  
  weighted_log_OR_b <- numerator/denominator
  weighted_log_OR_95CI_b <- quantile(weighted_log_OR_b, probs = c(0.025, 0.975))
  weighted_log_OR_p_b <- 1 - sum(weighted_log_OR_b >= 0)/n_post_draws
  weighted_log_OR_sd_b <- sd(weighted_log_OR_b)
  
  
  #calculate weighted OR with overall probability as weights 
  numerator <- rowSums((p_y_est[,j] + p_y_est[,k]) * 
                         log(OR_le[,j]))
  denominator <- rowSums(p_y_est[,j]) + rowSums(p_y_est[,k])
  weighted_OR_boverall <- exp(numerator/denominator)
  weighted_OR_95CI_boverall <- quantile(weighted_OR_boverall, probs = c(0.025, 0.975))
  weighted_OR_p_boverall <- 1 - sum(weighted_OR_boverall >= 1)/n_post_draws
  weighted_OR_sd_boverall <- sd(weighted_OR_boverall)
  
  weighted_log_OR_boverall <- numerator/denominator
  weighted_log_OR_95CI_boverall <- quantile(weighted_log_OR_boverall, probs = c(0.025, 0.975))
  weighted_log_OR_p_boverall <- 1 - sum(weighted_log_OR_boverall >= 0)/n_post_draws
  weighted_log_OR_sd_boverall <- sd(weighted_log_OR_boverall)
  
  # cumulative weights only
  numerator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * log(OR_le[,j]))
  denominator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])))
  weighted_OR_cum <- exp(numerator/denominator)
  weighted_OR_cum95CI <- quantile(weighted_OR_cum, probs = c(0.025, 0.975))
  weighted_OR_cump <- 1 - sum(weighted_OR_cum >= 0)/n_post_draws
  weighted_OR_cumsd <- sd(weighted_OR_cum)
  
  weighted_log_OR_cum <- numerator/denominator
  weighted_log_OR_cum95CI <- quantile(weighted_log_OR_cum, probs = c(0.025, 0.975))
  weighted_log_OR_cump <- 1 - sum(weighted_log_OR_cum >= 0)/n_post_draws
  weighted_log_OR_cumsd <- sd(weighted_log_OR_cum)
  
  # both weights
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * log(OR_le[,j]))
  denominator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])))
  weighted_OR_both <- exp(numerator/denominator)
  weighted_OR_both95CI <- quantile(weighted_OR_both, probs = c(0.025, 0.975))
  weighted_OR_bothp <- 1 - sum(weighted_OR_both >= 0)/n_post_draws
  weighted_OR_bothsd <- sd(weighted_OR_both)
  
  weighted_log_OR_both <- numerator/denominator
  weighted_log_OR_both95CI <- quantile(weighted_log_OR_both, probs = c(0.025, 0.975))
  weighted_log_OR_bothp <- 1 - sum(weighted_log_OR_both >= 0)/n_post_draws
  weighted_log_OR_bothsd <- sd(weighted_log_OR_both)
  
  # both weights with overall probability
  numerator <- rowSums((p_y_est[,j] + p_y_est[,k]) * (PPO_leq[,j] * (1-PPO_leq[,j])) * log(OR_le[,j]))
  denominator <- rowSums((p_y_est[,j] + p_y_est[,k]) * (PPO_leq[,j] * (1-PPO_leq[,j])))
  weighted_OR_both_ov <- exp(numerator/denominator)
  weighted_OR_both_ov95CI <- quantile(weighted_OR_both_ov, probs = c(0.025, 0.975))
  weighted_OR_both_ovp <- 1 - sum(weighted_OR_both_ov >= 0)/n_post_draws
  weighted_OR_both_ovsd <- sd(weighted_OR_both_ov)
  
  weighted_log_OR_both_ov <- numerator/denominator
  weighted_log_OR_both_ov95CI <- quantile(weighted_log_OR_both_ov, probs = c(0.025, 0.975))
  weighted_log_OR_both_ovp <- 1 - sum(weighted_log_OR_both_ov >= 0)/n_post_draws
  weighted_log_OR_both_ovsd <- sd(weighted_log_OR_both_ov)
  
  #unweighted OR 
  unweighted_OR <- rowMeans(log_OR_le, na.rm = T)
  unweighted_log_OR_95CI <- quantile(unweighted_OR, probs = c(0.025, 0.975))
  unweighted_log_OR_p <- 1 - sum(unweighted_OR >= 0)/n_post_draws
  unweighted_log_OR_sd <- sd(unweighted_OR)
  
  
  unweighted_OR_95CI <- quantile(exp(unweighted_OR), probs = c(0.025, 0.975))
  unweighted_OR_p <- 1 - sum(exp(unweighted_OR) >= 0)/n_post_draws
  unweighted_OR_sd <- sd(exp(unweighted_OR))
  
  # calculate weighted RR with control probabilities 
  numerator <- rowSums((t(replicate(n_post_draws, p_y_ctl))[,j] + t(replicate(n_post_draws, p_y_ctl))[,k]) * 
                         log_RR_le[,j])
  denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  weighted_log_RR_control <- numerator/denominator
  weighted_log_RR_95CI_control <- quantile(weighted_log_RR_control, probs = c(0.025, 0.975))
  weighted_log_RR_p_control <- 1 - sum(weighted_log_RR_control >= 0)/n_post_draws
  weighted_log_RR_sd_control <- sd(weighted_log_RR_control)
  
  # calculate weighted RR with overall probabilities 
  numerator <- rowSums((t(replicate(n_post_draws, p_y))[,j] + t(replicate(n_post_draws, p_y))[,k]) * 
                         log_RR_le[,j])
  denominator <- sum(p_y[j]) + sum(p_y[k])
  weighted_log_RR_overall <- numerator/denominator
  weighted_log_RR_95CI_overall <- quantile(weighted_log_RR_overall, probs = c(0.025, 0.975))
  weighted_log_RR_p_overall <- 1 - sum(weighted_log_RR_overall >= 0)/n_post_draws
  weighted_log_RR_sd_overall <- sd(weighted_log_RR_overall)
  
  # fully bayesian 
  # calculate weighted RR with control probabilities 
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
                         log_RR_le[,j])
  denominator <- rowSums(p_y_ctl_est[,j]) + rowSums(p_y_ctl_est[,k])
  weighted_log_RR_bcontrol <- numerator/denominator
  weighted_log_RR_95CI_bcontrol <- quantile(weighted_log_RR_bcontrol, probs = c(0.025, 0.975))
  weighted_log_RR_p_bcontrol <- 1 - sum(weighted_log_RR_bcontrol >= 0)/n_post_draws
  weighted_log_RR_sd_bcontrol <- sd(weighted_log_RR_bcontrol)
  
  # calculate weighted RR with overall probabilities 
  numerator <- rowSums((p_y_est[,j] + p_y_est[,k]) * 
                         log_RR_le[,j])
  denominator <- rowSums(p_y_est[,j]) + rowSums(p_y_est[,k])
  weighted_log_RR_boverall <- numerator/denominator
  weighted_log_RR_95CI_boverall <- quantile(weighted_log_RR_boverall, probs = c(0.025, 0.975))
  weighted_log_RR_p_boverall <- 1 - sum(weighted_log_RR_boverall >= 0)/n_post_draws
  weighted_log_RR_sd_boverall <- sd(weighted_log_RR_boverall)
  
  # cumulative weights only
  numerator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * log_RR_le[,j])
  denominator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])))
  weighted_log_RR_cum <- numerator/denominator
  weighted_log_RR_cum95CI <- quantile(weighted_log_RR_cum, probs = c(0.025, 0.975))
  weighted_log_RR_cump <- 1 - sum(weighted_log_RR_cum >= 0)/n_post_draws
  weighted_log_RR_cumsd <- sd(weighted_log_RR_cum)
  
  # both weights
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * log_RR_le[,j])
  denominator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])))
  weighted_log_RR_both <- numerator/denominator
  weighted_log_RR_both95CI <- quantile(weighted_log_RR_both, probs = c(0.025, 0.975))
  weighted_log_RR_bothp <- 1 - sum(weighted_log_RR_both >= 0)/n_post_draws
  weighted_log_RR_bothsd <- sd(weighted_log_RR_both)
  
  # both weights
  numerator <- rowSums((p_y_est[,j] + p_y_est[,k]) * (PPO_leq[,j] * (1-PPO_leq[,j])) * log_RR_le[,j])
  denominator <- rowSums((p_y_est[,j] + p_y_est[,k]) * (PPO_leq[,j] * (1-PPO_leq[,j])))
  weighted_log_RR_both_ov <- numerator/denominator
  weighted_log_RR_both_ov95CI <- quantile(weighted_log_RR_both_ov, probs = c(0.025, 0.975))
  weighted_log_RR_both_ovp <- 1 - sum(weighted_log_RR_both_ov >= 0)/n_post_draws
  weighted_log_RR_both_ovsd <- sd(weighted_log_RR_both_ov)
  
  #unweighted RR
  unweighted_RR <- rowMeans(log_RR_le, na.rm = T)
  unweighted_log_RR_95CI <- quantile(unweighted_RR, probs = c(0.025, 0.975))
  unweighted_log_RR_p <- 1 - sum(unweighted_RR >= 0)/n_post_draws
  unweighted_log_RR_sd <- sd(unweighted_RR)
  
  
  unweighted_RR_95CI <- quantile(exp(unweighted_RR), probs = c(0.025, 0.975))
  unweighted_RR_p <- 1 - sum(exp(unweighted_RR) >= 0)/n_post_draws
  unweighted_RR_sd <- sd(exp(unweighted_RR))
  
  # fully bayesian RR greater than or equal to 
  # calculate weighted RR with control probabilities 
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * 
                         log_RR_ge[,k])
  denominator <- rowSums(p_y_ctl_est[,j]) + rowSums(p_y_ctl_est[,k])
  weighted_log_RR_ge_bcontrol <- numerator/denominator
  weighted_log_RR_ge_95CI_bcontrol <- quantile(weighted_log_RR_ge_bcontrol, probs = c(0.025, 0.975))
  weighted_log_RR_ge_p_bcontrol <- 1 - sum(weighted_log_RR_ge_bcontrol >= 0)/n_post_draws
  weighted_log_RR_ge_sd_bcontrol <- sd(weighted_log_RR_ge_bcontrol)
  
  # calculate weighted RR with overall probabilities 
  numerator <- rowSums((p_y_est[,j] + p_y_est[,k]) * 
                         log_RR_ge[,k])
  denominator <- rowSums(p_y_est[,j]) + rowSums(p_y_est[,k])
  weighted_log_RR_ge_boverall <- numerator/denominator
  weighted_log_RR_ge_95CI_boverall <- quantile(weighted_log_RR_ge_boverall, probs = c(0.025, 0.975))
  weighted_log_RR_ge_p_boverall <- 1 - sum(weighted_log_RR_ge_boverall >= 0)/n_post_draws
  weighted_log_RR_ge_sd_boverall <- sd(weighted_log_RR_ge_boverall)
  
  # cumulative weights only
  numerator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * log_RR_ge[,k])
  denominator <- rowSums((PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])))
  weighted_log_RR_ge_cum <- numerator/denominator
  weighted_log_RR_ge_cum95CI <- quantile(weighted_log_RR_ge_cum, probs = c(0.025, 0.975))
  weighted_log_RR_ge_cump <- 1 - sum(weighted_log_RR_ge_cum >= 0)/n_post_draws
  weighted_log_RR_ge_cumsd <- sd(weighted_log_RR_ge_cum)
  
  # both weights
  numerator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])) * log_RR_ge[,k])
  denominator <- rowSums((p_y_ctl_est[,j] + p_y_ctl_est[,k]) * (PPO_ctl_leq[,j] * (1-PPO_ctl_leq[,j])))
  weighted_log_RR_ge_both <- numerator/denominator
  weighted_log_RR_ge_both95CI <- quantile(weighted_log_RR_ge_both, probs = c(0.025, 0.975))
  weighted_log_RR_ge_bothp <- 1 - sum(weighted_log_RR_ge_both >= 0)/n_post_draws
  weighted_log_RR_ge_bothsd <- sd(weighted_log_RR_ge_both)
  
  # both weights overall weights
  numerator <- rowSums((p_y_est[,j] + p_y_est[,k]) * (PPO_leq[,j] * (1-PPO_leq[,j])) * log_RR_ge[,k])
  denominator <- rowSums((p_y_est[,j] + p_y_est[,k]) * (PPO_leq[,j] * (1-PPO_leq[,j])))
  weighted_log_RR_ge_both_ov <- numerator/denominator
  weighted_log_RR_ge_both_ov95CI <- quantile(weighted_log_RR_ge_both_ov, probs = c(0.025, 0.975))
  weighted_log_RR_ge_both_ovp <- 1 - sum(weighted_log_RR_ge_both_ov >= 0)/n_post_draws
  weighted_log_RR_ge_both_ovsd <- sd(weighted_log_RR_ge_both_ov)
  
  #unweighted RR
  unweighted_RR_ge <- rowMeans(log_RR_ge, na.rm = T)
  unweighted_log_RR_ge_95CI <- quantile(unweighted_RR_ge, probs = c(0.025, 0.975))
  unweighted_log_RR_ge_p <- 1 - sum(unweighted_RR_ge >= 0)/n_post_draws
  unweighted_log_RR_ge_sd <- sd(unweighted_RR_ge)
  
  # calculate NB 
  NB <- rowSums(p_y_trt_est[,j] * PPO_ctl_geq[,(j+1)]) - (rowSums(p_y_ctl_est[,j] * PPO_trt_geq[,(j+1)]))
  NB_95CI <- quantile(NB, probs = c(0.025, 0.975))
  NB_p <- 1 - sum(NB >= 0)/n_post_draws
  NB_sd <- sd(NB)
  
  return(c(#p_y_ctl_CI = p_y_ctl_CI, 
    #p_y_trt_CI = p_y_trt_CI, 
    #RD_leq = RD_leq_95CI, 
    #RD_geq = RD_geq_95CI, 
    #OR = OR_le_95CI, 
    #log_OR = log_OR_le_95CI,
    weighted_RD_control_CI_PPO = weighted_RD_95CI,
    weighted_RD_p_control_PPO = weighted_RD_p,
    weighted_RD_sd_PPO = weighted_RD_sd, 
    weighted_RD_sd_overall_PPO = weighted_RD_sd_overall,
    weighted_RD_overall_CI_PPO = weighted_RD_95CI_overall,
    weighted_RD_p_overall_PPO = weighted_RD_p_overall,
    weighted_OR_CI_PPO = weighted_OR_95CI, 
    weighted_OR_p_PPO = weighted_OR_p,
    weighted_OR_sd_PPO = weighted_OR_sd,
    weighted_OR_overall_CI_PPO = weighted_OR_95CI_overall,
    weighted_OR_p_overall_PPO = weighted_OR_p_overall,
    weighted_OR_sd_overall_PPO = weighted_OR_sd_overall,
    weighted_log_OR_control_CI_PPO = weighted_log_OR_95CI,
    weighted_log_OR_p_control_PPO = weighted_log_OR_p,
    weighted_log_OR_sd_control_PPO = weighted_log_OR_sd,
    weighted_log_OR_overall_CI_PPO = weighted_log_OR_95CI_overall,
    weighted_log_OR_p_overall_PPO = weighted_log_OR_p_overall,
    weighted_log_OR_sd_overall_PPO = weighted_log_OR_sd_overall,
    weighted_log_RR_95CI_control_PPO = weighted_log_RR_95CI_control,
    weighted_log_RR_p_control_PPO = weighted_log_RR_p_control, 
    weighted_log_RR_sd_control_PPO = weighted_log_RR_sd_control,
    weighted_log_RR_95CI_overall_PPO = weighted_log_RR_95CI_overall,
    weighted_log_RR_p_overall_PPO = weighted_log_RR_p_overall, 
    weighted_log_RR_sd_overall_PPO = weighted_log_RR_sd_overall,
    
    weighted_RD_bcontrol_CI_PPO = weighted_RD_95CI_b,
    weighted_RD_p_bcontrol_PPO = weighted_RD_p_b,
    weighted_RD_sd_b_PPO = weighted_RD_sd_b, 
    weighted_RD_sd_boverall_PPO = weighted_RD_sd_boverall,
    weighted_RD_boverall_CI_PPO = weighted_RD_95CI_boverall,
    weighted_RD_p_boverall_PPO = weighted_RD_p_boverall,
    weighted_OR_CI_b_PPO = weighted_OR_95CI_b, 
    weighted_OR_p_b_PPO = weighted_OR_p_b,
    weighted_OR_sd_b_PPO = weighted_OR_sd_b,
    weighted_OR_boverall_CI_PPO = weighted_OR_95CI_boverall,
    weighted_OR_p_boverall_PPO = weighted_OR_p_boverall,
    weighted_OR_sd_boverall_PPO = weighted_OR_sd_boverall,
    weighted_log_OR_bcontrol_CI_PPO = weighted_log_OR_95CI_b,
    weighted_log_OR_p_bcontrol_PPO = weighted_log_OR_p_b,
    weighted_log_OR_sd_bcontrol_PPO = weighted_log_OR_sd_b,
    weighted_log_OR_boverall_CI_PPO = weighted_log_OR_95CI_boverall,
    weighted_log_OR_p_boverall_PPO = weighted_log_OR_p_boverall,
    weighted_log_OR_sd_boverall_PPO = weighted_log_OR_sd_boverall,
    weighted_log_RR_95CI_bcontrol_PPO = weighted_log_RR_95CI_bcontrol,
    weighted_log_RR_p_bcontrol_PPO = weighted_log_RR_p_bcontrol, 
    weighted_log_RR_sd_bcontrol_PPO = weighted_log_RR_sd_bcontrol,
    weighted_log_RR_95CI_boverall_PPO = weighted_log_RR_95CI_boverall,
    weighted_log_RR_p_boverall_PPO = weighted_log_RR_p_boverall, 
    weighted_log_RR_sd_boverall_PPO = weighted_log_RR_sd_boverall,
    weighted_log_RR_ge_95CI_bcontrol_PPO = weighted_log_RR_ge_95CI_bcontrol,
    weighted_log_RR_ge_p_bcontrol_PPO = weighted_log_RR_ge_p_bcontrol, 
    weighted_log_RR_ge_sd_bcontrol_PPO = weighted_log_RR_ge_sd_bcontrol,
    weighted_log_RR_ge_95CI_boverall_PPO = weighted_log_RR_ge_95CI_boverall,
    weighted_log_RR_ge_p_boverall_PPO = weighted_log_RR_ge_p_boverall, 
    weighted_log_RR_ge_sd_boverall_PPO = weighted_log_RR_ge_sd_boverall,
    
    unweighted_RD_CI_PPO = unweighted_RD_95CI,
    unweighted_RD_p_PPO = unweighted_RD_p,
    unweighted_RD_sd_PPO = unweighted_RD_sd, 
    unweighted_OR_CI_PPO = unweighted_OR_95CI,
    unweighted_OR_p_PPO = unweighted_OR_p,
    unweighted_OR_sd_PPO = unweighted_OR_sd,  
    unweighted_log_OR_CI_PPO = unweighted_log_OR_95CI,
    unweighted_log_OR_p_PPO = unweighted_log_OR_p,
    unweighted_log_OR_sd_PPO = unweighted_log_OR_sd,
    unweighted_RR_CI_PPO = unweighted_RR_95CI,
    unweighted_RR_p_PPO = unweighted_RR_p,
    unweighted_RR_sd_PPO = unweighted_RR_sd,  
    unweighted_log_RR_CI_PPO = unweighted_log_RR_95CI,
    unweighted_log_RR_p_PPO = unweighted_log_RR_p,
    unweighted_log_RR_sd_PPO = unweighted_log_RR_sd,
    unweighted_log_RR_ge_CI_PPO = unweighted_log_RR_ge_95CI,
    unweighted_log_RR_ge_p_PPO = unweighted_log_RR_ge_p,
    unweighted_log_RR_ge_sd_PPO = unweighted_log_RR_ge_sd,
    
    
    weighted_RD_cum_CI_PPO = weighted_RD_cum95CI,
    weighted_RD_cum_p_PPO = weighted_RD_cump,
    weighted_RD_cum_sd_PPO = weighted_RD_cumsd,
    weighted_RD_both_CI_PPO = weighted_RD_both95CI,
    weighted_RD_both_p_PPO = weighted_RD_bothp,
    weighted_RD_both_sd_PPO = weighted_RD_bothsd, 
    weighted_RD_both_ov_CI_PPO = weighted_RD_both_ov95CI,
    weighted_RD_both_ov_p_PPO = weighted_RD_both_ovp,
    weighted_RD_both_ov_sd_PPO = weighted_RD_both_ovsd, 
    weighted_log_OR_cum_CI_PPO = weighted_log_OR_cum95CI,
    weighted_log_OR_cum_p_PPO = weighted_log_OR_cump,
    weighted_log_OR_cum_sd_PPO = weighted_log_OR_cumsd,
    weighted_log_OR_both_CI_PPO = weighted_log_OR_both95CI,
    weighted_log_OR_both_p_PPO = weighted_log_OR_bothp,
    weighted_log_OR_both_sd_PPO = weighted_log_OR_bothsd,
    weighted_log_OR_both_ov_CI_PPO = weighted_log_OR_both_ov95CI,
    weighted_log_OR_both_ov_p_PPO = weighted_log_OR_both_ovp,
    weighted_log_OR_both_ov_sd_PPO = weighted_log_OR_both_ovsd,
    weighted_log_RR_cum_CI_PPO = weighted_log_RR_cum95CI,
    weighted_log_RR_cum_p_PPO = weighted_log_RR_cump,
    weighted_log_RR_cum_sd_PPO = weighted_log_RR_cumsd,
    weighted_log_RR_both_CI_PPO = weighted_log_RR_both95CI,
    weighted_log_RR_both_p_PPO = weighted_log_RR_bothp,
    weighted_log_RR_both_sd_PPO = weighted_log_RR_bothsd,
    weighted_log_RR_both_ov_CI_PPO = weighted_log_RR_both_ov95CI,
    weighted_log_RR_both_ov_p_PPO = weighted_log_RR_both_ovp,
    weighted_log_RR_both_ov_sd_PPO = weighted_log_RR_both_ovsd,
    weighted_log_RR_ge_cum_CI_PPO = weighted_log_RR_ge_cum95CI,
    weighted_log_RR_ge_cum_p_PPO = weighted_log_RR_ge_cump,
    weighted_log_RR_ge_cum_sd_PPO = weighted_log_RR_ge_cumsd,
    weighted_log_RR_ge_both_CI_PPO = weighted_log_RR_ge_both95CI,
    weighted_log_RR_ge_both_p_PPO = weighted_log_RR_ge_bothp,
    weighted_log_RR_ge_both_sd_PPO = weighted_log_RR_ge_bothsd,
    weighted_log_RR_ge_both_ov_CI_PPO = weighted_log_RR_ge_both_ov95CI,
    weighted_log_RR_ge_both_ov_p_PPO = weighted_log_RR_ge_both_ovp,
    weighted_log_RR_ge_both_ov_sd_PPO = weighted_log_RR_ge_both_ovsd,
    
    
    
    #WR = WR_95CI,
    NB_CI_PPO = NB_95CI,
    NB_p_PPO = NB_p,
    NB_sd_PPO = NB_sd))
  
}


# new function for weighted risk difference 
# assumes a proportional odds model 
# assumes the treatment is coded as -0.5 for control and 0.5 for treatment 
order_weighted_RD2_PO <- function(b = b, dat) { 
  # first calculate P(y >= i | ctl) and P(y >= i | trt) for all i 
  PO_ctl_geq <- NA 
  PO_trt_geq <- NA 
  PO_ctl_leq <- NA 
  PO_trt_leq <- NA 
  p_y_ctl_est <- NA
  p_y_trt_est <- NA
  c <- length(b$ylevels)
  
  for (i in 2:c) {
    # for the first level there is not a partial term, so only need first term and metformin term 
    PO_ctl_geq[i] <- plogis(coef(b)[i - 1] - 0.5 * coef(b)[c]) 
    PO_trt_geq[i] <- plogis(coef(b)[i - 1] + 0.5 * coef(b)[c])
    PO_ctl_leq[i - 1] <- 1 - PO_ctl_geq[i]
    PO_trt_leq[i - 1] <- 1 - PO_trt_geq[i]
  }
  
  # calculate p(Y = i | control) for i = 1, ... , c
  for (i in 1:c) {
    if (i == 1) {
      p_y_ctl_est[1] <-  PO_ctl_leq[1]
      p_y_trt_est[1] <-  PO_trt_leq[1]
    } else if (i == c) {
      p_y_ctl_est[c] <- 1 - sum(p_y_ctl_est[1:(c-1)])
      p_y_trt_est[c] <- 1 - sum(p_y_trt_est[1:(c-1)])
    } else {
      p_y_ctl_est[i] <-  PO_ctl_leq[i] - sum(p_y_ctl_est[1:(i-1)])
      p_y_trt_est[i] <-  PO_trt_leq[i] - sum(p_y_trt_est[1:(i-1)])
    }
  }
  
  # get weights from the data 
  p_y_ctl <- unname(table(dat$trt, dat$y)[1,])/500
  p_y_trt <- unname(table(dat$trt, dat$y)[2,])/500
  p_y <- unname(table(dat$y))/1000
  
  
  RD_leq <- PO_trt_leq - PO_ctl_leq
  unweighted_RD <- mean(RD_leq)
  RD_geq <- PO_ctl_geq - PO_trt_geq
  
  OR <- (PO_trt_leq / (1- PO_trt_leq)) / (PO_ctl_leq / (1- PO_ctl_leq))
  log_OR <- log(OR)
  unweighted_log_OR <- mean(log_OR)
  unweighted_OR <- exp(mean(log_OR))
  
  log_RR_le <- log(PO_trt_leq/PO_ctl_leq)
  unweighted_log_RR <- mean(log_RR_le)
  unweighted_RR <- exp(mean(log_RR_le))
  log_RR_ge <- log(PO_ctl_geq/PO_trt_geq)
  
  j <- 1:(c-1)
  k <- 2:c
  numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * (PO_trt_leq[j] - PO_ctl_leq[j]))
  denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  weighted_RD <- numerator/denominator
  
  #calculated weighted RD with overall probability as weights
  p_y <- (p_y_trt + p_y_ctl)/2
  numerator <- sum((p_y[j] + p_y[k]) * (PO_trt_leq[j] - PO_ctl_leq[j])) 
  denominator <- sum(p_y[j]) + sum(p_y[k])
  weighted_RD_overall <- numerator/denominator
  
  # calculate weighted RR with control probabilities 
  numerator <- sum((p_y_ctl[j] + p_y_ctl[k]) * log_RR_le[j]) 
  denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  weighted_log_RR_control <- numerator/denominator
  
  # calculate weighted RR with overall probabilities 
  numerator <- sum((p_y[j] + p_y[k]) * log_RR_le[j])
  denominator <- sum(p_y[j]) + sum(p_y[k])
  weighted_log_RR_overall <- numerator/denominator
  
  # calculate NB 
  NB <- sum(p_y_trt_est[j] * PO_ctl_geq[j+1]) - (sum(p_y_ctl_est[j] * PO_trt_geq[j+1]))
  
  return(c(#p_y_ctl = p_y_ctl, 
    #p_y_trt = p_y_trt,
    #RD_leq = RD_leq, 
    #RD_geq = RD_geq, 
    OR_PO = OR[1],
    log_OR_PO = log_OR[1],
    weighted_RD_control_PO = weighted_RD, 
    weighted_RD_overall_PO = weighted_RD_overall,
    weighted_log_RR_control_PO = weighted_log_RR_control,
    weighted_log_RR_overall_PO = weighted_log_RR_control,
    unweighted_log_RR = unweighted_log_RR,
    unweighted_RR = unweighted_RR, 
    unweighted_RD = unweighted_RD,
    unweighted_log_OR = unweighted_log_OR,
    unweighted_OR = unweighted_OR, 
    #WR = WR,
    NB_PO = NB))
  
}

# calculate the credible interval 
# assumes a partial proportional odds model 
# assumes the treatment is coded as -0.5 for control and 0.5 for treatment 
# b is the model 
order_weighted_RD2_PO_CI <- function(b, n_post_draws = 4000, dat) { 
  # first calculate P(y >= i | ctl) and P(y >= i | trt) for all i
  posterior_draws <- as.matrix(b$draws)
  c <- length(b$ylevels)
  PO_ctl_geq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PO_trt_geq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PO_ctl_leq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  PO_trt_leq <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  
  p_y_ctl_est <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  p_y_trt_est <- matrix(data = NA, nrow = n_post_draws, ncol = c) 
  
  for (i in 2:c) {
    # for the first level there is not a partial term, so only need first term and metformin term 
    PO_ctl_geq[,i] <- apply(posterior_draws, 1, function (b) plogis(b[i-1] - 0.5 * b[c]))
    PO_trt_geq[,i] <- apply(posterior_draws, 1, function (b) plogis(b[i-1] + 0.5 * b[c]))
    PO_ctl_leq[,(i - 1)] <- 1 - PO_ctl_geq[,i]
    PO_trt_leq[,(i - 1)] <- 1 - PO_trt_geq[,i]
  }
  
  # calculate p(Y = i | control) for i = 1, ... , c
  for (i in 1:c) {
    if (i == 1) {
      p_y_ctl_est[,1] <-  PO_ctl_leq[,1]
      p_y_trt_est[,1] <-  PO_trt_leq[,1]
    } else if (i == 2){
      p_y_ctl_est[,2] <- PO_ctl_leq[,i] - PO_ctl_leq[,1]
      p_y_trt_est[,2] <- PO_trt_leq[,i] - PO_trt_leq[,1]
    } else if (i == c) {
      p_y_ctl_est[,c] <- 1 - rowSums(p_y_ctl_est[,1:(c-1)])
      p_y_trt_est[,c] <- 1 - rowSums(p_y_trt_est[,1:(c-1)])
    } else {
      p_y_ctl_est[,i] <-  PO_ctl_leq[,i] - apply(p_y_ctl_est[,1:(i-1)], 1, sum)
      p_y_trt_est[,i] <-  PO_trt_leq[,i] - apply(p_y_trt_est[,1:(i-1)], 1, sum)
    }
  }
  
  # use data to properly weight each levels 
  p_y_ctl <- unname(table(dat$trt, dat$y)[1,])/500
  p_y_trt <- unname(table(dat$trt, dat$y)[2,])/500
  p_y <- as.vector(unname(table(dat$y))/1000)
  
  RD_leq <- PO_trt_leq - PO_ctl_leq
  RD_leq_95CI <- apply(RD_leq[,], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  RD_geq <- PO_ctl_geq - PO_trt_geq
  RD_geq_95CI <- apply(RD_geq[,], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  # put control in the numerator so the OR is greater than one if metformin has a positive effect
  OR <- (PO_ctl_geq / (1- PO_ctl_geq))/(PO_trt_geq / (1 - PO_trt_geq))
  OR_95CI <- apply(OR[, 2:4], 2, quantile, probs = c(0.025, 0.975), na.rm = T)[1:2]
  OR_p <- 1 - sum(OR[,2] >= 1)/n_post_draws
  OR_sd <- sd(OR[,2])
  #(PPO_trt_leq / (1- PPO_trt_leq)) / (PPO_ctl_leq / (1- PPO_ctl_leq))
  log_OR <- log(OR)
  log_OR_95CI <- apply(log_OR[, 2:4], 2, quantile, probs = c(0.025, 0.975), na.rm = T)[1:2]
  log_OR_p <- 1 - sum(log_OR[,2] >= 0)/n_post_draws
  log_OR_sd <- sd(log_OR[,2])
  
  log_RR_le <- log(PO_trt_leq / PO_ctl_leq)
  log_RR_le_95CI <- apply(log_RR_le[, 1:3], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  log_RR_ge <- log(PO_ctl_geq / PO_trt_geq)
  log_RR_ge_95CI <- apply(log_RR_ge[, 1:3], 2, quantile, probs = c(0.025, 0.975), na.rm = T)
  
  
  # calculate weighted RD with control probability as weights
  j <- 1:(c-1)
  k <- 2:c
  numerator <- rowSums((t(replicate(n_post_draws, p_y_ctl))[,j] + t(replicate(n_post_draws, p_y_ctl))[,k]) *
                         (PO_trt_leq[,j] - PO_ctl_leq[,j])) 
  denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  weighted_RD <- numerator/denominator
  weighted_RD_95CI <- quantile(weighted_RD, probs = c(0.025, 0.975))
  weighted_RD_p <- 1 - sum(weighted_RD >= 0)/n_post_draws
  weighted_RD_sd <- sd(weighted_RD)
  
  #calculate weighted risk difference with overall probability as weights
  numerator <- rowSums((t(replicate(n_post_draws, p_y))[,j] + t(replicate(n_post_draws, p_y))[,k]) * 
                         (PO_trt_leq[,j] - PO_ctl_leq[,j]))
  denominator <- sum(p_y[j]) + sum(p_y[k])
  weighted_RD_overall <- numerator/denominator
  weighted_RD_95CI_overall <- quantile(weighted_RD_overall, probs = c(0.025, 0.975))
  weighted_RD_p_overall <- 1 - sum(weighted_RD_overall >= 0)/n_post_draws
  weighted_RD_sd_overall <- sd(weighted_RD_overall)
  
  # calculate weighted RR with control probabilities 
  numerator <- rowSums((t(replicate(n_post_draws, p_y_ctl))[,j] + t(replicate(n_post_draws, p_y_ctl))[,k]) * 
                         log_RR_le[,j]) 
  denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  weighted_log_RR_control <- numerator/denominator
  weighted_log_RR_95CI_control <- quantile(weighted_log_RR_control, probs = c(0.025, 0.975))
  weighted_log_RR_p_control <- 1 - sum(weighted_log_RR_control >= 0)/n_post_draws
  weighted_log_RR_sd_control <- sd(weighted_log_RR_control)
  
  # calculate weighted RR with overall probabilities 
  numerator <- rowSums((t(replicate(n_post_draws, p_y))[,j] + t(replicate(n_post_draws, p_y))[,k]) * 
                         log_RR_le[,j])
  denominator <- sum(p_y[j]) + sum(p_y[k])
  weighted_log_RR_overall <- numerator/denominator
  weighted_log_RR_95CI_overall <- quantile(weighted_log_RR_overall, probs = c(0.025, 0.975))
  weighted_log_RR_p_overall <- 1 - sum(weighted_log_RR_overall >= 0)/n_post_draws
  weighted_log_RR_sd_overall <- sd(weighted_log_RR_overall)
  
  
  # calculate NB 
  NB <- rowSums(p_y_trt_est[j] * PO_ctl_geq[,(j+1)]) - (rowSums(p_y_ctl_est[j] * PO_trt_geq[,(j+1)]))
  NB_95CI <- quantile(NB, probs = c(0.025, 0.975))
  NB_p <- 1 - sum(NB >= 0)/n_post_draws 
  NB_sd <- sd(NB)
  
  return(c(#RD_leq = RD_leq_95CI, 
    #RD_geq = RD_geq_95CI, 
    OR_CI_PO = OR_95CI, 
    OR_p_PO = OR_p,
    OR_sd_PO = OR_sd,
    log_OR_CI_PO = log_OR_95CI, 
    log_OR_p_PO = log_OR_p,
    log_OR_sd_PO = log_OR_sd,
    weighted_RD_control_CI_PO = weighted_RD_95CI, 
    weighted_RD_p_control_PO = weighted_RD_p,
    weighted_RD_sd_PO = weighted_RD_sd,
    weighted_RD_overall_CI_PO = weighted_RD_95CI_overall, 
    weighted_RD_p_overall_PO = weighted_RD_p_overall,
    weighted_RD_sd_overall_PO = weighted_RD_sd_overall,
    weighted_log_RR_95CI_control_PO = weighted_log_RR_95CI_control,
    weighted_log_RR_p_control_PO = weighted_log_RR_p_control, 
    weighted_log_RR_sd_control_PO = weighted_log_RR_sd_control,
    weighted_log_RR_95CI_overall_PO = weighted_log_RR_95CI_overall,
    weighted_log_RR_p_overall_PO = weighted_log_RR_p_overall, 
    weighted_log_RR_sd_overall_PO = weighted_log_RR_sd_overall,
    #WR = WR_95CI, 
    NB_CI_PO = NB_95CI, 
    NB_p_PO = NB_p, 
    NB_sd_PO = NB_sd))
  
}

# write function to calculate order invariant weighted risk difference using polr 
polr_order_weighted_RD <- function(b = b, c) { 
  p_ctl_more <- NA
  p_trt_more <- NA
  p_ctl_less <- NA
  p_trt_less <- NA
  p_y_ctl <- NA
  
  for (i in 2:c) {
    # calculate p(y >= i | control) for i = 2, ... c
    p_ctl_more[i] <- 1 - plogis(b$zeta[i-1] + 0.5 * b$coefficients[1])
    # calculate p(y >= i | treatment) for i = 2, ... c
    # get all coefficients that have y>=i
    p_trt_more[i] <- 1 - plogis(b$zeta[i-1] - 0.5 * b$coefficients[1])
    
    # calculate p(y <= i | control) for i = 1, ..., c-1
    p_ctl_less[i-1] <- plogis(b$zeta[i-1] + 0.5 * b$coefficients[1]) 
    # calculate p(y <= i | treatment) for i = 2, ... c
    # get all coefficients that have y>=i
    p_trt_less[i-1] <- plogis(b$zeta[i-1] - 0.5 * b$coefficients[1])
  }
  
  # calculate p(Y = i) for i = 1, ... , c
  for (i in 1:c) {
    if (i == 1) {
      p_y_ctl[1] <-  p_ctl_less[1]
    } else if (i == c) {
      p_y_ctl[c] <- 1 - sum(p_y_ctl[1:(c-1)])
    } else {
      p_y_ctl[i] <-  p_ctl_less[i] - sum(p_y_ctl[1:(i-1)])
    }
  }
  
  RD_leq <- p_trt_less - p_ctl_less
  RD_geq <- p_ctl_more - p_trt_more
  
  OR <- (p_trt_less / (1- p_trt_less)) / (p_ctl_less / (1- p_ctl_less))
  
  j <- 1:(c-1)
  k <- 2:c
  numerator <- sum(p_y_ctl[j] * (p_trt_less[j] - p_ctl_less[j])) + sum(p_y_ctl[k] * (p_ctl_more[k] - p_trt_more[k]))
  denominator <- sum(p_y_ctl[j]) + sum(p_y_ctl[k])
  weighted_RD <- numerator/denominator
  return(list(RD_leq = RD_leq, RD_geq = RD_geq, OR = OR, weighted_RD = weighted_RD))
  
}
