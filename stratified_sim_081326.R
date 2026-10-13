#array simulation code 
source('/users/3/turne899/Stratified_Model/unadjusted_sim_081326_new/adjusted_strat_sim_functions_081326.R')
library(rmsb)
library(LaplacesDemon)
library(tidyverse)
library(parallel)
library(foreach)
library(BuyseTest)
library(ordinal)
library(doParallel)
library(doRNG)
library(rstan)

# simulation updated 2/23/25 to add in cumulative and both weights 

#this is pulling the array # (from my bash file you can see I am using array #s 1-8)
val <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

#all_results <- matrix(NA, nrow = 2000, ncol = 95)
#all_results <- matrix(NA, nrow = 400, ncol = 95)
start <- determine_start_i2(val = val)

myCluster <- makeCluster(8)
registerDoParallel(myCluster)



all_results <- foreach (i = start:(start + 499), .combine = 'rbind',
                        .packages = c("rmsb", "LaplacesDemon", "tidyverse", "rstan",
                                      "parallel", "foreach", "BuyseTest", "ordinal",
                                      "doParallel")) %dopar% {
                                        source('/users/3/turne899/Stratified_Model/unadjusted_sim_081326_new/adjusted_strat_sim_functions_081326.R')
                                        set.seed(12345 + i)
                                        dat <- DataGen_array(n = 1000, array = val)
                                        
                                        dat$trt <- dat$trt_05
                                        a <- data_setup_LT(formula = y ~ trt, 
                                                        ppo = ~ trt, 
                                                        data = dat,
                                                        stratum = 2)
                                        fit_covid <- stan(
                                          file = "/users/3/turne899/Stratified_Model/lrmcppo_LT.stan",  # Stan program
                                          data = a$d,    # named list of data
                                          chains = 4,             # number of Markov chains
                                          warmup = 1000,          # number of warmup iterations per chain
                                          iter = 2000,            # total number of iterations per chain
                                          cores = 4,              # number of cores (could use one per chain)
                                          refresh = 0             # no progress shown
                                        )
                                        
                                        utility <- c(100, 75, 50, 25, 0)
                                        strat_est <- order_weighted_RD2_strat(stanfit = fit_covid, 
                                                                              dat = dat, trt = "trt", outcome = "y",
                                                                              outcome_levels = 5, 
                                                                              utility = c(100, 75, 50, 25, 0))
                                        names(strat_est) <- paste0(names(strat_est), "_", paste(utility, collapse = '_'))
                                        strat_cis <- order_weighted_RD2_CI_strat(stanfit = fit_covid, 
                                                                                 dat = dat,
                                                                                 trt = "trt", outcome = "y",
                                                                                 outcome_levels = 5, 
                                                                                 utility = c(100, 75, 50, 25, 0))
                                        names(strat_cis) <- paste0(names(strat_cis), "_", paste(utility, collapse = '_'))
                                        
                                        utility <- c(100, 90, 60, 10, 0)
                                        strat_est2 <- order_weighted_RD2_strat(stanfit = fit_covid, 
                                                                              dat = dat, trt = "trt", outcome = "y",
                                                                              outcome_levels = 5, 
                                                                              utility = c(100, 90, 60, 10, 0))
                                        names(strat_est2) <- paste0(names(strat_est2), "_", paste(utility, collapse = '_'))
                                        
                                        strat_cis2 <- order_weighted_RD2_CI_strat(stanfit = fit_covid, 
                                                                                 dat = dat,
                                                                                 trt = "trt", outcome = "y",
                                                                                 outcome_levels = 5, 
                                                                                 utility = c(100, 90, 60, 10, 0))
                                        names(strat_cis2) <- paste0(names(strat_cis2), "_", paste(utility, collapse = '_'))
                                        
                                        utility <- c(100, 90, 80, 60, 0)
                                        strat_est3 <- order_weighted_RD2_strat(stanfit = fit_covid, 
                                                                               dat = dat, trt = "trt", outcome = "y",
                                                                               outcome_levels = 5, 
                                                                               utility = c(100, 90, 80, 60, 0))
                                        names(strat_est3) <- paste0(names(strat_est3), "_", paste(utility, collapse = '_'))
                                        
                                        strat_cis3 <- order_weighted_RD2_CI_strat(stanfit = fit_covid, 
                                                                                  dat = dat,
                                                                                  trt = "trt", outcome = "y",
                                                                                  outcome_levels = 5, 
                                                                                  utility = c(100, 90, 80, 60, 0))
                                        names(strat_cis3) <- paste0(names(strat_cis3), "_", paste(utility, collapse = '_'))
                                        
                                        
                                        c(i, strat_est, strat_cis, strat_est2, strat_cis2, strat_est3, strat_cis3)
                                        
                                      }

stopImplicitCluster()
write_rds(all_results, paste0('/users/3/turne899/Stratified_Model/unadjusted_sim_081326_new/results_array_081326_',  val, '.rds'))


