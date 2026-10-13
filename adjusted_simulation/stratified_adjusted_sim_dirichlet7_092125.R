#array simulation code 
# adjusted simulation
source('/users/3/turne899/Stratified_Model/adjusted_sim_7level_092125/adjusted_strat_sim_functions_092125.R')
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
library(rje)



#this is pulling the array # (from my bash file you can see I am using array #s 1-8)
val <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

#all_results <- matrix(NA, nrow = 2000, ncol = 95)
#all_results <- matrix(NA, nrow = 400, ncol = 95)
start <- determine_start_i40(val = val)

myCluster <- makeCluster(8)
registerDoParallel(myCluster)


all_results <- foreach (i = start:(start + 49), .combine = 'rbind',
                        .packages = c("rmsb", "LaplacesDemon", "tidyverse", "rstan",
                                      "parallel", "foreach", "BuyseTest", "ordinal", "rje",
                                      "doParallel")) %dopar% {
    source('/users/3/turne899/Stratified_Model/adjusted_sim_7level_092125/adjusted_strat_sim_functions_092125.R')
    set.seed(12345 + i)
    dat <- DataGen_array_adjusted7_091825(n = 1000, array = val)
    dat$trt <- dat$trt_05
                                        
    a <- data_setup_adj_LT(formula = y ~ trt + x2 + x3, 
                        ppo = ~ trt, data = dat, stratum = 2)
    fit_covid <- stan(
      file = "/users/3/turne899/Stratified_Model/lrmcppo_LT_test.stan",  # Stan program
      data = a$d,    # named list of data
      chains = 4,             # number of Markov chains
      warmup = 1000,          # number of warmup iterations per chain
      iter = 2000,            # total number of iterations per chain
      cores = 4,              # number of cores (could use one per chain)
      refresh = 0             # no progress shown
      )
                                        
    # fit adjusted model with blrm 
    b_ppo_model <- blrm(formula = y ~ trt_05 + x2 + x3, ppo = ~ trt_05, 
                        data = dat, method = "both", iter = 2000,
                        warmup = 1000, 
                        iprior = 0)
                                     
    utility1 <- rev(seq(0, 100, 100/6))
    strat_adj_est <- strat_adjust(stanfit = fit_covid, structured_dat = a,
                              dat = dat, trt = "trt", outcome = "y",
                              covs = c("x2", "x3"), utility = utility1, outcome_levels = 7)
    names(strat_adj_est) <- paste0(names(strat_adj_est), "_", paste(round(utility1), collapse = '_'))
    strat_adj_cis <- strat_adjust_CI(stanfit = fit_covid, structured_dat = a,
                                 dat = dat, trt = "trt", outcome = "y",
                                 covs = c("x2", "x3"), utility = utility1, outcome_levels = 7)
    names(strat_adj_cis) <- paste0(names(strat_adj_cis), "_", paste(round(utility1), collapse = '_'))
    blrm_est1 <- blrm_adj_bb_PPO_est(b = b_ppo_model, dat = dat, trt = "trt_05",
                                outcome = "y", covs = c("x2", "x3"), utility = utility1,
                                iprior = 0, outcome_levels = 7)
    names(blrm_est1) <- paste0(names(blrm_est1), "_", paste(round(utility1), collapse = '_'), "blrm")
    blrm_ci1 <- blrm_CI_adj_bb(b = b_ppo_model, dat = dat, trt = "trt_05",
                               outcome = "y", covs = c("x2", "x3"), utility = utility1,
                               iprior = 0, outcome_levels = 7)
    names(blrm_ci1) <- paste0(names(blrm_ci1), "_", paste(round(utility1), collapse = '_'), "blrm")
    
    #consider 7 levels: nothing, hypoxemia, ED visit, Hospitalization, hosp with vent, 
    #hosp with vent for >3 days, death
    utility2 <- c(100, 90, 60, 15, 10, 5, 0)
    strat_adj_est2 <- strat_adjust(stanfit = fit_covid, structured_dat = a,
                               dat = dat, trt = "trt", outcome = "y",
                               covs = c("x2", "x3"), utility = utility2, outcome_levels = 7)
    names(strat_adj_est2) <- paste0(names(strat_adj_est2), "_", paste(utility2, collapse = '_'))
    strat_adj_cis2 <- strat_adjust_CI(stanfit = fit_covid, structured_dat = a,
                                  dat = dat, trt = "trt", outcome = "y",
                                  covs = c("x2", "x3"), utility = utility2, outcome_levels = 7)
    names(strat_adj_cis2) <- paste0(names(strat_adj_cis2), "_", paste(utility2, collapse = '_'))
    blrm_est2 <- blrm_adj_bb_PPO_est(b = b_ppo_model, dat = dat, trt = "trt_05",
                                     outcome = "y", covs = c("x2", "x3"), utility = utility2,
                                     iprior = 0, outcome_levels = 7)
    names(blrm_est2) <- paste0(names(blrm_est2), "_", paste(utility2, collapse = '_'), "blrm")
    blrm_ci2 <- blrm_CI_adj_bb(b = b_ppo_model, dat = dat, trt = "trt_05",
                               outcome = "y", covs = c("x2", "x3"), utility = utility2,
                               iprior = 0, outcome_levels = 7)
    names(blrm_ci2) <- paste0(names(blrm_ci2), "_", paste(utility2, collapse = '_'), "blrm")
                             
    #consider 7 levels: nothing, hypoxemia, ED visit, Hospitalization, hosp with vent, 
    #hosp with vent for >3 days, death        
    utility3 <- c(100, 90, 80, 60, 50, 40, 0)
    strat_adj_est3 <- strat_adjust(stanfit = fit_covid, structured_dat = a,
                               dat = dat, trt = "trt", outcome = "y",
                               covs = c("x2", "x3"), utility = utility3, outcome_levels = 7)
    names(strat_adj_est3) <- paste0(names(strat_adj_est3), "_", paste(utility3, collapse = '_'))
                                        
    strat_adj_cis3 <- strat_adjust_CI(stanfit = fit_covid, structured_dat = a,
                                  dat = dat, trt = "trt", outcome = "y",
                                  covs = c("x2", "x3"), utility = utility3, outcome_levels = 7)
    names(strat_adj_cis3) <- paste0(names(strat_adj_cis3), "_", paste(utility3, collapse = '_'))
    
    blrm_est3 <- blrm_adj_bb_PPO_est(b = b_ppo_model, dat = dat, trt = "trt_05",
                                     outcome = "y", covs = c("x2", "x3"), utility = utility3,
                                     iprior = 0, outcome_levels = 7)
    names(blrm_est3) <- paste0(names(blrm_est3), "_", paste(utility3, collapse = '_'), "blrm")
    blrm_ci3 <- blrm_CI_adj_bb(b = b_ppo_model, dat = dat, trt = "trt_05",
                               outcome = "y", covs = c("x2", "x3"), utility = utility3,
                               iprior = 0, outcome_levels = 7)
    names(blrm_ci3) <- paste0(names(blrm_ci3), "_", paste(utility3, collapse = '_'), "blrm")
                                        
                                        
    # unadjusted model 
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
    
    strat_est <- order_weighted_RD2_strat(stanfit = fit_covid, 
                                          dat = dat, trt = "trt", outcome = "y",
                                          utility = utility1, outcome_levels = 7)
    names(strat_est) <- paste0(names(strat_est), "_", paste(round(utility1), collapse = '_'), "unadj")
    strat_cis <- order_weighted_RD2_CI_strat(stanfit = fit_covid, 
                                             dat = dat,
                                             trt = "trt", outcome = "y",
                                             utility = utility1, outcome_levels = 7)
    names(strat_cis) <- paste0(names(strat_cis), "_", paste(round(utility1), collapse = '_'), "unadj")
    
    strat_est2 <- order_weighted_RD2_strat(stanfit = fit_covid, 
                                           dat = dat, trt = "trt", outcome = "y",
                                           utility = utility2, outcome_levels = 7)
    names(strat_est2) <- paste0(names(strat_est2), "_", paste(utility2, collapse = '_'), "unadj")
    
    strat_cis2 <- order_weighted_RD2_CI_strat(stanfit = fit_covid, 
                                              dat = dat,
                                              trt = "trt", outcome = "y",
                                              utility = utility2, outcome_levels = 7)
    names(strat_cis2) <- paste0(names(strat_cis2), "_", paste(utility2, collapse = '_'), "unadj")
    
    strat_est3 <- order_weighted_RD2_strat(stanfit = fit_covid, 
                                           dat = dat, trt = "trt", outcome = "y",
                                           utility = utility3, outcome_levels = 7)
    names(strat_est3) <- paste0(names(strat_est3), "_", paste(utility3, collapse = '_'), "unadj")
    
    strat_cis3 <- order_weighted_RD2_CI_strat(stanfit = fit_covid, 
                                              dat = dat,
                                              trt = "trt", outcome = "y",
                                              utility = utility3, outcome_levels = 7)
    names(strat_cis3) <- paste0(names(strat_cis3), "_", paste(utility3, collapse = '_'), "unadj")
    

    c(i, strat_adj_est, strat_adj_cis, strat_adj_est2, strat_adj_cis2, strat_adj_est3, strat_adj_cis3, 
      blrm_est1, blrm_ci1, blrm_est2, blrm_ci2, blrm_est3, blrm_ci3, 
      strat_est, strat_cis, strat_est2, strat_cis2, strat_est3, strat_cis3)
                                        
    }

stopImplicitCluster()
write_rds(all_results, paste0('/users/3/turne899/Stratified_Model/adjusted_sim_7level_092125/results_array_7level_092125_',  val, '.rds'))


