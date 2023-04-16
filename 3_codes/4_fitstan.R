
# ------------------------------------------------------------------------------
# About this code
# Fit Stan code and conduct MCMC to estimate the main regressions
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("ggthemes")
  theme_set(theme_solarized())
  library("patchwork")
  
  library("rstan")
  
  # some functions
  source("util/saveMCMC.r")
  source("util/plot_dynamic.r")
  source("util/plot_heat.r")
  source("util/plot_density.r")
  
# set the parallel computation option-------------------------------------------
  
  # set options
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()) 
  
# ------------------------------------------------------------------------------
# Load the normalized data
# ------------------------------------------------------------------------------
  
  # load the normalized data
  load("../4_data/df_normalized.rda")
  
# ------------------------------------------------------------------------------
# run MCMC in multiple settings and save under 5_tmp folder
# ------------------------------------------------------------------------------
  
  # 1. horseshoe prior
  saveMCMC(df        = df.normalized,
           stan.file = "horseshoe")

  # 2. Normal-Gamma-Gamma prior with estimated hyperparameter
  saveMCMC(df        = df.normalized,
           stan.file = "NGG")
  
  # 3. Normal-Gamma-Gamma prior with given hyperparameter
  hyperparams <- 
    list(a_xi     = 0.1,
         c_xi     = 0.1,
         kappa_b  = 50,
         a_tau    = 0.5,
         c_tau    = 0.5,
         lambda_b = 50
         )
  
  saveMCMC(df          = df.normalized,
           stan.file   = "NGG_manual",
           hyperparams = hyperparams)
  
# ------------------------------------------------------------------------------
# plot the results
# ------------------------------------------------------------------------------

  # 1. horseshoe prior
  plot.heat(MCMC.name = "horseshoe")
  plot.dynamic(MCMC.name = "horseshoe")
  plot.density(MCMC.name = "horseshoe", type = 2) 
  
  # 2. NGG prior with estimated hyper-parameters
  plot.heat(MCMC.name = "NGG")
  plot.dynamic(MCMC.name = "NGG")
  plot.density(MCMC.name = "NGG", type = 2) 
  plot.density(MCMC.name = "NGG", type = 3)  
  