
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
  
# set the parallel computation option-------------------------------------------
  
  # set options
  # rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()) 
  
# ------------------------------------------------------------------------------
# Load the JST data
# ------------------------------------------------------------------------------
  
  # load the JST data
  load("../4_data/df_JST_normalized.rda")
  
  # keep only the relevant variables and remove NA rows:
  
  # data frame 1: crisis dummy with baseline explanatory variables
  df.crisis.baseline <- 
    df.JST.normalized %>% 
    select(year, country, crisis, 
           # macro variables
           starts_with("diff.credit"),
           starts_with("level.slope"),
           starts_with("growth.cpi"),
           starts_with("diff.money"),
           starts_with("growth.equity"),
           starts_with("growth.hpreal"),
           starts_with("diff.iy"),
           diff.ca.dom,
           diff.dsr.dom,
           # bank balance sheet variables
           starts_with("level.lev")
           ) %>% 
    na.omit()
  
# ------------------------------------------------------------------------------
# run MCMC in multiple settings and save under 5_tmp folder
# ------------------------------------------------------------------------------
  
  # 1. gaussian transition with df.crisis.baseline: horseshoe prior
  saveMCMC(df        = df.crisis.baseline,
           target    = "crisis",
           stan.file = "gaussian.stan",
           MCMC.name = "gaussian_crisis_baseline.rda")

  # 2. gaussian transition with df.crisis.baseline: NGG prior
  saveMCMC(df        = df.crisis.baseline,
           target    = "crisis",
           stan.file = "normal-gamma-gamma.stan",
           MCMC.name = "NGG_crisis_baseline.rda")
  
# ------------------------------------------------------------------------------
# plot the results
# ------------------------------------------------------------------------------

  # 1. gaussian transition with df.crisis.baseline
  plot.heat(MCMC.name  = "gaussian_crisis_baseline.rda",
            graph.name = "gaussian_crisis_baseline_heat.pdf")
  
  plot.dynamic(MCMC.name  = "gaussian_crisis_baseline.rda",
               graph.name = "gaussian_crisis_baseline_ts.pdf")

  # 2. gaussian transition with df.crisis.baseline: NGG prior
  plot.heat(MCMC.name  = "NGG_crisis_baseline.rda",
            graph.name = "NGG_crisis_baseline_heat.pdf")
  
  plot.dynamic(MCMC.name  = "NGG_crisis_baseline.rda",
               graph.name = "NGG_crisis_baseline_ts.pdf")
  