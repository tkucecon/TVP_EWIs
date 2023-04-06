
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
           starts_with("diff.iy"),
           diff.ca.dom,
           diff.dsr.dom,
           growth.hpreal.dom,
           # bank balance sheet variables
           starts_with("level.lev")
           ) %>% 
    na.omit()
  
  # data frame 2: bank equity return (f2) with baseline explanatory variables
  df.eqf2.baseline <- 
    df.JST.normalized %>% 
    select(year, country, bank.eq.f2, 
           # macro variables
           starts_with("diff.credit"),
           starts_with("level.slope"),
           starts_with("growth.cpi"),
           starts_with("diff.money"),
           starts_with("growth.equity"),
           starts_with("diff.iy"),
           diff.ca.dom,
           diff.dsr.dom,
           growth.hpreal.dom,
           # bank balance sheet variables
           starts_with("level.lev")
    ) %>% 
    na.omit()

  # data frame 3: crisis dummy with all explanatory variables (excluding gap vars)
  df.crisis.allvars <- 
    df.JST.normalized %>% 
    select(year, country, crisis, 
           # macro variables
           starts_with("level."),
           starts_with("diff."),
           starts_with("growth.")
    ) %>% 
    na.omit()
  
  # data frame 4: bank equity return (f2) with all explanatory variables (excluding gap vars)
  df.eqf2.allvars <- 
    df.JST.normalized %>% 
    select(year, country, bank.eq.f2, 
           # macro variables
           starts_with("level."),
           starts_with("diff."),
           starts_with("growth.")
    ) %>% 
    na.omit()
  
  
# ------------------------------------------------------------------------------
# run MCMC in multiple settings and save under 5_tmp folder
# ------------------------------------------------------------------------------
  
  # 1. gaussian transition with df.crisis.baseline
  saveMCMC(df        = df.crisis.baseline,
           target    = "crisis",
           stan.file = "gaussian.stan",
           MCMC.name = "gaussian_crisis_baseline.rda")
  
  # 2. gaussian transition with df.equity.baseline
  saveMCMC(df        = df.eqf2.baseline,
           target    = "bank.eq.f2",
           stan.file = "lmgaussian.stan",
           MCMC.name = "lmgaussian_eqf2_baseline.rda")
  
  # 3. gaussian transition with df.crisis.allvars
  saveMCMC(df        = df.crisis.allvars,
           target    = "crisis",
           stan.file = "gaussian.stan",
           MCMC.name = "gaussian_crisis_allvars.rda")
  
  # 4. gaussian transition with df.equity.allvars
  saveMCMC(df        = df.eqf2.allvars,
           target    = "bank.eq.f2",
           stan.file = "lmgaussian.stan",
           MCMC.name = "lmgaussian_eqf2_allvars.rda")
  
# ------------------------------------------------------------------------------
# plot the results
# ------------------------------------------------------------------------------

  # 1. gaussian transition with df.crisis.baseline
  plot.heat(MCMC.name  = "gaussian_crisis_baseline.rda",
            graph.name = "gaussian_crisis_baseline_heat.pdf")
  
  plot.dynamic(MCMC.name  = "gaussian_crisis_baseline.rda",
               graph.name = "gaussian_crisis_baseline_ts.pdf")

  # 2. gaussian transition with df.equity.baseline
  plot.heat(MCMC.name  = "lmgaussian_eqf2_baseline.rda",
            graph.name = "lmgaussian_eqf2_baseline_heat.pdf")
  
  plot.dynamic(MCMC.name  = "lmgaussian_eqf2_baseline.rda",
               graph.name = "lmgaussian_eqf2_baseline_ts.pdf")
  
  # 3. gaussian transition with df.equity.allvars
  plot.heat(MCMC.name  = "lmgaussian_eqf2_allvars.rda",
            graph.name = "lmgaussian_eqf2_allvars_heat.pdf")
  
  plot.dynamic(MCMC.name  = "lmgaussian_eqf2_allvars.rda",
               graph.name = "lmgaussian_eqf2_allvars_ts.pdf")
  
  # 4. gaussian transition with df.equity.baseline
  plot.heat(MCMC.name  = "lmgaussian_eqf2_allvars.rda",
            graph.name = "lmgaussian_eqf2_allvars_heat.pdf")
  
  plot.dynamic(MCMC.name  = "lmgaussian_eqf2_allvars.rda",
               graph.name = "lmgaussian_eqf2_allvars_ts.pdf")
  