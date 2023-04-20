
# ------------------------------------------------------------------------------
# About this code
# Fit Stan code to estimate the main regressions
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("ggthemes")
  theme_set(theme_solarized())
  library("patchwork")
  
  library("tidymodels")
  library("rstan")
  
  # some functions
  source("util/regdf.r")
  source("util/saveNUTS.r")
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
  
  # load the core data
  load("../4_data/df_core.rda")
  
# ------------------------------------------------------------------------------
# run MCMC in multiple settings and save under 5_tmp folder: for JST crisis 
# ------------------------------------------------------------------------------
  
  # 1. horseshoe prior
  saveNUTS(df        = df.core,
           target    = "JST",
           stan.file = "horseshoe")

  # 4. regularized horseshoe prior
  saveNUTS(df        = df.core,
           target    = "JST",
           stan.file = "reg_horseshoe",
           p0        = 4)

# ------------------------------------------------------------------------------
# plot the results
# ------------------------------------------------------------------------------

  # 1. horseshoe prior
  plot.heat("horseshoe_JST")
  plot.dynamic("horseshoe_JST")

  # regularized horseshoe prior
  plot.heat("reg_horseshoe_JST")
  plot.dynamic("reg_horseshoe_JST")

  