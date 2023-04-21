
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
  
  # 1. horseshoe prior with JST defined crisis data
  saveNUTS(df        = df.core,
           target    = "JST",
           stan.file = "horseshoe")
  
  saveNUTS(df           = df.core,
           target       = "JST",
           total.credit = FALSE,
           stan.file    = "horseshoe")

  # 2. horseshoe prior with joint crisis data
  saveNUTS(df        = df.core,
           target    = "joint",
           stan.file = "horseshoe")

  # 3. horseshoe prior with BVX crisis data
  saveNUTS(df        = df.core,
           target    = "BVX",
           stan.file = "horseshoe")
  
  # 4. regularized horseshoe prior
  saveNUTS(df        = df.core,
           target    = "JST",
           stan.file = "reghorse",
           p0        = 1)

  saveNUTS(df        = df.core,
           target    = "JST",
           stan.file = "reghorse",
           p0        = 2)
  
  saveNUTS(df        = df.core,
           target    = "JST",
           stan.file = "reghorse",
           p0        = 3)

  saveNUTS(df        = df.core,
           target    = "JST",
           stan.file = "reghorse",
           p0        = 4)

# ------------------------------------------------------------------------------
# plot the results
# ------------------------------------------------------------------------------

  # 1. horseshoe prior
  plot.heat(file.path = "fullsample", file.name = "horseshoe_JST_tot")
  plot.dynamic(file.path = "fullsample", file.name = "horseshoe_JST_tot")

  plot.heat(file.path = "fullsample", file.name = "horseshoe_JST_sep")
  plot.dynamic(file.path = "fullsample", file.name = "horseshoe_JST_sep")
  
  # 2. horseshoe prior
  plot.heat(file.path = "fullsample", file.name = "horseshoe_joint_tot")
  plot.dynamic(file.path = "fullsample", file.name = "horseshoe_joint_tot")
  
  # 3. horseshoe prior
  plot.heat(file.path = "fullsample", file.name = "horseshoe_BVX_tot")
  plot.dynamic(file.path = "fullsample", file.name = "horseshoe_BVX_tot")
  
  # 4. horseshoe prior
  plot.heat(file.path = "fullsample", file.name = "reghorse_JST_tot_tvp1")
  plot.dynamic(file.path = "fullsample", file.name = "reghorse_JST_tot_tvp1")

  plot.heat(file.path = "fullsample", file.name = "reghorse_JST_tot_tvp2")
  plot.dynamic(file.path = "fullsample", file.name = "reghorse_JST_tot_tvp2")

  plot.heat(file.path = "fullsample", file.name = "reghorse_JST_tot_tvp3")
  plot.dynamic(file.path = "fullsample", file.name = "reghorse_JST_tot_tvp3")

  plot.heat(file.path = "fullsample", file.name = "reghorse_JST_tot_tvp4")
  plot.dynamic(file.path = "fullsample", file.name = "reghorse_JST_tot_tvp4")

