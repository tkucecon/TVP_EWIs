
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
  source("util/predictMCMC.r")
  
# set the parallel computation option-------------------------------------------
  
  # set options
  # rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()) 
  
# ------------------------------------------------------------------------------
# Load the data
# ------------------------------------------------------------------------------
  
  # load the JST data
  load("../4_data/df_JST_normalized.rda")
  
  # crisis dummy with baseline explanatory variables
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
  
  # specify the reference year
  ref.year <- 2000
  
  # Split the data into training/test data set
  df.crisis.baseline.train <- 
    df.crisis.baseline %>% 
    filter(year < ref.year)
  
  df.crisis.baseline.test <- 
    df.crisis.baseline %>% 
    filter(year >= ref.year)

# ------------------------------------------------------------------------------
# estimate MCMC using the training sample and obtain the estimates of beta
# ------------------------------------------------------------------------------
  
  # gaussian transition with df.crisis.baseline.train
  saveMCMC(df        = df.crisis.baseline.train,
           target    = "crisis",
           stan.file = "gaussian.stan",
           MCMC.name = "gaussian_crisis_baseline_train2000.rda")
  
  
  # check the result of MCMC
  plot.heat(MCMC.name  = "gaussian_crisis_baseline_train2000.rda",
            graph.name = "gaussian_crisis_baseline_heat_train2000.pdf")
  
  plot.dynamic(MCMC.name  = "gaussian_crisis_baseline_train2000.rda",
               graph.name = "gaussian_crisis_baseline_ts_train2000.pdf")
  
# ------------------------------------------------------------------------------
# Compare ROC curves and AUC
# ------------------------------------------------------------------------------
  
  # obtain out-of-sample predictions with the latest estimates of beta
  df.pred <- 
    predictMCMC(MCMC.name = "gaussian_crisis_baseline_train2000.rda",
                df.test   = df.crisis.baseline.test)
  
  # check the confusion matrix

  # plot the ROC curve
  library("pROC")
  roc.MCMC <- roc(df.pred, crisis, pred.MCMC)
  plot(roc.MCMC)
  auc(roc.MCMC)
  
