
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
  
  # keep only the relevant variables and remove NA rows
  df.JST.normalized <- 
    df.JST.normalized %>% 
    select(year, country, crisis, 
           # macro variables
           starts_with("growth.gdpreal"),
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

# ------------------------------------------------------------------------------
# Prepare the data set
# ------------------------------------------------------------------------------
  
  # create the country ID according to countries
  df.JST.normalized <- 
    df.JST.normalized %>% 
    group_by(year) %>% 
    mutate(time.id = cur_group_id()) %>% 
    ungroup()
  
  # create a data frame indicating year and ID 
  df.time.id <- 
    df.JST.normalized %>% 
    select(year, time.id) %>% 
    unique() %>% 
    arrange(time.id)
  
  # matrix of explanatory variables
  X.JST <- 
    df.JST.normalized %>% 
    select(-year, -country, -crisis, -time.id) %>% 
    as.matrix()
  
  # add the intercept
  X <- cbind(1, X.JST)
  
  # vector of response dummy variable
  Y <- 
    df.JST.normalized %>% 
    select(crisis) %>% 
    mutate(crisis = as.numeric(crisis) - 1) %>% 
    unlist() %>% 
    as.numeric()
  
  # vector of time id
  Tid <- 
    df.JST.normalized$time.id
  
  # input data
  data.stan <- 
    list(I        = nrow(X), 
         p        = ncol(X),
         Tmax     = max(Tid),
         X        = X,
         Y        = Y,
         Tid      = Tid
    )
  
# ------------------------------------------------------------------------------
# Baseline: Gaussian state equation
# ------------------------------------------------------------------------------
  
  # fit the Bayesian model
  fit.stan <- 
    stan(file   = "stan/gaussian.stan",
         data   = data.stan,
         seed   = 2292,
         pars   = c("beta", "theta"),
         warmup = 2000,
         iter   = 3000)
  
  # This process takes some time... save the result in the tmp folder
  save(fit.stan, file = "../5_tmp/fit_gaussian.rda")
  
  # Check the posteriors of the MCMC
  # If you start from this line, run this code
  load("../5_tmp/fit_gaussian.rda")
  
  # extract the fitted model
  extracted.stan <- 
    extract(fit.stan)
  
  # plot the heat-map
  g.heat <- 
    plot.heat(significance = FALSE)
  
  # plot only significant variables
  g.sig.ts <- 
    plot.dynamic()
  
  # save the graph
  ggsave(plot = g.heat, 
         width = 10, height = 4, 
         filename = "../6_outputs/heat_gaussian.pdf")
  
  ggsave(plot = g.sig.ts, 
         width = 10, height = 4, 
         filename = "../6_outputs/ts_gaussian.pdf")
  
# ------------------------------------------------------------------------------
# Robustness check: structural change
# ------------------------------------------------------------------------------
  
  # fit the Bayesian model
  fit.stan <- 
    stan(file   = "stan/structural_change.stan",
         data   = data.stan,
         seed   = 2292,
         pars   = c("beta"),
         warmup = 2000,
         iter   = 3000)
  
  # This process takes some time... save the result in the tmp folder
  save(fit.stan, file = "../5_tmp/fit_structural_change.rda")
  
  # Check the posteriors of the MCMC
  # If you start from this line, run this code
  # load("../5_tmp/fit_structural_change.rda")
  
  # extract the fitted model
  extracted.stan <- 
    extract(fit.stan)
  
  # plot the heat-map
  g.heat <- 
    plot.heat(significance = FALSE)
  
  # plot only significant variables
  g.sig.ts <- 
    plot.dynamic()
  
  # save the graph
  ggsave(plot = g.heat, 
         width = 10, height = 4, 
         filename = "../6_outputs/heat_sc.pdf")
  
  ggsave(plot = g.sig.ts, 
         width = 10, height = 4, 
         filename = "../6_outputs/ts_sc.pdf")
  
  