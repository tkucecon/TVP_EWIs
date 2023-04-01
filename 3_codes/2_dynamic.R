
# ------------------------------------------------------------------------------
# About this code
# Estimate dynamic Bayesian logistic regression
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
  load("../4_data/df_JST.rda")
  
  # create a recipe to impute the variables
  recipe.bag <- 
    recipe(x = df.JST, formula = as.formula(crisis ~ .)) %>% 
    # remove year and country from the predictors %>% 
    update_role(year,    new_role = "time variable") %>% 
    update_role(country, new_role = "id variable") %>% 
    # normalize all the predictors to be distributed ~ N(0,1) 
    step_normalize(all_predictors(), skip = FALSE) %>% 
    # delete rows with no variance
    step_zv(all_predictors(), skip = FALSE) 
  
  # create a data frame with imputed variables
  df.JST <- 
    recipe.bag %>% 
    prep() %>% 
    bake(new_data = df.JST)
  
  # keep only the relevant variables and remove NA rows
  df.JST <- 
    df.JST %>% 
    select(year, country, crisis, 
           starts_with("credit"),
           starts_with("slope"),
           starts_with("pi"),
           starts_with("money"),
           starts_with("equity"),
           starts_with("iy"),
           rcon.dom,
           ca.dom,
           dsr.dom,
           hpreal.dom,
           starts_with("lev")) %>% 
    na.omit()

# ------------------------------------------------------------------------------
# Run MCMC: dynamic logistic regression
# ------------------------------------------------------------------------------
  
  # create the country ID according to countries
  df.JST <- 
    df.JST %>% 
    group_by(year) %>% 
    mutate(time.id = cur_group_id()) %>% 
    ungroup()
  
  # create a data frame indicating year and ID 
  df.time.id <- 
    df.JST %>% 
    select(year, time.id) %>% 
    unique() %>% 
    arrange(time.id)
  
  # matrix of explanatory variables
  X.JST <- 
    df.JST %>% 
    select(-year, -country, -crisis, -time.id) %>% 
    as.matrix()
  
  # add the intercept
  X <- cbind(1, X.JST)
  
  # vector of response dummy variable
  Y <- 
    df.JST %>% 
    select(crisis) %>% 
    mutate(crisis = as.numeric(crisis) - 1) %>% 
    unlist() %>% 
    as.numeric()
  
  # vector of time id
  Tid <- 
    df.JST$time.id
  
  # input data
  data.stan <- 
    list(I        = nrow(X), 
         p        = ncol(X),
         Tmax     = max(Tid),
         X        = X,
         Y        = Y,
         Tid      = Tid
    )
  
  # fit the Bayesian model
  fit.stan <- 
    stan(file   = "5_dynamic.stan",
         data   = data.stan,
         seed   = 2292,
         pars   = c("beta", "theta"),
         warmup = 2000,
         iter   = 3000)
  
  # This process takes some time... save the result in the tmp folder
  save(fit.stan, file = "../5_tmp/fit_dynamic.rda")
  
# ------------------------------------------------------------------------------
# Check the posteriors of the MCMC
# ------------------------------------------------------------------------------

  # If you start from this line, run this code
  # load("../5_tmp/fit_dynamic.rda")
  
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
  
  
  