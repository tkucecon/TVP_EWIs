
# ------------------------------------------------------------------------------
# About this code
# estimate by NUTS algorithm (MCMC) given a data frame and Stan code
# ------------------------------------------------------------------------------

saveNUTS <- 
  function(df,                  # data frame: explanatory variables and
           stan.file,           # name of stan file
           train       = FALSE, # indicates if this is used for training
           hyperparams = NULL   # hyper parameters: default is NULL
           ){
    
# ------------------------------------------------------------------------------
# Prepare the data set
# ------------------------------------------------------------------------------
  
  # create the country ID according to countries
  df <- 
    df %>% 
    group_by(year) %>% 
    mutate(time.id = cur_group_id()) %>% 
    ungroup()
  
  # create a data frame indicating year and ID 
  df.time.id <- 
    df %>% 
    select(year, time.id) %>% 
    unique() %>% 
    arrange(time.id)
  
  # matrix of explanatory variables
  X.exp <- 
    df %>% 
    select(-year, -country, -crisis, -time.id) %>% 
    as.matrix()
  
  # add the intercept
  X <- cbind(1, X.exp)
  
  # vector of dependent variable: crisis
  Y <- 
    df %>% 
    select(crisis) %>% 
    mutate(crisis = as.numeric(crisis) - 1) %>% 
    unlist() %>% 
    as.numeric()

  # vector of time id
  Tid <- 
    df$time.id
  
  # number of variables
  p <- ncol(X)
  
  # input data
  data.stan <- 
    list(I        = nrow(X), 
         p        = ncol(X),
         Tmax     = max(Tid),
         X        = X,
         Y        = Y,
         Tid      = Tid
    )
  
  # if hyper-parameter is given, merge with the input data
  data.stan <- 
    c(data.stan, hyperparams)
  
  # indicate output pars
  pars <- c("beta", "theta")
  
  # if NGG is indicated as the stan file, save the result of hyper-parameter estimation as well
  if (stan.file == "NGG") {
    hyper.pars <- c("a_xi", "c_xi", "kappa_b", "a_tau", "c_tau", "lambda_b")
    pars <- c(pars, hyper.pars)
  }
  
# ------------------------------------------------------------------------------
# run MCMC
# ------------------------------------------------------------------------------
  
  # fit the Bayesian model
  fit.stan <- 
    stan(file       = paste("stan/", stan.file, ".stan", sep = ""),
         data       = data.stan,
         pars       = pars,
         seed       = 2292,
         warmup     = 5000,
         iter       = 7000)
  
  # extract the MCMC result
  extracted.stan <- 
    extract(fit.stan)
  
# ------------------------------------------------------------------------------
# Save quantiles of beta
# ------------------------------------------------------------------------------
  
  # name of covariates: 
  varnames <- colnames(X)
  varnames[1] <- "intercept"
  
  # obtain the quantiles of beta
  for (i in 1:p) {
    
    # check the dynamics of the series
    df.b_i <- 
      extracted.stan$beta[, i, ] %>% 
      t() %>% 
      as_tibble() %>% 
      mutate(time.id = row_number()) %>% 
      gather(key = iter, value = value, -time.id) %>% 
      group_by(time.id) %>% 
      summarise(
        p05    = quantile(value, 0.05),
        p16    = quantile(value, 0.16),
        median = quantile(value, 0.5),
        p84    = quantile(value, 0.84),
        p95    = quantile(value, 0.95),
      ) %>% 
      left_join(df.time.id, by = "time.id") %>% 
      mutate(varname = varnames[i]) %>% 
      dplyr::select(varname, year, p05, p16, median, p84, p95)
    
    # combine with the output data frame
    if (i == 1) {
      # save as the output file
      df.beta <- df.b_i
    }else{
      # merge with the output file
      df.beta <- 
        rbind(df.beta, df.b_i)
    }
  }
  
  out.list <- list(df.beta)
  
# ------------------------------------------------------------------------------
# Save theta
# ------------------------------------------------------------------------------

  # obtain theta
  df.theta <- 
      extracted.stan$theta %>% 
      as_tibble() 
  
  # save the column name
  colnames(df.theta) <- varnames
  
  # save into output list
  out.list <- append(out.list, list(df.theta))
    
# ------------------------------------------------------------------------------
# Save hyper-parameters (if indicated)
# ------------------------------------------------------------------------------
  
  # save the result of hyper-parameter estimation
  # ... if NGG is indicated as the stan file
  if (stan.file == "NGG") {
    
    # obtain tibbles and merge with the output data frame
    for (current.par in hyper.pars) {
      
      # obtain a tibble for current hyper parameter
      df.par_i <- 
        extracted.stan[current.par] %>% 
        as_tibble() 
      
      # merge with the output data frame
      if (current.par == "a_xi") {
        df.hyperparam <- df.par_i
      } else {
        df.hyperparam <- 
          cbind(df.hyperparam, df.par_i)
      }
    }
    
    # save into output list
    out.list <- append(out.list, list(df.hyperparam))
  }

# ------------------------------------------------------------------------------
# Save the result
# ------------------------------------------------------------------------------
  
  # This process takes some time... 
  # save the result in the tmp folder
  if (train) {
    save(out.list, file = paste("../5_tmp/", stan.file, "_train.rda", sep = ""))
  } else {
    save(out.list, file = paste("../5_tmp/", stan.file, ".rda", sep = ""))
  }
}
