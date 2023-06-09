
# ------------------------------------------------------------------------------
# About this code
# estimate by NUTS algorithm (MCMC) given a data frame and Stan code
# ------------------------------------------------------------------------------

saveNUTS <- 
  function(df,                   # data frame
           stan.file,            # name of stan file
           target,               # target crisis variable
           total.credit = TRUE,  # use total credit instead of household/business credit
           train        = FALSE, # indicates if this is used for training
           hyperparams  = NULL,  # hyper parameters: default is NULL
           tau0         = NULL   # global scale if indicated
           ){
    
# ------------------------------------------------------------------------------
# Prepare the data set
# ------------------------------------------------------------------------------
  
  # process the input data set with regdf if train is False
  # otherwise, use the training data frame as given
  if (!is.numeric(train)) {
    df <- regdf(df           = df,
                target       = target,
                total.credit = total.credit)
  }
    
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
  
  # merge the hyper parameters is given from outside of the model
  data.stan <- 
    c(data.stan, hyperparams)
  
  # if p0 is given as number, save the global scale parameter
  if (is.numeric(tau0)) {
    data.stan <- 
      c(data.stan, list(scale_global = tau0))
  }
  
  # indicate output pars
  pars <- c("beta", "theta")
  
# ------------------------------------------------------------------------------
# run MCMC
# ------------------------------------------------------------------------------
  
  # fit the Bayesian model
  fit.stan <- 
    stan(file       = paste("stan/", stan.file, ".stan", sep = ""),
         data       = data.stan,
         pars       = pars,
         seed       = 2292,
         warmup     = 2000,
         iter       = 3000)
  
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
# Save the result
# ------------------------------------------------------------------------------
  
  # if train is given as a numeric, change the directory
  if (is.numeric(train)) {
    file.path <- paste("../5_tmp/train", train, "/", sep = "")
  } else{
    file.path <- "../5_tmp/fullsample/"
  }

  # save the method and target variable in file name
  file.name <- paste(stan.file, "_", target, sep = "")

  # if total.credit is TRUE, then indicate as "tot"
  if (total.credit) {
    file.name <- paste(file.name, "_tot", sep = "")
  } else {
    file.name <- paste(file.name, "_sep", sep = "")
  }
  
  # if p0 is given, save the number in the file name
  if (is.numeric(tau0)) {
    file.name <- paste(file.name, "_tau", tau0, sep = "")
  }
  
  # This process takes some time... 
  # save the result in the tmp folder
  save(out.list, file = paste(file.path, file.name, ".rda", sep = ""))
}
