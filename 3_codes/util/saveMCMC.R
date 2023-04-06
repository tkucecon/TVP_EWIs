
# ------------------------------------------------------------------------------
# About this code
# run MCMC given a data frame and Stan code
# ------------------------------------------------------------------------------

saveMCMC <- 
  function(df,            # data frame: explanatory variables and
           target,        # binary "crisis" or other numeric variables
           stan.file,     # name of stan file
           MCMC.name      # the name of the output file 
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
    select(-year, -country, -all_of(target), -time.id) %>% 
    as.matrix()
  
  # add the intercept
  X <- cbind(1, X.exp)
  
  # vector of independent variable
  if (target == "crisis") {
    Y <- 
      df %>% 
      select(crisis) %>% 
      mutate(crisis = as.numeric(crisis) - 1) %>% 
      unlist() %>% 
      as.numeric()
  } else {
    Y <- 
      df %>% 
      select(all_of(target)) %>% 
      unlist() %>% 
      as.numeric()
  }
  
  # vector of time id
  Tid <- 
    df$time.id
  
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
# run MCMC
# ------------------------------------------------------------------------------
  
  # fit the Bayesian model
  fit.stan <- 
    stan(file       = paste("stan/", stan.file, sep = ""),
         data       = data.stan,
         pars       = c("beta", "theta"),
         seed       = 2292,
         warmup     = 2000,
         iter       = 3000)
  
  # create a list of output data
  out.stan <- list(data.stan, fit.stan, df.time.id)

  # This process takes some time... 
  # save the result in the tmp folder
  save(out.stan, file = paste("../5_tmp/", MCMC.name, sep = ""))
}
