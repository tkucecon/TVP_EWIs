
# ------------------------------------------------------------------------------
# About this code
# Obtain out-of-sample predictions using the estimated stan file
# ------------------------------------------------------------------------------

predict.STVP <- 
  function(MCMC.name,
           df.test){
    
    # load the output data
    load(paste("../5_tmp/", MCMC.name, ".rda", sep = ""))  
    
    # load beta from the MCMC result 
    df.beta <- 
      out.list[[1]]
    
    # obtain the latest median estimates of beta
    df.beta.last <- 
      df.beta %>% 
      filter(year == max(year)) %>% 
      select(varname, median) 
    
    # save the varnames
    varnames <- unlist(df.beta.last$varname)
    
    # save the latest beta
    mat.beta.last <- as.matrix(df.beta.last$median)
    
    # obtain Y and X as matrix
    mat.X <- 
      df.test %>% 
      mutate(intercept = 1) %>% 
      select(all_of(varnames)) %>% 
      as.matrix()
    
    mat.Y <-
      df.test %>% 
      select(crisis) %>% 
      as.matrix()
    
    # calculate q's
    mat.q <- 
      mat.X %*% mat.beta.last
    
    # obtain the probability of crisis
    mat.prob <- 
      exp(mat.q) / (1 + exp(mat.q))
    
    # combine with the true crisis data and save as a data frame
    df.pred <- 
      data.frame(crisis    = as.numeric(mat.Y), 
                 pred.STVP = mat.prob) %>% 
      as_tibble()

    # return 
    return(df.pred)
}
