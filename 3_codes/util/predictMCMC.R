
# ------------------------------------------------------------------------------
# About this code
# Obtain out-of-sample predictions using the estimated stan file
# ------------------------------------------------------------------------------

predictMCMC <- 
  function(MCMC.name,
           df.test){
    
    # load the output data
    load(paste("../5_tmp/", MCMC.name, ".rda", sep = ""))  
    
    # load beta from the MCMC result 
    df.beta <- 
      out.list[[1]]
    
    # obtain the latest median estimates of beta
    mat.beta.last <- 
      df.beta %>% 
      filter(year == max(year)) %>% 
      select(median) %>% 
      as.matrix()
    
    # obtain Y and X as matrix
    mat.X <- 
      df.test %>% 
      select(-year, -crisis, -country) %>% 
      as.matrix()
    
    mat.Y <-
      df.test %>% 
      select(crisis) %>% 
      as.matrix()
    
    # calculate q's
    mat.q <- 
      cbind(1, mat.X) %*% mat.beta.last
    
    # obtain the probability of crisis
    mat.prob <- 
      exp(mat.q) / (1 + exp(mat.q))
    
    # combine with the true crisis data and save as a data frame
    df.pred <- 
      data.frame(crisis    = as.numeric(mat.Y), 
                 pred.MCMC = mat.prob) %>% 
      as_tibble() %>% 
      rename(pred.MCMC = median)

    # return 
    return(df.pred)
}
