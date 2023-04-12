
# ------------------------------------------------------------------------------
# About this code
# Obtain out-of-sample predictions using the estimated stan file
# ------------------------------------------------------------------------------

predictMCMC <- 
  function(MCMC.name,
           df.test){
    
    # load the MCMC data and extract necessary information
    load(paste("../5_tmp/", MCMC.name, sep = ""))
    
    # extracted data 
    extracted.stan <- 
      extract(out.stan[[2]])
    
    # correspondence data frame between time id and year
    df.time.id <- out.stan[[3]]
    
    # number of covariates
    p <- out.stan[[1]]$p
    
    # name of covariates
    varnames <- colnames(out.stan[[1]]$X)
    
    # obtain the median of beta
    for (i in 1:p) {
      
      # check the dynamics of the series
      df.b_i <- 
        extracted.stan$beta[, i, ] %>% 
        t() %>% 
        as_tibble() %>% 
        mutate(time.id = row_number()) %>% 
        gather(key = iter, value = value, -time.id) %>% 
        group_by(time.id) %>% 
        summarise(median = quantile(value, 0.5)) %>% 
        left_join(df.time.id, by = "time.id") %>% 
        dplyr::select(year, median)
      
      # combine with the output data frame
      if (i == 1) {
        # the first column corresponds to the intercept
        colnames(df.b_i) <- c("year", "intercept")
        # save as the output file
        df.beta <- df.b_i
      }else{
        # check the name of the current variable
        varname <- varnames[i]
        colnames(df.b_i) <- c("year", varname)
        # merge with the output file
        df.beta <- 
          left_join(df.beta, df.b_i, by = "year")
      }
    }
    
    # obtain the latest median estimates of beta
    mat.beta.last <- 
      df.beta %>% 
      tail(n = 1) %>% 
      select(-year) %>% 
      t()
    
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
                 pred.MCMC = mat.prob)
    
    # return 
    return(df.pred)
}
