
# ------------------------------------------------------------------------------
# About this code
# Obtain out-of-sample predictions using the estimated stan file
# ------------------------------------------------------------------------------

predict.STVP <- 
  function(file.path, 
           file.name,
           total.credit = TRUE,
           df.train,
           df.test){
    
    # load the output data
    load(paste("../5_tmp/", file.path, "/", file.name, ".rda", sep = ""))  
    
    # --------------------------------------------------------------------------
    # Obtain out of sample prediction for the Sparse TVP model
    # --------------------------------------------------------------------------
    
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
    
    # obtain X as matrix
    mat.X <- 
      df.test %>% 
      mutate(intercept = 1) %>% 
      select(all_of(varnames)) %>% 
      as.matrix()
    
    # obtain Y
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
    
    # --------------------------------------------------------------------------
    # Obtain in sample prediction for the Sparse TVP model
    # --------------------------------------------------------------------------
    
    # obtain medians of beta
    df.beta.median <- 
      df.beta %>% 
      select(varname, year, median) %>% 
      spread(key = varname, value = median) %>% 
      select(year, all_of(varnames))
    
    # obtain a series of beta as a matrix
    mat.beta.hist <- 
      df.train %>% 
      select(year) %>% 
      left_join(df.beta.median, by = "year") %>% 
      select(all_of(varnames)) %>% 
      as.matrix() %>% 
      t()
    
    mat.X.hist <- 
      df.train %>% 
      mutate(intercept = 1) %>% 
      select(all_of(varnames)) %>% 
      as.matrix()
    
    # obtain Y
    mat.Y.hist <-
      df.train %>% 
      select(crisis) %>% 
      as.matrix()
    
    # calculate q's
    mat.q.hist <- 
      diag(mat.X.hist %*% mat.beta.hist)
    
    # obtain the probability of crisis
    mat.prob.hist <- 
      exp(mat.q.hist) / (1 + exp(mat.q.hist))
    
    # combine with the true crisis data and save as a data frame
    df.pred.hist <- 
      data.frame(crisis    = as.numeric(mat.Y.hist), 
                 pred.STVP = mat.prob.hist) %>% 
      as_tibble()
    
    # --------------------------------------------------------------------------
    # Obtain prediction for the logit model
    # --------------------------------------------------------------------------
    
    # keep only relevant values
    df.logit.train <- 
      df.train %>% 
      select(-country, -year)
    
    df.logit.test <- 
      df.test %>% 
      select(-country, -year)
    
    # usual logistic regression
    model.logit <- 
      glm(as.factor(crisis) ~ ., data = df.logit.train, family = binomial())
    
    # predict
    pred.logit <- 
      predict(model.logit, newdata = df.logit.test, type = "response")

    pred.logit.hist <- 
      predict(model.logit, type = "response")
    
    # --------------------------------------------------------------------------
    # Obtain prediction for the LASSO model
    # --------------------------------------------------------------------------

    # store X and Y values as matrix for later use
    X.train <- 
      df.train %>% 
      select(-year, -country, -crisis) %>% 
      as.matrix()
    
    Y.train <- 
      df.train %>% 
      select(crisis) %>% 
      mutate(crisis = as.numeric(crisis) - 1) %>% 
      as.matrix()
    
    X.test <- 
      df.test %>% 
      select(-year, -country, -crisis) %>% 
      as.matrix()
    
    Y.test <- 
      df.test %>% 
      select(crisis) %>% 
      mutate(crisis = as.numeric(crisis) - 1) %>% 
      as.matrix()
    
    # conduct cross validation to decide lambda
    cv.lambda.lasso <- 
      cv.glmnet(x = cbind(1, X.train), # intercept should be added
                y = Y.train, 
                alpha = 1, # LASSO
                family = "binomial")
    
    # save the optimal lambda
    lambda.min <- cv.lambda.lasso$lambda.min
    
    # LASSO logistic regression
    model.lasso <- 
      glmnet(x = cbind(1, X.train),
             y = Y.train, 
             alpha = 1, # LASSO
             family = "binomial",
             lambda = lambda.min)
    
    # predict and assess the out-of-sample prediction 
    pred.lasso <- 
      predict.glmnet(model.lasso,
                     newx = cbind(1, X.test))
    
    # change into probability
    pred.lasso <-  exp(pred.lasso) / (1 + exp(pred.lasso))

    # predict and assess the out-of-sample prediction 
    pred.lasso.hist <- 
      predict.glmnet(model.lasso,
                     newx = cbind(1, X.train))
    
    # change into probability
    pred.lasso.hist <-  exp(pred.lasso.hist) / (1 + exp(pred.lasso.hist))
    
    # --------------------------------------------------------------------------
    # merge and get a comparison of predictions
    # --------------------------------------------------------------------------
    
    df.pred <- 
      df.pred %>% 
      cbind(pred.logit) %>% 
      cbind(pred.lasso) %>% 
      rename(pred.lasso = s0)

    df.pred.hist <- 
      df.pred.hist %>% 
      cbind(pred.logit.hist) %>% 
      cbind(pred.lasso.hist) %>% 
      rename(pred.logit = pred.logit.hist,
             pred.lasso = s0)
    
    # --------------------------------------------------------------------------
    # merge and get a comparison of beta
    # --------------------------------------------------------------------------
    
    # obtain the beta of logistic regression
    df.beta.logit <- 
      data.frame(logit       = model.logit$coefficients,
                 varname.raw = names(model.logit$coefficients)) %>% 
      mutate(varname = gsub("`", "", varname.raw)) %>% 
      mutate(varname = ifelse(varname.raw == "(Intercept)", "intercept", varname)) %>% 
      select(varname, logit) %>% 
      as_tibble()
    
    # obtain the beta of lasso regression
    df.beta.lasso <- 
      data.frame(
        LASSO       = as.numeric(model.lasso$beta),
        varname.raw = model.lasso$beta@Dimnames[[1]]
      ) %>% 
      mutate(varname = ifelse(varname.raw == "", "intercept", varname.raw)) %>% 
      select(varname, LASSO) %>% 
      as_tibble()
    
    # combine the coefficients
    df.beta <-
      df.beta %>% 
      left_join(df.beta.logit, by = "varname") %>% 
      left_join(df.beta.lasso, by = "varname") 
    
    # --------------------------------------------------------------------------
    # return two data frames
    # --------------------------------------------------------------------------
    
    list.out <- list(df.pred, df.pred.hist, df.beta)
    
    # return 
    return(list.out)
}
