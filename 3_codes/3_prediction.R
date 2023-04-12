
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
           # starts_with("growth.equity"),
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
           MCMC.name = "gaussian_crisis_baseline_train2000_woequity.rda")

  # check the result of MCMC
  plot.heat(MCMC.name  = "gaussian_crisis_baseline_train2000_woequity.rda",
            graph.name = "gaussian_crisis_baseline_heat_train2000_woequity.pdf")

  plot.dynamic(MCMC.name  = "gaussian_crisis_baseline_train2000_woequity.rda",
               graph.name = "gaussian_crisis_baseline_ts_train2000_woequity.pdf")
  
# ------------------------------------------------------------------------------
# obtain out of sample for MCMC
# ------------------------------------------------------------------------------
  
  # obtain out-of-sample predictions with the latest estimates of beta
  df.pred <- 
    predictMCMC(MCMC.name = "gaussian_crisis_baseline_train2000_woequity.rda",
                df.test   = df.crisis.baseline.test)
  
# ------------------------------------------------------------------------------
# estimate a usual logistic regression
# ------------------------------------------------------------------------------
  
  # keep only relevant values
  df.logit.train <- 
    df.crisis.baseline.train %>% 
    select(-country, -year)

  df.logit.test <- 
    df.crisis.baseline.test %>% 
    select(-country, -year)
  
  # usual logistic regression
  model.logit <- 
    glm(crisis ~ ., data = df.logit.train, family = binomial())
  
  # check the result
  summary(model.logit)

  # predict
  pred.logit <- 
    predict(model.logit, newdata = df.logit.test, type = "response")
  
  # bind the result to df.pred
  df.pred <- 
    df.pred %>% 
    cbind(pred.logit)
  
# ------------------------------------------------------------------------------
# estimate a LASSO logistic regression
# ------------------------------------------------------------------------------
  
  # store X and Y values as matrix for later use
  X.crisis.baseline.train <- 
    df.crisis.baseline.train %>% 
    select(-year, -country, -crisis) %>% 
    as.matrix()
  
  Y.crisis.baseline.train <- 
    df.crisis.baseline.train %>% 
    select(crisis) %>% 
    mutate(crisis = as.numeric(crisis) - 1) %>% 
    as.matrix()
  
  X.crisis.baseline.test <- 
    df.crisis.baseline.test %>% 
    select(-year, -country, -crisis) %>% 
    as.matrix()
  
  Y.crisis.baseline.test <- 
    df.crisis.baseline.test %>% 
    select(crisis) %>% 
    mutate(crisis = as.numeric(crisis) - 1) %>% 
    as.matrix()
  
  # install packages
  library("glmnet")
  
  # set the seed
  set.seed(2292)
  
  # conduct cross validation to decide lambda
  cv.lambda.lasso <- 
    cv.glmnet(x = cbind(1, X.crisis.baseline.train), # intercept should be added
              y = Y.crisis.baseline.train, 
              alpha = 1,
              family = "binomial")
  
  # check the CV result
  plot(cv.lambda.lasso)
  
  # save the optimal lambda
  lambda.min <- cv.lambda.lasso$lambda.min
  
  # LASSO logistic regression
  model.lasso <- 
    glmnet(x = cbind(1, X.crisis.baseline.train),
           y = Y.crisis.baseline.train, 
           alpha = 1,
           family = "binomial",
           lambda = lambda.min)
    
  # check the result
  model.lasso$beta
  
  # predict and assess the out-of-sample prediction 
  pred.lasso <- 
    predict.glmnet(model.lasso,
                   newx = cbind(1, X.crisis.baseline.test))

  # change into probability
  pred.lasso <-  exp(pred.lasso) / (1 + exp(pred.lasso))
  
  # bind the result to df.pred
  df.pred <- 
    df.pred %>% 
    cbind(pred.lasso) %>% 
    rename(pred.lasso = s0)

# ------------------------------------------------------------------------------
# Compare the coefficients
# ------------------------------------------------------------------------------
  
  # save the coefficients
  # coef.MCMC  <- as.numeric(mat.beta.last)
  coef.logit <- model.logit$coefficients
  coef.lasso <- as.numeric(model.lasso$beta)

  # # combine the coefficients
  # df.coefs <- 
  #   cbind(coef.MCMC, coef.logit, coef.lasso, varname) %>% 
  #   as.data.frame() 
  # 
  # # obtain the name of variables
  # varnames <- rownames(df.coefs)
  # 
  # # create a column of variable name
  # df.coefs <- 
  #   cbind(df.coefs, varnames) %>% 
  #   as_tibble()
  # 
  # # plot 
  # df.coefs %>% 
  #   gather(key = "key", value = "value", -varnames) %>% 
  #   ggplot() + 
  #   geom_point(aes(x = value, y = varnames, color = key))
  
# ------------------------------------------------------------------------------
# Plot ROC curves
# ------------------------------------------------------------------------------
  
  # install package
  library("pROC")
  
  # estimate ROC curves
  roc.MCMC  <- roc(df.pred, crisis, pred.MCMC)
  roc.logit <- roc(df.pred, crisis, pred.logit)
  roc.lasso <- roc(df.pred, crisis, pred.lasso)
  
  # save the following pdf file
  pdf(file   = "../6_outputs/roc_2000_woequity.pdf",
      width  = 6,
      height = 4)
  
  # plot ROC curves
  plot(roc.MCMC)
  plot(roc.logit, add = TRUE, lty = 2, col = "blue")
  plot(roc.lasso, add = TRUE, lty = 3, col = "red")
  legend("topright", 
         c("MCMC", "Logit", "LASSO"),
         lty = c(1, 2, 3),
         col = c("black", "blue", "red"),
         bty = "n"
  )
  
  # close the file
  dev.off()
  
  # compare AUCs
  auc(roc.MCMC)
  auc(roc.logit)
  auc(roc.lasso)
