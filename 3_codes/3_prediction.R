
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
  source("util/plot_comparison.r")
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
           starts_with("diff.money"),
           starts_with("growth.hpreal"),
           starts_with("diff.iy"),
           growth.cpi.dom,
           growth.equity.dom,
           diff.ca.dom,
           diff.dsr.dom,
           # bank balance sheet variables
           level.lev.dom
    ) %>% 
    na.omit()
  
  # specify the reference year
  ref.year <- 1990
  
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
           stan.file = "gaussian.stan",
           MCMC.name = "gaussian_crisis_baseline_train1990.rda")

  # check the result of MCMC
  plot.heat(MCMC.name  = "gaussian_crisis_baseline_train1990.rda",
            graph.name = "gaussian_crisis_baseline_heat_train1990.pdf")

  plot.dynamic(MCMC.name  = "gaussian_crisis_baseline_train1990.rda",
               graph.name = "gaussian_crisis_baseline_ts_train1990.pdf")
  
# ------------------------------------------------------------------------------
# obtain out of sample prediction for MCMC
# ------------------------------------------------------------------------------
  
  # obtain out-of-sample predictions with the latest estimates of beta
  df.pred.MCMC <- 
    predictMCMC(MCMC.name = "gaussian_crisis_baseline_train1990.rda",
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
  
# ------------------------------------------------------------------------------
# Create a data frame containing all betas
# ------------------------------------------------------------------------------
  
  # obtain the historical beta of MCMC
  load("../5_tmp/gaussian_crisis_baseline_train1990.rda")
  
  # obtain the beta of logistic regression
  df.beta.logit <- 
    data.frame(logit       = model.logit$coefficients,
               varname.raw = names(model.logit$coefficients)) %>% 
    mutate(varname = ifelse(varname.raw == "(Intercept)", "intercept", varname.raw)) %>% 
    select(varname, logit) %>% 
    as_tibble()

  # obtain the beta of lasso regression
  df.beta.lasso <- 
    data.frame(
      LASSO = as.numeric(model.lasso$beta),
      varname.raw = model.lasso$beta@Dimnames[[1]]
    ) %>% 
    mutate(varname = ifelse(varname.raw == "", "intercept", varname.raw)) %>% 
    select(varname, LASSO) %>% 
    as_tibble()
  
  # combine the coefficients
  df.beta.all <-
    df.beta %>% 
    left_join(df.beta.logit, by = "varname") %>% 
    left_join(df.beta.lasso, by = "varname") 

# ------------------------------------------------------------------------------
# Plot and compare the coefficients of some variables
# ------------------------------------------------------------------------------
  
  # Compare the coefficients
  g.comparison <- 
    plot.comparison(df.beta.all, "diff.credit.dom", "none") +
    plot.comparison(df.beta.all, "diff.money.glo", "none") +
    plot.comparison(df.beta.all, "diff.money.dom", c(0.75, 0.8))
  
  # save the result
  ggsave(filename = "../6_outputs/compare_1990.pdf",
         plot     = g.comparison,
         width    = 10 ,
         height   = 4)
  
# ------------------------------------------------------------------------------
# Plot ROC curves
# ------------------------------------------------------------------------------
  
  # bind the result to df.pred
  df.pred <- 
    df.pred.MCMC %>% 
    cbind(pred.logit) %>% 
    cbind(pred.lasso) %>% 
    rename(pred.lasso = s0)
  
  # install package
  library("pROC")
  
  # estimate ROC curves
  roc.MCMC  <- roc(df.pred, crisis, pred.MCMC)
  roc.logit <- roc(df.pred, crisis, pred.logit)
  roc.lasso <- roc(df.pred, crisis, pred.lasso)
  
  # save the following pdf file
  pdf(file   = "../6_outputs/roc_1990.pdf",
      width  = 6,
      height = 4)
  
  # plot ROC curves
  plot(roc.MCMC)
  plot(roc.logit, add = TRUE, lty = 2, col = "blue")
  plot(roc.lasso, add = TRUE, lty = 3, col = "red")
  legend("topright", 
         c("Bayesian", "Logit", "LASSO"),
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
  
  
# ------------------------------------------------------------------------------
# Plot PR curves
# ------------------------------------------------------------------------------
  
  # prediction data frame
  df.pred <- 
    df.pred %>% 
    mutate(crisis = as.factor(crisis))
  
  # check the precision
  df.pr.auc.MCMC <- 
    df.pred %>% 
    yardstick::pr_auc(truth = crisis, pred.MCMC) %>% 
    mutate(method = "MCMC")
  
  df.pr.auc.logit <- 
    df.pred %>% 
    yardstick::pr_auc(truth = crisis, pred.logit) %>% 
    mutate(method = "logit")
  
  df.pr.auc.lasso <- 
    df.pred %>% 
    yardstick::pr_auc(truth = crisis, pred.lasso) %>% 
    mutate(method = "LASSO")
  
  # combine all
  df.pr.auc <- 
    rbind(df.pr.auc.MCMC, df.pr.auc.logit, df.pr.auc.lasso)
  
  # plot the comparison of AUC of PR curves
  g.pr.auc <- 
    df.pr.auc %>% 
    ggplot() + 
    geom_bar(mapping = aes(x = reorder(x = method, X = .estimate), 
                           y = .estimate), 
             stat    = "identity") + 
    labs(x = "method", 
         y = "AUC of PR curves")
  
  # save the result
  ggsave(filename = "../6_outputs/AUC_pr_1990.pdf",
         plot     = g.pr.auc,
         width    = 6 ,
         height   = 4)
  