
# ------------------------------------------------------------------------------
# About this code
# Compare the out-of-sample prediction accuracy
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("ggthemes")
  theme_set(theme_solarized())
  library("patchwork")
  
  library("rstan")
  library("tidymodels")
  library("glmnet")
  library("pROC")
  
  # some functions
  source("util/regdf.r")
  source("util/saveNUTS.r")
  source("util/plot_dynamic.r")
  source("util/plot_heat.r")
  source("util/plot_comparison.r")
  source("util/predict_STVP.r")

# set the parallel computation option-------------------------------------------

  # set options
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()) 
  
  # set the seed
  set.seed(2292)
  
# ------------------------------------------------------------------------------
# Load the data
# ------------------------------------------------------------------------------
  
  # load the core data
  load("../4_data/df_core.rda")
  
  # keep only the relevant data?
  df.core <- 
    df.core %>% 
    select(-`Public Debt to GDP`, -`Exchange Rates Change`)
  
  # normalize the data
  df.normalized <- 
    regdf(df = df.core,
          target = "joint")
          
  # specify the reference year
  ref.year <- 1990
  
  # Split the data into training/test data set
  df.train <- 
    df.normalized %>% 
    filter(year < ref.year)
  
  df.test <- 
    df.normalized %>% 
    filter(year >= ref.year)
  
# ------------------------------------------------------------------------------
# estimate Sparse TVP model using the training sample and obtain the estimates of beta
# ------------------------------------------------------------------------------
  
  # 1. horseshoe prior (free parameter)
  saveNUTS(df        = df.train,
           target    = "joint",
           train     = ref.year,
           stan.file = "horseshoe")
  
  # 2. regularized horseshoe prior with different global scale assumptions
  for (tau0 in seq(0.2, 0.8, 0.2)) {
    saveNUTS(df        = df.train,
             target    = "joint",
             train     = ref.year,
             stan.file = "reghorse",
             tau0      = tau0)
  }
  
# ------------------------------------------------------------------------------
# obtain out of sample prediction for MCMC
# ------------------------------------------------------------------------------
  
  # obtain in-sample and out of sample predictions
  out.horseshoe <- 
    predict.STVP(file.path = "train1990",
                 file.name = "horseshoe_joint_tot",
                 df.train  = df.train,
                 df.test   = df.test)
  
  out.horseshoe <-
    predict.STVP(file.path = "train1990",
                 file.name = "reghorse_joint_tot_tau0.5",
                 df.train  = df.train,
                 df.test   = df.test)

# ------------------------------------------------------------------------------
# compare AUCs with different settings
# ------------------------------------------------------------------------------

  # save the result of usual horseshoe as a vector
  auc.oos <-
    out.horseshoe[[1]] %>%
    roc(crisis, pred.STVP) %>%
    auc()

  auc.is <-
    out.horseshoe[[2]] %>%
    roc(crisis, pred.STVP) %>%
    auc()

  df.auc <-
    data.frame(out.of.sample = auc.oos,
               in.sample = auc.is) %>%
    mutate(method = "horseshoe")

  for (tau0 in seq(0.2, 0.8, 0.2)) {

    out.reghorse <-
      predict.STVP(file.path = "train1990",
                   file.name = paste("reghorse_joint_tot_tau", tau0, sep = ""),
                   df.train  = df.train,
                   df.test   = df.test)

    auc.oos <-
      out.reghorse[[1]] %>%
      roc(crisis, pred.STVP) %>%
      auc()

    auc.is <-
      out.reghorse[[2]] %>%
      roc(crisis, pred.STVP) %>%
      auc()

    df.reghorse <-
      data.frame(out.of.sample = auc.oos,
                 in.sample = auc.is) %>%
      mutate(method = paste("tau0:", tau0, sep = " "))

    df.auc <-
      rbind(df.auc, df.reghorse)
  }

  # check the result graphically
  df.auc %>%
    ggplot() +
    geom_bar(aes())

# ------------------------------------------------------------------------------
# Plot ROC curves
# ------------------------------------------------------------------------------
  
  # calculate ROC for out-of-sample prediction
  roc.STVP.horse  <- roc(out.horseshoe[[1]], crisis, pred.STVP)
  roc.logit.horse <- roc(out.horseshoe[[1]], crisis, pred.logit)
  roc.lasso.horse <- roc(out.horseshoe[[1]], crisis, pred.lasso)
  # save the out-of-sample prediction as a plot
  pdf(file   = "../6_outputs/1990_roc_horseshoe_oos.pdf",
      width  = 4,
      height = 4)
    # plot ROC curves
    plot(roc.STVP.horse)
    plot(roc.logit.horse, add = TRUE, lty = 2, col = "blue")
    plot(roc.lasso.horse, add = TRUE, lty = 3, col = "red")
    legend("bottomright", 
           c(paste("Bayesian (", round(auc(roc.STVP.horse), 2), ")", sep = ""),
             paste("Logit (", round(auc(roc.logit.horse), 2), ")" , sep = ""), 
             paste("LASSO (", round(auc(roc.lasso.horse), 2), ")" , sep = "")),
           lty = c(1, 2, 3),
           col = c("black", "blue", "red"),
           bty = "n"
    )
  # close the file
  dev.off()
  
  # calculate ROC for in-sample prediction
  roc.STVP.horse  <- roc(out.horseshoe[[2]], crisis, pred.STVP)
  roc.logit.horse <- roc(out.horseshoe[[2]], crisis, pred.logit)
  roc.lasso.horse <- roc(out.horseshoe[[2]], crisis, pred.lasso)
  # save the in-sample prediction as a plot
  pdf(file   = "../6_outputs/1990_roc_horseshoe_is.pdf",
      width  = 4,
      height = 4)
    # plot ROC curves
    plot(roc.STVP.horse)
    plot(roc.logit.horse, add = TRUE, lty = 2, col = "blue")
    plot(roc.lasso.horse, add = TRUE, lty = 3, col = "red")
    legend("bottomright", 
           c(paste("Bayesian (", round(auc(roc.STVP.horse), 2), ")", sep = ""),
             paste("Logit (", round(auc(roc.logit.horse), 2), ")" , sep = ""), 
             paste("LASSO (", round(auc(roc.lasso.horse), 2), ")" , sep = "")),
           lty = c(1, 2, 3),
           col = c("black", "blue", "red"),
           bty = "n"
    )
  # close the file
  dev.off()
  
# ------------------------------------------------------------------------------
# Plot and compare the coefficients of some variables
# ------------------------------------------------------------------------------
  
  out.reghorse <- 
    predict.STVP(file.path = "train1990",
                 file.name = "reghorse_joint_tot_tau6",
                 df.train  = df.train,
                 df.test   = df.test)
  
  # Compare the coefficients
  g.comparison <- 
    plot.comparison(out.reghorse[[3]], "Credit to GDP Change", "none") +
    plot.comparison(out.reghorse[[3]], "Credit to GDP Change*", "none") +
    plot.comparison(out.reghorse[[3]], "Exchange Rates Change", c(0.75, 0.8))
  
  plot.comparison(out.reghorse[[3]], "intercept", "none")
    
  # save the result
  ggsave(filename = "../6_outputs/compare_1990.pdf",
         plot     = g.comparison,
         width    = 10 ,
         height   = 4)