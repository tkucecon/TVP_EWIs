
# ------------------------------------------------------------------------------
# About this code
# Drop unnecessary variables from the master data and save only core variables
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("ggthemes")
  theme_set(theme_solarized())
  library("patchwork")
  
  library("tidymodels")
  library("corrplot")
  
# ------------------------------------------------------------------------------
# Load the JST master data
# ------------------------------------------------------------------------------
  
  # load the JST data
  load("../4_data/df_master.rda")

# ------------------------------------------------------------------------------
# Check the number of missing values
# ------------------------------------------------------------------------------

  # number of missing values
  df.master %>% 
    skimr::skim()
  
# ------------------------------------------------------------------------------
# Check the correlation matrix
# ------------------------------------------------------------------------------
  
  # level variables
  df.master %>% 
    select(starts_with("level")) %>% 
    cor(use = "pairwise.complete.obs") %>% 
    corrplot(addCoef.col = "black")
  
  # Finding:
  # global variables should be excluded basically (except for yield curve slope)
  # strong correlation between noncore and loan-to-value
  # slight correlation between leverage and ltd
  # slight correlation between leverage and ltd
  
  # diff variables
  df.master %>% 
    select(starts_with("diff")) %>% 
    cor(use = "pairwise.complete.obs") %>% 
    corrplot(addCoef.col = "black")
  
  # Finding:
  # ltd should be excluded
  # pdebt should be excluded
  # mortgage and thh should be excluded
  # strong correlation between domestic and global dsr
  # strong correlation between global money and global iy
  # slight correlation between current account and iy
  # slight correlation between credit and dsr
  # slight correlation between credit and money
  
  # growth variables
  df.master %>% 
    select(starts_with("growth")) %>% 
    select(-growth.cpi.glo, -growth.equity.glo, -growth.rcon.glo) %>% 
    cor(use = "pairwise.complete.obs") %>% 
    corrplot(addCoef.col = "black")
  
  # Finding:
  # strong correlation between domestic and global cpi
  # strong correlation between domestic and global equity
  # strong correlation between domestic and global consumption
  # slight correlation between hpreal and consumption
  
# ------------------------------------------------------------------------------
# Define core data and save for later analysis
# ------------------------------------------------------------------------------
  
  df.core <- 
    df.master %>% 
    select(
      # necessary variables
      year, country, crisis,
      # level variables
      level.lev.dom,
      level.ltd.dom,
      level.slope.dom,
      level.slope.glo,
      # difference variables
      diff.ca.dom,
      diff.credit.dom,
      diff.credit.glo,
      diff.dsr.dom,
      diff.iy.dom,
      diff.lev.dom,
      diff.money.dom,
      diff.money.glo,
      diff.noncore.dom,
      diff.noncore.glo,
      # growth variables
      growth.cpi.dom,
      growth.equity.dom,
      growth.hpreal.dom,
      growth.rcon.dom,
    ) 
  
  # check the correlation
  df.core %>% 
    select(-year, -country, -crisis) %>% 
    cor(use = "pairwise.complete.obs") %>% 
    corrplot(addCoef.col = "black")
  
  # save the core data for later use
  # save the data
  save(df.core, file = "../4_data/df_core.rda")
  
# ------------------------------------------------------------------------------
# Normalize the data: this will be useful to compare the coefficients
# ------------------------------------------------------------------------------
  
  # create a recipe to scale the variables
  recipe.normalize <- 
    recipe(x = df.core, formula = as.formula(crisis ~ .)) %>% 
    # remove some variables from the predictors %>% 
    update_role(year,             new_role = "time variable") %>% 
    update_role(country,          new_role = "id variable") %>% 
    # normalize all the predictors to be distributed ~ N(0,1) 
    step_normalize(all_predictors(), skip = FALSE) %>% 
    # delete rows with no variance
    step_zv(all_predictors(), skip = FALSE) 
  
  # create an empty data frame to stack the data
  df.normalized <- 
    df.core %>% 
    filter(country == "hogehoge")
  
  # save the countries
  countries.JST <- 
    df.core %>% 
    select(country) %>% 
    unique() %>% 
    unlist()
  
  # apply the recipe to all countries: loop used because group_by is not allowed for recipes
  for (target.country in countries.JST) {
    # create normalized data frame for a specific country
    df.core.ctry <- 
      df.core %>% 
      # keep only one specific country
      filter(country == target.country)
    
    # create a data frame with normalized variables
    df.normalized.tmp <- 
      recipe.normalize %>% 
      prep() %>% 
      bake(new_data = df.core.ctry)
    
    # stack the data into the global data frame
    df.normalized <- 
      bind_rows(df.normalized, df.normalized.tmp)  
  }
  
  # remove NA rows
  df.normalized <- 
    df.normalized %>% 
    na.omit()
  
  # save the data
  save(df.normalized, file = "../4_data/df_normalized.rda")
  