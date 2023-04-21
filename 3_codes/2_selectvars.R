
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
  
  library("corrplot")
  
# ------------------------------------------------------------------------------
# Load the JST master data
# ------------------------------------------------------------------------------
  
  # load the JST data
  load("../4_data/df_master.rda")

# ------------------------------------------------------------------------------
# Check the number of missing values
# ------------------------------------------------------------------------------

  # # check the number of missing values at this point
  # df.master %>% 
  #   skimr::skim()
  
# ------------------------------------------------------------------------------
# Check the correlation matrix
# ------------------------------------------------------------------------------
  
  # # level variables
  # df.master %>% 
  #   select(starts_with("level")) %>% 
  #   cor(use = "pairwise.complete.obs") %>% 
  #   corrplot(addCoef.col = "black")
  # 
  # # Finding:
  # # global variables should be excluded basically (except for yield curve slope)
  # 
  # # diff variables
  # df.master %>% 
  #   select(starts_with("diff")) %>% 
  #   select(ends_with("dom")) %>% 
  #   cor(use = "pairwise.complete.obs") %>% 
  #   corrplot(addCoef.col = "black")
  # 
  # # Finding:
  # # ltd should be excluded
  # # pdebt should be excluded
  # # mortgage and thh should be excluded
  # # strong correlation between domestic and global dsr
  # # strong correlation between global money and global iy
  # # slight correlation between credit and dsr
  # # slight correlation between credit and money
  # # slight correlation between credit to household and business
  # 
  # # growth variables
  # df.master %>% 
  #   select(starts_with("growth")) %>% 
  #   select(-growth.cpi.glo, -growth.equity.glo, -growth.rcon.glo) %>% 
  #   cor(use = "pairwise.complete.obs") %>% 
  #   corrplot(addCoef.col = "black")
  # 
  # # Finding:
  # # strong correlation between domestic and global cpi
  # # strong correlation between domestic and global equity
  # # strong correlation between domestic and global consumption
  # # slight correlation between hpreal and consumption
  
# ------------------------------------------------------------------------------
# Define core data and save for later analysis
# ------------------------------------------------------------------------------
  
  df.core <- 
    df.master %>% 
    select(
      # necessary variables
      year, country, JST, joint, BVX,
      # level variables
      # level.lev.dom,
      level.slope.dom,
      level.slope.glo,
      level.xrusd.dom,
      # difference variables
      diff.credit.dom,
      diff.credit.glo,
      diff.credit.hh.dom,
      diff.credit.bus.dom,
      diff.ca.dom,
      diff.pdebt.dom,
      diff.iy.dom,
      diff.xrusd.dom,
      diff.lev.dom,
      diff.noncore.dom,
      # growth variables
      growth.cpi.dom,
      growth.equity.dom,
      growth.rcon.dom,
      growth.hpreal.dom
    ) %>% 
    rename(
      # level variables
      # `Capital Asset Ratio`    = level.lev.dom,
      `Slope of Yield Curves`  = level.slope.dom,
      `Slope of Yield Curves*` = level.slope.glo,
      `Exchange Rates`         = level.xrusd.dom,
      # difference variables
      `Credit to GDP Change`           = diff.credit.dom,
      `Credit to GDP Change*`          = diff.credit.glo,
      `Household Credit to GDP Change` = diff.credit.hh.dom,
      `Business Credit to GDP Change`  = diff.credit.bus.dom,
      `Current Account Change`         = diff.ca.dom,
      `Public Debt Change`             = diff.pdebt.dom,
      `Investment to GDP Change`       = diff.iy.dom,
      `Exchange Rates Change`          = diff.xrusd.dom,
      `Capital Asset Ratio Change`     = diff.lev.dom,
      `Noncore Funding Ratio Change`   = diff.noncore.dom,
      # growth variables
      `Inflation Rate`          = growth.cpi.dom,
      `Equity Growth`           = growth.equity.dom,
      `Consumption Growth`      = growth.rcon.dom,
      `Real Houseprice Growth`  = growth.hpreal.dom,
    )
    
  # save the core data for later use
  save(df.core, file = "../4_data/df_core.rda")
  
  # Correlation matrix can be seen as follows
  cor.mat <- 
    df.core %>% 
    select(-year, -country, -JST, -joint, -BVX) %>% 
    cor(use = "pairwise.complete.obs") 
  
  # check the test
  cor.p <- 
    cor.mtest(cor.mat)
  
  # check the matrix
  cor.mat %>% 
    corrplot(addCoef.col = "black",  p.mat = cor.p$p, sig.level = 0.01)
  
  