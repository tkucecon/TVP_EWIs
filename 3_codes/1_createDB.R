
# ------------------------------------------------------------------------------
# About this code
# ...creates the master data for 18 advanced economies covering 1870-2017

# crisis data part: merge crisis series from the following three data
  # 1. Jorda Schularick and Taylor (2017): JST dataset hereafter
  # 2. Baron, Verner and Xiong (2021): BVX dataset hereafter

# explanatory variable part: all data based on JST dataset
  # 1. keep only relevant variables as the level data
  # 2. transform and define some variables as difference/growth ones
  # ... for the growth predictors, winsorize and remove the effects of outliers

# merge crisis and explanatory datasets
# we obtain a data frame "df.master" 
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  library("ggthemes")
  theme_set(theme_solarized())
  
  library("haven")
  library("tidymodels")
  library("DescTools")
  
# ------------------------------------------------------------------------------
# Crisis Data Part
# ------------------------------------------------------------------------------
  
  # download the JST data
  df.JST.original <- 
    read_dta("http://data.macrohistory.net/JST/JSTdatasetR5.dta")
  
  # keep only the crisis variables in the JST data
  df.JST.crisis <- 
    df.JST.original %>% 
    select(year, country, iso, crisisJST) %>% 
    rename(crisis.JST = crisisJST)
  
  # load the BVX data
  # .dta data saved under 4_data/input folder
  df.BVX.original <- 
    read_dta("../4_data/input/BVX.dta")

  # keep only relevant values to merge
  df.BVX.crisis <- 
    df.BVX.original %>% 
    select(year, ISO3, JC, C_B30) %>% 
    rename(iso          = ISO3,
           crisis.joint = JC,
           crisis.BVX   = C_B30) 
  
  # merge the three crisis data frame
  df.crisis <- 
    df.JST.crisis %>% 
    left_join(df.BVX.crisis, by = c("iso", "year")) %>% 
    # define narrative joint crisis variable as "crisis.joint"
    # ... if either JST or RR definition shows crisis, then indicate as 1 (just to deal with update of JST data)
    mutate(crisis.joint = pmap(select(., crisis.JST, crisis.joint), ~pmax(..., na.rm = TRUE))) %>% 
    mutate(crisis.joint = as.numeric(crisis.joint)) %>% 
    # keep only relevant variables
    select(year, country, crisis.JST, crisis.joint, crisis.BVX) %>% 
    rename(JST   = crisis.JST,
           joint = crisis.joint,
           BVX   = crisis.BVX)

  # remove unnecessary data frames from the global environment
  rm(df.BVX.crisis, df.BVX.original, 
     df.JST.crisis)
  
# ------------------------------------------------------------------------------
# Explanatory Variable Part
# ------------------------------------------------------------------------------
  
  # define some level variables and leave only relevant ones
  df.JST.level <- 
    df.JST.original %>% 
    # group by the country
    group_by(country) %>% 
    # define some features
    mutate(
      level.slope       = ltrate - stir,         # slope of the yield curve
      level.credit      = tloans / gdp,          # credit to GDP ratio
      level.credit.hh   = thh / gdp,             # household credit to GDP ratio
      level.credit.bus  = tbus / gdp,            # business credit to GDP ratio
      level.money       = money / gdp,           # money to GDP ratio
      level.ca          = ca / gdp,              # current account to GDP ratio
      level.dsr         = tloans * ltrate / gdp, # debt service ratio
      level.hpreal      = hpnom / cpi * 100,     # real house price
      level.gdpreal     = gdp / cpi * 100        # real GDP
      ) %>% 
    # rename some variables to explicitly show that they are level variables
    rename(
      level.stir        = stir,    # short-term interest rates
      level.rcon        = rconpc,  # real consumption per capita
      level.iy          = iy,      # investment to GDP ratio
      level.cpi         = cpi,     # consumer price
      level.pdebt       = debtgdp, # public debt to GDP ratio
      level.lev         = lev,     # capital-to-asset ratio of banks
      level.ltd         = ltd,     # loan to deposit ratio
      level.noncore     = noncore, # noncore funding ratio
      level.xrusd       = xrusd,   # exchange rates against US dollars
      dummy.peg         = peg      # exchange rate peg dummy
      ) %>% 
    # keep only relevant variables for simplicity
    select(year, country, starts_with("level"), starts_with("dummy"), eq_tr)
  
  # Define some variables as 2-year difference of GDP-ratio * 100
  df.JST.difference <- 
    df.JST.level %>% 
    # group by the country
    group_by(country) %>% 
    # define some variables as 2-year difference of GDP-ratio * 100
    mutate(
      diff.credit     = (level.credit     - lag(level.credit,     n = 2)) * 100, # total credit
      diff.credit.hh  = (level.credit.hh  - lag(level.credit.hh,  n = 2)) * 100, # household credit
      diff.credit.bus = (level.credit.bus - lag(level.credit.bus, n = 2)) * 100, # business credit
      diff.dsr        = (level.dsr        - lag(level.dsr,        n = 2)) * 100, # debt service ratio
      diff.iy         = (level.iy         - lag(level.iy,         n = 2)) * 100, # investment
      diff.pdebt      = (level.pdebt      - lag(level.pdebt,      n = 2)) * 100, # public debt
      diff.money      = (level.money      - lag(level.money,      n = 2)) * 100, # money
      diff.ca         = (level.ca         - lag(level.ca,         n = 2)) * 100, # current account
      diff.lev        = (level.lev        - lag(level.lev,        n = 2))      , # bank capital to asset ratio
      diff.ltd        = (level.ltd        - lag(level.ltd,        n = 2))      , # bank loan to deposit ratio
      diff.noncore    = (level.noncore    - lag(level.noncore,    n = 2))      , # bank noncore funding ratio
      diff.xrusd      = (level.xrusd      - lag(level.xrusd,      n = 2))        # exchange rates against US dollars
    ) %>% 
    # keep only difference variables and key indicators to merge
    select(year, country, starts_with("diff"))
    
  # Define some variables as 2-year growth rates
  df.JST.growth <- 
    df.JST.level %>% 
    # group by the country
    group_by(country) %>% 
    # define some variables as 2-year growth rate of index
    mutate(
      growth.cpi    = level.cpi    / lag(level.cpi,    n = 2) * 100 - 100, # growth rate of the CPI
      growth.rcon   = level.rcon   / lag(level.rcon,   n = 2) * 100 - 100, # growth rate of the consumption
      growth.hpreal = level.hpreal / lag(level.hpreal, n = 2) * 100 - 100, # growth rate of the real house prices
      growth.equity = ((1 + eq_tr) * (1 + lag(eq_tr, n = 1)) - 1) * 100    # growth rate of the equity price
           ) %>% 
    # adjust the equity growth with CPI growth
    mutate(growth.equity = growth.equity - growth.cpi) %>% 
    # winsorize the variables and remove the effects of outliers
    mutate(growth.cpi    = Winsorize(growth.cpi,    probs = c(0.05, 0.95), na.rm = TRUE),
           growth.rcon   = Winsorize(growth.rcon,   probs = c(0.05, 0.95), na.rm = TRUE),
           growth.hpreal = Winsorize(growth.hpreal, probs = c(0.05, 0.95), na.rm = TRUE),
           growth.equity = Winsorize(growth.equity, probs = c(0.05, 0.95), na.rm = TRUE)
           ) %>% 
    select(year, country, starts_with("growth"))
  
  # merge all the data as the domestic data set
  df.JST.domestic <- 
    # merge all the data
    df.JST.level %>% 
    # keep only necessary level data (drop if before transformation)
    select(year, country, level.stir, level.slope, level.lev, level.noncore, level.ltd, level.xrusd, dummy.peg) %>% 
    # merge transformed data
    left_join(df.JST.difference, by = c("country", "year")) %>% 
    left_join(df.JST.growth,     by = c("country", "year")) %>% 
    # reset the grouping
    ungroup() %>% 
    # rename the variables to explicitly indicate that they are domestic variables
    gather(key = "key", value = "value", -year, -country) %>% 
    mutate(key = paste(key, ".dom", sep = "")) %>% 
    spread(key = "key", value = "value") %>% 
    arrange(country, year)
  
  # df.JST.domestic now includes only the domestic data
  # calculate global variables

  # obtain the name of all the countries in the data
  countries.JST <- 
    df.JST.domestic %>% 
    select(country) %>% 
    unique() %>% 
    unlist() %>% 
    as.vector()
  
  variables.JST <- 
    df.JST.domestic %>% 
    select(ends_with(".dom")) %>% 
    colnames()

  # create an empty data frame to stack the data
  df.JST.global <- 
    df.JST.domestic %>% 
    rename_with(\(x) str_replace(x, ".dom", ".glo"),
                ends_with(".dom")) %>% 
    filter(country == "hogehoge")
  
  # repeat the process for all the countries
  for (target.country in countries.JST) {
    # create global variables as the average of other countries
    df.JST.global.tmp <- 
      # the base data is domestic data
      df.JST.domestic %>% 
      # exclude the target country from the data
      filter(country != target.country) %>% 
      # group by each year
      group_by(year) %>% 
      # calculate the global factor as the average of other countries
      summarise_at(variables.JST, mean, na.rm = TRUE) %>% 
      # leave the name of the target country
      mutate(country = target.country) %>% 
      ungroup() %>% 
      rename_with(\(x) str_replace(x, ".dom", ".glo"),
                  ends_with(".dom"))

    # stack the data into the global data frame
    df.JST.global <- 
      bind_rows(df.JST.global, df.JST.global.tmp)  
  }
  
  # merge the domestic and global data into a single master data frame
  df.exp <- 
    df.JST.domestic %>% 
    # keep only the samples available in the domestic data frame: left join
    left_join(df.JST.global, by = c("year", "country"))
  
# ------------------------------------------------------------------------------
# save the data
# ------------------------------------------------------------------------------
  
  # merge the crisis data and explanatory data 
  df.master <- 
    df.crisis %>% 
    left_join(df.exp, by = c("year", "country"))
  
  # save the data
  save(df.master, file = "../4_data/df_master.rda")
  
