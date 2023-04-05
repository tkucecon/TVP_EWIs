
# ------------------------------------------------------------------------------
# About this code
# download the JST data and process the data for prediction

# difference with the original paper...
# NA rows are not deleted at this point
# more variables added to the prediction
  # level of exchange rates to the US dollar
  # level of bank capital ratio
  # level of bank loan to deposit ratio
  # level of bank noncore funding ratio
  # currency peg dummy
  # two-year growth rates of house prices
  # two-year change of mortgage loans / GDP ratio
  # two-year change of household loans / GDP ratio
  # two-year change of business loans / GDP ratio
  # two-year change of level of bank capital ratio
  # two-year change of level of bank loan to deposit ratio
  # two-year change of level of bank noncore funding ratio
# global variables constructed for all variables
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  library("haven")
  library("ggthemes")
  theme_set(theme_solarized())
  
  library("skimr")
  library("mFilter")
  library("tidymodels")

# ------------------------------------------------------------------------------
# Download the JST data
# ------------------------------------------------------------------------------
  
  # download the JST data
  df.JST.original <- 
    read_dta("http://data.macrohistory.net/JST/JSTdatasetR5.dta")
  
  # check the entire sample size and compare with the original paper
  df.JST.original %>% 
    select(year, country, crisisJST) %>% 
    skim()

  # it seems to be updated from the BOE paper
  # ... 18 developed countries are included: recent data about Ireland is now included
  # ... 2017 data is now available
  
# ------------------------------------------------------------------------------
# Preprocess the JST data
# ------------------------------------------------------------------------------
  
  # define some level variables and leave only relevant ones
  df.JST.level <- 
    df.JST.original %>% 
    # group by the country
    group_by(country) %>% 
    # define the outcome variable: 1 and 2 year before the crisis is the target
    mutate(
      crisis.f1 = lead(crisisJST, n = 1),
      crisis.f2 = lead(crisisJST, n = 2)) %>%
    mutate(crisis = if_else(
      crisis.f1 == 1 | crisis.f2 == 1, 1, 0
    )) %>% 
    # factorize the outcome variable: not numeric
    mutate(crisis = as.factor(crisis)) %>% 
    # define some features
    mutate(
      level.slope       = ltrate - stir,         # slope of the yield curve
      level.credit      = tloans / gdp,          # credit to GDP ratio
      level.money       = money / gdp,           # money to GDP ratio
      level.ca          = ca / gdp,              # current account to GDP ratio
      level.dsr         = tloans * ltrate / gdp, # debt service ratio
      level.hpreal      = hpnom / cpi * 100,     # real house price
      level.tmort       = tmort / gdp,           # mortgage loan to GDP ratio
      level.thh         = thh / gdp,             # household credit to GDP ratio
      level.gdpreal     = gdp / cpi * 100        # real GDP
             ) %>% 
    # rename some variables to explicitly show that they are level variables
    rename(
      level.stir        = stir,                  # short-term interest rates
      level.rcon        = rconpc,                # real consumption per capita
      level.iy          = iy,                    # investment to GDP ratio
      level.cpi         = cpi,                   # consumer price
      level.pdebt       = debtgdp,               # public debt to GDP ratio
      level.lev         = lev,                   # leverage of banks
      level.ltd         = ltd,                   # loan to deposit ratio
      level.noncore     = noncore                # noncore funding ratio
    ) %>% 
    # keep only relevant variables for simplicity
    select(year, country, iso, crisis, crisisJST, starts_with("level"), eq_tr)
  
  # Define some variables as 2-year difference of GDP-ratio * 100
  df.JST.difference <- 
    df.JST.level %>% 
    mutate(
      diff.credit   = (level.credit  - lag(level.credit,  n = 2)) * 100, # credit
      diff.dsr      = (level.dsr     - lag(level.dsr,     n = 2)) * 100, # debt service ratio
      diff.iy       = (level.iy      - lag(level.iy,      n = 2)) * 100, # investment
      diff.pdebt    = (level.pdebt   - lag(level.pdebt,   n = 2)) * 100, # public debt
      diff.money    = (level.money   - lag(level.money,   n = 2)) * 100, # money
      diff.ca       = (level.ca      - lag(level.ca,      n = 2)) * 100, # current account
      diff.mortgage = (level.tmort   - lag(level.tmort,   n = 2)) * 100, # mortgage loan
      diff.thh      = (level.thh     - lag(level.thh,     n = 2)) * 100, # household credit
      diff.lev      = (level.lev     - lag(level.lev,     n = 2)) * 100, # household credit
      diff.ltd      = (level.ltd     - lag(level.ltd,     n = 2)) * 100, # household credit
      diff.noncore  = (level.noncore - lag(level.noncore, n = 2)) * 100  # household credit
    ) %>% 
    # keep only difference variables and key indicators to merge
    select(year, country, starts_with("diff"))
    
  # Define some variables as 2-year growth rates
  df.JST.growth <- 
    df.JST.level %>% 
    # define some variables as 2-year growth rate of index
    mutate(
      growth.cpi    = level.cpi    / lag(level.cpi,    n = 2) * 100 - 100, # growth rate of the CPI
      growth.rcon   = level.rcon   / lag(level.rcon,   n = 2) * 100 - 100, # growth rate of the consumption
      growth.hpreal = level.hpreal / lag(level.hpreal, n = 2) * 100 - 100, # growth rate of the real house prices
      growth.equity = ((1 + eq_tr) * (1 + lag(eq_tr, n = 1)) - 1) * 100    # growth rate of the equity price
           ) %>% 
    select(year, country, starts_with("growth"))
  
  # Define some variables as the gap from HP filtered series
  df.JST.gap <- 
    df.JST.level %>% 
    select(year, country, starts_with("level")) %>% 
    ungroup() %>% 
    gather(key = "key", value = "value", starts_with("level")) %>% 
    group_by(country, key) %>% 
    drop_na() %>% 
    mutate(cycle = hpfilter(value, type = "lambda", freq = 6.25)$cycle) %>% 
    select(-value) %>% 
    ungroup() %>% 
    spread(key = "key", value = "cycle") %>% 
    arrange(country, year) %>% 
    rename(
      gap.credit  = level.credit,
      gap.gdpreal = level.gdpreal,
      gap.thh     = level.thh,
      gap.tmort   = level.tmort,
      gap.lev     = level.lev,
      gap.ltd     = level.ltd,
      gap.dsr     = level.dsr,
      gap.ca      = level.ca,
      gap.noncore = level.noncore
    ) %>% 
    select(year, country, starts_with("gap"))
  
  # merge all the data as the domestic data set
  df.JST.domestic <- 
    # merge all the data
    df.JST.level %>% 
    select(-eq_tr) %>% 
    left_join(df.JST.difference, by = c("country", "year")) %>% 
    left_join(df.JST.growth,     by = c("country", "year")) %>% 
    left_join(df.JST.gap,        by = c("country", "year")) %>% 
    # create a dummy to exclude the crisis year and four subsequent years
    mutate(crisis.ex = if_else(
        crisisJST == 1 | 
        lag(crisisJST, n = 1) == 1 | 
        lag(crisisJST, n = 2) == 1 | 
        lag(crisisJST, n = 3) == 1 | 
        lag(crisisJST, n = 4) == 1,
      1, 0
    )) %>% 
    # exclude the crisis year and the four subsequent years
    filter(crisis.ex == 0) %>% 
    # exclude some unusual periods
    filter(!(year >= 1933 & year <= 1939)) %>% # exclude the great depression period
    filter(!(year >= 1914 & year <= 1918)) %>% # exclude the WW1 period
    filter(!(year >= 1939 & year <= 1945)) %>% # exclude the WW2 period
    # reset the grouping
    ungroup() %>% 
    # keep only the relevant variables
    select(-crisisJST, -crisis.ex) %>% 
    # rename the variables to explicitly indicate that they are domestic variables
    gather(key = "key", value = "value", -year, -country, -iso, -crisis) %>% 
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
    select(-crisis, -iso) %>% 
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
  df.JST <- 
    df.JST.domestic %>% 
    # keep only the samples available in the domestic data frame: left join
    left_join(df.JST.global, by = c("year", "country"))
  
  # drop if the crisis is NA
  df.JST <- 
    df.JST %>% 
    filter(!is.na(crisis))
  
  # done! this procedure leaves us 1814 observations

  # save the data
  save(df.JST, file = "../4_data/df_JST.rda")
  
# ------------------------------------------------------------------------------
# Prepare the equity value data from Baron et al. (2020)
# ------------------------------------------------------------------------------
  
# ------------------------------------------------------------------------------
# normalize the data: this will be useful to compare the coefficients
# ------------------------------------------------------------------------------
  
  # create a recipe to scale the variables
  recipe.normalize <- 
    recipe(x = df.JST, formula = as.formula(crisis ~ .)) %>% 
    # remove year and country from the predictors %>% 
    update_role(year,    new_role = "time variable") %>% 
    update_role(country, new_role = "id variable") %>% 
    update_role(iso,     new_role = "id variable") %>% 
    # normalize all the predictors to be distributed ~ N(0,1) 
    step_normalize(all_predictors(), skip = FALSE) %>% 
    # delete rows with no variance
    step_zv(all_predictors(), skip = FALSE) 
  
  # create an empty data frame to stack the data
  df.JST.normalized <- 
    df.JST %>% 
    filter(country == "hogehoge")
  
  # apply the recipe to all countries: now group_by is not allowed for recipes
  for (target.country in countries.JST) {
    # create normalized data frame for a specific country
    df.JST.ctry <- 
      df.JST %>% 
      # keep only one specific country
      filter(country == target.country)
    
    # create a data frame with imputed variables
    df.JST.normalized.tmp <- 
      recipe.normalize %>% 
      prep() %>% 
      bake(new_data = df.JST.ctry)

    # stack the data into the global data frame
    df.JST.normalized <- 
      bind_rows(df.JST.normalized, df.JST.normalized.tmp)  
  }
  
  # save the data
  save(df.JST.normalized, file = "../4_data/df_JST_normalized.rda")
  