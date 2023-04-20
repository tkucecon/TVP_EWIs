
# ------------------------------------------------------------------------------
# About this code
# create a data frame for regression from a core data frame
# this function
  # removes the unnecessary crisis variables
  # creates target variables based on the crisis variable (defined as 1 and 2 year before the crisis)
  # omits rows with missing values
  # normalizes all the explanatory variables
# ------------------------------------------------------------------------------

regdf <- 
  function(df,                 # original data frame
           target,             # name of the crisis variable
           total.credit = TRUE # Boolian indicating whether to use total credit or not (household and business credit)
           ){

    # ------------------------------------------------------------------------------
    # Keep only relevant variables
    # ------------------------------------------------------------------------------
    
    # all the crisis variables
    crisis.vars <- c("JST", "joint", "BVX")
    
    # crisis variables other than target
    crisis.remove <- setdiff(crisis.vars, target)
    
    # if total.credit is TRUE, then drop household and business credit to GDP ratio
    if (total.credit) {
      credit.remove <- c("Household Credit to GDP Change", "Business Credit to GDP Change")
    } else {
      credit.remove <- "Credit to GDP Change"
    }
    
    # remove unnecessary variables
    df <- 
      df %>% 
      select(-all_of(crisis.remove)) %>% 
      select(-all_of(credit.remove)) %>% 
      rename(target = all_of(target))
    
    # ------------------------------------------------------------------------------
    # Drop unusual periods
    # ------------------------------------------------------------------------------
    
    # sample selection part: remove unusual periods
    # ... this part follows the method in Bluwstein et al. (2020)
    # 1. drop the two World War periods
    # 2. exclude the recovery periods after financial crisis
    # ... exclusion periods differ according to the crisis variables
    
    df <- 
      df %>% 
      # group by countries
      group_by(country) %>% 
      # define the outcome variable: 1 and 2 year before the crisis is the target
      mutate(
        crisis.f1 = lead(target, n = 1),
        crisis.f2 = lead(target, n = 2)) %>%
      mutate(crisis = if_else(
        crisis.f1 == 1 | crisis.f2 == 1, 1, 0
      )) %>% 
    # create a dummy to exclude the crisis year and four subsequent years
    mutate(crisis.ex = if_else(
      target == 1 | 
        lag(target, n = 1) == 1 | 
        lag(target, n = 2) == 1 | 
        lag(target, n = 3) == 1 | 
        lag(target, n = 4) == 1,
      1, 0
    )) %>% 
      # exclude the crisis year and the four subsequent years
      filter(crisis.ex != 1) %>% 
      # exclude some unusual periods
      filter(!(year >= 1914 & year <= 1918)) %>% # exclude the WW1 period
      filter(!(year >= 1933 & year <= 1939)) %>% # exclude the great depression period
      filter(!(year >= 1939 & year <= 1945)) %>% # exclude the WW2 period
      # reset the grouping
      ungroup() %>% 
      # drop if target is missing
      filter(!is.na(crisis)) %>% 
      # drop unnecessary series %>% 
      select(-target, -crisis.f1, -crisis.f2, -crisis.ex) %>% 
      select(year, country, crisis, everything()) 

    # ------------------------------------------------------------------------------
    # Normalize the data: this will be useful to compare the coefficients
    # ------------------------------------------------------------------------------
    
    # create a recipe to scale the variables
    recipe.normalize <- 
      recipe(x = df, formula = as.formula(crisis ~ .)) %>% 
      # remove some variables from the predictors %>% 
      update_role(year,             new_role = "time variable") %>% 
      update_role(country,          new_role = "id variable") %>% 
      # normalize all the predictors to be distributed ~ N(0,1) 
      step_normalize(all_predictors(), skip = FALSE) %>% 
      # delete rows with no variance
      step_zv(all_predictors(), skip = FALSE) 
    
    # create an empty data frame to stack the data
    df.normalized <- 
      df %>% 
      filter(country == "hogehoge")
    
    # save the countries
    countries <- 
      df %>% 
      select(country) %>% 
      unique() %>% 
      unlist()
    
    # apply the recipe to all countries: loop used because group_by is not allowed for recipes
    for (target.country in countries) {
      # create normalized data frame for a specific country
      df.ctry <- 
        df %>% 
        # keep only one specific country
        filter(country == target.country)
      
      # create a data frame with normalized variables
      df.normalized.tmp <- 
        recipe.normalize %>% 
        prep() %>% 
        bake(new_data = df.ctry)
      
      # stack the data into the global data frame
      df.normalized <- 
        bind_rows(df.normalized, df.normalized.tmp)  
    }
    
    # remove NA rows
    df.normalized <- 
      df.normalized %>% 
      na.omit()
    
    # return the output data
    return(df.normalized)
}
