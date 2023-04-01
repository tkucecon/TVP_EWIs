
# ------------------------------------------------------------------------------
# About this code
# Plot heat-map of the significance
# ------------------------------------------------------------------------------

plot.heat <- 
  function(significance){
    
  # clean up the global environment to avoid conflicts
  if (exists("df.plot")) {
    rm(df.plot)
  }
  
  # check the dimension of the coefficients
  p <- dim(extracted.stan$beta)[2]
    
  # repeat for all the variables
  for (i in 2:p) {
    
    # check the name of the current variable
    varname <- colnames(X)[i]
    
    # check the dynamics of the series
    df.b_i <- 
      extracted.stan$beta[, i, ] %>% 
      t() %>% 
      as_tibble() %>% 
      mutate(time.id = row_number()) %>% 
      gather(key = iter, value = value, -time.id) %>% 
      group_by(time.id) %>% 
      summarise(p05 = quantile(value, 0.05),
                p50 = quantile(value, 0.5),
                p95 = quantile(value, 0.95)) %>% 
      left_join(df.time.id, by = "time.id")
    
    # if input significance is true, check if there are some significant periods
    # otherwise return the median of posteriors
    if (significance) {
      df.b_coef <- 
        df.b_i %>% 
        mutate(significance = case_when(
          p05 > 0 ~ 1,
          p95 < 0 ~ -1,
          TRUE    ~ 0
        )) %>% 
        select(year, significance)
    } else {
      df.b_coef <- 
        df.b_i %>% 
        select(year, p50)
    }
    
    colnames(df.b_coef) <- c('year', varname)
    
    # combine with the output file
    if (exists("df.plot")) {
      df.plot <- 
        df.plot %>% 
        inner_join(df.b_coef, by = 'year')
    }else{
      df.plot <- df.b_coef
    }
  }
  
  # fill missing years with NA values
  df.plot.filled <- 
    df.plot %>% 
    complete(year = full_seq(year, 1))
  
  # create a heatmap plot
  g.heat <-
    df.plot.filled %>% 
    gather(key = "variables", value = "coefs", -year) %>% 
    ggplot() +
    geom_tile(aes(x = year, y = variables, fill = coefs)) + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
  
  # return the heat-map
  return(g.heat)
}
