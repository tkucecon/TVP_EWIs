
# ------------------------------------------------------------------------------
# About this code
# Plot only significant coefficients and combine as a single plot
# ------------------------------------------------------------------------------

plot.dynamic <- 
  function(){
  
  # clean up the global environment to avoid conflicts
  if (exists("g.all")) {
    rm(g.all)
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
    
    # check if there are some significant periods
    significance <- 
      df.b_i %>% 
      mutate(significance = case_when(
        p05 > 0 ~ 1,
        p95 < 0 ~ 1,
        TRUE    ~ 0
      )) %>% 
      summarise(significance = max(significance)) %>% 
      unlist()
    
    if (significance == 1) {
      # plot the current variable
      g.b_i <- 
        df.b_i %>% 
        ggplot() + 
        geom_ribbon(aes(x    = year,
                        ymin = p05,
                        ymax = p95),
                    fill = "gray") + 
        geom_line(aes(x = year, y = p50)) + 
        geom_hline(yintercept = 0, linetype = "dashed") + 
        labs(x = "Year", 
             y = varname)
      
      # combine with the graph output file
      if (exists("g.all")) {
        g.all <- g.all + g.b_i
      }else{
        g.all <- g.b_i
      }
    }
  }
  
  return(g.all)
}
