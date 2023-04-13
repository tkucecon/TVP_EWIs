
# ------------------------------------------------------------------------------
# About this code
# Plot only significant coefficients and combine as a single plot
# ------------------------------------------------------------------------------

plot.dynamic <- 
  function(MCMC.name,
           graph.name){
  
  # load the MCMC data and extract necessary information
  load(paste("../5_tmp/", MCMC.name, sep = ""))
  
  # extracted data 
  extracted.stan <- 
    extract(out.stan[[2]])
  
  # correspondence data frame between time id and year
  df.time.id <- out.stan[[3]]
  
  # number of covariates
  p <- out.stan[[1]]$p
  
  # name of covariates: 
  varnames <- colnames(out.stan[[1]]$X)
    
  # clean up the global environment to avoid conflicts
  if (exists("g.all")) {
    rm(g.all)
  }
    
  # repeat for all the variables
  for (i in 2:p) {
    # check the name of the current variable
    varname <- varnames[i]
    
    # check the dynamics of the series
    df.b_i <- 
      extracted.stan$beta[, i, ] %>% 
      t() %>% 
      as_tibble() %>% 
      mutate(time.id = row_number()) %>% 
      gather(key = iter, value = value, -time.id) %>% 
      group_by(time.id) %>% 
      summarise(p05 = quantile(value, 0.05),
                p16 = quantile(value, 0.16),
                p50 = quantile(value, 0.5),
                p84 = quantile(value, 0.84),
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
      summarise(significance = max(significance, na.rm = TRUE)) %>% 
      unlist()
    
    # fill missing years with NA values
    df.b_i <- 
      df.b_i %>% 
      complete(year = full_seq(year, 1))
    
    # plot if the series contains significant periods
    # skip otherwise
    if (significance == 1) {
      # plot the current variable
      g.b_i <- 
        df.b_i %>% 
        ggplot() + 
        geom_rect(aes(xmin = 1913, xmax = 1919,
                      ymin = -Inf, ymax = Inf),
                  fill = "gray", alpha = 1) + 
        geom_rect(aes(xmin = 1932, xmax = 1946,
                      ymin = -Inf, ymax = Inf),
                  fill = "gray", alpha = 1) + 
        geom_ribbon(aes(x    = year,
                        ymin = p16,
                        ymax = p84),
                    fill = "royal blue", 
                    alpha = 0.5) + 
        geom_ribbon(aes(x    = year,
                        ymin = p05,
                        ymax = p95),
                    fill = "royal blue", 
                    alpha = 0.3) + 
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
  
  # arrange the layout
  g.all <- g.all + plot_layout(nrow = 2)
  
  # save under the 6_output folder
  ggsave(plot = g.all, 
         width = 10, height = 4, 
         filename = paste("../6_outputs/", graph.name, sep = ""))
  
  # return and show the plot
  return(g.all)
}
