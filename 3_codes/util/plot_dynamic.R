
# ------------------------------------------------------------------------------
# About this code
# Plot only significant coefficients and combine as a single plot
# ------------------------------------------------------------------------------

plot.dynamic <- 
  function(MCMC.name){
  
  # load the output data
  load(paste("../5_tmp/", MCMC.name, ".rda", sep = ""))  
  
  # load beta from the MCMC result 
  df.beta <- 
    out.list[[1]]
    
  # clean up the global environment to avoid conflicts
  if (exists("g.all")) {
    rm(g.all)
  }
    
  # obtain the variable name
  varnames <- 
    df.beta %>% 
    select(varname) %>% 
    unique() %>% 
    unlist()
  
  # number of variables
  p <- length(varnames)
  
  # repeat for all the variables
  for (i in 2:p) {
    
    # check the name of the current variable
    current.var <- varnames[i]
    
    # keep only the current variable
    df.b_i <- 
      df.beta %>% 
      filter(varname == current.var)
      
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
    
    # plot if the series contains significant periods and skip otherwise
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
        geom_line(aes(x = year, y = median)) + 
        geom_hline(yintercept = 0, linetype = "dashed") + 
        labs(x = "Year", 
             y = current.var)
      
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
         filename = paste("../6_outputs/", MCMC.name, "_ts.pdf", sep = ""))
  
  # return and show the plot
  return(g.all)
}
