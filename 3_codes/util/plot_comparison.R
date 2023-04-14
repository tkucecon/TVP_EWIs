
# ------------------------------------------------------------------------------
# About this code
# Plot only significant coefficients and combine as a single plot
# ------------------------------------------------------------------------------

plot.comparison <- 
  function(df.beta.all,
           current.var,
           legend){
  
    # keep only relevant variables
    df.b_i <- 
      df.beta.all %>% 
      filter(varname == current.var) %>% 
      complete(year = full_seq(year, 1))
    
    # define the prediction length
    pred.length <- 2020 - max(df.b_i$year)
    
    # create a base of prediction data frame
    df.b_i.pred <- 
      df.b_i %>% 
      filter(year == max(year)) 
    
    # create the prediction data frame
    for (t in 1:pred.length) {
      
      # create the next row
      df.b_i.pred.next <- 
        df.b_i.pred %>% 
        filter(year == max(year)) %>% 
        mutate(year = year + 1)
      
      # merge with the pred data
      df.b_i.pred <- 
        rbind(df.b_i.pred, df.b_i.pred.next)
    }
      
    # draw the graph
    g.b_i <-
      df.b_i %>% 
      rename(Bayesian = median) %>% 
      gather(key = "method", value = "value", Bayesian, logit, LASSO) %>% 
      ggplot() + 
      # plot the graph of 
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
      geom_line(aes(x = year, y = value, color = method)) + 
      scale_color_manual(values = c(Bayesian = "black", 
                                    logit  = "purple",
                                    LASSO  = "orange")) + 
      geom_hline(yintercept = 0, linetype = "dashed") + 
      # add prediction 
      geom_line(data = df.b_i.pred,
                mapping = aes(x = year, y = median),
                linetype = "dotdash",
                color = "black",
                linewidth = 1) +
      geom_line(data = df.b_i.pred,
                mapping = aes(x = year, y = logit),
                linetype = "dotdash",
                color = "purple",
                linewidth = 1) +
      geom_line(data = df.b_i.pred,
                mapping = aes(x = year, y = LASSO),
                linetype = "dotdash",
                color = "orange",
                linewidth = 1) +
      # label
      labs(x = "Year", 
           y = current.var) +
      # lenend position
      theme(legend.position = legend)
      
  # return and show the plot
  return(g.b_i)
}
