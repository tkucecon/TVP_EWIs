
# ------------------------------------------------------------------------------
# About this code
# Plot heat-map of the significance
# ------------------------------------------------------------------------------

plot.heat <- 
  function(file.path, file.name){
  
  # load the output data
  load(paste("../5_tmp/", file.path, "/", file.name, ".rda", sep = ""))  
    
  # load beta from the MCMC result 
  df.beta <- 
    out.list[[1]]
  
  # clean up the global environment to avoid conflicts
  if (exists("df.plot")) {
    rm(df.plot)
  }
  
  # keep only medians of the data as a wide data frame
  df.plot <- 
    df.beta %>% 
    select(varname, year, median) %>% 
    spread(key = "varname", value = "median") %>% 
    select(-intercept)
  
  # fill missing years with NA values
  df.plot.filled <- 
    df.plot %>% 
    complete(year = full_seq(year, 1))
  
  # create a heat-map plot
  g.heat <-
    df.plot.filled %>% 
    gather(key = "variables", value = "coefs", -year) %>% 
    ggplot() +
    geom_tile(aes(x = year, y = variables, fill = coefs)) + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
  
  # save under the 6_output folder
  ggsave(plot = g.heat, 
         width = 10, height = 4, 
         filename = paste("../6_outputs/",file.path, "/", file.name, "_heat.pdf", sep = ""))
  
  # return the heat-map
  return(g.heat)
}
